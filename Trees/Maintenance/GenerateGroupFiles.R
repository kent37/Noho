source(here::here('Trees/Maintenance/AssignPeopleToTrees.R'))
source(here::here('Trees/Maintenance/MapHelpers.R'))
library(htmlwidgets)

gpkg_path = here::here('Trees/Maintenance/assignments/Assignments.gpkg')
out_dir   = here::here('Trees/Maintenance/maps_and_data')
dir.create(out_dir, showWarnings = FALSE)

dat            = load_polygon_assignments(gpkg_path)
polygons       = dat$polygons
trees_assigned = dat$trees_assigned

for (pid in sort(unique(polygons$person_id))) {
  message('Generating Group ', pid, ' ...')

  group_poly  = polygons |> filter(person_id == pid) |> st_buffer(10) |> st_transform(4326)
  group_trees = trees_assigned |> filter(person_id == pid)

  trees_recent = group_trees |>
    filter(Year >= 2023) |>
    mutate(label = make_tree_label(Addr, Year, name_label)) |>
    st_transform(4326)

  trees_mid = group_trees |>
    filter(Year >= 2020, Year <= 2022) |>
    mutate(label = make_tree_label(Addr, Year, name_label)) |>
    st_transform(4326)

  trees_early = group_trees |>
    filter(Year >= 2017, Year <= 2019) |>
    mutate(label = make_tree_label(Addr, Year, name_label)) |>
    st_transform(4326)

  recent = sum(group_trees |> st_drop_geometry() |> filter(Year >= 2023) |> pull(count))
  mid    = sum(group_trees |> st_drop_geometry() |> filter(Year >= 2020, Year <= 2022) |> pull(count))
  early  = sum(group_trees |> st_drop_geometry() |> filter(Year >= 2017, Year <= 2019) |> pull(count))

  bbox = if (nrow(group_trees) > 0) {
    st_bbox(c(st_geometry(group_poly), st_transform(st_geometry(group_trees), 4326)))
  } else {
    st_bbox(group_poly)
  }

  map = leaflet(width = '100%', height = '100vh') |>
    addProviderTiles('CartoDB.Positron',  group = 'Street') |>
    addProviderTiles('Esri.WorldImagery', group = 'Satellite') |>
    fitBounds(bbox[['xmin']], bbox[['ymin']], bbox[['xmax']], bbox[['ymax']]) |>
    # addPolygons(
    #   data        = group_poly,
    #   fillColor   = '#882E72',
    #   stroke      = FALSE,
    #   fillOpacity = 0.05
    # ) |>
    add_tree_layers(trees_recent, trees_mid, trees_early, size_add = 1) |>
    add_tree_legend() |>
    addControl(
      html     = make_group_summary_html(pid, recent, mid, early),
      position = 'topleft'
    ) |>
    addLayersControl(
      baseGroups    = c('Street', 'Satellite'),
      overlayGroups = c('2023–2025', '2020–2022', '2017–2019'),
      options       = layersControlOptions(collapsed = FALSE)
    )

  withr::with_dir(out_dir, {
    saveWidget(map,
               title = glue::glue('Group {pid}'),
               file  = glue::glue('Group_{pid}_map.html'),
               selfcontained = TRUE)
  })

  group_trees |>
    st_drop_geometry() |>
    mutate(
      Year    = as.integer(Year),
      Details = str_replace_all(name_label, '<br>', ', ')
    ) |>
    select(Address = Addr, Year, Count = count, Location, Details) |>
    arrange(Address, Year) |>
    write_excel_csv(file.path(out_dir, glue::glue('Group_{pid}_data.csv')))
}

message('Done. Files written to ', out_dir)
