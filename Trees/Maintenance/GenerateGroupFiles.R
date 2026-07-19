source(here::here('Trees/Maintenance/AssignPeopleToTrees.R'))
source(here::here('Trees/Maintenance/MapHelpers.R'))
library(htmlwidgets)
library(openxlsx2)

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

  recent = sum(trees_recent$count)
  mid    = sum(trees_mid$count)
  early  = sum(trees_early$count)

  bbox = if (nrow(group_trees) > 0) {
    st_bbox(c(st_geometry(group_poly), st_transform(st_geometry(group_trees), 4326)))
  } else {
    st_bbox(group_poly)
  }

  map = leaflet(width = '100%', height = '100vh') |>
    addProviderTiles('CartoDB.Positron',  group = 'Positron') |>
    addProviderTiles('CartoDB.Voyager', group = 'Voyager') |>
    addProviderTiles('OpenStreetMap.Mapnik', group = 'OSM') |>
    addProviderTiles('Stadia.StamenTonerLite', group='TonerLite') |> 
    fitBounds(bbox[['xmin']], bbox[['ymin']], bbox[['xmax']], bbox[['ymax']]) |>
    add_tree_layers(trees_recent, trees_mid, trees_early, size_add = 2) |>
    add_tree_legend() |>
    addControl(
      html     = make_group_summary_html(pid, recent, mid, early),
      position = 'topleft'
    ) |>
    addLayersControl(
      baseGroups    = c('Positron', 'Voyager', 'OSM', 'TonerLite'),
      overlayGroups = c('2023–2025', '2020–2022', '2017–2019'),
      options       = layersControlOptions(collapsed = FALSE)
    ) |>
    add_mobile_sizing()

  html_file = file.path(out_dir, glue::glue('Group_{pid}_map.html'))

  withr::with_dir(out_dir, {
    saveWidget(map,
               title = glue::glue('Group {pid}'),
               file  = glue::glue('Group_{pid}_map.html'),
               selfcontained = TRUE)
  })

  # htmlwidgets::saveWidget omits a viewport meta tag, so mobile browsers use a
  # ~980px virtual viewport and fitBounds calculates the wrong zoom. Injecting
  # it here (into <head> before page load) fixes that; JS injection via onRender
  # is too late for the browser to act on it.
  html = readLines(html_file, warn = FALSE)
  if (!any(grepl('name="viewport"', html, fixed = TRUE))) {
    head_idx = which(grepl('<head>', html, fixed = TRUE))[1]
    html = append(html,
      '<meta name="viewport" content="width=device-width, initial-scale=1">',
      after = head_idx)
    writeLines(html, html_file)
  }

  df = group_trees |>
    st_drop_geometry() |>
    mutate(
      Year    = as.integer(Year),
      Period  = case_when(
        Year >= 2023 ~ '2023–2025',
        Year >= 2020 ~ '2020–2022',
        Year >= 2017 ~ '2017–2019'
      ),
      Details = str_replace_all(name_label, '<br>', ', '),
      Notes = ""
    ) |>
    arrange(desc(Period), Street, Num) |>
    select(Num, Street, Year, Period, Count = count, Location, Details, Notes)

  n_rows      = nrow(df)
  details_col = which(names(df) == 'Details')

  wb = wb_workbook() |>
    wb_add_worksheet('Trees') |>
    wb_add_data_table(x = df, table_style = 'none') |>
    wb_add_font(
      dims = wb_dims(rows = 1:(n_rows + 1), cols = 1:ncol(df)),
      name = 'Arial',
      size = 14
    ) |>
    wb_set_col_widths(
      cols   = seq_len(ncol(df)),
      widths = c(6, 20, 7, 12, 7, 12, 44, 25)
    ) |>
    wb_page_setup(orientation = 'landscape', fit_to_width = 1) |>
    wb_set_grid_lines(show = TRUE, print = TRUE)

  if (n_rows > 0) {
    wb = wb |> wb_add_cell_style(
      dims      = wb_dims(rows = 2:(n_rows + 1), cols = details_col),
      wrap_text = TRUE
    )
  }

  wb_save(wb, file.path(out_dir, glue::glue('Group_{pid}_data.xlsx')))
}

message('Done. Files written to ', out_dir)
