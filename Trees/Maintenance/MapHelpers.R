# Shared helpers for tree maintenance map documents.
# Assumes AssignPeopleToTrees.R has been sourced (provides `people`, `all_trees`).
library(leaflet)

# Load polygon assignments from a GeoPackage.
# Returns a named list: polygons, trees_all, trees_assigned.
load_polygon_assignments = function(gpkg_path) {
  polygons = st_read(gpkg_path, layer = 'polygon_areas_edited-expanded', quiet = TRUE) |>
    left_join(
      st_drop_geometry(people) |> select(person_id, color, Addr),
      join_by(person_id)
    )

  trees_all = all_trees |>
    filter(Year >= 2017, Year <= 2025) |>
    mutate(
      Street = str_to_title(Street),
      Addr   = make_address(Num, Street)
    )

  trees_assigned = st_join(
    trees_all,
    polygons |> select(person_id),
    join = st_within
  )

  list(polygons = polygons, trees_all = trees_all, trees_assigned = trees_assigned)
}

# Build a hover label for a tree point.
make_tree_label = function(addr, year, name_label) {
  glue::glue('{addr} ({year})<br>{name_label}') |>
    as.character() |>
    lapply(htmltools::HTML) |>
    unname()
}

# Add three circle-marker layers (by year range) to a leaflet map.
# Each layer is skipped when its dataset is empty: passing list() to leaflet's
# safeLabel causes sum(sapply(list(), ...)) to fail with a type error.
add_tree_layers = function(map, trees_recent, trees_mid, trees_early, size_add = 0) {
  if (nrow(trees_recent) > 0) {
    map = map |> addCircleMarkers(
      data        = trees_recent,
      radius      = 3 + size_add,
      label       = ~label,
      fillColor   = 'black',
      fillOpacity = 1,
      stroke      = FALSE,
      group       = '2023–2025'
    )
  }
  if (nrow(trees_mid) > 0) {
    map = map |> addCircleMarkers(
      data        = trees_mid,
      radius      = 2.6 + size_add,
      label       = ~label,
      fillColor   = '#1f78b4',
      fillOpacity = 1,
      stroke      = FALSE,
      group       = '2020–2022'
    )
  }
  if (nrow(trees_early) > 0) {
    map = map |> addCircleMarkers(
      data        = trees_early,
      radius      = 1.6  + size_add,
      label       = ~label,
      fillColor   = '#fa3f46',
      fillOpacity = 1,
      stroke      = FALSE,
      group       = '2017–2019'
    )
  }
  map
}

# Add a legend for the three tree year-range symbols.
add_tree_legend = function(map) {
  map |>
    addLegend(
      position = 'bottomright',
      colors   = c('black', '#1f78b4', '#fa3f46'),
      labels   = c('2023–2025', '2020–2022', '2017–2019'),
      title    = 'Year planted',
      opacity  = 1
    )
}

# Join tree count summaries onto a polygon sf object and add hover labels.
# Returns the sf with additional columns: recent, mid, early, label.
make_polygon_labels = function(polygons_4326, trees_assigned_summary) {
  polygons_4326 |>
    left_join(trees_assigned_summary, join_by(person_id)) |>
    replace_na(list(recent = 0, mid = 0, early = 0)) |>
    mutate(
      label = glue::glue(
        '<b>Group {person_id}</b> ({recent + mid + early} trees)<br>',
        '2023–2025: <b>{recent}</b><br>',
        '2020–2022: <b>{mid}</b><br>',
        '2017–2019: <b>{early}</b>'
      ) |> lapply(htmltools::HTML) |> unname()
    )
}

# A 1×1 transparent GIF icon — use with addMarkers to place permanent text
# labels at points without a visible marker glyph.
transparent_icon = makeIcon(
  iconUrl    = 'data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7',
  iconWidth  = 1,
  iconHeight = 1
)

# Build a styled HTML summary panel for the top of a group map.
# Injects CSS to make the top-left leaflet controls flow as a row so the panel
# sits to the right of the zoom buttons rather than below them.
make_group_summary_html = function(person_id, recent, mid, early) {
  total = recent + mid + early
  glue::glue(
    '<style>',
    'html,body{{margin:0;padding:0}}',
    '.leaflet-top.leaflet-left{{display:flex;flex-direction:row;align-items:flex-start;gap:6px}}',
    '</style>',
    '<div style="background:white;padding:8px 12px;border-radius:4px;',
    'border:1px solid #ccc;font-size:13px;line-height:1.6">',
    '<b>Group {person_id}</b> ({total} trees)<br>',
    '2023–2025: <b>{recent}</b><br>',
    '2020–2022: <b>{mid}</b><br>',
    '2017–2019: <b>{early}</b>',
    '</div>'
  )
}
