# Compare tree canopy cover between any two NLCD Tree Canopy Cover layers,
# drawn from either the 2011-2021 series (v2021-4 tif files) or the
# 1985-2023 series (v2023-5 WGS84 zip archives).
#
# Usage:
#   shiny::runApp(here::here('Trees/Canopy/Canopy_compare'))

library(shiny)
library(bslib)
library(leaflet)
library(terra)
library(sf)
library(tidyverse)

source(here::here('Trees/Canopy/NLCD_helpers.R'))

# --- Layer discovery ---------------------------------------------------

old_series_layers = function() {
  files = list.files(
    here::here('data/nlcd_tcc_CONUS_all'),
    pattern = '^nlcd_tcc_conus_\\d{4}_v2021-4\\.tif$',
    full.names = TRUE, recursive = TRUE)
  tibble(series = '2011-2021 (v2021-4)',
         version = 'v4',
         year = str_extract(basename(files), '\\d{4}'),
         path = files)
}

new_series_layers = function() {
  files = list.files(
    here::here('data/nlcd_tcc_CONUS_1985_2023_v2023-5_wgs84'),
    pattern = '^nlcd_tcc_CONUS_\\d{4}_v2023-5_wgs84\\.zip$',
    full.names = TRUE)
  tibble(series = '1985-2023 (v2023-5, WGS84)',
         version = 'v5',
         year = str_extract(basename(files), '\\d{4}'),
         path = files)
}

canopy_layers = bind_rows(old_series_layers(), new_series_layers()) |>
  arrange(series, year)

default_from_path = canopy_layers$path[canopy_layers$version == 'v5' & canopy_layers$year == '1985']
default_to_path   = canopy_layers$path[canopy_layers$version == 'v5' & canopy_layers$year == '2023']

# Named list of named vectors -> selectInput renders these as <optgroup>s,
# so From/To each list both series with their years underneath. The (v4)/
# (v5) suffix on each entry makes same-year picks from different series
# easy to tell apart.
layer_choices = split(canopy_layers, canopy_layers$series) |>
  map(~ set_names(.x$path, paste0(.x$year, ' (', .x$version, ')')))

layer_label = function(path) {
  row = canopy_layers[canopy_layers$path == path, ]
  paste0(row$year, ' (', row$version, ')')
}

# --- Data reading / diff computation ------------------------------------

# 254 marks a non-processing area in the NLCD TCC data; treat as 0 canopy,
# matching the convention used in the Tree_canopy_change reports.
read_tcc_layer = function(path) {
  layer = read_layer(path)
  layer[layer == 254] = 0
  layer
}

# Reproject `from_layer` onto `to_layer`'s exact grid -- terra requires
# matching geometries for raster algebra, and the two layers may come from
# different series with different native CRSs -- then return to - from in
# EPSG:4326 for display on a leaflet map. Cells with no change are set to
# NA so they don't obscure the base map.
compute_canopy_diff = function(from_path, to_path) {
  from_layer = read_tcc_layer(from_path)
  to_layer   = read_tcc_layer(to_path)

  to_4326   = project(to_layer, 'epsg:4326')
  from_4326 = project(from_layer, to_4326)

  diff = to_4326 - from_4326
  diff[diff == 0] = NA
  diff
}

# --- Map palette and base layers, matching the canopy change map in the
# Tree_canopy_change_*.qmd reports ---------------------------------------

tc_colors     = colorNumeric('PiYG', c(-100, 100), na.color = NA)
tc_colors_rev = colorNumeric('PiYG', c(-100, 100), na.color = NA, reverse = TRUE)

base_map = function() {
  leaflet(width = '100%', height = '700px') |>
    # addRasterImage() renders through a GridLayer, which defaults to the
    # same 'tilePane' as the base layers below. The first time a base
    # layer other than the initial one is selected, Leaflet instantiates
    # it and appends it as the pane's last child, painting over the
    # raster (same z-index, later in DOM order wins). Giving the raster
    # its own pane with a higher z-index keeps it on top regardless of
    # which base layer is active or when it was first selected.
    addMapPane('canopy_pane', zIndex = 450) |>
    setView(-72.667, 42.330, 12) |>
    addProviderTiles('CartoDB.Positron', group = 'Basic map') |>
    addTiles(group = 'OpenStreetMap') |>
    addProviderTiles('Esri.WorldImagery', group = 'Satellite') |>
    addPolygons(data = noho |> st_transform(4326),
                weight = 1, fill = FALSE, color = 'grey') |>
    addLayersControl(
      baseGroups = c('Basic map', 'OpenStreetMap', 'Satellite'),
      overlayGroups = 'Change',
      options = layersControlOptions(collapsed = FALSE)) |>
    addLegend(pal = tc_colors_rev, values = seq(-100, 100, 20),
              labFormat = labelFormat(
                transform = function(x) sort(x, decreasing = TRUE)),
              title = 'Canopy change %')
}

# --- UI ------------------------------------------------------------------

ui = page_sidebar(
  title = 'Tree canopy layer comparison',
  theme = bs_theme(version = 5),
  sidebar = sidebar(
    width = 220,
    selectInput('from_path', 'From', choices = layer_choices,
                selected = default_from_path),
    selectInput('to_path', 'To', choices = layer_choices,
                selected = default_to_path),
    actionButton('compare_btn', 'Compare', class = 'btn-primary w-100'),
    p(class = 'text-muted small mt-2', textOutput('map_title'))
  ),
  card(
    full_screen = TRUE,
    leafletOutput('map', height = '700px')
  )
)

# --- Server ----------------------------------------------------------------

server = function(input, output, session) {

  # ignoreNULL/ignoreInit default to firing only on an actual button click,
  # not on app load -- input$from_path/to_path aren't guaranteed to have
  # synced from the client yet at load time, so triggering immediately can
  # race ahead of them.
  comparison = eventReactive(input$compare_btn, {
    req(input$from_path, input$to_path)
    withProgress(message = 'Reading and comparing layers...', {
      list(
        diff  = compute_canopy_diff(input$from_path, input$to_path),
        label = paste(layer_label(input$from_path), '→', layer_label(input$to_path))
      )
    })
  })

  # Before the first click, show the base map/legend with no Change layer
  # and a placeholder title, rather than an error.
  safe_comparison = function() tryCatch(comparison(), error = function(e) NULL)

  output$map_title = renderText({
    result = safe_comparison()
    if (is.null(result)) 'Select two years and click Compare' else result$label
  })

  output$map = renderLeaflet({
    result = safe_comparison()
    map = base_map()
    if (!is.null(result)) {
      map = map |> addRasterImage(result$diff, colors = tc_colors, group = 'Change',
                                   options = gridOptions(pane = 'canopy_pane'))
    }
    map
  })
}

shinyApp(ui, server)
