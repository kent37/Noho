# Compare tree canopy cover between any two years of the NLCD Tree Canopy
# Cover 1985-2023 series. Reads from canopy_data.gpkg, a pre-clipped extract
# built by Trees/Canopy/ExtractCanopy.R -- small enough to deploy on
# shinyapps.io's free tier, unlike the ~135 GB of source CONUS data.
#
# Usage:
#   shiny::runApp(here::here('Trees/Canopy/Canopy_compare'))

library(shiny)
library(bslib)
library(leaflet)
library(terra)
library(sf)
library(tidyverse)

data_path = 'canopy_data.gpkg'

noho  = read_sf(data_path, layer = 'noho_outline')
wards = read_sf(data_path, layer = 'wards_precincts')

# --- Layer discovery ---------------------------------------------------

discover_canopy_layers = function() {
  tables = terra::describe(data_path, sds = TRUE)$var
  tibble(year = str_extract(tables, '\\d{4}'),
         path = paste0('GPKG:', data_path, ':', tables))
}

canopy_layers = discover_canopy_layers() |> arrange(year)

default_from_path = canopy_layers$path[canopy_layers$year == '2000']
default_to_path   = canopy_layers$path[canopy_layers$year == '2023']

layer_choices = set_names(canopy_layers$path, canopy_layers$year)

layer_label = function(path) {
  canopy_layers$year[canopy_layers$path == path]
}

# --- Data reading / diff computation ------------------------------------

# 254 marks a non-processing area in the NLCD TCC data; treat as 0 canopy,
# matching the convention used in the Tree_canopy_change reports.
read_tcc_layer = function(path) {
  layer = rast(path)
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
    # Wards need their own pane above canopy_pane, otherwise they'd sit in
    # the default overlayPane (zIndex 400) and the change raster would
    # paint over their outlines.
    addMapPane('wards_pane', zIndex = 460) |>
    setView(-72.667, 42.330, 12) |>
    addProviderTiles('CartoDB.Positron', group = 'Basic map') |>
    addTiles(group = 'OpenStreetMap') |>
    addProviderTiles('Esri.WorldImagery', group = 'Satellite') |>
    addPolygons(data = wards |> st_transform(4326),
                weight = 1, color = 'steelblue',
                fill = FALSE, group = 'Wards & Precincts',
                label = ~WardPrec,
                labelOptions = labelOptions(direction = 'center', permanent = TRUE, textOnly = TRUE),
                options = pathOptions(pane = 'wards_pane')) |>
    addPolygons(data = noho |> st_transform(4326),
                weight = 1, fill = FALSE, color = 'grey') |>
    addLayersControl(
      baseGroups = c('Basic map', 'OpenStreetMap', 'Satellite'),
      overlayGroups = c('Change', 'Wards & Precincts'),
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

  compute_comparison = function(from_path, to_path) {
    withProgress(message = 'Reading and comparing layers...', {
      list(
        diff  = compute_canopy_diff(from_path, to_path),
        label = paste(layer_label(from_path), '→', layer_label(to_path))
      )
    })
  }

  # Seeded with the default From/To comparison so the map shows a result
  # as soon as the app loads, rather than waiting for a first click.
  comparison = reactiveVal(compute_comparison(default_from_path, default_to_path))

  observeEvent(input$compare_btn, {
    req(input$from_path, input$to_path)
    comparison(compute_comparison(input$from_path, input$to_path))
  })

  output$map_title = renderText({
    comparison()$label
  })

  output$map = renderLeaflet({
    result = comparison()
    base_map() |>
      addRasterImage(result$diff, colors = tc_colors, group = 'Change',
                      options = gridOptions(pane = 'canopy_pane'))
  })
}

shinyApp(ui, server)
