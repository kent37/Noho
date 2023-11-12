# Helpers for working with NLCD data

library(tidyverse)
library(leaflet)
library(plotly)
library(sf)
library(terra)

# Get a reference CRS
nlcd_crs = local({
  ref_path = here::here("data/NLCD_Tree_and_Land/NLCD_2019_Land_Cover_L48_20210604_cE3J3qNGK7bbzFDvKwex.tiff")
  crs(rast(ref_path))
})

# Northampton outline
noho = st_read(
  here::here('Shapefiles/Noho_outline/Noho_outline.gpkg'),
  quiet=TRUE) |> 
  st_transform(nlcd_crs)

# Read a raster layer and clip to a boundary
read_layer = function(path, mask_layer=noho) {
  rast = rast(path)
  
  # Cropping to extent is fast, do that first
  cropped = crop(rast, vect(mask_layer))
  clipped = mask(cropped, vect(mask_layer))
  clipped
}

# Clip all layers to a mask and summarize yearly class counts
clip_and_count_all = function(mask) {
  lc_counts = 
    map_dfr(lc_layers, 
        ~clip_and_count_layer(.x, mask),
        .id='path', .progress='Clipping')

  lc_counts %>% 
    mutate(Class = class_lookup[as.character(value)],
           Year = as.integer(str_extract(path, '\\d{4}'))) %>% 
    select(-path, -value) %>% 
    group_by(Year, Class) %>% 
    summarize(n=sum(n), .groups='drop_last') %>% 
    mutate(Fraction=n/sum(n),
           Acres=n * acre_per_raster / res_bump^2) %>% 
    ungroup()
}

# Clip a single layer to a mask and report the category counts
# Very fast version using exactextractr::exact_extract
clip_and_count_layer = function(rast, mask) {
  exactextractr::exact_extract(rast, mask, function(df) {
    # df has columns `value` and `coverage_fraction` 
    # with one row per raster cell
    df %>%
      group_by(value) %>%
      summarize(n = sum(coverage_fraction))
  }, summarize_df = TRUE, progress = FALSE)
}

# Slower version using mask
clip_and_count_layer_slow = function(rast, mask) {
  clipped = mask(rast, vect(mask))
  counts = table(values(clipped), mat=FALSE) %>% 
    as_tibble(.name_repair='universal') %>% 
    rename(value=`...1`)
  counts
}

# Clip all layers to a mask and report the mean value
clip_and_mean_all = function(mask) {
  map_dfr(lc_layers, 
          ~clip_and_mean_layer(.x, mask)) |> 
    mutate(Year = as.integer(
      str_extract(names(lc_layers), '\\d{4}')))
}

# Clip a single layer to a mask and report the 
# number of acres in the mask and the mean value
# as a fractional percent
clip_and_mean_layer = function(rast, mask) {
  clipped = mask(rast, vect(mask))
  tibble(
    Coverage=mean(values(clipped), na.rm=TRUE)/100,
    Area = sum(!is.na(values(clipped)))
      * acre_per_raster / res_bump^2
  )
}

# Make a tibble with land cover values, names and colors
make_land_cover_legend = function() {
  # Classes in the order we want to report them
  lc_classes = c("Developed",  "Forest", "Cultivated", "Water/Wetland",
                 "Herbaceous/Shrub", "Barren")
  # The full list is in a CSV file
  lc_names = read_csv(
    here::here('data/NLCD_Tree_and_Land/NLCD_landcover_legend.csv'), 
    skip=1,
    show_col_types=FALSE) %>% 
    filter(!is.na(Legend)) %>% 
    mutate(Class=factor(Class, levels=lc_classes))
  
  # Colors are in a space-separated file
  colors = read_delim(
      here::here('data/NLCD_Tree_and_Land/Land_cover_values.clr'),
      delim=' ', col_names=FALSE, 
      col_select=1:5, show_col_types=FALSE) %>% 
    set_names(c('Value', 'R', 'G', 'B', 'Alpha')) %>% 
    mutate(Color=rgb(R, G, B, Alpha, maxColorValue=255)) %>% 
    select(Value, Color)
  
  lc_names %>% left_join(colors, by='Value')
}

legend = make_land_cover_legend()
name_lookup = deframe(legend %>% select(Value, Legend))
class_lookup = deframe(legend %>% select(Value, Class))
color_map = deframe(legend %>% select(Legend, Color))
class_color_map = read_csv(show_col_types=FALSE,
  here::here('Trees/Class_colors.csv')) %>% 
  deframe()

raster_count = 102881 # Number of 30 m^2 raster elements in NoHo
acre_per_sq_meter = 0.000247105
acre_per_raster = 900 * acre_per_sq_meter
noho_acres = raster_count * acre_per_raster

zoning_categories = read_csv(
  here::here('Trees/Zoning_categories.csv'),
  show_col_types=FALSE
) |> 
  left_join(read_csv(here::here('Shapefiles/Zone_names.csv'),
                      col_select=1:2, col_names=c('Name', 'Zone'), 
                      show_col_types=FALSE),
            by='Zone')

all_zoning = st_read(
    here::here('Shapefiles/zoning_20220429/zoning_districts_20220429.shp'),
    quiet=TRUE
  ) %>% st_transform(nlcd_crs) %>% 
    select(NAME)

aggregated_zoning = function() {
  all_zoning %>% 
    mutate(Class=deframe(zoning_categories |> select(Zone, Category))[NAME]) %>% 
    group_by(Class) %>% 
    summarize()
}

# Make a map showing forest loss within a specific zoning region
map_forest_loss = function(lost_forest_poly, region=NULL) {
  lost = lost_forest_poly %>% st_transform(4326)
  overlay_groups = 'Forest loss'

  # Generate colors and figure out what should be in the legend
  lost$Color = unname(class_color_map[as.character(lost$Class)])
  legend_labels = sort(unique(as.character(lost$Class)))
  legend_colors = class_color_map[legend_labels]
  
  if (!is.null(region)) {
    # Show loss only within region and convert to EPSG:4326
    region_name = paste(region, ' zone')
    region = zoning %>% filter(Class==region) %>% st_transform(4326)
    lost = st_intersection( lost, region)
    overlay_groups = c(region_name, overlay_groups)
  }
  
  map = leaflet(width='95%', height='600px') %>% 
    setView(-72.667, 42.330, 12) %>% 
    addProviderTiles('CartoDB.Positron', group='Basic map') %>% 
    addTiles(group='OpenStreetMap') %>% 
    addProviderTiles('Esri.WorldImagery', group='Satellite')
  
  if (!is.null(region))
      map = map %>% 
        addPolygons(data=region, stroke=FALSE, group=region_name,
                fillColor='lightgray', fillOpacity=0.5)
      
  map %>%
    addPolygons(data=lost, stroke=FALSE, group='Forest loss',
                fillColor=~Color,
                fillOpacity=0.8,
                label=~Class) %>% 
    addPolygons(data=noho %>% st_transform(4326),
                weight=1, fill=FALSE, color='grey') %>% 
    addLayersControl(
      baseGroups=c('Basic map', 'OpenStreetMap', 'Satellite'),
      overlayGroups=overlay_groups,
      options=layersControlOptions(collapsed=FALSE)) %>% 
    addLegend(colors=legend_colors, labels=legend_labels,
              title='2019 land use')
}

# Format as percent with no decimal
int_pct = function(val) scales::percent(val, accuracy=1)
