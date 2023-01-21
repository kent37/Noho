# Helpers for working with NLCD data

library(tidyverse)
library(leaflet)
library(plotly)
library(sf)
library(sp)

# Northampton outline
noho = st_read(
  here::here('Shapefiles/Noho_outline/Noho_outline.gpkg'),
  quiet=TRUE)

# Get a reference CRS
nlcd_crs = local({
  ref_path = here::here("data/NLCD_Tree_and_Land/NLCD_2019_Land_Cover_L48_20210604_cE3J3qNGK7bbzFDvKwex.tiff")
  raster::crs(raster::raster(ref_path))
})

# Transform noho to match NLCD
# Convert to Spatial for compatibility with raster::mask
noho_sp = as_Spatial(st_transform(noho, nlcd_crs))

# Read a raster layer and return a tibble with counts of each value
count_layer = function(path, mask_layer=noho_sp) {
  clipped = read_layer(path, mask_layer)
  counts = table(raster::getValues(clipped)) %>% 
    as_tibble(.name_repair='universal') %>% 
    rename(value=`...1`)
  counts
}
  
# Read a raster layer and clip to NoHo boundary
read_layer = function(path, mask_layer=noho_sp) {
  rast = raster::raster(path)
  clipped = raster::mask(rast, mask_layer)
  clipped
}

# Make a tibble with land cover values, names and colors
make_land_cover_legend = function() {
  # Classes in the order we want to report them
  lc_classes = c("Developed",  "Forest", "Cultivated", "Water/Wetland",
                 "Herbaceous/Shrub", "Other")
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
)
aggregated_zoning = function() {
  zoning = st_read(
    here::here('Shapefiles/zoning_20220429/zoning_districts_20220429.shp'),
    quiet=TRUE
  ) %>% st_transform(nlcd_crs) %>% 
    select(NAME) %>% 
    mutate(Class=deframe(zoning_categories)[NAME]) %>% 
    group_by(Class) %>% 
    summarize()
}

# Make a map showing forest loss within a specific zoning region
map_forest_loss = function(lost_forest_poly, region=NULL) {
  lost = lost_forest_poly %>% st_transform(4326)
  overlay_groups = 'Forest loss'

  if (!is.null(region)) {
    # Show loss only within region and convert to EPSG:4326
    region_name = paste(region, ' zone')
    region = zoning %>% filter(Class==region) %>% st_transform(4326)
    lost = st_intersection( lost, region)
    overlay_groups = c(region_name, overlay_groups)
  }
  
  map = leaflet(width='95%', height='600px') %>% 
    setView(-72.667, 42.330, 12) %>% 
    addProviderTiles('CartoDB.Positron')
  
  if (!is.null(region))
      map = map %>% 
        addPolygons(data=region, stroke=FALSE, group=region_name,
                fillColor='lightgray', fillOpacity=0.5)
      
  map %>%
    addPolygons(data=lost, stroke=FALSE, group='Forest loss',
                fillColor='red', fillOpacity=0.8) %>% 
    addPolygons(data=noho %>% st_transform(4326),
                weight=1, fill=FALSE, color='grey') %>% 
    addLayersControl(overlayGroups=overlay_groups,
                     options=layersControlOptions(collapsed=FALSE))
}