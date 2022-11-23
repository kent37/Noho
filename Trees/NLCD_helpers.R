# Helpers for working with NLCD data

library(raster) # Before tidyverse to prefer dplyr::select
library(tidyverse)
library(plotly)
library(sf)
library(sp)

# Northampton outline
noho = st_read(
  here::here('Shapefiles/Noho_outline/Noho_outline.gpkg'),
  quiet=TRUE)

# Get a reference CRS and transform noho to match
# Convert to Spatial for compatibility with raster::mask
noho_sp = local({
  ref_path = here::here("data/NLCD_Tree_and_Land/NLCD_2019_Land_Cover_L48_20210604_cE3J3qNGK7bbzFDvKwex.tiff")
  ref_crs = crs(raster(ref_path))
  as_Spatial(st_transform(noho, ref_crs))
})

# Read a raster layer and return a tibble with counts of each value
count_layer = function(path) {
  clipped = read_layer(path)
  counts = table(getValues(clipped)) %>% 
    as_tibble(.name_repair='universal') %>% 
    rename(value=`...1`)
  counts
}
  
# Read a raster layer and clip to NoHo boundary
read_layer = function(path) {
  rast = raster::raster(path)
  clipped = raster::mask(rast, noho_sp)
  clipped
}

# Make a tibble with land cover values, names and colors
make_land_cover_legend = function() {
  # The names are in a CSV file
  lc_names = read_csv(
    here::here('data/NLCD_Tree_and_Land/NLCD_landcover_legend.csv'),     skip=1,
    show_col_types=FALSE) %>% 
    filter(!is.na(Legend))
  
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
