# 8 View Ave helpers

library(tidyverse)
library(exactextractr)
library(sf)
source(here::here('Trees/NLCD_helpers.R'))

gpkg_path = here::here('8 View Ave/8 View Ave.gpkg')

st_layers(gpkg_path)

# Get a bbox to clip temperature data
# Give a little extra
bbox = noho |> 
  st_buffer(100) |> 
  st_transform(4326) |> 
  st_bbox()

#      xmin      ymin      xmax      ymax 
# -72.74243  42.28324 -72.58300  42.37614 

project_limit = 
  read_sf(here::here('8 View Ave/8 View Ave.gpkg'), 'Project limit') |> 
  st_transform(nlcd_crs)

trees_21_path = here::here('data/nlcd_tcc_CONUS_all/nlcd_tcc_CONUS_2021_v2021-4/nlcd_tcc_conus_2021_v2021-4.tif')
trees_21 = read_layer(trees_21_path)
  
# Some layers have 254 values for NA. Treat them as 0
trees_21[trees_21==254] = 0
res_bump=1 # No need for this when using exact_extract()
#trees_21=terra::disagg(trees_21, res_bump)

clip_and_mean_layer(trees_21, project_limit) |> 
  mutate(Coverage_Area=Coverage * Area)

#       Area  Coverage Coverage_Area
# 1 1.683795 0.7223869      1.216351

clipped = mask(trees_21, vect(project_limit))
plot(crop(clipped, project_limit))
plot(project_limit, add=T)

eversource = read_sf(here::here('8 View Ave/8 View Ave.gpkg'), 
                     'Eversource trees') |> 
  st_transform(nlcd_crs)

clip_and_mean_layer(trees_21, eversource) |> 
  mutate(Coverage_Area=Coverage * Area)

#        Area  Coverage Coverage_Area
# 1 0.6234037 0.3169654     0.1975974

clipped = mask(trees_21, vect(eversource))
plot(crop(clipped, eversource))
plot(eversource, add=T)

# Convert raster values to degrees F
to_F = function(v) {
  v/255*(130-70)+70
}

# Average temperature across wards
temps = terra::rast(here::here(
  '8 View Ave/Noho_temperature_7_2_2024_greyscale.tif'
))

wards = read_sf(here::here(
  'Shapefiles/Wards_Precincts_2020_Northampton_20211022/Wards_Precincts_2020_Northampton.shp')) |> 
  st_transform(crs(temps))

exact_extract(temps, wards, fun='mean', append_cols='WardPrec') |> 
  mutate(mean=to_F(mean)) |> 
  arrange(mean)

