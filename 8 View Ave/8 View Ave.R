# 8 View Ave helpers

library(tidyverse)
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
res_bump=8
trees_21=terra::disagg(trees_21, res_bump)

clip_and_mean_layer(trees_21, project_limit) |> 
  mutate(Coverage_Area=Coverage * Area)

#   Coverage  Area Coverage_Area
# 1    0.711  2.03          1.44

clipped = mask(trees_21, vect(project_limit))
plot(crop(clipped, project_limit))
plot(project_limit, add=T)

eversource = read_sf(here::here('8 View Ave/8 View Ave.gpkg'), 
                     'Eversource trees') |> 
  st_transform(nlcd_crs)

clip_and_mean_layer(trees_21, eversource) |> 
  mutate(Coverage_Area=Coverage * Area)

#   Coverage  Area Coverage_Area
# 1    0.285 0.754         0.215

clipped = mask(trees_21, vect(eversource))
plot(crop(clipped, eversource))
plot(eversource, add=T)

