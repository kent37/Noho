# Find 300-meter walking distance from parks and open space
library(tidyverse)
library(sf)
library(openrouteservice)

# Access points to open space, created by hand in QGIS (Open space isochrones.qgz)
access_pts = read_sf(here::here('300 meters/300_meters.gpkg'),
                     layer='Access_points') |>
  st_transform(4326)

access_pts = access_pts[!st_is_empty(access_pts),]

access_coords = st_coordinates(access_pts) |> as_tibble()

# We can only process five points at at time...
# Might as well just do one at a time
# This is slow, we are rate-limited to 20/min (and 500/day) at OpenRouteService
isochrones = pmap(access_coords,
                 function(X, Y) {
                   ors_isochrones(c(X, Y), 
                         range=300, range_type='distance',
                         profile=ors_profile('walking'), 
                         output='sf')
                   })


buffer = do.call(c, map(isochrones, st_geometry))
buffer = st_union(st_make_valid(buffer))
buffer = st_as_sf(buffer, crs=4326)

mapview(st_geometry(buffer)) |> garnishMap(addMeasure, primaryLengthUnit='meters')

st_write(buffer, here::here('300 meters/300_meters.gpkg'), 
         layer='Buffer', delete_layer=TRUE)

buffer = st_read(here::here('300 meters/300_meters.gpkg'), 
         layer='Buffer')

# Added a few points
isochrones2 = map(196:201,
                 ~ors_isochrones(unclass(access_pts$geometry[.x][[1]]), range = 300, 
                      profile=ors_profile('walking'), output = "sf"))

buffer2 = do.call(c, map(isochrones2, st_geometry))
buffer2 = st_union(st_make_valid(buffer2))
buffer2 = st_as_sf(buffer2, crs=4326)
mapview(st_geometry(buffer2)) |> garnishMap(addMeasure, primaryLengthUnit='meters')

buffer = st_union(buffer, buffer2)
st_write(buffer, here::here('300 meters/300_meters.gpkg'), 
         layer='Buffer', delete_layer=TRUE)

# And a few more...
isochrones3 = map(194:195,
                 ~ors_isochrones(unclass(access_pts$geometry[.x][[1]]), range = 300, 
                      profile=ors_profile('walking'), output = "sf"))

buffer3 = do.call(c, map(isochrones3, st_geometry))
buffer3 = st_union(st_make_valid(buffer3))
buffer3 = st_as_sf(buffer3, crs=4326)

buffer = st_union(buffer, buffer3)
st_write(buffer, here::here('300 meters/300_meters.gpkg'), 
         layer='Buffer', delete_layer=TRUE)

# We seem to have gotten more like a 400m border ??
a_fox = access_pts[144,] # SW corner of Agnes Fox playground
fox_buf = ors_isochrones(unclass(a_fox$geometry[1][[1]]), 
                         range=300, range_type='distance',
                         profile=ors_profile('walking'), 
                         output='sf')
mapview(st_as_sf(fox_buf$geometry, crs=4326)) |> 
  garnishMap(addMeasure, primaryLengthUnit='meters')
                                                            
                                                            
dingle = access_pts[146,] # Mary's Dingle
mapview(dingle)
dingle_buf = ors_isochrones(unclass(dingle$geometry[1][[1]]), 
                         range=300, range_type='distance',
                         profile=ors_profile('walking'), 
                         output='sf')
mapview(st_as_sf(dingle_buf$geometry, crs=4326)) |> 
  garnishMap(addMeasure, primaryLengthUnit='meters')

buffer = st_union(buffer, st_as_sf(dingle_buf$geometry, crs=4326))                    st_write(buffer, here::here('300 meters/300_meters.gpkg'), 
         layer='Buffer', delete_layer=TRUE)
                                      
                                                            
