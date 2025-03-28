# Look at population near transit and bike path
library(tidyverse)
library(mapview)
library(sf)

# Get some data
noho = read_sf(here::here('Shapefiles/Noho_outline/Noho_outline.gpkg'))

# Bike path access and census is in the 300 meters data
bike_access = read_sf(
  here::here('300 meters/300_meters.gpkg'),
  query="select * from Access_points where type='Bike path'") |> 
  st_transform(26986) # NAD State Plane (meters)

census = read_sf(
  here::here('300 meters/300_meters.gpkg'), 'census_blocks')

bus_stops = read_sf(
  here::here('Transit/data/PVTA_stops.geojson')) |> 
  select(stop_name) |> 
  st_transform(26986) 

bus_stops = bus_stops[noho,]

mapview(census, zcol='POP20') + 
  mapview(bike_access) + 
  mapview(bus_stops))

# Make some buffers
meters_per_mile = 1609.34

# Bus
quarter_mile_to_bus = st_buffer(bus_stops, 1/4*meters_per_mile) |> st_union()
half_mile_to_bus = st_buffer(bus_stops, 1/2*meters_per_mile) |> st_union()

quarter_to_half_mile_to_bus = 
  st_difference(half_mile_to_bus, quarter_mile_to_bus)
over_half_mile_to_bus = st_difference(noho, half_mile_to_bus)

mapview(bus_stops) + 
  mapview(quarter_mile_to_bus |> st_cast('POLYGON') |> st_as_sf()) + 
  mapview(quarter_to_half_mile_to_bus |> st_cast('POLYGON') |> st_as_sf()) + 
  mapview(over_half_mile_to_bus |> st_cast('POLYGON') |> st_as_sf())

# Same thing for bikes
quarter_mile_to_bike = st_buffer(bike_access, 1/4*meters_per_mile) |> st_union()
half_mile_to_bike = st_buffer(bike_access, 1/2*meters_per_mile) |> st_union()
quarter_to_half_mile_to_bike = st_difference(half_mile_to_bike, quarter_mile_to_bike)
over_half_mile_to_bike = st_difference(noho, half_mile_to_bike)

mapview(bike_access) + 
  mapview(quarter_mile_to_bike |> st_cast('POLYGON') |> st_as_sf()) + 
  mapview(quarter_to_half_mile_to_bike |> st_cast('POLYGON') |> st_as_sf()) + 
  mapview(over_half_mile_to_bike |> st_cast('POLYGON') |> st_as_sf())

# Look at intersections
census[quarter_mile_to_bike,, op=st_within] |> st_geometry() |> plot()
