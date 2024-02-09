# Unpaved roads in the vicinity
library(tidyverse)
library(osmdata)
library(sf)
library(tigris)

# Similar to this query to https://overpass-turbo.eu/
overpass = '
[out:json][timeout:25];
// gather results
(
  way["surface"="unpaved"][highway="primary"]({{bbox}});
  way["surface"="unpaved"][highway="secondary"]({{bbox}});
  way["surface"="unpaved"][highway="tertiary"]({{bbox}});
  way["surface"="unpaved"][highway="residential"]({{bbox}});
  way["surface"="unpaved"][highway="service"]({{bbox}});
  way["surface"="unpaved"][highway="unclassified"]({{bbox}});
  way[highway="track"][name~".+"]({{bbox}});
);
// print results
out geom;
'
bbox = counties(state='MA') |> 
  filter(NAME %in% c('Hampshire', 'Franklin')) |> 
  st_union() |> 
  st_bbox()

bbox_vec = unclass(bbox)
attributes(bbox_vec) = NULL
streets_raw = opq(bbox_vec)  |> 
  add_osm_feature(key = "highway", 
                  value = c("residential", "primary", 
                            "secondary", "tertiary", 
                            'service', 'unclassified')) |> 
  add_osm_feature(key='surface', value='unpaved') |> 
  osmdata_sf()

streets = streets_raw$osm_lines |> 
  mutate(name=if_else(is.na(name), '', name))

tracks_raw = opq(bbox_vec) |> 
  add_osm_feature(key='highway', value='track') |> 
  osmdata_sf()
tracks_raw = tracks_raw$osm_lines

tracks = tracks_raw |> 
  filter(!is.na(name))
unnamed_tracks = tracks_raw |> 
  filter(is.na(name))

mapviewOptions(basemaps = c("OpenStreetMap", "OpenTopoMap", "CartoDB.Positron", 
"Esri.WorldImagery"))

mapview(streets, label='name', color='darkred') +
  mapview(tracks, label='name', color='red3') +
  mapview(unnamed_tracks, color='lightpink3')
