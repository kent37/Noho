# Noho isochrones
library(tidyverse)
library(leaflet)
library(osmdata)
# remotes::install_github("GIScience/openrouteservice-r")
library(openrouteservice)
library(mapview)
library(sf)

noho = getbb("Northampton, Massachusetts")
streets = noho %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "secondary", "tertiary")) %>%
  osmdata_sf()
small_streets = noho %>% 
  opq() %>% 
  add_osm_feature(key = "highway",
                  value = c("residential", "living_street", "unclassified", "service", "footway")) %>% 
  osmdata_sf()

gg1 = ggplot() +
  geom_sf(data = small_streets$osm_lines, col = 'grey40', size = .1) +
  geom_sf(data = streets$osm_lines, col = 'grey40', size = .4) +
  #geom_pointdensity(aes(geo_longitude, geo_latitude), size = 2, alpha = .8) +
  geom_sf(data = small_streets$osm_lines, col = alpha('grey40', .2), size = .1) +
  geom_sf(data = streets$osm_lines, col = alpha('grey40', .2), size = .4) +
  scale_color_viridis_c(option = 'inferno') +
  coord_sf(xlim = noho[1,], ylim = noho[2,], expand = TRUE) + 
  geom_blank() +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#282828"))



pie_bar = c(-72.6674863,42.3349417)
the_roost = c(-72.62826, 42.320975)
aldrich_st = c(-72.6380252,42.3240853)

# Florence Pie Bar
res <- ors_isochrones(pie_bar, range = 45*60, interval = 15*60, 
                      profile=ors_profile('bike'), output = "sf")
values <- levels(factor(res$value))
ranges <- split(res, values)
ranges <- ranges[rev(values)]

names(ranges) <- sprintf("%s min", as.numeric(names(ranges))/60)
mapview(ranges, alpha.regions = 0.2, homebutton = FALSE, legend = FALSE)

## The Roost
roost_res <- ors_isochrones(the_roost, range = 30*60, interval = 10*60, 
                      profile=ors_profile('bike'), output = "sf")
values <- levels(factor(roost_res$value))
roost_ranges <- split(roost_res, values)
roost_ranges <- roost_ranges[rev(values)]

names(roost_ranges) <- sprintf("%s min", as.numeric(names(roost_ranges))/60)
mapview(ranges, alpha.regions = 0.2, homebutton = FALSE, legend = FALSE) +
  mapview(roost_ranges, col.regions='darkred', alpha.regions = 0.2, 
          homebutton = FALSE, legend = FALSE)

gg1 + geom_sf(data=roost_res, aes(color=value), alpha=0.1)

# Aldrich st by time
aldrich_bike = ors_isochrones(aldrich_st, range = 45*60, interval = 15*60, 
                      profile=ors_profile('bike'), output = "sf")

aldrich_walk = ors_isochrones(aldrich_st, range = 45*60, interval = 15*60, 
                      profile=ors_profile('walking'), output = "sf")

leaflet( ) %>% 
 addProviderTiles('CartoDB.Positron') %>% 
  addPolygons(data=st_geometry(aldrich_bike), weight=1, group='bike',
              color='gray', fillColor='steelblue', fillOpacity=0.1) %>% 
  addPolygons(data=st_geometry(aldrich_walk), weight=1, group='walk',
              color='green', fillColor='green', fillOpacity=0.1) %>% 
  addLayersControl(overlayGroups = c('bike', 'walk'))

# Aldrich st by distance
meters_per_mile = 1609.34
aldrich_walk_mi = ors_isochrones(aldrich_st, range = 4*meters_per_mile,
                                 interval = 1*meters_per_mile, 
                                 range_type='distance',
                                 smoothing=0,
                      profile=ors_profile('walking'), output = "sf")

leaflet() %>% 
 addProviderTiles('CartoDB.Positron') %>% 
  addPolygons(data=st_geometry(aldrich_walk_mi), weight=1, group='walk',
              color='green', fillColor='green', fillOpacity=0.1)


# Query OSM based on https://overpass-turbo.eu/
# (
#  node
#   [amenity~".*"]
#   ({{bbox}});
#  node
#    [shop~".*"]
#   ({{bbox}});
#  node
#    [tourism~".*"]
#   ({{bbox}});
#  node
#    [healthcare~".*"]
#   ({{bbox}});
# );
#  out;
