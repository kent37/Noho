library(tidyverse)
library(googlesheets4)
library(leaflet)
library(sf)

qmile_data_raw = read_sheet(
  'https://docs.google.com/spreadsheets/d/1BPOBrVKXX7ROS-gORve0q3MeRDxBtJ7s4KPwdU4Kl4I/edit#gid=0',
  sheet=1, skip=1,
  col_types='c'
)
qmile_data = qmile_data_raw |> 
  select(-...16) |> 
  mutate(
    ident = str_glue('{Sector}-{`Site ID`}'),
    Street=str_to_title(
      if_else(is.na(`St/Rd/Ave/            Terr/Pl/Ct/                         Cir/etc`), 
              `Street name`,
              str_glue(
      '{`Street name`} {`St/Rd/Ave/            Terr/Pl/Ct/                         Cir/etc`}'))),
    Address = if_else(is.na(`Street number`), Street, str_glue('{`Street number`} {Street}'))) |> 
    rename(Number="Number of  Trees",
         Sidewalk="Sidewalk?       Y/N",
         Ownership='Ownership:         R/S/?/B/C',
         `Under wire`='Under wire? Y/N/B/X',
         Area="Planting area       S/M/L",
         Rank="Rank                              1/2/3",
         `Street ext`="St/Rd/Ave/            Terr/Pl/Ct/                         Cir/etc",
         Location="Front  Side   or Back        F/S/B"
        )


# The actual sites, from Molly Hale
qmile_locs = st_read(
  here::here('Trees/Quadrant maps/All planting sites/All planting sites.shp')) |> 
  select(ident:x_proj)

qmile = right_join(qmile_locs, qmile_data)
write_sf(qmile, here::here('Trees/Quadrant maps/All planting sites with data.gpkg'))
write_sf(qmile, here::here('Trees/Quadrant maps/All planting sites with data/All planting sites with data.shp'))

qmile |> 
  filter(`Ownership:         R/S/?/B/C`=='S', `Under wire? Y/N/B/X`=='N') |> 
  st_transform(4326) |> 
  leaflet(width='796px', height='700px') |> # 796 is the container width
  #fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) |> 
  addProviderTiles('CartoDB.Positron', group='Street') |> 
  addProviderTiles('Esri.WorldImagery', group='Satellite') |> 
  addCircleMarkers(radius=5, label=~Address,
                   group='Trees',
                   stroke=FALSE, fillColor="#1B9E77", fillOpacity=0.5) |> 
  addLayersControl(baseGroups=c('Street', 'Satellite'),
    overlayGroups='Trees',
    options=layersControlOptions(collapsed=FALSE))
