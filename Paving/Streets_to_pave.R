library(tidyverse)
library(sf)
library(mapview)

streets = st_read(here::here('Shapefiles/roads.geojson'))
pave= read_csv(here::here('Paving/2025 Paving - Streets List and Map (2).csv'))

to_pave = streets |> 
  inner_join(pave, by=c(name='Street'))

mapview(to_pave, zcol='name')

st_write(to_pave, here::here('Paving/To_pave_2025.gpkg'))
## Edit in QGIS
to_pave = st_read(here::here('Paving/To_pave_2025.gpkg'))
mapview(to_pave, zcol='name')
