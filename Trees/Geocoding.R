# Help with geocoding planting data files
library(tidyverse)
library(sf)

# Use MassGIS data and Northampton wards to create an address reference
# that uses ward to resolve duplicate street names
massgis = read_sf(here::here('Shapefiles/AddressPts_M214/AddressPts_M214.shp')) |> 
  select(-(ADDR_PT_ID:NAD_ID), -REL_LOC, -(WING:SUBSITE),
         -NEIGHBORHD, -(STATE:PRE_TYPE), -(POST_DIR:ADDRTWN_ID)) |> 
  distinct()

wards = read_sf(here::here('Shapefiles/vote_ward_precinct_2010_20190916/vote_ward_precinct_2010_20190916.shp')) |> 
  mutate(ward=str_sub(WARD, 1, 1)) |> 
  group_by(ward) |> 
  summarize()

massgis_wards = st_join(massgis, wards) |> 
  mutate(ADDR_NUM=as.numeric(ADDR_NUM))

# Some addresses have multiple points, e.g. 73 Barrett St which
# includes all of Hampton Garden
# Just use the centroid
massgis_wards = massgis_wards |> 
  group_by(ADDR_NUM, STREETNAME, ward) |>
  summarize(.groups='drop') |> 
  mutate(geometry=map(geometry, st_centroid))

massgis_wards$geometry = st_as_sfc(massgis_wards$geometry, crs=st_crs(massgis))

# Change street abbreviations from the tree data to the form used by MasGIS
expand_st_to_street = function(sts) {
    str_replace_all(sts, c(
    ' DR$' = ' DRIVE',
    ' ST$' = ' STREET',
    ' PL$' = ' PLACE',
    ' RD$' = ' ROAD',
    ' RD EXT$' = 'ROAD',
    ' AVE$' = ' AVENUE',
    ' CT$' = ' COURT',
    ' TERR$' = ' TERRACE',
    ' LN$' = ' LANE',
    ' CIR$' = ' CIRCLE',
    ' PLZ$' = ' PLAZA'
  ))
}
