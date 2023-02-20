# Explore Noho tree planting data
library(tidyverse)
library(googlesheets4)
library(gt)
library(lubridate)
library(mapview)
library(sf)
library(tidygeocoder)

planted_raw = read_sheet(
  'https://docs.google.com/spreadsheets/d/1rIJKxHEv54ULyM4BhTkrGKDITdbv34X5bEDFp6gRx_4/edit#gid=1562980960',
  sheet=1,
  col_types='c'
)

planted = planted_raw |> 
  select(`#`:Notes) |> 
  rename(Num=`#`) |> 
  mutate(Num=parse_number(Num),
         Year = year(mdy(`Date Planted`)),
         Year = if_else(is.na(Year), parse_number(`Date Planted`), Year))

# Sort out the section of dead trees
planted$died = FALSE
planted$died[1929:2039] = TRUE

# Can we geocode the addresses?
# Yikes, we have duplicate addresses in different wards!!
addresses = planted |> 
  filter(!is.na(Street)) |> 
  select(Num, Street, Ward) |> 
  unique() |> 
  mutate(Street_Addr=if_else(is.na(Num), Street, paste(Num, Street)),
         Address=paste0(Street_Addr, ', Northampton, MA, USA'))

# What addresses are duplicated with different wards?
duplicates = addresses[duplicated(addresses |> select(Num, Street)),]
duplicates = duplicates |> 
  select(Num, Street) |> 
  left_join(addresses, multiple='all')

# This has lots of fails, it does not do well with addresses 
# in Florence and Leeds
# coded = geocode(addresses,
#                 street=Street, city=City, state=State, country=Country,
#                 method='geocodio',
#                 full_results=TRUE)

# This is slow - one address per second, no batch processing :-(
# The results are better than geocod.io, though
# coded = geocode(addresses,
#                 address=Address,
#                 method='opencage')
# 
# coded_sf = coded |> 
#   filter(!is.na(long)) |> 
#   select(-annotations.UN_M49.statistical_groupings, # Remove list columns
#          -annotations.currency.alternate_symbols,
#          -`components.ISO_3166-2`) |> 
#   st_as_sf(coords=c('long', 'lat'), crs=4326)

# mapview(coded_sf, label='Street', zcol='confidence')
# mapview(coded_sf |> filter(confidence==10), label='Street')
# mapview(coded_sf|> filter(confidence!=10), label='Street', zcol='confidence')

# Try matching up with MassGIS addresses by ward
massgis = st_read(here::here('Shapefiles/AddressPts_M214/AddressPts_M214.shp')) |> 
  select(-(ADDR_PT_ID:NAD_ID), -REL_LOC, -(WING:SUBSITE),
         -NEIGHBORHD, -(STATE:PRE_TYPE), -(POST_DIR:ADDRTWN_ID)) |> 
  distinct()

wards = st_read(here::here('Shapefiles/vote_ward_precinct_2010_20190916/vote_ward_precinct_2010_20190916.shp')) |> 
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

# MassGIS spells out ROAD, etc. We have to edit addresses for this.
# We also have to strip out FLORENCE and LEEDS
# What do we have to replace?
addresses$Street |> str_extract(' ([^ ]+)$', group=1) |> unique()

# Remember the original Street for the eventual join with planted
addresses$Street_orig = addresses$Street
addresses$Street = addresses$Street |> 
  str_remove(', (FLORENCE|LEEDS)$')

addresses$Street = addresses$Street |> 
  str_replace_all(c(
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

matched_addresses = 
  inner_join(addresses, massgis_wards,
            by=c(Num='ADDR_NUM', Street='STREETNAME', Ward='ward')) |> 
  st_as_sf()

unmatched_addresses = 
  anti_join(addresses, massgis_wards,
            by=c(Num='ADDR_NUM', Street='STREETNAME', Ward='ward'))

# Can we match these with OpenCage?
coded2 = geocode(unmatched_addresses,
                address=Address,
                method='opencage')
coded2_sf = coded2 |> 
  filter(!is.na(long)) |> 
  st_as_sf(coords=c('long', 'lat'), crs=4326)

# A few fails still, do these by hand in QGIS
not_coded = st_read(here::here('Shapefiles/Not_coded_addresses.gpkg')) |> 
  mutate(Num=NA, .before=1) |>
  st_transform(4326)

coded2_sf = coded2_sf |> 
  filter(!Street %in% not_coded$Street)
matched_addresses2 =   
  inner_join(addresses, 
             coded2_sf |> select(-Street_Addr, -Address),
            by=c('Num', 'Street', 'Ward')) |> 
  st_as_sf()

matched_addresses3 = 
  inner_join(addresses, not_coded) |> 
  st_as_sf()

all_matched = 
  bind_rows(matched_addresses |> st_transform(4326), 
            matched_addresses2, matched_addresses3)

mapview(all_matched, zcol='Ward', label='Street_Addr')

st_write(all_matched, 
         here::here('Shapefiles/Tree_locations.gpkg'), delete_layer=TRUE)

# This should cover all the addresses in `planted`
missing = anti_join(planted |> filter(!is.na(Street)), 
                    all_matched,
                    by=c('Num', Street='Street_orig', 'Ward'))
nrow(missing) # Should be zero!
