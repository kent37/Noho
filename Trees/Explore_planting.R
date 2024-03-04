# Explore Noho tree planting data
library(tidyverse)
library(googlesheets4)
library(gt)
library(lubridate)
library(mapview)
library(sf)
library(tidygeocoder)

source(here::here('Trees/Geocoding.R'))

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

# MassGIS spells out ROAD, etc. We have to edit addresses for this.
# We also have to strip out FLORENCE and LEEDS
# What do we have to replace?
addresses$Street |> str_extract(' ([^ ]+)$', group=1) |> unique()

# Remember the original Street for the eventual join with planted
addresses$Street_orig = addresses$Street
addresses$Street = addresses$Street |> 
  str_remove(', (FLORENCE|LEEDS)$')

addresses$Street = addresses$Street |> expand_st_to_street()

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
# See here::here('Addresses.qgz')
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

# Geocode some places that may not have trees associated with them
# These tribbles are duplicates from the priority planting report
businesses = tribble(
  ~Location, ~address,
  'Spare Time', '525 Pleasant St',
  'Daily Hampshire Gazette', '115 Conz St',
  'Cooley Dickenson Hospital', '30 Locust St',
  'Kollmorgen', '50 Prince St',
  'St. Elizabeth Ann Church', '91 King St', # Actually 99 but we have loc'n for 91
  'Northampton Tire', '182 King St',
  'Acme Automotive', '220 King St',
  '200 King St', '200 King St',
  '206 King St', '206 King St',
  'Bluebonnet Diner', '324 King St',
  'NAPA Auto Parts', '348 King St',
  'Woodward & Grinnell', '8 North King St',
  'Enterprise Car Rentals', '24 North King St',
  'Big Y', '136 North King St',
  'Cooke Ave. Apts', '316A Hatfield St',
  'Cooke Ave. Apts', '364A Hatfield St',
  'Cooke Ave. Apts', '380A Hatfield St'
) |> 
  mutate(Category='Business')

low_income_bldg = tribble(
  ~address, ~Location, 
  '80 Damon Rd', 'River Run Apts',
  '241 Jackson St', 'Hampshire Heights',
  '178 Florence Rd', 'Florence Heights',
  '491 Bridge Rd', 'Meadowbrook Apts',
  '81 Conz St', 'Salvo House',
  '49 Old South St', 'MacDonald House',
  '91 Grove St', 'Grove Street Inn'
) |> 
  mutate(Category='Low income')

locations = bind_rows(businesses, low_income_bldg) |> 
  separate(address, into=c('ADDR_NUM', 'STREETNAME'),
           sep=' ', extra='merge') |> 
  mutate(STREETNAME=expand_st_to_street(str_to_upper(STREETNAME))) |> 
  right_join(massgis |> select(ADDR_NUM, STREETNAME, SITE_NAME), y=_, multiple='all') |> 
  st_buffer(20) |> 
  select(-ADDR_NUM) |> 
  nest(data=geometry) |> 
  mutate(data=map(data, ~st_convex_hull(st_union(st_geometry(.x)))[[1]])) |> 
  st_as_sf(sf_column_name='data') |> 
  st_set_crs(st_crs(massgis)) |> 
  st_transform(4326)

st_write(locations, 
         here::here('Shapefiles/Extra_locations.gpkg'), delete_layer=TRUE)
