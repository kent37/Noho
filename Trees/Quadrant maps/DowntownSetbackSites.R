# Prepare data for downtown Northampton setback tree program

library(tidyverse)
library(sf)

qmile_raw = read_sf(here::here('Trees/Quadrant maps/All planting sites with data.gpkg'))

qmile = qmile_raw |> 
  select(-c(Latitude, Longitude, Sidewalk)) |> 
  filter(str_starts(Sector, 'N'), Ownership=='S') |> 
  mutate(ST_NUM=as.integer(`Street number`), 
         STREET_NAME = str_to_upper(Street))

qmile_names = names(qmile)
  
with_num = qmile |> filter(!is.na(ST_NUM))
no_num = qmile |> filter(is.na(ST_NUM))

# How many of these match up with assessor addresses?
assess = readxl::read_excel(here::here('data/AllParcelsFY24part2.xlsx'))

with_num_match = inner_join(with_num, assess, relationship = "many-to-many",
                            by=c(ST_NUM='ST NUM', 
                                 STREET_NAME='STREET NAME'))

no_num_match = anti_join(with_num, assess,
                         by=c(ST_NUM='ST NUM', 
                                 STREET_NAME='STREET NAME'))

# We have to try to fix these up by hand
no_num_all = bind_rows(no_num, no_num_match)
write_csv(no_num_all, 
          here::here('Trees/Quadrant maps/Sites_without_matching_address.csv'),
          na='')

# Hand editing here...
# Read in the results
hand_num = read_csv(
            here::here('Trees/Quadrant maps/Sites_without_matching_address.csv'),
            na='') |> 
  mutate(`Site ID`=as.character(`Site ID`)) |> 
  select(-geom)

# We have to fix up the geom and Number columns. Easiest way is to join with the
# original data
hand_num = hand_num |> 
  select(-Number) |> 
  left_join(qmile |> select(ident, Number)) |> 
  st_as_sf()

hand_num_match = inner_join(hand_num, assess, relationship = "many-to-many",
                            by=c(ST_NUM='ST NUM', 
                                 STREET_NAME='STREET NAME'))

all_match = bind_rows(with_num_match, hand_num_match)

# Hmm, maybe it was a mistake to remove Lat Long
all_match = all_match |> 
  left_join(st_drop_geometry(qmile_raw) |> select(ident, Latitude, Longitude))

# Keep the original order
all_match = all_match |> select(names(qmile_raw |> select(-Sidewalk)), everything())

# Add location for 11 Conz St
all_match = all_match |> 
  mutate(
    x_proj = if_else(ident == "N3-47", 106795.3, x_proj),
    y_proj = if_else(ident == "N3-47", 896717.0, y_proj)
  )

# Take out columns that are will be confusing to users
# Note: ST_NUM and STREET_NAME do not alway match the actual address
# Some of them were changed in the hand-editing step above to an address
# that matches the property owner in the assessor's data
all_match = all_match |> 
  st_drop_geometry() |> 
  select(-c(ST_NUM, STREET_NAME, '#', 'REVCODE'))

write_csv(all_match,
            here::here('Trees/Quadrant maps/Downtown_sites_with_owners.csv'),
            na='')

# Read it back
all_match = read_csv(here::here('Trees/Quadrant maps/Downtown_sites_with_owners.csv'),
            na='') |> 
  select(-geom)  |> 
  filter(!is.na(x_proj)) |> 
  st_as_sf(coords=c('x_proj', 'y_proj'), crs=st_crs(qmile_raw))
