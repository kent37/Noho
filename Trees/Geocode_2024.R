# Geocode new addresses in the 2024 planting file

source(here::here('Trees/Geocoding.R'))

# Not sure why the reference data has ward instead of Ward...
massgis_wards = massgis_wards |> 
  rename(Ward=ward)

wards = wards |> 
  rename(Ward=ward)

planted_raw_2024 = readxl::read_xlsx(
  here::here('Trees/Hamp_Trees_Planted_2024.xlsx'), 
  skip=1) |> 
  select(-6) # Blank column

all_matched = read_sf(here::here('Shapefiles/Tree_locations.gpkg'))

planted_2024 = planted_raw_2024 |> 
  rename(Num=Number) |> 
  mutate(Num=parse_number(Num),
         Year = 2024)

addresses_2024 = planted_2024 |> 
  filter(!is.na(Street)) |> 
  select(Num, Street) |> 
  unique() |> 
  mutate(
    Street_Addr=if_else(is.na(Num), Street, paste(Num, Street)),
         Address=paste0(Street_Addr, ', Northampton, MA, USA'))

# Restrict to just the addresses that are not already geocoded
addresses_2024 = anti_join(addresses_2024, 
                    all_matched,
                    by=c('Num', 'Street'='Street_orig'))

addresses_2024$Street_orig = addresses_2024$Street

addresses_2024$Street = addresses_2024$Street |> expand_st_to_street()

matched_addresses_2024 = 
  inner_join(addresses_2024, massgis_wards,
            by=c(Num='ADDR_NUM', Street='STREETNAME')) |> 
  st_as_sf()

unmatched_addresses_2024 = 
  anti_join(addresses_2024, massgis_wards,
            by=c(Num='ADDR_NUM', Street='STREETNAME'))

# Can we match these with OpenCage?
coded2_2024 = tidygeocoder::geocode(unmatched_addresses_2024,
                address=Address,
                method='opencage')

# All good except St Mary's Cemetery
coded2_2024[2, c('lat', 'long')] = list(lat=42.33729,long=-72.65334)
coded2_sf_2024 = coded2_2024 |> 
  filter(!is.na(long)) |> 
  st_as_sf(coords=c('long', 'lat'), crs=4326)

coded2_sf_2024 = st_join(coded2_sf_2024, wards |> st_transform(4326))

library(mapview)
mapview(coded2_sf_2024 |> mutate(label=str_glue('{Street_Addr}, {Ward}')),
        label='label') + mapview(wards, label='Ward')

#### START HERE ####


all_matched_2024 = bind_rows(matched_addresses_2024 |> st_transform(4326), 
                                coded2_sf_2024)

mapview(all_matched_2024, zcol='Ward', label='Street_Addr') +
  mapview(wards, zcol='Ward')

# Update the master address list
all_matched = bind_rows(all_matched,
                        all_matched_2024 |> rename(geom=geometry))

# This should cover all the addresses in `planted_2024`
# if we munge the street appropriately
planted_2024 |> 
  filter(!is.na(Street)) |> 
  anti_join(all_matched,
            by=c('Num', Street='Street_orig')) |> 
  nrow() # Should be zero!

st_write(all_matched, 
         here::here('Shapefiles/Tree_locations.gpkg'), delete_layer=TRUE)
