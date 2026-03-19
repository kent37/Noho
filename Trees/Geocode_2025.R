# Geocode new addresses in the 2025 planting file

source(here::here('Trees/Geocoding.R'))

# Not sure why the reference data has ward instead of Ward...
massgis_wards = massgis_wards |> 
  rename(Ward=ward)

wards = wards |> 
  rename(Ward=ward)

planted_raw_2025 = readxl::read_xlsx(
  here::here("Trees/Kent's Copy of 2025 Trees Planted-corrected.xlsx"), 
  range=cell_cols("A:S"))

all_matched = read_sf(here::here('Shapefiles/Tree_locations.gpkg'))

planted_2025 = planted_raw_2025 |>
  rename(Num=`#`) |>
  mutate(
    Num = parse_number(Num),
    Ward = as.character(Ward),
    Year = 2025
  )

addresses_2025 = planted_2025 |>
  filter(!is.na(Street)) |>
  select(Num, Street, Ward) |>
  unique() |>
  mutate(
    Street_Addr = if_else(is.na(Num), Street, paste(Num, Street)),
    Address = paste0(Street_Addr, ', Northampton, MA, USA')
  )

# Hack in an address that was added to the original master list
addresses_2025 = addresses_2025 |> 
  bind_rows(tibble(
    Num = 27,
    Street = "WILSON AVE", 
    Ward = "3", 
    Street_Addr = "27 WILSON AVE", 
    Address = "27 WILSON AVE, Northampton, MA, USA")
  )

# Restrict to just the addresses not already geocoded
addresses_2025 = anti_join(addresses_2025,
                            all_matched,
                            by = c('Num', Street='Street_orig', 'Ward'))

addresses_2025$Street_orig = addresses_2025$Street
addresses_2025$Street = addresses_2025$Street |>
  str_remove(', (FLORENCE|LEEDS)$') |>
  expand_st_to_street()

matched_addresses_2025 =
  inner_join(addresses_2025, massgis_wards,
             by = c(Num='ADDR_NUM', Street='STREETNAME', Ward='Ward')) |>
  st_as_sf()

unmatched_addresses_2025 =
  anti_join(addresses_2025, massgis_wards,
            by = c(Num='ADDR_NUM', Street='STREETNAME', Ward='Ward'))

# Fall back to OpenCage for anything MassGIS couldn't match
coded2_2025 = tidygeocoder::geocode(unmatched_addresses_2025,
                                     address = Address,
                                     method = 'opencage')

coded2_sf_2025 = coded2_2025 |>
  filter(!is.na(long)) |>
  st_as_sf(coords = c('long', 'lat'), crs = 4326)

# Not needed, we already have wards
#coded2_sf_2025 = st_join(coded2_sf_2025, wards |> st_transform(4326))

library(mapview)
mapview(coded2_sf_2025 |> mutate(label = str_glue('{Street_Addr}, {Ward}')),
        zcol = 'Ward', label = 'label') + mapview(wards, label = 'Ward')

#### Review OpenCage results above; fix any bad coordinates manually ####
# e.g. coded2_2025[n, c('lat', 'long')] = list(lat=..., long=...)

all_matched_2025 = bind_rows(
  matched_addresses_2025 |> st_transform(4326),
  coded2_sf_2025
)

mapview(all_matched_2025, zcol='Ward', label='Street_Addr') +
  mapview(wards, zcol='Ward')

# This should cover all the addresses in `planted_2025`
planted_2025 |>
  filter(!is.na(Street)) |>
  anti_join(bind_rows(all_matched,
                        all_matched_2025 |> rename(geom=geometry)),
            by = c('Num', Street='Street_orig', 'Ward')) |>
  nrow() # Should be zero!

# Update the master address list
all_matched = bind_rows(all_matched,
                        all_matched_2025 |> rename(geom=geometry))

# Some historical errors here...
mapview(all_matched, zcol='Ward', label='Street_Addr') +
  mapview(wards, zcol='Ward')

st_write(all_matched,
         here::here('Shapefiles/Tree_locations.gpkg'), delete_layer=TRUE)
