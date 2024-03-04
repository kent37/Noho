# Geocode new addresses in the 2023 planting file

source(here::here('Trees/Geocoding.R'))

planted_raw_2023 = googlesheets4::read_sheet(
  'https://docs.google.com/spreadsheets/d/1gkjDBEGSVutFosuvBT2iWY-dUm571EnoIdrMg-n-Npw/edit#gid=2048232294',
  sheet=1,
  range="A:U",
  col_types='c'
)

all_matched = read_sf(here::here('Shapefiles/Tree_locations.gpkg'))

planted_2023 = planted_raw_2023 |> 
  rename(Num=`#`) |> 
  mutate(Num=parse_number(Num),
         Year = year(mdy(`Date Planted`)),
         Year = if_else(is.na(Year), parse_number(`Date Planted`), Year))

addresses_2023 = planted_2023 |> 
  filter(!is.na(Street)) |> 
  select(Num, Street, Ward) |> 
  unique() |> 
  mutate(
    Street_Addr=if_else(is.na(Num), Street, paste(Num, Street)),
         Address=paste0(Street_Addr, ', Northampton, MA, USA'))

# Restrict to just the addresses that are not already geocoded
addresses_2023 = anti_join(addresses_2023, 
                    all_matched,
                    by=c('Num', Street='Street_orig', 'Ward'))

addresses_2023$Street_orig = addresses_2023$Street
addresses_2023$Street = addresses_2023$Street |> 
  str_remove(', (FLORENCE|LEEDS)$')

addresses_2023$Street = addresses_2023$Street |> expand_st_to_street()

matched_addresses_2023 = 
  inner_join(addresses_2023, massgis_wards,
            by=c(Num='ADDR_NUM', Street='STREETNAME', Ward='ward')) |> 
  st_as_sf()

unmatched_addresses_2023 = 
  anti_join(addresses_2023, massgis_wards,
            by=c(Num='ADDR_NUM', Street='STREETNAME', Ward='ward'))

# Can we match these with OpenCage?
coded2_2023 = tidygeocoder::geocode(unmatched_addresses_2023,
                address=Address,
                method='opencage')

coded2_sf_2023 = coded2_2023 |> 
  filter(!is.na(long)) |> 
  st_as_sf(coords=c('long', 'lat'), crs=4326)

mapview(coded2_sf_2023, zcol='Ward', label='Street_Addr')

# See here::here('Addresses.qgz')
# We used more accurate names for the missing locations
not_coded = read_sf(here::here('Shapefiles/Not_coded_addresses.gpkg')) |> 
  filter(Street %in% c("PRINCE X LAUREL", "MUSANTE DRIVE X 50", 
                       "INDUSTRIAL DRIVE 106-168", "FLORENCE FIELDS", 
                       "MOSER STREET X 4-14")) |> 
  mutate(Num=NA, .before=1) |>
  st_transform(4326)

# Only the first one is really correct, the others are too vague
# or simply wrong
coded2_sf_2023 = coded2_sf_2023[1,]

matched_addresses2_2023 =   
  inner_join(addresses_2023, 
             coded2_sf_2023 |> select(-Street_Addr, -Address, -Street_orig),
            by=c('Num', 'Street', 'Ward')) |> 
  st_as_sf()

# Munge unmatched_address_2023$Street to match what is in not_coded
matched_addresses3_2023 = unmatched_addresses_2023 |> 
  filter(is.na(Num)) |> 
  mutate(Street = case_when(
    Street == "INDUSTRIAL DRIVE" ~ "INDUSTRIAL DRIVE 106-168",
    Street == "MOSER STREET" ~ "MOSER STREET X 4-14",
    Street == "MUSANTE DRIVE" ~ "MUSANTE DRIVE X 50",
    Street == "PRINCE STREET" ~ "PRINCE X LAUREL",
    .default=Street
  )) |> 
  inner_join(not_coded) |> 
  st_as_sf()

all_matched_2023 = bind_rows(matched_addresses_2023 |> st_transform(4326), 
                                matched_addresses2_2023,
                                matched_addresses3_2023)

mapview(all_matched_2023, zcol='Ward', label='Street_Addr') +
  mapview(wards, zcol='ward')

# Update the master address list
all_matched = bind_rows(all_matched,
                        all_matched_2023 |> rename(geom=geometry))

# This should cover all the addresses in `planted_2023`
# if we munge the street appropriately
planted_2023 |> 
  filter(!is.na(Street)) |> 
  mutate(Street = if_else(is.na(Num), case_when(
    Street == "INDUSTRIAL DRIVE" ~ "INDUSTRIAL DRIVE 106-168",
    Street == "MOSER STREET" ~ "MOSER STREET X 4-14",
    Street == "MUSANTE DRIVE" ~ "MUSANTE DRIVE X 50",
    Street == "PRINCE STREET" ~ "PRINCE X LAUREL",
    .default=Street
  ), Street)) |> 
  anti_join(all_matched,
            by=c('Num', Street='Street_orig', 'Ward')) |> 
  nrow() # Should be zero!

st_write(all_matched, 
         here::here('Shapefiles/Tree_locations.gpkg'), delete_layer=TRUE)
