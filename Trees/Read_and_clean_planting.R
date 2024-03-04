# Read and clean Noho tree planting data
library(tidyverse)
library(googlesheets4)
source(here::here('Trees/Binomial_name.R'))

planted_raw = read_sheet(
  'https://docs.google.com/spreadsheets/d/1rIJKxHEv54ULyM4BhTkrGKDITdbv34X5bEDFp6gRx_4/edit#gid=1562980960',
  sheet=1,
  range="A:R",
  col_types='c'
)

planted_raw_2023 = read_sheet(
  'https://docs.google.com/spreadsheets/d/1gkjDBEGSVutFosuvBT2iWY-dUm571EnoIdrMg-n-Npw/edit#gid=2048232294',
  sheet=1,
  range="A:U",
  col_types='c'
) |> 
  # Munge some streets to match the geocoded address list
  mutate(Street = if_else(is.na(`#`), case_when(
    Street == "INDUSTRIAL DRIVE" ~ "INDUSTRIAL DRIVE 106-168",
    Street == "MOSER STREET" ~ "MOSER STREET X 4-14",
    Street == "MUSANTE DRIVE" ~ "MUSANTE DRIVE X 50",
    Street == "PRINCE STREET" ~ "PRINCE X LAUREL",
    .default=Street
  ), Street)) |> 
  select(-`Tree Diaper`, -starts_with('Watering Date'))

planted = planted_raw |> 
  bind_rows(planted_raw_2023) |> 
  rename(Num=`#`) |> 
  mutate(Num=parse_number(Num),
         Year = year(mdy(`Date Planted`, quiet=TRUE)),
         Year = if_else(is.na(Year), parse_number(`Date Planted`), Year),
         dead = is.na(Species),
         Genus = case_match(Genus, # Fix misspellings
                             'Liquidamber' ~ 'Liquidambar',
                             .default = Genus,
                             ))

# Clean up some scientific names
planted = planted |> 
  mutate(`Scientific Name` = case_when(
    Species == "A canadensis" ~ "Amelanchier canadensis",
    Species == "M ?" ~ "Malus spp.",
    Genus=='Amelanchier' & Species == 'grandiflora' ~ 'Amelanchier grandiflora',
    Genus=='Amelanchier' & Species == 'laevis' ~ 'Amelanchier laevis',
    `Scientific Name`=='TIlia tomentosa Sterling®' ~ 'Tilia tomentosa Sterling®',
    TRUE ~ str_replace_all(`Scientific Name`, # Fix misspellings
                           c('×'='x', 
                             'tulipfera'='tulipifera',
                             'tomemtosa'='tomentosa'))
  ))


# Fill in species, genus, family and scientific name for the dead trees
planted = local({
  living = planted |> filter(!dead)
  dead = planted |> 
    filter(dead) |> 
    select(-Species, -Genus, -Family, -`Scientific Name`)
  
  species_lookup = living |> 
    select(`Common Name`, Species, Genus, Family, `Scientific Name`) |> 
    unique()
  
  # Not all common names uniquely identify the species, for example
  # three different species of serviceberry all have the same
  # common name. Here we just pick one, what else can we do??
  species_lookup = species_lookup[!duplicated(species_lookup$`Common Name`), ]
  
  # Fix some different spellings
  dead = dead |> 
    mutate(`Common Name` = case_match(`Common Name`,
      "GINKGO BILOBA" ~ "GINKGO",
      "LONDON PLANE \"EXCLAMATION\"" ~ "EXCLAMATION LONDONPLANE",
      "LONDON PLANE TREE 'BLOODGOOD'" ~ "LONDON PLANE \"BLOODGOOD\"",
      "PRINCETON GINKGO" ~ "GINKGO \"PRINCETON SENTRY\"",
      "STREET KEEPER HONEY LOCUST" ~ "HONEY LOCUST \"STREET KEEPER\"",
      .default=`Common Name`
    ))
  
  # Check
  stopifnot(all(dead$`Common Name` %in% species_lookup$`Common Name`))
  
  dead = dead |> left_join(species_lookup)
  
  # Put it all back together
  bind_rows(living, dead)
})

# Now we can add binomial name
planted = planted |> 
  mutate(Binomial = binomial_name(`Scientific Name`))

# Locations
planting_locations = read_sf(here::here('Shapefiles/Tree_locations.gpkg')) |> 
  st_transform(crs=4326) |> 
  select(Num, Street=Street_orig, Ward)

summarize_names = function(common_names) {
  tibble(names=str_to_title(common_names)) |> 
    count(names, sort=TRUE) |> 
    mutate(label=paste(n, names)) |> 
    pull(label) |> 
    paste(collapse='<br>')
}

# Summarize planting by location
map_data = planted |> 
  filter(!dead) |> 
  group_by(Num, Street, Ward) |> 
  summarize(count=n(),
            name_label=summarize_names(`Common Name`),
            Location=str_c(sort(unique(Location)), collapse=','),
            .groups='drop'
  ) |> 
  mutate(address=str_to_title(if_else(is.na(Num), Street, paste(Num, Street))),
         label=paste0(address, ' (', count, ' trees)<br><br>', name_label) |> 
           lapply(htmltools::HTML) |> unname())

map_data = 
  inner_join(map_data, planting_locations) |> 
  st_as_sf()

# Helpers
int_pct = function(val) scales::percent(val, accuracy=1)
