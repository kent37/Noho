# Read and clean Noho tree planting data
library(tidyverse)
library(googlesheets4)

planted_raw = read_sheet(
  'https://docs.google.com/spreadsheets/d/1rIJKxHEv54ULyM4BhTkrGKDITdbv34X5bEDFp6gRx_4/edit#gid=1562980960',
  sheet=1,
  col_types='c'
)

planted = planted_raw |> 
  select(`#`:Notes) |> 
  rename(Num=`#`) |> 
  mutate(Num=parse_number(Num),
         Year = year(mdy(`Date Planted`, quiet=TRUE)),
         Year = if_else(is.na(Year), parse_number(`Date Planted`), Year),
         dead = is.na(Species))

# Fill in species, genus and family for the dead trees
living = planted |> filter(!dead)
dead = planted |> 
  filter(dead) |> 
  select(-Species, -Genus, -Family)

species_lookup = living |> 
  select(`Common Name`, Species, Genus, Family) |> 
  unique()

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
planted = bind_rows(living, dead)

# Helpers
int_pct = function(val) scales::percent(val, accuracy=1)
