library(tidyverse)
library(sf)

# Read and clean Davey inventory file
inventory = read_sf(here::here(
  'Shapefiles/shade_tree_data_package/data/trees.shp')) |> 
  st_drop_geometry() |> 
  # Some cleanup
  mutate(SPP_BOT = case_match(
    SPP_BOT,
    'X Cupressocyparis leylandii' ~ 'Cupressus × leylandii', # Parseable variant
    "Tillia cordata 'greenspire'" ~ "Tilia cordata 'greenspire'", # Misspellings
    'Ginko biloba' ~ 'Ginkgo biloba',
    "Ginko biloba 'princeton sentry'" ~ "Ginkgo biloba 'princeton sentry'",
    .default = SPP_BOT
    )) |> 
  mutate(Genus = str_split_i(SPP_BOT, ' ', 1),
         Binomial = str_remove(SPP_BOT, " *'.*'$")) |> 
  mutate(Binomial = if_else(Binomial=='Gleditsia triacanthos inermis', 
                            'Gleditsia triacanthos', Binomial))
