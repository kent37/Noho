# Explore shade tree data

library(tidyverse)
library(mapview)
library(sf)

trees = read_sf(here::here(
  'Shapefiles/shade_tree_data_package/data/trees.shp')) |> 
  mutate(genus = str_split_i(SPP_BOT, ' ', 1), .after=3)

trees |> st_drop_geometry() |> count(genus, sort=TRUE)

stumps = read_sf(here::here(
  'Shapefiles/shade_tree_data_package/data/stumps.shp'))

vacant = read_sf(here::here(
  'Shapefiles/shade_tree_data_package/data/vacants.shp'))

mapview(trees, col.regions='green') + 
  mapview(stumps, col.regions='black') +
  mapview(vacant, col.regions='red')

# Planted from Explore_planting.R

# Do the genera match?
n_distinct(trees$genus) # 74
sum(!unique(trees$genus) %in% planted$Genus) # 43 !

setdiff(unique(trees$genus), unique(planted$Genus))


# Can we find vacant tree wells that have not been planted?
# We have to look at counts by address to avoid meaningless
# one-to-many and many-to-many joins
vacant_counts =  vacant |>
  summarize(.by=c(ADDRESS, STREET), Vacant=n())
planted_counts = planted |> 
               summarize(.by=c(Num, Street), Planted=n())

still_vacant = vacant_counts |> 
  left_join(planted_counts, by=c(ADDRESS='Num', STREET='Street')) |> 
  mutate(Planted=replace_na(Planted, 0))  |> 
  filter(Vacant > Planted) |> 
  mutate(remaining_vacant = Vacant-Planted)

sum(still_vacant$remaining_vacant) # 1669

# This is very slow for some reason
mapview(still_vacant)

# One dot per location
# doesn't work
st_geometry(still_vacant) = st_as_sfc(map(st_geometry(still_vacant), ~st_cast(.x, 'POINT')))
mapview(still_vacant)

# This doesn't work, we have only geocoded the planting locations...
planting_locations = read_sf(here::here('Shapefiles/Tree_locations.gpkg')) |> 
  st_transform(crs=4326) |> 
  select(Num, Street=Street_orig, Ward)

still_vacant_locs = still_vacant |> 
  st_drop_geometry() |> 
  left_join(planting_locations, by=c(ADDRESS='Num', STREET='Street'))
