library(tidyverse)
library(sf)

source(here::here('Trees/Maintenance/AssignPeopleToTrees.R'))

# Read pre-defined polygon areas, drop JFK School
areas = st_read(here::here('Trees/Maintenance/assignments/Assignments.gpkg',
                           layer='Original_Assignments'), quiet = TRUE) |>
  filter(Description != 'JFK School') |>
  st_transform(crs = 26986) |>
  mutate(polygon_idx = row_number())

# Assign each tree to a polygon via spatial join
trees_in_polygons = trees |>
  st_join(areas |> select(polygon_idx, Description), join = st_within)

# Trees that fall outside all polygons — assign to nearest polygon
outside = trees_in_polygons |> filter(is.na(polygon_idx))
if (nrow(outside) > 0) {
  nearest_polygon = st_nearest_feature(outside, areas)
  trees_in_polygons$polygon_idx[is.na(trees_in_polygons$polygon_idx)] = nearest_polygon
  trees_in_polygons$Description[is.na(trees_in_polygons$Description)] =
    areas$Description[nearest_polygon]
}

n_polygons = nrow(areas)

# --- Assign polygons to people -----------------------------------------------
# Same algorithm as the cluster-to-person step in assign_geographic_clusters:
# each person i receives exactly people$count[i] polygons; process polygons
# nearest-to-some-person first, assign each to closest person with capacity.

dist_tbl = function(from, to) {
  st_distance(from, to) |>
    units::drop_units() |>
    as_tibble(.name_repair = ~ paste0('d', seq_along(.)))
}

# Use polygon centroids as cluster centers
centers = areas |>
  st_centroid() |>
  select(polygon_idx)

cluster_person_dists = dist_tbl(centers, people)

ranked_people_per_polygon = cluster_person_dists |>
  mutate(polygon_idx = row_number()) |>
  pivot_longer(-polygon_idx, names_to = 'person', values_to = 'dist') |>
  mutate(person_idx = as.integer(str_remove(person, 'd'))) |>
  arrange(polygon_idx, dist) |>
  group_by(polygon_idx) |>
  summarize(ranked = list(person_idx))

polygon_order = cluster_person_dists |>
  rowwise() |>
  mutate(min_d = min(c_across(everything()))) |>
  ungroup() |>
  mutate(polygon_idx = row_number()) |>
  arrange(min_d) |>
  pull(polygon_idx)

person_capacity   = people$count
person_totals     = rep(0L, nrow(people))
polygon_to_person = rep(NA_integer_, n_polygons)

for (pl in polygon_order) {
  candidates = ranked_people_per_polygon$ranked[[pl]]
  for (pid in candidates) {
    if (person_totals[pid] < person_capacity[pid]) {
      polygon_to_person[pl] = pid
      person_totals[pid]    = person_totals[pid] + 1L
      break
    }
  }
}

# Attach person_id to each tree
assigned = trees_in_polygons |>
  mutate(person_id = polygon_to_person[polygon_idx]) |>
  select(-polygon_idx, -Description)

save_assignments(assigned, 'polygon_areas')

cat('Saved as polygon_areas\n')
cat('Trees per person:\n')
print(summarize_counts(assigned))
