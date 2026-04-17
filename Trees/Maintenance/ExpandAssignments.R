# Functions for expanding tree assignments into spatial polygons.

library(sf)
library(tidyverse)
library(here)

source(here::here('Trees/Maintenance/AssignPeopleToTrees.R'))

# Read a named assignment and write per-person convex hull polygons to
# Assignments.gpkg, using the assignment name as the layer name.
#
# Hulls are built from tree locations only (person location not included),
# with a 5-meter buffer.
#
# @param name character; assignment name (CSV stem passed to load_assignments)
# @return sf object of hull polygons, invisibly
assignment_polygons = function(name) {
  assigned = load_assignments(name)

  hulls = assigned |>
    group_by(person_id) |>
    summarize(geom = st_union(geom), .groups = 'drop') |>
    mutate(geom = st_convex_hull(geom) |> st_buffer(5)) |>
    st_set_geometry('geom')

  gpkg_path = here::here('Trees/Maintenance/assignments/Assignments.gpkg')
  st_write(hulls, gpkg_path, layer = name, delete_layer = TRUE, quiet = TRUE)

  invisible(hulls)
}

# Read a named polygon layer, assign new trees to the nearest polygon,
# recompute hulls incorporating the new trees, and write the result as
# a new layer named "<layer_name>-expanded".
#
# @param layer_name character; layer to read from Assignments.gpkg
# @param new_trees sf object of new tree locations
# @return sf object of expanded hull polygons, invisibly
expand_assignments = function(layer_name, new_trees) {
  gpkg_path = here::here('Trees/Maintenance/assignments/Assignments.gpkg')
  polygons = st_read(gpkg_path, layer = layer_name, quiet = TRUE)

  new_trees = st_transform(new_trees, st_crs(polygons))

  # Assign each new tree to the nearest polygon
  nearest_idx = st_nearest_feature(new_trees, polygons)
  assigned_trees = st_sf(
    person_id = polygons$person_id[nearest_idx],
    geom      = st_geometry(new_trees),
    crs       = st_crs(polygons)
  )

  # Union original polygon with assigned trees per person, then re-hull
  expanded = bind_rows(
    polygons |> select(person_id, geom),
    assigned_trees
  ) |>
    group_by(person_id) |>
    summarize(geom = st_union(geom), .groups = 'drop') |>
    mutate(geom = st_convex_hull(geom) |> st_buffer(5)) |>
    st_set_geometry('geom')

  new_layer = paste0(layer_name, '-expanded')
  st_write(expanded, gpkg_path, layer = new_layer, delete_layer = TRUE, quiet = TRUE)

  invisible(expanded)
}
