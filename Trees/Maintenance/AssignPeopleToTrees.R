# Try different ways to group trees for maintenance

library(tidyverse)
library(gt)
library(leaflet)
library(leaflet.extras)
library(sf)

make_address = function(num, street) {
  if_else(is.na(num), street, glue::glue('{num} {street}'))
}

# Trees for 2023 and newer
trees = read_sf(here::here('Trees/Planting_by_year.gpkg'), 'map_data') |> 
  st_transform(26986) |> # Use MA Mainland CRS for consistent geometry
  filter(Year >= 2023) |> # For now, just recent trees
  mutate(Street = str_to_title(Street),
         Addr = make_address(Num, Street))

# Write a GeoJSON file for import into geojson.io
# Use the colors from Planting by year
# year_colors = tribble(
#   ~Year, ~`marker-color`,
#   2023, '#000000',
#   2024, '#a52a2a'
# )
# st_write(trees |> left_join(year_colors) |> select(`marker-color`) |> st_transform(4326),
#          here::here('Trees/RecentTrees.geojson'),
#         delete_dsn=TRUE, driver = "GeoJSON", 
#              layer_options = "RFC7946=YES")

# Read people and summarize to one row per address
people = read_csv(here::here('Trees/Maintenance/Tree Maintenance Roster_0206.csv'),
                  comment='#') |> 
  filter(!is.na(geometry)) |> 
  mutate(
    # Replace NBSP with regular space
    geometry = str_replace_all(geometry, "\u00a0", " ")
    ) |>
  summarize(count=n(), .by=c(Num, Street, geometry)) |> 
  arrange(Street) |> 
  mutate(person_id = row_number(), 
         Street = replace_values(Street, "xBridge Rd" ~ "Bridge Rd"), # fix a hack
         Addr = make_address(Num, Street)) |>
  st_as_sf(wkt='geometry', crs=26986) |>
  # Assign colors to addresses here for consistency
  # This the Paul Tol rainbow palette
  mutate(color = khroma::color('discrete rainbow')(n()))

noho = read_sf(here::here('Shapefiles/Noho_outline/Noho_outline.gpkg')) |> 
  st_geometry()

# Compute assignment quality metrics
#
# @param assigned sf object with tree assignments (must have 'person_id' and 'count' columns)
# @param people sf object with people locations
# @return named list: cv, total_miles, max_single_tree_miles
compute_metrics = function(assigned, people) {
  # Normalize tree counts by number of people at each location so that
  # locations with more people are not penalized for having more trees
  counts = tibble(person_id = seq_len(nrow(people)), n_people = people$count) |>
    left_join(
      assigned |> st_drop_geometry() |> summarize(count = sum(count), .by = person_id),
      join_by(person_id)
    ) |>
    replace_na(list(count = 0)) |>
    mutate(trees_per_person = count / n_people) |>
    pull(trees_per_person)

  cv = sd(counts) / mean(counts)

  # x = sort(counts)
  # n = length(x)
  # gini = sum((2 * seq_len(n) - n - 1) * x) / (n * sum(x))

  dists_m = assigned |>
    rowwise() |>
    mutate(distance_m = as.numeric(st_distance(geom, people$geometry[person_id]))) |>
    ungroup() |>
    pull(distance_m)

  total_miles           = sum(dists_m) * 0.000621371
  max_single_tree_miles = max(dists_m) * 0.000621371

  # Total convex hull perimeter across all volunteers (trees + person location)
  hull_perimeter_miles = map_dbl(seq_len(nrow(people)), \(pid) {
    tree_geoms  = st_geometry(assigned)[assigned$person_id == pid]
    person_geom = st_geometry(people)[pid]
    hull = st_union(c(tree_geoms, person_geom)) |> st_convex_hull()
    tryCatch(as.numeric(st_perimeter(hull)), error = \(e) 0)
  }) |> sum() * 0.000621371

  # Normalized composite score (lower is better).
  # Distances are normalized by Voronoi baselines stored in voronoi_total_miles
  # and voronoi_max_miles, which are set after trees_voronoi is computed.
  score = tryCatch(
    cv + total_miles / voronoi_total_miles + 
      max_single_tree_miles / voronoi_max_miles +
      hull_perimeter_miles / voronoi_hull_perimeter,
    error = \(e) NA_real_
  )

  list(
    cv                    = cv,
#    gini                  = gini,
    total_miles           = total_miles,
    max_single_tree_miles = max_single_tree_miles,
    hull_perimeter_miles  = hull_perimeter_miles,
    score                 = score
  )
}

# Summarize tree assignments by person
#
# Computes the total number of trees assigned to each person location.
#
# @param assigned sf object with tree assignments (must have 'person_id' and 'count' columns)
# @return gt table with per-person counts and distances, plus assignment quality metrics
summarize_assignments = function(assigned, people, title=NULL) {
  if (is.null(title)) title = deparse(substitute(assigned))

  stats = summarize_counts(assigned) |>
    left_join(summarize_distances(assigned, people), join_by(person_id))

  data = people |> st_drop_geometry() |> select(person_id, Addr) |>
    left_join(stats, join_by(person_id)) |>
    replace_na(list(count=0, total_miles=0))

  metrics = compute_metrics(assigned, people)

  data |>
#    select(-person_id) |>
    gt(rowname_col='person_id') |>
    tab_header(title=title,
               subtitle=glue::glue(
      'CV: {round(metrics$cv, 2)}  |  ',
      'Max tree distance: {round(metrics$max_single_tree_miles, 1)} mi  |  ',
      'Score: {round(metrics$score, 2)}'
    )) |>
    cols_align('left', columns='Addr') |>
    cols_label(Addr='Address',
               count=gt::html('Number<br>of trees'),
               total_miles=gt::html('Total<br>miles')) |>
    fmt_number(total_miles, decimals=1) |>
    grand_summary_rows(
      columns = total_miles,
      fns = list(Total ~ sum(.)),
      fmt = ~fmt_number(., decimals=1)
    ) |>
    tab_options(table.font.size='70%', data_row.padding='4px') |>
    opt_css('div.gt_container { padding-top: 0 !important; padding-bottom: 0 !important; }')
}

# Compare assignment algorithms using rank aggregation
#
# Computes metrics for each algorithm and ranks them on CV, total distance,
# and max single-tree distance. Lower rank sum = better overall assignment.
#
# @param assignments named list of sf objects with tree assignments
# @param people sf object with people locations
# @return gt table sorted by rank sum
compare_assignments = function(assignments, people) {
  map(assignments, \(a) as_tibble(compute_metrics(a, people))) |>
    list_rbind(names_to = 'algorithm') |>
    mutate(
      rank_cv        = rank(cv),
      rank_total     = rank(total_miles),
      rank_max       = rank(max_single_tree_miles),
      rank_perimeter = rank(hull_perimeter_miles),
      rank_sum       = rank_cv + rank_total + rank_max + rank_perimeter
    ) |>
#    arrange(rank_sum) |>
    select(algorithm, cv, total_miles, max_single_tree_miles,
           hull_perimeter_miles, score, rank_sum) |>
    gt() |>
    tab_header(title = 'Algorithm Comparison') |>
    cols_label(
      algorithm            = 'Algorithm',
      cv                   = 'CV',
      total_miles          = gt::html('Total<br>miles'),
      max_single_tree_miles = gt::html('Max tree<br>dist (mi)'),
      hull_perimeter_miles = gt::html('Hull<br>perim (mi)'),
      score                = 'Score',
      rank_sum             = gt::html('Rank<br>sum')
    ) |>
    fmt_number(c(cv, score), decimals = 2) |>
    fmt_number(c(total_miles, max_single_tree_miles, hull_perimeter_miles), decimals = 1) |>
    tab_options(table.font.size = '80%', data_row.padding = '4px')
}

# Summarize counts per person
summarize_counts = function(assigned) {
  assigned |>
    st_drop_geometry() |>
    summarize(count=sum(count), .by=person_id) |>
    arrange(person_id)
}

# Compute total distance to assigned trees per person
#
# For each person location, sums the point-to-point distances to all assigned
# tree locations. Does not weight by tree or people counts - just raw geometric
# distance summed across all assignments.
#
# @param assigned sf object with tree assignments (must have 'person_id' column)
# @param people sf object with people locations
# @return tibble with person_id and total_distance_miles
summarize_distances = function(assigned, people) {
  assigned |>
    rowwise() |>
    mutate(
      distance_m = as.numeric(st_distance(geom, people$geometry[person_id]))
    ) |>
    ungroup() |>
    st_drop_geometry() |>
    summarize(
      total_distance_m = sum(distance_m),
      .by = person_id
    ) |>
    mutate(total_miles = total_distance_m * 0.000621371) |>
    select(person_id, total_miles) |>
    arrange(person_id)
}

# Display tree assignments on an interactive map
#
# Creates a leaflet visualization with trees colored by assigned person_id.
#
# @param assigned sf object with tree assignments (must have 'person_id' column)
# @param people sf object with people locations
# @return leaflet object
show_assignments = function(assigned, people) {
  map_data = left_join(assigned, st_drop_geometry(people),
                       join_by(person_id), suffix=c('.tree', '')) |>
    st_transform(4326) |> 
    mutate(label=glue::glue('{Addr.tree} - {count.tree} trees<br>(by {Addr})') |> 
             lapply(htmltools::HTML) |> unname())
  
  people_pts = people |>
    st_transform(4326) |>
    select(person_id, color) |>
    rename(geom = geometry)

  hulls = map_data |>
    select(person_id, geom, color) |>
    bind_rows(people_pts) |>
    group_by(person_id) |>
    summarize(geom = st_union(geom), color=color[1]) |>
    mutate(hull = st_convex_hull(geom) |> st_buffer(50)) |>
    st_set_geometry('hull')
  
  people_data = people |> 
    left_join(summarize_counts(assigned), join_by(person_id), suffix=c('', '.tree')) |>
    mutate(
      count.tree = replace_na(count.tree, 0),
      label = glue::glue('{Addr} ({count.tree} trees)'))
  
  bbox = st_bbox(st_union(st_geometry(map_data), 
                          st_geometry(people_data) |> st_transform(4326)))

  leaflet(width='100%', height='60vh') |>
    fitBounds(bbox[['xmin']], bbox[['ymin']], bbox[['xmax']], bbox[['ymax']]) |>
    addProviderTiles('CartoDB.Positron', group='Street') |> 
    addProviderTiles('Esri.WorldImagery', group='Satellite') |> 
    addPolygons(data=hulls, fillColor=~color, stroke=FALSE, fillOpacity=0.3,
                group='Groups') |> 
    addCircleMarkers(data=map_data, radius=~5+sqrt(count.tree), label=~label,
                   group='Trees',
                   stroke=FALSE, fillColor=~color, fillOpacity=1) |> 
    addCircleMarkers(data = people_data |> st_transform(4326),
                     label = ~label, group='Maintainers',
                     color='black', fillColor=~color, fillOpacity=1,
                     weight=2, radius=4)|> 
    addLayersControl(baseGroups=c('Street', 'Satellite'),
      overlayGroups=c('Trees', 'Maintainers', 'Groups'),
      options=layersControlOptions(collapsed=FALSE)) |> 
    # addLegend(position='bottomright',
    #           colors=people$color, labels=people$Street, opacity=1)|>
    addFullscreenControl()
}

# Algorithm 1: Voronoi (Nearest-Person Assignment)
#
# Assigns each tree to the nearest person location using Voronoi tessellation.
# Creates Voronoi polygons around each person location and assigns trees via
# spatial join. This is the simplest allocation that purely optimizes for
# spatial proximity without considering balance.
#
# @param trees sf object with tree locations (must have 'count' column)
# @param people sf object with people locations (must have 'count' column)
# @return sf object of trees with added 'person_id' column
segment_voronoi = function(trees, people) {
  polys = st_collection_extract(st_voronoi(do.call(c, st_geometry(people)), 
                                   envelope=noho, point_order=TRUE)) |> 
    st_sf(crs=st_crs(people)) |> 
    st_intersection(noho)
  
  polys = st_set_geometry(polys, 'geometry')
  st_join(trees, polys |> mutate(person_id = row_number()))
}

# Algorithm 2: Capacitated Nearest-Neighbor (Greedy Balanced)
#
# Assigns trees to their nearest person while enforcing capacity constraints.
# Processes trees in order of their minimum distance to any person (closest first).
# Each person location has capacity based on the number of people there.
# If the nearest person is at capacity, assigns to the next-nearest person with capacity.
#
# This produces a more balanced allocation than pure Voronoi while still prioritizing
# spatial proximity. Some trees may not be assigned to their absolute nearest person.
#
# @param trees sf object with tree locations (must have 'count' column)
# @param people sf object with people locations (must have 'count' column)
# @param tolerance acceptable overflow fraction above capacity (default: 0.1 = 10%)
# @return sf object of trees with added 'person_id' column
assign_capacitated_nearest = function(trees, people, tolerance = 0.1) {
  # Compute distance matrix: trees x people
  dist_matrix = st_distance(trees, people)

  # Compute target trees per individual person
  total_trees = sum(trees$count)
  total_people = sum(people$count)
  target_per_person = total_trees / total_people

  # Capacity for each people location (people at that location × target per person)
  capacities = people$count * target_per_person

  # Initialize people location running totals
  person_totals = rep(0, nrow(people))

  # For each tree, find minimum distance to any person
  min_dists = apply(dist_matrix, 1, min)

  # Sort tree indices by minimum distance (process closest trees first)
  tree_order = order(min_dists)

  # Initialize assignment vector
  assignments = rep(NA_integer_, nrow(trees))

  # Greedy assignment
  for (i in tree_order) {
    tree_count = trees$count[i]

    # Rank people by distance to this tree
    person_order = order(dist_matrix[i, ])

    # Try to assign to closest person location that stays at or below capacity
    assigned = FALSE
    for (person_idx in person_order) {
      new_total = person_totals[person_idx] + tree_count

      if (new_total <= capacities[person_idx] * (1 + tolerance)) {
        assignments[i] = person_idx
        person_totals[person_idx] = new_total
        assigned = TRUE
        break
      }
    }

    # If no location has capacity, assign to closest location anyway
    if (!assigned) {
      person_idx = person_order[1]
      assignments[i] = person_idx
      person_totals[person_idx] = person_totals[person_idx] + tree_count
    }
  }

  # Add person_id to trees
  trees |>
    mutate(person_id = assignments)
}

# Post-processing: Pairwise Swap to Reduce Total Distance
#
# Iteratively swaps tree assignments between people to reduce total travel
# distance. For each pair of trees assigned to different people, swaps their
# assignments if it reduces total distance and keeps both people within
# tolerance of their capacity.
#
# Can be applied after any allocation algorithm.
#
# @param assigned sf object with tree assignments (must have 'person_id' and 'count' columns)
# @param people sf object with people locations (must have 'count' column)
# @param tolerance acceptable overflow fraction above capacity (default: 0.1 = 10%)
# @param max_iter maximum swap rounds (default: 20)
# @return sf object with updated 'person_id' column
swap_assignments = function(assigned, people, tolerance = 0.1, max_iter = 20) {
  dist_matrix = as.numeric(st_distance(assigned, people))
  dim(dist_matrix) = c(nrow(assigned), nrow(people))

  total_trees = sum(assigned$count)
  total_people = sum(people$count)
  capacities = people$count * (total_trees / total_people)

  assignments = assigned$person_id

  for (iter in seq_len(max_iter)) {
    made_swap = FALSE

    for (i in seq_len(nrow(assigned) - 1)) {
      for (j in (i + 1):nrow(assigned)) {
        a = assignments[i]
        b = assignments[j]
        if (a == b) next

        # Distance saving from swapping
        saving = (dist_matrix[i, a] + dist_matrix[j, b]) -
                 (dist_matrix[i, b] + dist_matrix[j, a])
        if (saving <= 0) next

        # Check balance: swapping shifts counts between a and b
        delta = assigned$count[i] - assigned$count[j]
        totals_a = sum(assigned$count[assignments == a])
        totals_b = sum(assigned$count[assignments == b])
        new_a = totals_a - delta
        new_b = totals_b + delta

        if (new_a <= capacities[a] * (1 + tolerance) &&
            new_b <= capacities[b] * (1 + tolerance) &&
            new_a >= 0 && new_b >= 0) {
          assignments[i] = b
          assignments[j] = a
          made_swap = TRUE
        }
      }
    }

    if (!made_swap) break
  }

  assigned |> mutate(person_id = assignments)
}

# Algorithm 3: Optimal Transport (Greedy Auction)
#
# Uses a greedy auction-based approach to approximate optimal transport.
# Assigns trees by maximizing benefit (negative distance × tree count) weighted by
# how much each person needs trees (capacity gap). This produces assignments similar
# to optimal transport but runs in O(n²) time instead of requiring LP solving.
#
# Multiple rounds allow refinement: trees can be reassigned from over-capacity
# people to better balance the allocation.
#
# @param trees sf object with tree locations (must have 'count' column)
# @param people sf object with people locations (must have 'count' column)
# @param num_rounds number of auction rounds for refinement (default: 3)
# @return sf object of trees with added 'person_id' column
assign_optimal_transport = function(trees, people, num_rounds = 3) {
  # Compute distance matrix (drop units)
  dist_matrix = as.numeric(st_distance(trees, people))
  dim(dist_matrix) = c(nrow(trees), nrow(people))

  # Compute target trees per individual person
  total_trees = sum(trees$count)
  total_people = sum(people$count)
  target_per_person = total_trees / total_people

  # Capacity for each people location
  capacities = people$count * target_per_person

  # Initialize assignments and running totals
  assignments = rep(NA_integer_, nrow(trees))
  person_totals = rep(0, nrow(people))

  # Compute base benefit: negative distance weighted by tree count
  benefit_base = -dist_matrix * trees$count

  # Greedy auction over multiple rounds
  for (round in 1:num_rounds) {
    # Weight benefits by capacity gaps (people with more gap are more attractive)
    capacity_gaps = pmax(capacities - person_totals, 0)
    capacity_weights = (capacity_gaps / capacities + 0.1)  # Add baseline to avoid zeros

    benefit_matrix = sweep(benefit_base, 2, capacity_weights, "*")

    # Assign unassigned trees
    unassigned = which(is.na(assignments))
    if (length(unassigned) == 0) break

    for (i in unassigned) {
      best_person = which.max(benefit_matrix[i, ])
      assignments[i] = best_person
      person_totals[best_person] = person_totals[best_person] + trees$count[i]
    }

    # If not last round, consider reassigning to improve balance
    if (round < num_rounds) {
      over_idx = which(person_totals > capacities * 1.1)
      if (length(over_idx) > 0) {
        for (over_person in over_idx) {
          their_trees = which(assignments == over_person)
          if (length(their_trees) > 1) {
            # Find their farthest tree
            dists = dist_matrix[their_trees, over_person]
            farthest = their_trees[which.max(dists)]
            # Unassign for reassignment
            assignments[farthest] = NA
            person_totals[over_person] = person_totals[over_person] - trees$count[farthest]
          }
        }
      }
    }
  }

  trees |> mutate(person_id = assignments)
}

# Algorithm 4: Voronoi with Rebalancing
#
# Starts with Voronoi assignment, then iteratively transfers boundary trees
# from over-allocated to under-allocated people to improve balance while
# preserving spatial coherence.
#
# Trees are transferred only if they are near the boundary (distance ratio below
# threshold) and the transfer improves balance. Each person location's capacity
# is based on the number of people there.
#
# @param trees sf object with tree locations (must have 'count' column)
# @param people sf object with people locations (must have 'count' column)
# @param max_iter maximum number of rebalancing iterations (default: 10)
# @param distance_ratio_threshold max ratio of distances for boundary transfers (default: 1.5)
# @param tolerance acceptable deviation from capacity as fraction (default: 0.1 = 10%)
# @return sf object of trees with added 'person_id' column
assign_voronoi_rebalanced = function(trees, people, max_iter = 10,
                                     distance_ratio_threshold = 1.5,
                                     tolerance = 0.1) {
  # Add tree IDs for tracking
  trees_with_id = trees |> mutate(tree_id = row_number())

  # Start with Voronoi assignment
  assigned = segment_voronoi(trees_with_id, people)

  # Compute target trees per individual person
  total_trees = sum(trees$count)
  total_people = sum(people$count)
  target_per_person = total_trees / total_people

  # Capacity for each people location
  capacities = people$count * target_per_person

  # Compute distance matrix (trees x people)
  dist_matrix = st_distance(trees, people)

  # Iterative rebalancing
  for (iter in 1:max_iter) {
    # Compute current totals per person location
    current_totals = assigned |>
      st_drop_geometry() |>
      summarize(total = sum(count), .by = person_id) |>
      complete(person_id = 1:nrow(people), fill = list(total = 0)) |>
      mutate(
        capacity = capacities[person_id],
        diff = total - capacity,
        status = case_when(
          diff > capacity * tolerance ~ "over",
          diff < -capacity * tolerance ~ "under",
          .default = "balanced"
        )
      )

    # If all balanced, stop
    if (all(current_totals$status == "balanced")) {
      break
    }

    over_allocated = current_totals |> filter(status == "over") |> pull(person_id)
    under_allocated = current_totals |> filter(status == "under") |> pull(person_id)

    if (length(over_allocated) == 0 || length(under_allocated) == 0) {
      break
    }

    # Find potential transfers from over-allocated to under-allocated
    candidates = assigned |>
      st_drop_geometry() |>
      filter(person_id %in% over_allocated)

    if (nrow(candidates) == 0) break

    transfers = candidates |>
      select(tree_id, from_id = person_id, tree_count = count) |>
      cross_join(tibble(to_id = under_allocated)) |>
      rowwise() |>
      mutate(
        dist_to_current = as.numeric(dist_matrix[tree_id, from_id]),
        dist_to_neighbor = as.numeric(dist_matrix[tree_id, to_id]),
        ratio = dist_to_neighbor / dist_to_current
      ) |>
      ungroup() |>
      filter(ratio <= distance_ratio_threshold) |>
      arrange(ratio)

    if (nrow(transfers) == 0) {
      break
    }

    # Execute transfers that improve balance
    made_transfer = FALSE
    for (t in 1:nrow(transfers)) {
      transfer = transfers[t, ]

      from_total = current_totals$total[current_totals$person_id == transfer$from_id]
      to_total = current_totals$total[current_totals$person_id == transfer$to_id]
      from_capacity = capacities[transfer$from_id]
      to_capacity = capacities[transfer$to_id]

      new_from_total = from_total - transfer$tree_count
      new_to_total = to_total + transfer$tree_count

      # Check if transfer improves balance
      from_improves = abs(new_from_total - from_capacity) < abs(from_total - from_capacity)
      to_doesnt_worsen = abs(new_to_total - to_capacity) <= abs(to_total - to_capacity) + to_capacity * tolerance

      if (from_improves && to_doesnt_worsen) {
        # Execute transfer
        assigned$person_id[assigned$tree_id == transfer$tree_id] = transfer$to_id

        # Update running totals
        current_totals$total[current_totals$person_id == transfer$from_id] = new_from_total
        current_totals$total[current_totals$person_id == transfer$to_id] = new_to_total

        # Update status
        current_totals = current_totals |>
          mutate(
            diff = total - capacity,
            status = case_when(
              diff > capacity * tolerance ~ "over",
              diff < -capacity * tolerance ~ "under",
              .default = "balanced"
            )
          )

        made_transfer = TRUE
      }
    }

    if (!made_transfer) {
      break
    }
  }

  # Remove tree_id before returning
  assigned |> select(-tree_id)
}

# Algorithm 5: Weighted Voronoi (Iterative Power Diagram)
#
# Creates a weighted Voronoi tessellation (power diagram) by iteratively adjusting
# weights for each person location. Higher weights make a location "attract" more
# distant trees, effectively expanding their cell.
#
# In each iteration:
# - Trees are assigned by minimizing (distance^2 - weight) to each person
# - Weights are adjusted: under-allocated locations get higher weights (more attractive),
#   over-allocated locations get lower weights (less attractive)
# - Converges when all locations are within tolerance of their capacity
#
# This produces clean spatial partitions with approximately balanced allocations.
# Each person location's capacity is based on the number of people there.
#
# @param trees sf object with tree locations (must have 'count' column)
# @param people sf object with people locations (must have 'count' column)
# @param max_iter maximum iterations (default: 50)
# @param tolerance convergence tolerance as fraction of capacity (default: 0.05 = 5%)
# @return sf object of trees with added 'person_id' column
assign_weighted_voronoi = function(trees, people, max_iter = 200, tolerance = 0.05) {
  # Compute target trees per individual person
  total_trees = sum(trees$count)
  total_people = sum(people$count)
  target_per_person = total_trees / total_people

  # Capacity for each people location
  capacities = people$count * target_per_person

  # Initialize weights to 0
  weights = rep(0, nrow(people))

  # Compute distance matrix (drop units to avoid units mismatch with weights)
  dist_matrix = st_distance(trees, people)
  dist_matrix_sq = as.numeric(dist_matrix)^2
  dim(dist_matrix_sq) = dim(dist_matrix)  # Restore matrix dimensions

  # Per-person learning rates, floored at the median nn_dist_sq so that central
  # volunteers (small nn_dist_sq) still converge within max_iter. Distant
  # volunteers (large nn_dist_sq) get larger alphas to accumulate enough weight
  # to win trees.
  nn_dist_sq = apply(dist_matrix_sq, 2, min)
  alpha_floor = median(nn_dist_sq)
  alphas = pmax(nn_dist_sq, alpha_floor) / (capacities * max_iter * 0.5)

  # Iterative assignment — track the best allocation seen across all iterations
  # since the algorithm can oscillate past a good solution
  best_assigned    = NULL
  best_max_rel_diff = Inf

  for (iter in 1:max_iter) {
    # Assign each tree to person minimizing distance^2 - weight
    # Higher weight = more attractive (lower effective distance)
    assignments = apply(sweep(dist_matrix_sq, 2, weights, "-"), 1, which.min)

    # Add assignments to trees
    assigned = trees |> mutate(person_id = assignments)

    # Compute current totals per person location
    current_totals = assigned |>
      st_drop_geometry() |>
      summarize(total = sum(count), .by = person_id) |>
      complete(person_id = 1:nrow(people), fill = list(total = 0)) |>
      arrange(person_id)

    # Track best allocation
    diffs = current_totals$total - capacities
    max_rel_diff = max(abs(diffs) / capacities)

    if (max_rel_diff < best_max_rel_diff) {
      best_max_rel_diff = max_rel_diff
      best_assigned = assigned
    }

    if (max_rel_diff <= tolerance) break

    # Update weights: locations with too few trees get higher weight (more attractive)
    # locations with too many trees get lower weight (less attractive)
    weights = weights - alphas * diffs
  }

  best_assigned
}

# Algorithm 6: Draft Pick (Highest Need First)
#
# Volunteers take turns picking their nearest available tree location.
# At each step, the volunteer with the most remaining capacity (furthest below
# their fair share) picks next. This guarantees all volunteers receive trees,
# including those geographically far from the main cluster.
#
# @param trees sf object with tree locations (must have 'count' column)
# @param people sf object with people locations (must have 'count' column)
# @return sf object of trees with added 'person_id' column
assign_draft = function(trees, people) {
  dist_matrix = as.numeric(st_distance(trees, people))
  dim(dist_matrix) = c(nrow(trees), nrow(people))

  total_trees = sum(trees$count)
  total_people = sum(people$count)
  capacities = people$count * (total_trees / total_people)

  assignments = rep(NA_integer_, nrow(trees))
  available = rep(TRUE, nrow(trees))
  person_totals = rep(0, nrow(people))

  for (pick in seq_len(nrow(trees))) {
    # Volunteer with most remaining need picks next
    current_person = which.max(capacities - person_totals)

    # Their nearest available tree location
    available_idx = which(available)
    nearest = available_idx[which.min(dist_matrix[available_idx, current_person])]

    assignments[nearest] = current_person
    available[nearest] = FALSE
    person_totals[current_person] = person_totals[current_person] + trees$count[nearest]
  }

  trees |> mutate(person_id = assignments)
}

# Algorithm 7: Geographic Clustering (Balanced k-means)
#
# Creates k = sum(people$count) geographically compact, balanced clusters of
# trees, ignoring volunteer locations. Uses k-means++ initialization followed
# by iterative balanced assignment (trees go to nearest centroid with remaining
# capacity). Clusters are then assigned to people by a second capacitated greedy
# step, where person i receives exactly people$count[i] clusters.
#
# @param trees sf object with tree locations (must have 'count' column)
# @param people sf object with people locations (must have 'count' column)
# @param max_iter maximum k-means iterations (default: 50)
# @param tolerance convergence tolerance as fraction of capacity (default: 0.1 = 10%)
# @return sf object of trees with added 'person_id' column
assign_geographic_clusters = function(trees, people, max_iter = 50, tolerance = 0.1) {
  k           = sum(people$count)
  total_trees = sum(trees$count)
  capacity    = total_trees / k   # target tree-count per cluster

  # Helper: convert an st_distance() result to a plain tibble with
  # columns named "d1", "d2", ... (one per column of the distance matrix)
  dist_tbl = function(from, to) {
    st_distance(from, to) |>
      units::drop_units() |>
      as_tibble(.name_repair = ~ paste0('d', seq_along(.)))
  }

  # --- k-means++ initialization -------------------------------------------
  # First center is a random tree; each subsequent center is sampled with
  # probability proportional to squared distance from the nearest existing center.
  set.seed(4)
  centers = trees[sample.int(nrow(trees), 1), ] |> select(geom)

  for (i in seq_len(k - 1)) {
    min_dist_sq = dist_tbl(trees, centers) |>
      rowwise() |>
      mutate(min_d = min(c_across(everything())) ^ 2) |>
      pull(min_d)

    new_idx = sample.int(nrow(trees), 1, prob = min_dist_sq)
    centers = bind_rows(centers, trees[new_idx, ] |> select(geom))
  }

  # --- Iterative balanced assignment ----------------------------------------
  # Each iteration:
  #   1. Compute distances from every tree to every center
  #   2. Pre-rank each tree's cluster preferences by distance
  #   3. Process trees in order of "commitment" (how much they prefer their
  #      nearest center over their second-nearest)
  #   4. Assign each tree to its nearest center with remaining capacity
  #   5. Recompute centers as weighted-mean coordinates of assigned trees
  cluster_assignments = rep(NA_integer_, nrow(trees))

  for (iter in seq_len(max_iter)) {
    distances = dist_tbl(trees, centers)

    # Pre-compute each tree's ranked cluster list (nearest first)
    ranked_clusters_per_tree = distances |>
      mutate(tree_idx = row_number()) |>
      pivot_longer(-tree_idx, names_to = 'cluster', values_to = 'dist') |>
      mutate(cluster_idx = as.integer(str_remove(cluster, 'd'))) |>
      arrange(tree_idx, dist) |>
      group_by(tree_idx) |>
      summarize(ranked = list(cluster_idx))

    # Process order: trees most committed to their nearest center go first
    tree_order = distances |>
      rowwise() |>
      mutate(
        d_sorted = list(sort(c_across(everything()))),
        min1     = d_sorted[1],
        min2     = d_sorted[2]
      ) |>
      ungroup() |>
      mutate(
        tree_idx   = row_number(),
        preference = min2 / (min1 + 1e-9)
      ) |>
      arrange(desc(preference)) |>
      pull(tree_idx)

    cluster_totals  = rep(0, k)
    new_assignments = rep(NA_integer_, nrow(trees))

    for (i in tree_order) {
      candidates = ranked_clusters_per_tree$ranked[[i]]
      for (cl in candidates) {
        if (cluster_totals[cl] + trees$count[i] <= capacity * (1 + tolerance)) {
          new_assignments[i] = cl
          cluster_totals[cl] = cluster_totals[cl] + trees$count[i]
          break
        }
      }
      if (is.na(new_assignments[i])) {
        # All clusters at capacity — assign to nearest regardless
        cl = candidates[1]
        new_assignments[i] = cl
        cluster_totals[cl] = cluster_totals[cl] + trees$count[i]
      }
    }

    # Update centers: weighted-mean coordinates of assigned trees
    centers = trees |>
      st_drop_geometry() |>
      mutate(
        cluster = new_assignments,
        x       = st_coordinates(trees)[, 1],
        y       = st_coordinates(trees)[, 2]
      ) |>
      summarize(
        x = sum(x * count) / sum(count),
        y = sum(y * count) / sum(count),
        .by = cluster
      ) |>
      arrange(cluster) |>
      st_as_sf(coords = c('x', 'y'), crs = st_crs(trees)) |>
      rename(geom = geometry)

    if (identical(cluster_assignments, new_assignments)) break
    cluster_assignments = new_assignments
  }
  print(str_glue('Cluster assignment ended after {iter} iterations'))
  
  # --- Assign clusters to people -------------------------------------------
  # Each person i receives exactly people$count[i] clusters.
  # Greedy: process clusters nearest-to-some-person first; assign each to
  # the closest person with remaining capacity.
  person_capacity   = people$count
  person_totals     = rep(0L, nrow(people))
  cluster_to_person = rep(NA_integer_, k)

  cluster_person_dists = dist_tbl(centers, people)

  ranked_people_per_cluster = cluster_person_dists |>
    mutate(cluster_idx = row_number()) |>
    pivot_longer(-cluster_idx, names_to = 'person', values_to = 'dist') |>
    mutate(person_idx = as.integer(str_remove(person, 'd'))) |>
    arrange(cluster_idx, dist) |>
    group_by(cluster_idx) |>
    summarize(ranked = list(person_idx))

  cluster_order = cluster_person_dists |>
    rowwise() |>
    mutate(min_d = min(c_across(everything()))) |>
    ungroup() |>
    mutate(cluster_idx = row_number()) |>
    arrange(min_d) |>
    pull(cluster_idx)

  for (cl in cluster_order) {
    candidates = ranked_people_per_cluster$ranked[[cl]]
    for (pid in candidates) {
      if (person_totals[pid] < person_capacity[pid]) {
        cluster_to_person[cl] = pid
        person_totals[pid]    = person_totals[pid] + 1L
        break
      }
    }
  }

  trees |> mutate(person_id = cluster_to_person[cluster_assignments])
}

# Save a tree assignment to a named CSV file
#
# Writes all columns of the assignment sf object to Trees/assignments/<name>.csv.
# The geom column is stored as WKT so the file is self-contained.
#
# @param assigned sf object with tree assignments
# @param name character; filename stem (no .csv extension)
# @return assigned invisibly
save_assignments = function(assigned, name) {
  dir = here::here('Trees/Maintenance/assignments')
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  assigned |>
    mutate(wkt = st_as_text(geom)) |>
    st_drop_geometry() |>
    write_csv(file.path(dir, paste0(name, '.csv')))
  invisible(assigned)
}

# Load a named tree assignment from CSV
#
# Reads Trees/assignments/<name>.csv and reconstructs the sf object.
# Stops with an informative error if required columns are missing.
#
# @param name character; filename stem (no .csv extension)
# @return sf object with tree assignments in CRS 26986
load_assignments = function(name) {
  path = here::here('Trees/Maintenance/assignments', paste0(name, '.csv'))
  data = read_csv(path, show_col_types = FALSE) |> 
    mutate(Ward = as.character(Ward))
  required = c('person_id', 'count', 'wkt', 'Addr')
  missing = setdiff(required, names(data))
  if (length(missing) > 0) {
    stop('load_assignments: missing required columns: ', paste(missing, collapse = ', '))
  }
  st_as_sf(data, wkt = 'wkt', crs = 26986) |> 
    rename(geom=wkt)
}

render_assignment = function(name, title = name) {
  quarto::quarto_render(
    here::here('Trees/Maintenance/ShowTreeAssignments.qmd'),
    execute_params = list(title = title, assignment_name = name),
    output_file    = paste0('Show', name, 'Assignment.html')
  )
}

# Voronoi baselines used to normalize distance metrics in compute_metrics.
# Hard-coded so scoring works independently of the voronoi assignment.
voronoi_total_miles    = 97.35843
voronoi_max_miles      =  3.860317
voronoi_hull_perimeter = 33.46792
