# Tree-to-People Spatial Allocation

## Problem Description

We have two sf point datasets in the same geographic area:

- **trees**: ~300 point features representing tree locations
- **people**: ~13 point features representing people locations

The goal is to assign every tree to exactly one person. We want to explore multiple allocation strategies that trade off between spatial proximity (each person gets the trees closest to them) and equity (each person gets roughly the same number of trees).

## Data Assumptions

- Both datasets are `sf` objects with point geometries in the same CRS (or can be transformed to match).
- Each row in `trees` represents a location that may contain multiple trees. The `count` column gives the number of trees at that location. A tree location is assigned as a whole unit — individual trees within a location cannot be split across people.
- The output for each algorithm should be `trees` with an added column identifying the assigned person (e.g., `person_id`).
- **Total trees** = `sum(trees$count)`. **Target allocation per person** = `total_trees / nrow(people)`. Because tree locations cannot be split, perfect balance is generally not achievable. Algorithms should minimize imbalance while respecting the indivisibility of each tree location.
- When evaluating balance, "number of trees" means the sum of `count` for assigned locations, not the number of locations.

## Technology Stack

- **R** with **tidyverse** and **sf** as core packages
- Prefer tidyverse idioms (pipes, dplyr verbs, tidy data structures)
- Additional packages as needed per algorithm (noted below)

## Algorithms to Implement

### Algorithm 1: Voronoi (Nearest-Person Assignment)

**Concept:** Build Voronoi polygons around the 13 people. Assign each tree to the person whose Voronoi cell it falls within. This is equivalent to assigning each tree to its single nearest person.

**Approach:**
1. Compute Voronoi polygons from people using `sf::st_voronoi()`.
2. Spatial join trees to Voronoi polygons with `sf::st_join()`.

**Expected result:** Each tree is assigned to its nearest person. Tree counts per person will be unequal. (This algorithm is unaffected by the `count` field in its assignment logic, but summaries should report total tree count per person, not number of locations.)

**Packages:** sf, tidyverse

---

### Algorithm 2: Capacitated Nearest-Neighbor (Greedy Balanced)

**Concept:** Assign tree locations to their nearest person, but enforce a per-person capacity based on total tree count so that each person gets approximately the same number of trees. Process tree locations in order of how close they are to their nearest person (closest first). If a tree location's nearest person would exceed their capacity by accepting it, assign it to the next-nearest person who still has capacity.

**Approach:**
1. Compute the full distance matrix: `sf::st_distance(trees, people)` → N×13 matrix (where N = number of tree locations).
2. For each tree location, rank people by distance.
3. Compute target: `sum(trees$count) / nrow(people)`.
4. Greedily assign: iterate through tree locations (sorted by minimum distance to any person), assigning each to its closest person whose running total of `count` still has room. "Has room" means adding this location's `count` would bring them closer to (or not excessively over) the target. Since locations are indivisible, a person may go over target when they accept a high-count location.

**Expected result:** Approximately balanced tree counts per person, but not exactly equal due to the indivisibility of tree locations. Some tree locations will not be assigned to their absolute nearest person.

**Packages:** sf, tidyverse

---

### Algorithm 3: Optimal Transport (Linear Programming)

**Concept:** Minimize total distance across all assignments, subject to each person receiving approximately the same total tree count. This is a classic transportation problem from operations research. Each tree location supplies `count` units rather than 1.

**Approach:**
1. Compute the full distance matrix: `sf::st_distance(trees, people)` → N×13 matrix (costs).
2. Set supply: each tree location supplies `count` units (i.e., `trees$count`).
3. Set demand: distribute `sum(trees$count)` across 13 people as evenly as possible. Since tree locations are indivisible, use an integer programming formulation rather than continuous transport.
4. Formulate as a **binary assignment problem**: for each tree location `i` and person `j`, let `x[i,j]` ∈ {0, 1} indicate assignment. Minimize `sum(distance[i,j] * count[i] * x[i,j])`. Constraints: each tree location assigned to exactly one person (`sum_j x[i,j] = 1` for all `i`); each person's total count is close to the target. The balance constraint can be expressed as bounds: `target - slack <= sum_i (count[i] * x[i,j]) <= target + slack` for each person `j`, with `slack` set to accommodate the granularity of `count` values.
5. Solve with `lpSolve::lp()` (MIP formulation) or `ompr` with a MILP solver like `glpk`.
6. Extract assignments from the solution.

**Note:** If exact integer programming is too slow or complex, an approximate approach is acceptable: solve the continuous transportation problem with `lp.transport()` and then round fractional assignments, assigning each tree location to the person with the largest fractional allocation.

**Expected result:** Near-optimal balance of proximity and equity. Approximately equal tree counts per person, with imbalance driven by the granularity of `count` values.

**Packages:** sf, tidyverse, lpSolve (or ompr + ROI.plugin.glpk)

---

### Algorithm 4: Voronoi with Rebalancing

**Concept:** Start with the Voronoi assignment (Algorithm 1), then iteratively transfer trees from over-allocated people to under-allocated neighbors to improve balance while preserving most of the spatial coherence.

**Approach:**
1. Start with Algorithm 1 assignments.
2. Compute target tree count per person: `sum(trees$count) / nrow(people)`.
3. Identify over-allocated people (total `count` above target) and under-allocated people (below target).
4. For each over-allocated person, find tree locations near the boundary with an under-allocated neighbor. "Near the boundary" means the tree location's distance to the under-allocated neighbor is close to its distance to its current (over-allocated) person — e.g., the distance ratio is below some threshold like 1.5.
5. Transfer the most boundary-adjacent tree locations first (those with the smallest distance ratio). After each transfer, update running totals using the transferred location's `count`.
6. Repeat until all people are within an acceptable tolerance of the target or no more beneficial transfers exist. Note that a single transfer of a high-count location may overshoot, so the stopping condition should allow some tolerance.

**Expected result:** Mostly preserves Voronoi spatial coherence. Approximately equal tree counts. Some boundary tree locations shift to neighboring people. Perfect balance may not be achievable due to indivisible locations.

**Packages:** sf, tidyverse

---

### Algorithm 5: Weighted Voronoi (Iterative Power Diagram)

**Concept:** Modify the Voronoi tessellation by assigning a weight to each person. The weight effectively expands or contracts their cell. Iteratively adjust weights until each cell contains approximately the target number of trees. Geometrically, this produces a "power diagram" (a weighted Voronoi diagram).

**Approach:**
1. Initialize all weights to 0.
2. Assign each tree to the person minimizing `distance(tree, person)^2 - weight(person)`. (A higher weight makes a person "attract" more distant trees.)
3. Count total trees per person using `sum(count)` for their assigned locations. For people with too few trees, increase their weight; for people with too many, decrease their weight.
4. Repeat until tree counts per person are within an acceptable tolerance of the target or a maximum number of iterations is reached. Perfect convergence may not be achievable due to indivisible locations with varying counts.
5. The weight update rule can be: `weight(p) += alpha * (count(p) - target)` where `alpha` is a tuning parameter (start with something like the mean nearest-neighbor distance squared, and adjust).

**Expected result:** A clean spatial partition where every tree location is in a spatially contiguous cell around its person, with approximately equal total tree counts. Convergence may require tuning `alpha` and a max iteration cap. The indivisibility of locations with varying `count` values means some imbalance is expected.

**Packages:** sf, tidyverse

---

## Output Specification

For each algorithm, produce:

1. **The assigned trees tibble**: the original `trees` sf object with an added `person_id` column.
2. **A summary table**: a tibble with one row per person showing `person_id`, `n_locations` (number of tree locations assigned), `n_trees` (sum of `count` for assigned locations), and `mean_dist` (mean distance to assigned tree locations, optionally weighted by `count`).
3. **A map**: a leaflet or ggplot map showing trees colored by assigned person, with people shown as labeled markers.

## Comparison

After implementing all five algorithms, produce a comparison summary showing, for each algorithm:

- Distribution of total tree count per person (min, max, mean, sd) — using `sum(count)`, not number of locations
- Total weighted distance (sum of all `distance * count` for each assignment)
- Mean weighted distance per tree
- A side-by-side or faceted map of all five assignments
