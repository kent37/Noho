# Tree Assignment Editor — Design Spec

**Date:** 2026-03-26
**Status:** Draft

## Overview

A Shiny-based GUI for manually editing tree maintenance assignments after running one
of the automated allocation algorithms. The user launches it from the R console with
any assignment object, edits assignments by clicking tree markers on a map, and saves
named snapshots to CSV files.

## Goals

- Provide a map-based interface for reassigning individual tree locations between volunteers
- Show assignment summary and metrics feedback that updates in real time as edits are made
- Allow named snapshots to be saved and reloaded across sessions
- Keep the Shiny code separate from the allocation algorithm code

## Non-Goals

- Running allocation algorithms from within the GUI
- Multi-user or hosted deployment
- Undo/redo beyond reloading a saved snapshot

---

## File Structure

```
Trees/
  AssignPeopleToTrees.R     # existing — add save_assignments / load_assignments
  EditAssignments.R         # new — Shiny app + edit_assignments() entry point
  assignments/              # created on first save; one CSV per named assignment
    <name>.csv
  docs/superpowers/specs/
    2026-03-26-tree-assignment-editor-design.md
```

### Refactoring `AssignPeopleToTrees.R`

The algorithm calls at the bottom of `AssignPeopleToTrees.R` (`trees_voronoi = ...`,
`trees_nearest = ...`, etc.) are moved into the setup chunk of
`CompareTreeAssignments.qmd`, which already sources the file. This turns
`AssignPeopleToTrees.R` into a pure function library:

- Data loading: `trees`, `people`, `noho`
- Hard-coded baselines: `voronoi_total_miles`, `voronoi_max_miles`, `voronoi_hull_perimeter`
- All function definitions

`EditAssignments.R` sources `AssignPeopleToTrees.R` to access these without
triggering any algorithm runs.

---

## Assignment Object Schema

All assignment objects in this project are sf objects in CRS 26986 (MA Mainland)
produced by the allocation functions. Required columns:

| Column | Type | Description |
|--------|------|-------------|
| `geom` | sfc_POINT | Tree location in CRS 26986 |
| `person_id` | integer | Index into `people` (1-based) |
| `count` | integer | Number of trees at this location |
| `Year` | integer | Planting year |
| `Street` | character | Street name |
| `Num` | character/NA | Street number |
| `Addr` | character | Formatted address (tree location) |

`people` is an sf object with columns `person_id`, `count`, `Addr`, `color`,
and `geometry` (CRS 26986).

---

## Entry Point

```r
edit_assignments(assigned, people)
```

Launches a blocking Shiny app via `shiny::runApp(app, launch.browser = TRUE)`.
The app is stopped explicitly by a **Done** button in the sidebar, which calls
`shiny::stopApp(rv$assigned)`. `edit_assignments` returns the final assignment
object, allowing:

```r
trees_edited <- edit_assignments(trees_nearest, people)
```

Closing the browser tab without clicking Done is not a supported exit path; the
function will continue waiting. The Done button is the only exit.

---

## Save / Load Functions (in `AssignPeopleToTrees.R`)

### `save_assignments(assigned, name)`

1. Creates `here::here('Trees/assignments/')` if it does not exist
2. Converts `geom` to WKT text via `st_as_text()`
3. Drops sf geometry and writes all columns to `Trees/assignments/<name>.csv`
4. Returns `assigned` invisibly

The WKT includes CRS information, so the CRS roundtrips correctly.

### `load_assignments(name)`

1. Reads `Trees/assignments/<name>.csv`
2. Converts the `geom` WKT column back to sf geometry with `st_as_sf(wkt = 'geom', crs = 26986)`
3. Validates that required columns (`person_id`, `count`, `geom`, `Addr`) are present; stops with an informative error if not
4. Returns the sf object

---

## UI Layout

Built with `bslib::page_sidebar`.

### Sidebar

Sections top to bottom:

**Load:**
- `selectInput` — lists filenames (without `.csv`) from `Trees/assignments/`. Populated on
  app launch and refreshed after every save. Shows empty choices if the directory does
  not exist yet.
- "Load" action button

**Save:**
- `textInput` for assignment name. The Save button is disabled (`shinyjs::disable`) when
  this field is empty.
- "Save" action button. If `<name>.csv` already exists, shows a confirmation modal
  ("Overwrite existing snapshot?") before writing.

**Done:**
- "Done" action button — exits the app and returns `rv$assigned`

### Main area — top row

Two cards side by side (~60 / 40 split):

| Card | Content |
|------|---------|
| Map | `leafletOutput` — colored convex hull polygons per person, tree circle markers sized by `count` and colored by person, person markers with black border |
| Assignment summary | `gt_output` — one row per person: address, tree count, total miles. Grand total row. Rerenders when `rv$assigned` changes. |

### Main area — bottom row

One full-width card: **Metrics history**

A `gt_output` table showing one row per history entry, newest row first.
Columns: Label, CV, Total miles, Max tree dist (mi), Hull perim (mi), Score.

Row labels:
- `"(starting)"` — the initial `assigned` object passed to `edit_assignments`, added on launch
- `"edit 1"`, `"edit 2"`, … — appended after every tree reassignment (Update button)
- The save name — appended on each Save (in addition to the auto-labelled edit row for
  that state, so the named checkpoint is visible in the history)

Loading a snapshot does NOT append to the history (it only changes `rv$assigned`).
The edit counter resets to 1 after a Load.

The Name column is highlighted (bold or background color) for named-save rows so they
stand out from auto-labelled edit rows.

The metrics come from `compute_metrics()` in `AssignPeopleToTrees.R`, which returns:
`cv`, `total_miles`, `max_single_tree_miles`, `hull_perimeter_miles`, `score`.
Score is `cv + total_miles/voronoi_total_miles + max_single_tree_miles/voronoi_max_miles`
(lower is better).

---

## Map Interaction

Tree and person markers use distinct `layerId` prefixes to avoid collision:
- Tree markers: `layerId = paste0("tree_", i)` where `i` is the row index
- Person markers: `layerId = paste0("person_", j)` — not clickable for reassignment

`observeEvent(input$map_marker_click, ...)` filters for IDs starting with `"tree_"`.
Clicking a person marker is ignored.

On a tree click, `showModal()` displays:
- Tree address (`Addr`) and current assignee name (display only)
- `selectInput("new_person_id", "Assign to:", choices = setNames(people$person_id, people$Addr), selected = <current person_id>)`
- "Update" action button and "Cancel" button

Clicking **Update**:
1. Updates `rv$assigned$person_id[row_index]` to the selected person
2. Calls `removeModal()`
3. Map, summary table, and metrics display update reactively

Map rerenders use `leafletProxy("map")` to redraw hulls and tree markers without
a full re-render (avoids losing map zoom/pan state).

---

## Reactive Data Flow

```
rv$assigned  ──► map via leafletProxy (hulls + tree markers)
             ──► summary table (gt)

rv$history   ──► metrics history table (gt)

Tree click   ──► modal → Update → rv$assigned
                                    ──► append "edit N" row to rv$history

Save button  ──► save_assignments(rv$assigned, input$save_name)
             ──► append named row to rv$history
             ──► refresh load dropdown

Load button  ──► load_assignments(input$load_choice) → rv$assigned
             ──► reset edit counter to 1

Done button  ──► stopApp(rv$assigned)
```

---

## Error Handling

| Scenario | Behavior |
|----------|----------|
| `assignments/` dir missing on launch | Load dropdown shows no choices; no error |
| Save with empty name | Save button disabled; not reachable |
| Save with duplicate name | Confirmation modal before overwrite |
| Load of incompatible CSV | `load_assignments` stops with informative error; app continues |
| `compute_metrics` fails (e.g., missing voronoi baselines) | Score shows `NA`; other metrics still display |

---

## Key Dependencies

| Package | Use |
|---------|-----|
| `shiny` | App framework |
| `bslib` | Page layout and cards |
| `shinyjs` | Disable Save button when name is empty |
| `leaflet` | Interactive map |
| `gt` | Summary and metrics tables |
| `sf` | Geometry operations and WKT conversion |
| `tidyverse` | Data manipulation |
| `here` | File paths |
