# Tree Assignment Editor Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a Shiny GUI (`EditAssignments.R`) that lets the user manually reassign tree locations between volunteers, with live metrics feedback and named save/load snapshots.

**Architecture:** `AssignPeopleToTrees.R` is refactored into a pure function library (data loading + all functions, no algorithm calls). Algorithm calls move to `CompareTreeAssignments.qmd`. `EditAssignments.R` sources `AssignPeopleToTrees.R` and provides `edit_assignments(assigned, people)`, which runs a blocking Shiny app returning the final edited assignment.

**Tech Stack:** R, Shiny, bslib, shinyjs, leaflet, gt, sf, tidyverse, here

---

## File Map

| File | Action | Responsibility |
|------|--------|----------------|
| `Trees/AssignPeopleToTrees.R` | Modify | Remove algorithm calls from bottom; add `save_assignments()` and `load_assignments()` |
| `Trees/CompareTreeAssignments.qmd` | Modify | Add algorithm calls to setup chunk |
| `Trees/EditAssignments.R` | Create | Shiny app + `edit_assignments()` entry point |
| `Trees/assignments/` | Create (on first save) | Named CSV snapshots |

---

## Task 1: Refactor `AssignPeopleToTrees.R`

Move the algorithm calls at the bottom of the file to `CompareTreeAssignments.qmd`. The hard-coded baselines (`voronoi_total_miles`, etc.) stay in `AssignPeopleToTrees.R` since `compute_metrics()` references them.

**Files:**
- Modify: `Trees/AssignPeopleToTrees.R`
- Modify: `Trees/CompareTreeAssignments.qmd`

- [ ] **Step 1: Remove algorithm calls from `AssignPeopleToTrees.R`**

  Delete these lines from the bottom of `AssignPeopleToTrees.R` (keep the baselines above them):

  ```r
  trees_voronoi = segment_voronoi(trees, people)
  trees_nearest = assign_capacitated_nearest(trees, people, tolerance=0.3) |>
    swap_assignments(people)
  trees_optimal = assign_optimal_transport(trees, people)
  trees_voronoi_balanced = assign_voronoi_rebalanced(trees, people)
  trees_voronoi_weighted = assign_weighted_voronoi(trees, people)
  trees_draft = assign_draft(trees, people)
  ```

  The commented-out `summarize_assignments` / `show_assignments` calls can be deleted too.

  The bottom of the file should end with:

  ```r
  # Voronoi baselines used to normalize distance metrics in compute_metrics.
  # Hard-coded so scoring works independently of the voronoi assignment.
  voronoi_total_miles    = 97.35843
  voronoi_max_miles      =  3.860317
  voronoi_hull_perimeter = 33.46792
  ```

- [ ] **Step 2: Add algorithm calls to `CompareTreeAssignments.qmd` setup chunk**

  The existing setup chunk in `CompareTreeAssignments.qmd` is:

  ```r
  #| label: setup
  #| include: false
  source(here::here('Trees/AssignPeopleToTrees.R'))
  ```

  Extend it to:

  ```r
  #| label: setup
  #| include: false
  source(here::here('Trees/AssignPeopleToTrees.R'))

  trees_voronoi          = segment_voronoi(trees, people)
  trees_nearest          = assign_capacitated_nearest(trees, people, tolerance = 0.3) |>
    swap_assignments(people)
  trees_optimal          = assign_optimal_transport(trees, people)
  trees_voronoi_balanced = assign_voronoi_rebalanced(trees, people)
  trees_voronoi_weighted = assign_weighted_voronoi(trees, people)
  trees_draft            = assign_draft(trees, people)
  ```

- [ ] **Step 3: Verify `AssignPeopleToTrees.R` sources cleanly**

  In R console:
  ```r
  source(here::here('Trees/AssignPeopleToTrees.R'))
  ```
  Expected: no errors, no algorithm objects created. `ls()` should NOT contain `trees_voronoi`, `trees_nearest`, etc. It SHOULD contain `trees`, `people`, `compute_metrics`, `save_assignments` (once added in Task 2), etc.

- [ ] **Step 4: Verify the Quarto document still renders**

  In R console or terminal:
  ```r
  quarto::quarto_render(here::here('Trees/CompareTreeAssignments.qmd'))
  ```
  Expected: renders without error; output HTML contains all six algorithm tabs.

- [ ] **Step 5: Commit**

  ```bash
  git add Trees/AssignPeopleToTrees.R Trees/CompareTreeAssignments.qmd
  git commit -m "refactor: move algorithm calls from AssignPeopleToTrees into qmd setup chunk"
  ```

---

## Task 2: Add `save_assignments` and `load_assignments`

**Files:**
- Modify: `Trees/AssignPeopleToTrees.R`

- [ ] **Step 1: Add `save_assignments` to `AssignPeopleToTrees.R`**

  Add after the `load_assignments` / `save_assignments` block near the other utility functions:

  ```r
  # Save a tree assignment to a named CSV file
  #
  # Writes all columns of the assignment sf object to Trees/assignments/<name>.csv.
  # The geom column is stored as WKT so the file is self-contained.
  #
  # @param assigned sf object with tree assignments
  # @param name character; filename stem (no .csv extension)
  # @return assigned invisibly
  save_assignments = function(assigned, name) {
    dir = here::here('Trees/assignments')
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    assigned |>
      mutate(geom = st_as_text(geom)) |>
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
    path = here::here('Trees/assignments', paste0(name, '.csv'))
    data = read_csv(path, show_col_types = FALSE)
    required = c('person_id', 'count', 'geom', 'Addr')
    missing = setdiff(required, names(data))
    if (length(missing) > 0) {
      stop('load_assignments: missing required columns: ', paste(missing, collapse = ', '))
    }
    st_as_sf(data, wkt = 'geom', crs = 26986)
  }
  ```

- [ ] **Step 2: Verify save and load round-trip correctly**

  In R console (after sourcing the file and running the algorithm calls):
  ```r
  source(here::here('Trees/AssignPeopleToTrees.R'))
  trees_nearest = assign_capacitated_nearest(trees, people, tolerance = 0.3) |>
    swap_assignments(people)

  save_assignments(trees_nearest, 'test_save')
  reloaded = load_assignments('test_save')

  # Verify person_ids match
  identical(trees_nearest$person_id, reloaded$person_id)  # TRUE
  # Verify geometry is preserved
  all(st_equals(trees_nearest$geom, reloaded$geom, sparse = FALSE))  # TRUE
  ```

- [ ] **Step 3: Commit**

  ```bash
  git add Trees/AssignPeopleToTrees.R
  git commit -m "feat: add save_assignments and load_assignments functions"
  ```

---

## Task 3: Create `EditAssignments.R` skeleton

Create the file with the `edit_assignments()` entry point and stub UI/server. Verify the app launches and displays a placeholder.

**Files:**
- Create: `Trees/EditAssignments.R`

- [ ] **Step 1: Create `Trees/EditAssignments.R` with skeleton**

  ```r
  # Interactive GUI for manually editing tree maintenance assignments.
  #
  # Usage:
  #   source(here::here('Trees/EditAssignments.R'))
  #   trees_edited <- edit_assignments(trees_nearest, people)

  library(shiny)
  library(bslib)
  library(shinyjs)
  library(leaflet)
  library(gt)
  library(tidyverse)
  library(sf)
  library(here)

  source(here::here('Trees/AssignPeopleToTrees.R'))

  # Launch an interactive Shiny app to manually edit tree assignments.
  #
  # Click tree markers on the map to reassign them to different volunteers.
  # Use the Save button to write named snapshots; Load to restore them.
  # Click Done to exit and return the current assignment.
  #
  # @param assigned sf object with tree assignments (person_id, count, geom, Addr columns)
  # @param people sf object with volunteer locations (person_id, count, Addr, color, geometry)
  # @return sf object with the final edited assignments (returned when Done is clicked)
  edit_assignments = function(assigned, people) {
    app = shinyApp(
      ui     = editor_ui(),
      server = editor_server(assigned, people)
    )
    shiny::runApp(app, launch.browser = TRUE)
  }

  editor_ui = function() {
    page_sidebar(
      title = 'Tree Assignment Editor',
      sidebar = sidebar(
        h5('Placeholder sidebar')
      ),
      p('Placeholder main area')
    )
  }

  editor_server = function(assigned, people) {
    function(input, output, session) {
      rv = reactiveValues(assigned = assigned)
    }
  }
  ```

- [ ] **Step 2: Verify the skeleton launches**

  In R console:
  ```r
  source(here::here('Trees/EditAssignments.R'))
  # Then (with a trees_nearest object available):
  edit_assignments(trees_nearest, people)
  ```
  Expected: browser opens showing "Tree Assignment Editor" with placeholder content. No errors in console.

- [ ] **Step 3: Commit**

  ```bash
  git add Trees/EditAssignments.R
  git commit -m "feat: add EditAssignments.R skeleton with edit_assignments entry point"
  ```

---

## Task 4: Build the sidebar controls

Replace the placeholder sidebar with Load, Save, and Done controls.

**Files:**
- Modify: `Trees/EditAssignments.R`

- [ ] **Step 1: Replace `editor_ui` sidebar with real controls**

  ```r
  editor_ui = function() {
    page_sidebar(
      title = 'Tree Assignment Editor',
      useShinyjs(),
      sidebar = sidebar(
        width = 220,
        h6('Load'),
        selectInput('load_name', label = NULL,
                    choices = list_saved_names()),
        actionButton('load_btn', 'Load', class = 'btn-sm btn-outline-secondary w-100'),
        hr(),
        h6('Save'),
        textInput('save_name', label = NULL, placeholder = 'Assignment name'),
        actionButton('save_btn', 'Save', class = 'btn-sm btn-outline-primary w-100'),
        hr(),
        actionButton('done_btn', 'Done', class = 'btn-sm btn-success w-100')
      ),
      p('Placeholder main area')
    )
  }
  ```

  Add this helper function (outside `editor_ui`, at module level):

  ```r
  list_saved_names = function() {
    dir = here::here('Trees/assignments')
    if (!dir.exists(dir)) return(character(0))
    tools::file_path_sans_ext(list.files(dir, pattern = '\\.csv$'))
  }
  ```

- [ ] **Step 2: Add server logic for sidebar**

  Replace `editor_server` with:

  ```r
  editor_server = function(assigned, people) {
    function(input, output, session) {
      rv = reactiveValues(
        assigned   = assigned,
        edit_count = 0L
      )

      # Disable Save button when name is empty
      observe({
        if (nchar(trimws(input$save_name)) == 0) {
          shinyjs::disable('save_btn')
        } else {
          shinyjs::enable('save_btn')
        }
      })

      # Save button
      observeEvent(input$save_btn, {
        name = trimws(input$save_name)
        path = here::here('Trees/assignments', paste0(name, '.csv'))
        if (file.exists(path)) {
          showModal(modalDialog(
            title = 'Overwrite?',
            glue::glue('"{name}" already exists. Overwrite?'),
            footer = tagList(
              actionButton('save_confirm', 'Overwrite', class = 'btn-danger'),
              modalButton('Cancel')
            )
          ))
        } else {
          do_save(rv, name, people)
          updateSelectInput(session, 'load_name', choices = list_saved_names())
        }
      })

      observeEvent(input$save_confirm, {
        removeModal()
        name = trimws(input$save_name)
        do_save(rv, name, people)
        updateSelectInput(session, 'load_name', choices = list_saved_names())
      })

      # Load button
      observeEvent(input$load_btn, {
        req(input$load_name)
        tryCatch({
          rv$assigned   = load_assignments(input$load_name)
          rv$edit_count = 0L
        }, error = function(e) {
          showNotification(conditionMessage(e), type = 'error', duration = 8)
        })
      })

      # Done button
      observeEvent(input$done_btn, {
        shiny::stopApp(rv$assigned)
      })
    }
  }
  ```

  Add this helper (at module level, outside server):

  ```r
  # Append a metrics row to rv$history and increment edit counter
  do_save = function(rv, name, people) {
    save_assignments(rv$assigned, name)
    # history appending happens in Task 8 — placeholder for now
  }
  ```

- [ ] **Step 3: Verify sidebar controls work**

  Source and launch. Verify:
  - Save button is disabled when name field is empty; enabled after typing
  - Typing a name and clicking Save writes a CSV to `Trees/assignments/`
  - Load dropdown lists saved names
  - Done button closes the app

- [ ] **Step 4: Commit**

  ```bash
  git add Trees/EditAssignments.R
  git commit -m "feat: add sidebar load/save/done controls to tree assignment editor"
  ```

---

## Task 5: Render the initial map

Add the leaflet map card to the main area. Use the same visual style as `show_assignments()` (hulls, tree markers, person markers). Build a helper that (re)draws the full map given an assignment, and a second helper that updates an existing map via `leafletProxy`.

**Files:**
- Modify: `Trees/EditAssignments.R`

- [ ] **Step 1: Add map card to `editor_ui`**

  Replace `p('Placeholder main area')` with:

  ```r
  layout_columns(
    col_widths = c(7, 5),
    card(
      full_screen = TRUE,
      card_header('Map'),
      leafletOutput('map', height = '65vh')
    ),
    card(
      card_header('Assignments'),
      gt_output('summary_table')
    )
  ),
  card(
    card_header('Metrics history'),
    gt_output('metrics_table')
  )
  ```

- [ ] **Step 2: Add map render function**

  Add this helper at module level:

  ```r
  render_map = function(assigned, people) {
    map_data = left_join(assigned, st_drop_geometry(people),
                         join_by(person_id), suffix = c('.tree', '')) |>
      st_transform(4326) |>
      mutate(
        label = glue::glue('{Addr.tree} - {count.tree} trees<br>(by {Addr})') |>
          lapply(htmltools::HTML) |> unname()
      )

    people_pts = people |>
      st_transform(4326) |>
      select(person_id, color) |>
      rename(geom = geometry)

    hulls = map_data |>
      select(person_id, geom, color) |>
      bind_rows(people_pts) |>
      group_by(person_id) |>
      summarize(geom = st_union(geom), color = color[1]) |>
      mutate(hull = st_convex_hull(geom) |> st_buffer(50)) |>
      st_set_geometry('hull')

    people_data = people |>
      left_join(
        assigned |> st_drop_geometry() |> summarize(count = sum(count), .by = person_id),
        join_by(person_id)
      ) |>
      mutate(
        count     = replace_na(count, 0),
        label     = glue::glue('{Addr} ({count} trees)')
      )

    bbox = st_bbox(st_union(st_geometry(map_data),
                            st_geometry(people_data) |> st_transform(4326)))

    leaflet(width = '100%') |>
      fitBounds(bbox[['xmin']], bbox[['ymin']], bbox[['xmax']], bbox[['ymax']]) |>
      addProviderTiles('CartoDB.Positron', group = 'Street') |>
      addProviderTiles('Esri.WorldImagery', group = 'Satellite') |>
      addPolygons(data = hulls, fillColor = ~color, stroke = FALSE,
                  fillOpacity = 0.3, group = 'Groups') |>
      addCircleMarkers(data = map_data,
                       radius = ~5 + sqrt(count.tree),
                       label  = ~label,
                       group  = 'Trees',
                       stroke = FALSE,
                       fillColor   = ~color,
                       fillOpacity = 1,
                       layerId = paste0('tree_', seq_len(nrow(map_data)))) |>
      addCircleMarkers(data = people_data |> st_transform(4326),
                       label       = ~label,
                       group       = 'Maintainers',
                       color       = 'black',
                       fillColor   = ~color,
                       fillOpacity = 1,
                       weight = 2, radius = 4,
                       layerId = paste0('person_', seq_len(nrow(people_data)))) |>
      addLayersControl(
        baseGroups    = c('Street', 'Satellite'),
        overlayGroups = c('Trees', 'Maintainers', 'Groups'),
        options = layersControlOptions(collapsed = FALSE)
      )
  }
  ```

  Note: `layerId` uses the row index of `map_data` (trees after the join/transform), not of `assigned`. Store the row-to-tree mapping in `rv` so the click handler can look up the original `assigned` row index. Add to `rv` initialization:

  ```r
  rv = reactiveValues(
    assigned   = assigned,
    edit_count = 0L,
    map_data   = NULL   # will hold the transformed map_data for click lookup
  )
  ```

- [ ] **Step 3: Add map output to server**

  In `editor_server`, after the Done observer, add:

  ```r
  output$map = renderLeaflet({
    md = left_join(rv$assigned, st_drop_geometry(people),
                   join_by(person_id), suffix = c('.tree', '')) |>
      st_transform(4326)
    rv$map_data = md
    render_map(rv$assigned, people)
  })
  ```

- [ ] **Step 4: Verify the map renders**

  Source and launch. The map should show colored hulls, tree markers, and person markers matching the style from `show_assignments()`.

- [ ] **Step 5: Commit**

  ```bash
  git add Trees/EditAssignments.R
  git commit -m "feat: add initial map render to tree assignment editor"
  ```

---

## Task 6: Implement tree click and reassignment modal

Clicking a tree marker opens a modal with a person dropdown. Confirming the update changes `rv$assigned`.

**Files:**
- Modify: `Trees/EditAssignments.R`

- [ ] **Step 1: Add click handler and modal to server**

  Add inside `editor_server` function body (after the map output):

  ```r
  # Tree marker click → open reassignment modal
  observeEvent(input$map_marker_click, {
    click = input$map_marker_click
    req(click, !is.null(rv$map_data))  # map_data populated by renderLeaflet; guard first click
    if (!startsWith(click$id, 'tree_')) return()

    map_row    = as.integer(sub('tree_', '', click$id))
    tree_row   = which(
      rv$assigned$Addr == rv$map_data$Addr.tree[map_row] &
      rv$assigned$Year == rv$map_data$Year[map_row]
    )[1]

    current_pid  = rv$assigned$person_id[tree_row]
    # people$Addr[current_pid] works because person_id is a 1-based sequential
    # row index into people — guaranteed by how AssignPeopleToTrees.R constructs people
    # and preserved through save/load (person_id is stored in CSV and reloaded as-is)
    current_name = people$Addr[current_pid]
    tree_addr    = rv$assigned$Addr[tree_row]

    showModal(modalDialog(
      title = glue::glue('Reassign: {tree_addr}'),
      p(strong('Current assignee: '), current_name),
      selectInput(
        'new_person_id',
        'Assign to:',
        choices  = setNames(people$person_id, people$Addr),
        selected = current_pid
      ),
      footer = tagList(
        actionButton('reassign_confirm', 'Update', class = 'btn-primary'),
        modalButton('Cancel')
      ),
      easyClose = TRUE
    ))

    rv$pending_tree_row = tree_row
  })

  # Update button in modal
  observeEvent(input$reassign_confirm, {
    removeModal()
    row = rv$pending_tree_row
    req(!is.null(row))

    new_pid = as.integer(input$new_person_id)
    if (new_pid == rv$assigned$person_id[row]) return()

    rv$assigned$person_id[row] = new_pid
    rv$edit_count = rv$edit_count + 1L
  })
  ```

  Add `pending_tree_row = NULL` to the `rv = reactiveValues(...)` initializer.

- [ ] **Step 2: Add `leafletProxy` update when assignment changes**

  The map output (`renderLeaflet`) renders the initial map. Subsequent updates use `leafletProxy` to avoid losing zoom/pan. Add this observer after the reassign observer:

  ```r
  # Redraw tree markers and hulls when assignment changes
  # (triggered by rv$edit_count incrementing)
  observeEvent(rv$edit_count, {
    req(rv$edit_count > 0)

    map_data = left_join(rv$assigned, st_drop_geometry(people),
                         join_by(person_id), suffix = c('.tree', '')) |>
      st_transform(4326) |>
      mutate(
        label = glue::glue('{Addr.tree} - {count.tree} trees<br>(by {Addr})') |>
          lapply(htmltools::HTML) |> unname()
      )
    rv$map_data = map_data

    people_pts = people |>
      st_transform(4326) |>
      select(person_id, color) |>
      rename(geom = geometry)

    hulls = map_data |>
      select(person_id, geom, color) |>
      bind_rows(people_pts) |>
      group_by(person_id) |>
      summarize(geom = st_union(geom), color = color[1]) |>
      mutate(hull = st_convex_hull(geom) |> st_buffer(50)) |>
      st_set_geometry('hull')

    people_counts = rv$assigned |>
      st_drop_geometry() |>
      summarize(count = sum(count), .by = person_id)

    people_data = people |>
      left_join(people_counts, join_by(person_id)) |>
      mutate(
        count = replace_na(count, 0),
        label = glue::glue('{Addr} ({count} trees)')
      ) |>
      st_transform(4326)

    leafletProxy('map') |>
      clearShapes() |>
      clearMarkers() |>
      addPolygons(data = hulls, fillColor = ~color, stroke = FALSE,
                  fillOpacity = 0.3, group = 'Groups') |>
      addCircleMarkers(data = map_data,
                       radius = ~5 + sqrt(count.tree),
                       label  = ~label,
                       group  = 'Trees',
                       stroke = FALSE,
                       fillColor   = ~color,
                       fillOpacity = 1,
                       layerId = paste0('tree_', seq_len(nrow(map_data)))) |>
      addCircleMarkers(data = people_data,
                       label       = ~label,
                       group       = 'Maintainers',
                       color       = 'black',
                       fillColor   = ~color,
                       fillOpacity = 1,
                       weight = 2, radius = 4,
                       layerId = paste0('person_', seq_len(nrow(people_data))))
  })
  ```

- [ ] **Step 3: Verify reassignment works**

  Source and launch. Click a tree marker, select a different person, click Update. Verify:
  - The tree marker changes color to the new person's color
  - The hull polygons redraw
  - The zoom/pan position is preserved

- [ ] **Step 4: Commit**

  ```bash
  git add Trees/EditAssignments.R
  git commit -m "feat: add tree click modal and leafletProxy reassignment update"
  ```

---

## Task 7: Add assignment summary table

Render a `gt` table (one row per person) that updates when `rv$assigned` changes.

**Files:**
- Modify: `Trees/EditAssignments.R`

- [ ] **Step 1: Add summary table output to server**

  ```r
  output$summary_table = render_gt({
    stats = summarize_counts(rv$assigned) |>
      left_join(summarize_distances(rv$assigned, people), join_by(person_id))

    people |>
      st_drop_geometry() |>
      select(person_id, Addr) |>
      left_join(stats, join_by(person_id)) |>
      replace_na(list(count = 0, total_miles = 0)) |>
      select(-person_id) |>
      gt() |>
      cols_label(Addr = 'Address',
                 count = html('Trees'),
                 total_miles = html('Total<br>miles')) |>
      fmt_number(total_miles, decimals = 1) |>
      grand_summary_rows(
        columns = c(count, total_miles),
        fns     = list(Total ~ sum(.)),
        fmt     = ~fmt_number(., decimals = 1)
      ) |>
      tab_options(table.font.size = '70%', data_row.padding = '4px') |>
      opt_css('div.gt_container { padding-top: 0 !important; }')
  })
  ```

- [ ] **Step 2: Verify table updates on reassignment**

  Source and launch. Reassign a tree and confirm the tree count and total miles update in the summary table.

- [ ] **Step 3: Commit**

  ```bash
  git add Trees/EditAssignments.R
  git commit -m "feat: add live assignment summary table to tree assignment editor"
  ```

---

## Task 8: Add metrics history table

Show a running log of metrics: one row per edit and one row per save. Named-save rows are bolded. Newest row is at the top.

**Files:**
- Modify: `Trees/EditAssignments.R`

- [ ] **Step 1: Initialize metrics history in server**

  Add to `rv = reactiveValues(...)`:

  ```r
  history = {
    m = compute_metrics(assigned, people)
    tibble(
      label                 = '(starting)',
      is_save               = FALSE,
      cv                    = m$cv,
      total_miles           = m$total_miles,
      max_single_tree_miles = m$max_single_tree_miles,
      hull_perimeter_miles  = m$hull_perimeter_miles,
      score                 = m$score
    )
  }
  ```

- [ ] **Step 2: Append a row after each reassignment**

  In the `observeEvent(input$reassign_confirm, ...)` block, after updating `rv$assigned$person_id[row]` and incrementing `rv$edit_count`, add:

  ```r
  m = compute_metrics(rv$assigned, people)
  rv$history = bind_rows(
    tibble(
      label                 = paste0('edit ', rv$edit_count),
      is_save               = FALSE,
      cv                    = m$cv,
      total_miles           = m$total_miles,
      max_single_tree_miles = m$max_single_tree_miles,
      hull_perimeter_miles  = m$hull_perimeter_miles,
      score                 = m$score
    ),
    rv$history
  )
  ```

- [ ] **Step 3: Append a row on Save**

  Replace the placeholder `do_save` helper with:

  ```r
  do_save = function(rv, name, people) {
    save_assignments(rv$assigned, name)
    m = compute_metrics(rv$assigned, people)
    rv$history = bind_rows(
      tibble(
        label                 = name,
        is_save               = TRUE,
        cv                    = m$cv,
        total_miles           = m$total_miles,
        max_single_tree_miles = m$max_single_tree_miles,
        hull_perimeter_miles  = m$hull_perimeter_miles,
        score                 = m$score
      ),
      rv$history
    )
  }
  ```

  Note: `do_save` modifies `rv` which is a `reactiveValues` object (reference semantics), so this works without returning a value.

- [ ] **Step 4: Render the metrics history table**

  ```r
  output$metrics_table = render_gt({
    rv$history |>
      select(-is_save) |>
      gt() |>
      cols_label(
        label                 = 'Label',
        cv                    = 'CV',
        total_miles           = html('Total<br>miles'),
        max_single_tree_miles = html('Max tree<br>dist (mi)'),
        hull_perimeter_miles  = html('Hull<br>perim (mi)'),
        score                 = 'Score'
      ) |>
      fmt_number(c(cv, score), decimals = 2) |>
      fmt_number(c(total_miles, max_single_tree_miles, hull_perimeter_miles), decimals = 1) |>
      tab_style(
        style = cell_text(weight = 'bold'),
        locations = cells_body(rows = rv$history$is_save)
      ) |>
      tab_options(table.font.size = '80%', data_row.padding = '4px')
  })
  ```

- [ ] **Step 5: Verify metrics history**

  Source and launch. Verify:
  - `(starting)` row is present on launch
  - Each tree reassignment adds an `edit N` row at the top
  - Saving adds a named bold row at the top
  - Metrics numbers change plausibly after reassignments

- [ ] **Step 6: Commit**

  ```bash
  git add Trees/EditAssignments.R
  git commit -m "feat: add live metrics history table to tree assignment editor"
  ```

---

## Task 9: Integration test and cleanup

Verify the full app workflow end-to-end and clean up any rough edges.

**Files:**
- Modify: `Trees/EditAssignments.R`

- [ ] **Step 1: Full workflow test**

  In R console:
  ```r
  source(here::here('Trees/AssignPeopleToTrees.R'))
  trees_nearest = assign_capacitated_nearest(trees, people, tolerance = 0.3) |>
    swap_assignments(people)

  source(here::here('Trees/EditAssignments.R'))
  trees_edited = edit_assignments(trees_nearest, people)
  ```

  Walk through:
  1. App launches in browser
  2. Map, summary table, and `(starting)` metrics row are visible
  3. Click a tree → modal appears with correct current assignee
  4. Change assignee → map redraws (correct color), summary updates, `edit 1` row added to history
  5. Type a name in Save field → Save button enables → click Save → file written to `Trees/assignments/`
  6. Named row appears bold in history table; load dropdown refreshes
  7. Click Done → `trees_edited` in console has updated `person_id` values
  8. Verify: `identical(trees_edited$person_id, trees_nearest$person_id)` is FALSE (edits persisted)

- [ ] **Step 2: Test load workflow**

  Continue in same session:
  ```r
  trees_edited2 = edit_assignments(trees_edited, people)
  ```
  1. Select the saved name in Load dropdown → click Load
  2. Verify map and summary table reflect the loaded assignment
  3. Edit counter resets (next edit is `edit 1`)
  4. Click Done

- [ ] **Step 3: Test `load_assignments` error handling**

  ```r
  # Write a bad CSV and verify error
  write_csv(tibble(x = 1), here::here('Trees/assignments/bad.csv'))
  tryCatch(
    load_assignments('bad'),
    error = function(e) message('Got expected error: ', conditionMessage(e))
  )
  # Expected: "Got expected error: load_assignments: missing required columns: ..."
  ```

- [ ] **Step 4: Final commit**

  ```bash
  git add Trees/EditAssignments.R Trees/AssignPeopleToTrees.R Trees/CompareTreeAssignments.qmd
  git commit -m "feat: complete tree assignment editor with save/load and metrics history"
  ```
