# Interactive GUI for manually editing tree maintenance assignments.
#
# Usage:
#   source(here::here('Trees/Maintenance/EditAssignments.R'))
#   trees_edited <- edit_assignments(trees_nearest, people)

library(shiny)
library(bslib)
library(shinyjs)
library(leaflet)
library(gt)
library(tidyverse)
library(sf)
library(here)

source(here::here('Trees/Maintenance/AssignPeopleToTrees.R'))

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
  shiny::runApp(app, launch.browser = TRUE) |> invisible()
}

# List names of saved assignment CSV files (without extension)
list_saved_names = function() {
  dir = here::here('Trees/Maintenance/assignments')
  if (!dir.exists(dir)) return(character(0))
  tools::file_path_sans_ext(list.files(dir, pattern = '\\.csv$'))
}

# Build a leaflet map from an assignment object.
# Tree markers use layerId = "tree_<i>" where i is the row index of map_data
# (the joined/transformed sf object), not of assigned.
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
      assigned |> st_drop_geometry() |> summarize(n_trees = sum(count), .by = person_id),
      join_by(person_id)
    ) |>
    mutate(
      n_trees = replace_na(n_trees, 0),
      label   = glue::glue('{Addr} ({n_trees} trees)')
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
                     radius      = ~5 + sqrt(count.tree),
                     label       = ~label,
                     group       = 'Trees',
                     stroke      = FALSE,
                     fillColor   = ~color,
                     fillOpacity = 1,
                     layerId     = paste0('tree_', seq_len(nrow(map_data)))) |>
    addCircleMarkers(data        = people_data |> st_transform(4326),
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
      options       = layersControlOptions(collapsed = FALSE)
    )
}

# Append a named metrics row to rv$history and write the CSV
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

editor_ui = function() {
  page_sidebar(
    title = 'Tree Assignment Editor',
    useShinyjs(),
    sidebar = sidebar(
      width = 220,
      h6('Load'),
      selectInput('load_name', label = NULL,
                  choices = list_saved_names()),
      actionButton('load_btn', 'Load',
                   class = 'btn-sm btn-outline-secondary w-100'),
      hr(),
      h6('Save'),
      textInput('save_name', label = NULL, placeholder = 'Assignment name'),
      actionButton('save_btn', 'Save',
                   class = 'btn-sm btn-outline-primary w-100'),
      hr(),
      actionButton('done_btn', 'Done',
                   class = 'btn-sm btn-success w-100')
    ),
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
  )
}

editor_server = function(assigned, people) {
  function(input, output, session) {

    rv = reactiveValues(
      assigned         = assigned,
      edit_count       = 0L,
      full_redraw      = 0L,     # increments to trigger full map re-render (fitBounds)
      map_data         = NULL,   # joined/transformed tree data for click lookup
      pending_tree_row = NULL,
      history          = {
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
    )

    # --- Sidebar: disable Save when name is empty ----------------------------

    observe({
      if (nchar(trimws(input$save_name)) == 0) {
        shinyjs::disable('save_btn')
      } else {
        shinyjs::enable('save_btn')
      }
    })

    # --- Sidebar: Save -------------------------------------------------------

    observeEvent(input$save_btn, {
      name = trimws(input$save_name)
      path = here::here('Trees/Maintenance/assignments', paste0(name, '.csv'))
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

    # --- Sidebar: Load -------------------------------------------------------

    observeEvent(input$load_btn, {
      req(input$load_name)
      tryCatch({
        rv$assigned     = load_assignments(input$load_name)
        rv$edit_count   = 0L
        rv$full_redraw  = rv$full_redraw + 1L
      }, error = function(e) {
        showNotification(conditionMessage(e), type = 'error', duration = 8)
      })
    })

    # --- Sidebar: Done -------------------------------------------------------

    observeEvent(input$done_btn, {
      shiny::stopApp(rv$assigned)
    })

    # --- Map: initial render (and after Load) --------------------------------
    # Depends only on rv$full_redraw so that individual edits (which use
    # leafletProxy below) do not trigger a full re-render and fitBounds reset.

    output$map = renderLeaflet({
      rv$full_redraw  # reactive dependency — do not remove
      current = isolate(rv$assigned)
      md = left_join(current, st_drop_geometry(people),
                     join_by(person_id), suffix = c('.tree', '')) |>
        st_transform(4326)
      rv$map_data = md
      render_map(current, people)
    })

    # --- Map: tree marker click → reassignment modal -------------------------

    observeEvent(input$map_marker_click, {
      click = input$map_marker_click
      req(click, !is.null(rv$map_data))
      if (!startsWith(click$id, 'tree_')) return()

      map_row  = as.integer(sub('tree_', '', click$id))
      tree_row = which(
        rv$assigned$Addr == rv$map_data$Addr.tree[map_row] &
        rv$assigned$Year == rv$map_data$Year[map_row]
      )[1]

      current_pid  = rv$assigned$person_id[tree_row]
      # person_id is a 1-based sequential row index into people — preserved
      # through save/load since it is stored as a plain integer column
      current_name = people$Addr[current_pid]
      tree_addr    = rv$assigned$Addr[tree_row]

      showModal(modalDialog(
        title = glue::glue('Reassign: {tree_addr}'),
        p(strong('Current assignee: '), current_name),
        selectInput(
          'new_person_id',
          'Assign to:',
          choices  = setNames(people$person_id, people$Addr),
          selected = current_pid,
          selectize = FALSE,
          size = nrow(people)
        ),
        footer = tagList(
          actionButton('reassign_confirm', 'Update', class = 'btn-primary'),
          modalButton('Cancel')
        ),
        easyClose = TRUE
      ))

      rv$pending_tree_row = tree_row
    })

    # --- Map: confirm reassignment -------------------------------------------

    observeEvent(input$reassign_confirm, {
      removeModal()
      row = rv$pending_tree_row
      req(!is.null(row))

      new_pid = as.integer(input$new_person_id)
      if (new_pid == rv$assigned$person_id[row]) return()

      rv$assigned$person_id[row] = new_pid
      rv$edit_count = rv$edit_count + 1L

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
    })

    # --- Map: proxy update after each reassignment ---------------------------

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

      people_data = people |>
        left_join(
          rv$assigned |> st_drop_geometry() |>
            summarize(n_trees = sum(count), .by = person_id),
          join_by(person_id)
        ) |>
        mutate(
          n_trees = replace_na(n_trees, 0),
          label   = glue::glue('{Addr} ({n_trees} trees)')
        ) |>
        st_transform(4326)

      leafletProxy('map') |>
        clearShapes() |>
        clearMarkers() |>
        addPolygons(data = hulls, fillColor = ~color, stroke = FALSE,
                    fillOpacity = 0.3, group = 'Groups') |>
        addCircleMarkers(data        = map_data,
                         radius      = ~5 + sqrt(count.tree),
                         label       = ~label,
                         group       = 'Trees',
                         stroke      = FALSE,
                         fillColor   = ~color,
                         fillOpacity = 1,
                         layerId     = paste0('tree_', seq_len(nrow(map_data)))) |>
        addCircleMarkers(data        = people_data,
                         label       = ~label,
                         group       = 'Maintainers',
                         color       = 'black',
                         fillColor   = ~color,
                         fillOpacity = 1,
                         weight = 2, radius = 4,
                         layerId = paste0('person_', seq_len(nrow(people_data))))
    })

    # --- Assignment summary table --------------------------------------------

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
        cols_label(Addr        = 'Address',
                   count       = html('Trees'),
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

    # --- Metrics history table -----------------------------------------------

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
        fmt_number(c(total_miles, max_single_tree_miles, hull_perimeter_miles),
                   decimals = 1) |>
        tab_style(
          style     = cell_text(weight = 'bold'),
          locations = cells_body(rows = rv$history$is_save)
        ) |>
        tab_options(table.font.size = '80%', data_row.padding = '4px')
    })
  }
}
