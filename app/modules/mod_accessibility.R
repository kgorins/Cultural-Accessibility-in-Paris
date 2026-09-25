# Accessibility: choropleth of precomputed accessibility metrics on the 200 m grid.

mod_accessibility_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      title = "Map settings",
      width = 290,
      category_select_input(ns("category")),
      radioButtons(ns("metric"), "Accessibility metric", choices = metric_choices_for(all_culture_label)),
      radioButtons(
        ns("scale"), "Colour scale",
        choices = c("Quantile bins" = "quantile", "Continuous (linear)" = "linear")
      ),
      checkboxInput(ns("show_facilities"), "Overlay facilities of this category", value = FALSE),
      p(
        class = "small text-muted",
        "Quantile bins give each colour a similar number of cells and reveal contrast",
        "in skewed distances; the linear scale preserves absolute magnitudes."
      )
    ),
    layout_columns(
      col_widths = c(8, 4),
      card(
        full_screen = TRUE,
        card_header(textOutput(ns("map_title"), inline = TRUE)),
        card_body(padding = 0, leafletOutput(ns("map"), height = "100%"))
      ),
      layout_columns(
        col_widths = 12,
        row_heights = c(3, 2),
        card(
          full_screen = TRUE,
          card_header("Distribution across cells"),
          plotlyOutput(ns("histogram"), height = "100%"),
          p(class = "chart-note", "Each bar counts 200 m cells; the dashed line marks the population-weighted mean.")
        ),
        card(
          card_header("Summary"),
          uiOutput(ns("summary"))
        )
      )
    )
  )
}

mod_accessibility_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    grid <- app_data$population_access_wgs84
    culture <- app_data$culture
    map_ready <- map_ready_signal(input, "map")

    # The 500 m count exists only for all facilities combined.
    observeEvent(input$category, {
      choices <- metric_choices_for(input$category)
      selected <- if (isolate(input$metric) %in% choices) isolate(input$metric) else "distance"
      updateRadioButtons(session, "metric", choices = choices, selected = selected)
    })

    selection <- reactive({
      req(input$category, input$metric)
      column <- get_metric_column(input$category, input$metric)
      # Wait for the metric input to catch up after a category change.
      req(column %in% names(grid))
      list(category = input$category, metric = input$metric, column = column)
    })

    metric_values <- reactive(grid[[selection()$column]])

    output$map_title <- renderText({
      paste0(get_metric_label(selection()$metric), " — ", selection()$category)
    })

    output$map <- renderLeaflet({
      base_leaflet() |>
        addMapPane("facilities", zIndex = 450)
    })

    observe({
      req(map_ready())
      sel <- selection()
      values <- metric_values()
      palette <- build_palette(values, input$scale)
      suffix <- if (get_metric_type(sel$metric) == "distance") " m" else ""

      leafletProxy("map") |>
        clearGroup("grid") |>
        removeControl("legend") |>
        addPolygons(
          data = grid,
          group = "grid",
          fillColor = palette(values),
          fillOpacity = 0.78,
          color = ui_colors$surface,
          weight = 0.4,
          popup = create_grid_popup(grid, sel$category, sel$metric),
          label = paste0(format_count(grid$ind), " residents · ", format_metric(values, sel$metric)),
          highlightOptions = highlightOptions(weight = 2, color = ui_colors$text, bringToFront = TRUE)
        ) |>
        addLegend(
          layerId = "legend",
          position = "bottomright",
          pal = palette,
          values = values,
          title = get_metric_label(sel$metric, short = TRUE),
          labFormat = labelFormat(suffix = suffix, big.mark = ",", digits = 0),
          opacity = 0.9
        )
    })

    observe({
      req(map_ready())
      proxy <- leafletProxy("map") |> clearGroup("facilities")
      req(isTRUE(input$show_facilities))

      category <- selection()$category
      facilities <- if (category == all_culture_label) culture else filter(culture, cultural_category == category)

      proxy |>
        addCircleMarkers(
          data = facilities,
          group = "facilities",
          lng = ~longitude,
          lat = ~latitude,
          radius = 3.5,
          stroke = TRUE,
          color = ui_colors$surface,
          weight = 1,
          fillColor = ui_colors$highlight,
          fillOpacity = 0.95,
          label = ~name,
          popup = create_facility_popup(facilities),
          options = pathOptions(pane = "facilities")
        )
    })

    output$histogram <- renderPlotly({
      sel <- selection()
      values <- metric_values()
      mean_value <- weighted_mean_safe(values, grid$ind)

      plot_ly(
        x = values,
        type = "histogram",
        nbinsx = 40,
        marker = list(color = ui_colors$accent, line = list(color = ui_colors$surface, width = 1)),
        hovertemplate = "%{x}: %{y} cells<extra></extra>"
      ) |>
        style_plotly(
          x_title = get_metric_label(sel$metric, short = TRUE),
          y_title = "Cells",
          bargap = 0.02,
          shapes = list(list(
            type = "line", xref = "x", yref = "paper",
            x0 = mean_value, x1 = mean_value, y0 = 0, y1 = 1,
            line = list(color = ui_colors$text, dash = "dash", width = 1.5)
          ))
        )
    })

    output$summary <- renderUI({
      sel <- selection()
      values <- metric_values()
      weights <- grid$ind

      share_label <- if (get_metric_type(sel$metric) == "distance") {
        "Residents more than 500 m away"
      } else {
        "Residents with none nearby"
      }
      share_value <- if (get_metric_type(sel$metric) == "distance") {
        sum(weights[values > 500], na.rm = TRUE) / sum(weights)
      } else {
        sum(weights[values == 0], na.rm = TRUE) / sum(weights)
      }

      items <- list(
        "Population-weighted mean" = format_metric(weighted_mean_safe(values, weights), sel$metric, count_accuracy = 0.1),
        "Median across cells" = format_metric(median(values, na.rm = TRUE), sel$metric),
        "90th percentile of cells" = format_metric(quantile(values, 0.9, na.rm = TRUE, names = FALSE), sel$metric)
      )
      items[[share_label]] <- format_percent(share_value)
      stat_list(.items = items)
    })
  })
}
