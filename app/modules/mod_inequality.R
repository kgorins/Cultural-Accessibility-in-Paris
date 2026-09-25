# Inequality: accessibility compared across cell-based poverty quintiles.

mod_inequality_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      title = "Comparison",
      width = 290,
      category_select_input(ns("category")),
      radioButtons(ns("metric"), "Metric", choices = comparison_metric_choices),
      checkboxGroupInput(
        ns("groups"),
        "Poverty groups",
        choices = poverty_group_labels,
        selected = poverty_group_labels
      ),
      div(
        class = "alert alert-light small mb-0",
        strong("Cell-based quintiles. "),
        "Each group holds one fifth of populated cells ranked by poverty rate, not one fifth",
        "of residents, so group populations differ."
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      fill = FALSE,
      card(
        full_screen = TRUE,
        card_header(textOutput(ns("bar_title"), inline = TRUE)),
        plotlyOutput(ns("bar_chart"), height = "340px"),
        p(class = "chart-note", "Means are weighted by cell population (ind).")
      ),
      card(
        full_screen = TRUE,
        card_header("Distribution across cells"),
        plotlyOutput(ns("box_chart"), height = "340px"),
        p(class = "chart-note", "Unweighted distribution of cell values; box = interquartile range, line = median.")
      )
    ),
    navset_card_underline(
      title = "Summary tables",
      nav_panel("Selected category", DTOutput(ns("summary_table"))),
      nav_panel(
        "All categories compared",
        p(
          class = "chart-note",
          "Population-weighted mean distance (m) by category and poverty group. The ratio compares",
          "the highest- with the lowest-poverty group: above 1 means facilities of that category are",
          "on average farther from residents of high-poverty cells."
        ),
        DTOutput(ns("category_table"))
      )
    )
  )
}

mod_inequality_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    grid <- app_data$grid_attributes

    selected_cells <- reactive({
      validate(need(length(input$groups) > 0, "Select at least one poverty group."))
      grid |>
        filter(poverty_group %in% input$groups) |>
        mutate(poverty_group = droplevels(poverty_group))
    })

    value_column <- reactive({
      req(input$category, input$metric)
      get_metric_column(input$category, input$metric)
    })

    group_summary <- reactive({
      distance_col <- get_distance_column(input$category)
      count_col <- get_count_column(input$category, 1000)

      selected_cells() |>
        group_by(poverty_group) |>
        summarise(
          cells = n(),
          population = sum(ind),
          mean_poverty_rate = weighted_mean_safe(poverty_rate, ind),
          mean_distance = weighted_mean_safe(.data[[distance_col]], ind),
          mean_count = weighted_mean_safe(.data[[count_col]], ind),
          .groups = "drop"
        )
    })

    output$bar_title <- renderText({
      paste0("Population-weighted ", tolower(get_metric_label(input$metric, short = TRUE)), " — ", input$category)
    })

    output$bar_chart <- renderPlotly({
      summary <- group_summary() |>
        mutate(
          value = if (input$metric == "distance") mean_distance else mean_count,
          value_label = format_metric(value, input$metric, count_accuracy = 0.1)
        )

      plot_ly(
        summary,
        x = ~poverty_group,
        y = ~value,
        type = "bar",
        marker = list(
          color = unname(poverty_group_colors[as.character(summary$poverty_group)]),
          line = list(color = ui_colors$surface, width = 2)
        ),
        text = ~value_label,
        textposition = "outside",
        textfont = list(color = ui_colors$text),
        cliponaxis = FALSE,
        customdata = ~format_count(population),
        hovertemplate = "<b>%{x}</b><br>%{text}<br>Residents: %{customdata}<extra></extra>"
      ) |>
        style_plotly(
          x_title = NULL,
          y_title = get_metric_label(input$metric, short = TRUE),
          yaxis = list(rangemode = "tozero")
        )
    })

    output$box_chart <- renderPlotly({
      cells <- selected_cells()
      column <- value_column()

      traces <- split(cells, cells$poverty_group)
      p <- plot_ly()
      for (group in names(traces)) {
        p <- p |>
          add_trace(
            x = rep(group, nrow(traces[[group]])),
            y = traces[[group]][[column]],
            type = "box",
            name = group,
            boxpoints = FALSE,
            fillcolor = scales::alpha(poverty_group_colors[[group]], 0.35),
            line = list(color = poverty_group_colors[[group]], width = 1.5)
          )
      }

      p |>
        style_plotly(
          x_title = NULL,
          y_title = get_metric_label(input$metric, short = TRUE),
          xaxis = list(categoryorder = "array", categoryarray = names(traces))
        )
    })

    output$summary_table <- renderDT({
      group_summary() |>
        transmute(
          `Poverty group` = poverty_group,
          Cells = cells,
          Residents = population,
          `Mean poverty rate` = mean_poverty_rate,
          `Mean distance (m)` = mean_distance,
          `Facilities within 1 km` = mean_count
        ) |>
        datatable(rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE), class = "compact") |>
        formatRound(c("Cells", "Residents", "Mean distance (m)"), digits = 0) |>
        formatRound("Facilities within 1 km", digits = 1) |>
        formatPercentage("Mean poverty rate", digits = 1)
    })

    # Computed on all groups regardless of the checkbox, to keep the ratio meaningful.
    output$category_table <- renderDT({
      comparison <- map(category_choices, \(category) {
        column <- get_distance_column(category)
        grid |>
          group_by(poverty_group) |>
          summarise(value = weighted_mean_safe(.data[[column]], ind), .groups = "drop") |>
          mutate(Category = category)
      }) |>
        list_rbind() |>
        pivot_wider(names_from = poverty_group, values_from = value) |>
        mutate(`Highest / lowest` = `Highest poverty` / `Lowest poverty`)

      datatable(comparison, rownames = FALSE, class = "compact",
                options = list(dom = "t", paging = FALSE)) |>
        formatRound(poverty_group_labels, digits = 0) |>
        formatRound("Highest / lowest", digits = 2) |>
        formatStyle(
          "Highest / lowest",
          fontWeight = "600",
          color = styleInterval(c(0.95, 1.05), c("#1c5cab", ui_colors$text, "#b8431a"))
        )
    })
  })
}
