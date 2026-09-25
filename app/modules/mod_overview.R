# Overview: headline metrics, project framing and two compact figures.

mod_overview_ui <- function(id) {
  ns <- NS(id)

  headline_box <- function(title, output_id, icon_name, note = NULL) {
    value_box(
      title = title,
      value = textOutput(ns(output_id), inline = TRUE),
      showcase = icon(icon_name),
      if (!is.null(note)) tags$p(class = "value-box-note", note)
    )
  }

  tagList(
    div(
      class = "page-intro",
      h2("How close are Parisians to culture?"),
      p(
        class = "lead",
        "An exploration of how access to cultural facilities varies across Paris's",
        "200 m population grid, and how it is associated with local socioeconomic conditions."
      )
    ),
    layout_column_wrap(
      width = 1/3,
      fill = FALSE,
      class = "headline-boxes",
      headline_box("Cultural facilities", "n_facilities", "landmark", "Ministry of Culture, within Paris"),
      headline_box("Populated 200 m cells", "n_cells", "border-all", "INSEE Filosofi grid"),
      headline_box("Residents represented", "population", "users", "Sum of cell populations"),
      headline_box("Mean distance to nearest facility", "weighted_distance", "person-walking", "Population-weighted, straight line"),
      headline_box("Median cell distance", "median_distance", "ruler-horizontal", "Unweighted, across cells"),
      headline_box("Facilities within 1 km", "mean_within_1km", "circle-dot", "Population-weighted mean")
    ),
    layout_columns(
      col_widths = c(5, 7),
      fill = FALSE,
      card(
        card_header("About the project"),
        card_body(
          p(
            "The project combines cultural-facility locations from the French Ministry of Culture",
            "(Basilic) with the INSEE Filosofi 200 m grid. For every populated cell it measures",
            "the straight-line distance to the nearest facility and the number of facilities nearby,",
            "overall and for nine cultural categories."
          ),
          h6("Research questions"),
          tags$ol(
            class = "compact-list",
            tags$li("How are cultural facilities distributed across Paris?"),
            tags$li("How far are residents from the nearest cultural facility?"),
            tags$li("How does accessibility differ between cultural categories?"),
            tags$li("Is cultural accessibility associated with local socioeconomic conditions?")
          ),
          h6("Reading the results"),
          uiOutput(ns("poverty_pattern")),
          p(
            class = "text-muted small mb-0",
            "Patterns are descriptive associations between area-level indicators and",
            "facility locations, not evidence of causal effects. See the Methodology tab for limitations."
          )
        )
      ),
      navset_card_underline(
        title = "Accessibility and supply by category",
        nav_panel(
          "Distance by category",
          plotlyOutput(ns("distance_by_category"), height = "360px"),
          p(class = "chart-note", "Population-weighted mean straight-line distance from cell centroids to the nearest facility of each category.")
        ),
        nav_panel(
          "Facilities by category",
          plotlyOutput(ns("facilities_by_category"), height = "360px"),
          p(class = "chart-note", "Number of facilities per category. Categories differ greatly in size, which affects distances.")
        )
      )
    )
  )
}

mod_overview_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    grid <- app_data$grid_attributes
    culture <- app_data$culture

    output$n_facilities <- renderText(format_count(nrow(culture)))
    output$n_cells <- renderText(format_count(nrow(grid)))
    output$population <- renderText(format_count(sum(grid$ind)))
    output$weighted_distance <- renderText(format_distance(weighted_mean_safe(grid$nearest_culture_m, grid$ind)))
    output$median_distance <- renderText(format_distance(median(grid$nearest_culture_m, na.rm = TRUE)))
    output$mean_within_1km <- renderText(format_count(weighted_mean_safe(grid$culture_within_1000m, grid$ind), accuracy = 0.1))

    category_distances <- tibble(category = facility_categories) |>
      mutate(
        mean_distance = map_dbl(category, \(cat) weighted_mean_safe(grid[[get_distance_column(cat)]], grid$ind))
      ) |>
      arrange(mean_distance)

    # Built from the data so the narrative stays in sync with the pipeline outputs.
    output$poverty_pattern <- renderUI({
      by_group <- grid |>
        group_by(poverty_group) |>
        summarise(distance = weighted_mean_safe(nearest_culture_m, ind), .groups = "drop")
      group_distance <- \(group) format_distance(by_group$distance[by_group$poverty_group == group])

      tags$ul(
        class = "compact-list",
        tags$li(
          "Overall access does not follow a simple poverty gradient: mean distance is ",
          strong(group_distance("Lowest poverty")), " in the lowest-poverty cells, ",
          strong(group_distance("Middle")), " in the middle group and ",
          strong(group_distance("Highest poverty")), " in the highest-poverty cells."
        ),
        tags$li(
          "Patterns differ across categories — some (e.g. museums, heritage) are farther from",
          "high-poverty cells, while others (e.g. performing arts) show the opposite pattern."
        ),
        tags$li("Socioeconomic indicators explain only a small share of the variation in cinema distance.")
      )
    })

    output$distance_by_category <- renderPlotly({
      plot_ly(
        category_distances,
        x = ~mean_distance,
        y = ~factor(category, levels = rev(category)),
        type = "bar",
        orientation = "h",
        marker = list(color = unname(category_colors[category_distances$category])),
        text = ~format_distance(mean_distance),
        textposition = "outside",
        textfont = list(color = ui_colors$text),
        cliponaxis = FALSE,
        hovertemplate = "%{y}: %{text}<extra></extra>"
      ) |>
        style_plotly(x_title = "Population-weighted mean distance (m)", y_title = NULL,
                     xaxis = list(range = c(0, max(category_distances$mean_distance) * 1.15)))
    })

    output$facilities_by_category <- renderPlotly({
      counts <- culture |>
        count(cultural_category) |>
        arrange(n)

      plot_ly(
        counts,
        x = ~n,
        y = ~factor(cultural_category, levels = cultural_category),
        type = "bar",
        orientation = "h",
        marker = list(color = unname(category_colors[counts$cultural_category])),
        text = ~format_count(n),
        textposition = "outside",
        textfont = list(color = ui_colors$text),
        cliponaxis = FALSE,
        hovertemplate = "%{y}: %{text} facilities<extra></extra>"
      ) |>
        style_plotly(x_title = "Number of facilities", y_title = NULL,
                     xaxis = list(range = c(0, max(counts$n) * 1.15)))
    })
  })
}
