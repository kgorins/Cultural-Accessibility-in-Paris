# Relationships: exploratory bivariate associations between socioeconomic
# indicators and accessibility. Descriptive only — no causal interpretation.

mod_relationships_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      title = "Variables",
      width = 290,
      selectInput(ns("x_var"), "Socioeconomic indicator (x)", choices = socio_choices),
      category_select_input(ns("category"), label = "Cultural category (y)", selected = "Cinema"),
      radioButtons(ns("metric"), "Accessibility metric (y)", choices = comparison_metric_choices),
      checkboxInput(ns("show_fit"), "Show regression line", value = TRUE),
      checkboxInput(ns("log_x"), "Log scale for x", value = FALSE)
    ),
    div(
      class = "alert alert-warning small d-flex gap-2 align-items-start",
      icon("triangle-exclamation"),
      div(
        strong("Exploratory analysis. "),
        "Correlations describe how two area-level measures vary together across cells. They do not",
        "show that one causes the other, and p-values are optimistic because neighbouring cells are",
        "spatially autocorrelated."
      )
    ),
    layout_columns(
      col_widths = c(9, 3),
      fill = FALSE,
      card(
        full_screen = TRUE,
        card_header(textOutput(ns("chart_title"), inline = TRUE)),
        plotlyOutput(ns("scatter"), height = "520px"),
        p(class = "chart-note", "Each point is one populated 200 m cell. Hover for details.")
      ),
      card(
        card_header("Association"),
        uiOutput(ns("stats")),
        p(
          class = "small text-muted mb-0",
          "Pearson correlation and a simple linear model y ~ x, unweighted, across cells."
        )
      )
    )
  )
}

mod_relationships_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    grid <- app_data$grid_attributes

    pair <- reactive({
      req(input$x_var, input$category, input$metric)
      y_column <- get_metric_column(input$category, input$metric)

      data <- grid |>
        transmute(
          idcar_200m, ind, poverty_rate, mean_living_standard,
          x = .data[[input$x_var]],
          y = .data[[y_column]]
        ) |>
        filter(is.finite(x), is.finite(y))

      if (isTRUE(input$log_x)) data <- filter(data, x > 0)

      validate(need(nrow(data) >= 10, "Not enough cells with valid values for this combination."))
      list(
        data = data,
        x_label = get_socio_label(input$x_var),
        y_label = paste0(get_metric_label(input$metric, short = TRUE), " — ", input$category)
      )
    })

    fit <- reactive({
      data <- pair()$data
      # A constant variable (e.g. no facilities within 1 km anywhere) has no correlation.
      validate(need(sd(data$x) > 0 && sd(data$y) > 0, "One of the variables is constant; no association can be estimated."))
      model_x <- if (isTRUE(input$log_x)) log10(data$x) else data$x
      list(
        test = cor.test(model_x, data$y),
        model = lm(data$y ~ model_x),
        model_x = model_x
      )
    })

    output$chart_title <- renderText(paste0(pair()$y_label, " vs ", pair()$x_label))

    output$scatter <- renderPlotly({
      current <- pair()
      data <- current$data

      hover <- paste0(
        "<b>", data$idcar_200m, "</b>",
        "<br>Population: ", format_count(data$ind),
        "<br>Poverty rate: ", format_percent(data$poverty_rate),
        "<br>Mean living standard: ", format_euro(data$mean_living_standard),
        "<br>", current$x_label, ": ", format_socio(data$x, input$x_var),
        "<br>", current$y_label, ": ", format_metric(data$y, input$metric)
      )

      p <- plot_ly(
        x = data$x,
        y = data$y,
        type = "scatter",
        mode = "markers",
        marker = list(
          size = 8, color = ui_colors$accent, opacity = 0.45,
          line = list(color = ui_colors$surface, width = 0.5)
        ),
        text = hover,
        hoverinfo = "text",
        name = "Cells"
      )

      if (isTRUE(input$show_fit)) {
        model <- fit()
        grid_x <- seq(min(model$model_x), max(model$model_x), length.out = 100)
        fitted_y <- coef(model$model)[[1]] + coef(model$model)[[2]] * grid_x
        p <- p |>
          add_lines(
            x = if (isTRUE(input$log_x)) 10^grid_x else grid_x,
            y = fitted_y,
            line = list(color = ui_colors$highlight, width = 2.5),
            hoverinfo = "skip",
            name = "Linear fit",
            inherit = FALSE
          )
      }

      p |>
        style_plotly(
          x_title = current$x_label,
          y_title = current$y_label,
          xaxis = list(type = if (isTRUE(input$log_x)) "log" else "linear"),
          yaxis = list(rangemode = "tozero")
        )
    })

    output$stats <- renderUI({
      model <- fit()
      r <- unname(model$test$estimate)
      p_value <- model$test$p.value
      r_squared <- summary(model$model)$r.squared

      strength <- cut(abs(r), c(-Inf, 0.1, 0.3, 0.5, Inf), labels = c("very weak", "weak", "moderate", "strong"))
      direction <- if (r < 0) "negative" else "positive"

      tagList(
        stat_list(
          "Pearson r" = format_count(r, accuracy = 0.001),
          "p-value" = if (p_value < 0.001) "< 0.001" else format_count(p_value, accuracy = 0.001),
          "R² (linear model)" = format_percent(r_squared),
          "Cells" = format_count(nrow(pair()$data))
        ),
        p(
          class = "small",
          "A ", strong(paste(strength, direction)), " association: a straight-line fit on x captures about ",
          strong(format_percent(r_squared)), " of the variation in y across cells."
        )
      )
    })
  })
}
