# Data Explorer: searchable, filterable tables of the processed datasets.

explorer_default_columns <- list(
  facilities = c("name", "cultural_category", "facility_type", "arrondissement", "adresse"),
  grid = c(
    "idcar_200m", "ind", "poverty_group", "poverty_rate", "mean_living_standard",
    "nearest_culture_m", "culture_within_1000m", "nearest_facility_name",
    "nearest_cinema_m", "nearest_museum_m", "nearest_books_reading_m", "nearest_performing_arts_m"
  )
)

mod_data_explorer_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      title = "Table",
      width = 290,
      radioButtons(
        ns("dataset"), "Dataset",
        choices = c("Cultural facilities" = "facilities", "Population / accessibility grid" = "grid")
      ),
      selectizeInput(
        ns("columns"), "Columns",
        choices = NULL, multiple = TRUE,
        options = list(plugins = list("remove_button", "drag_drop"))
      ),
      actionButton(ns("default_columns"), "Default columns", class = "btn-outline-secondary btn-sm"),
      hr(),
      downloadButton(ns("download_full"), "Download full dataset (CSV)", class = "btn-sm"),
      p(
        class = "small text-muted mt-2",
        "The CSV button above the table exports the visible columns with current filters applied.",
        "Geometry is never included."
      )
    ),
    card(
      full_screen = TRUE,
      card_header(textOutput(ns("table_title"), inline = TRUE)),
      DTOutput(ns("table"))
    )
  )
}

mod_data_explorer_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    datasets <- list(
      facilities = app_data$culture,
      grid = app_data$grid_attributes
    )
    dataset_labels <- c(facilities = "Cultural facilities", grid = "Population / accessibility grid")

    current_data <- reactive({
      req(input$dataset)
      datasets[[input$dataset]]
    })

    reset_columns <- function() {
      updateSelectizeInput(
        session, "columns",
        choices = names(current_data()),
        selected = explorer_default_columns[[input$dataset]]
      )
    }

    observeEvent(input$dataset, reset_columns())
    observeEvent(input$default_columns, reset_columns())

    visible_data <- reactive({
      data <- current_data()
      # Ignore stale selections from the previously shown dataset.
      columns <- intersect(input$columns, names(data))
      validate(need(length(columns) > 0, "Select at least one column."))
      data[, columns, drop = FALSE]
    })

    output$table_title <- renderText({
      paste0(dataset_labels[[input$dataset]], " · ", format_count(nrow(current_data())), " rows")
    })

    # Client-side processing keeps search, filters and CSV export consistent
    # across all rows; both tables are small enough for this.
    output$table <- renderDT({
      data <- visible_data()

      table <- datatable(
        data,
        rownames = FALSE,
        filter = "top",
        extensions = "Buttons",
        class = "compact stripe hover",
        options = list(
          dom = "Bfrtip",
          buttons = list(list(extend = "csv", text = "CSV", filename = paste0("paris_", input$dataset))),
          pageLength = 15,
          scrollX = TRUE,
          autoWidth = FALSE
        )
      )

      rate_columns <- intersect(names(data), c("poverty_rate", "owner_rate", "single_parent_rate", "social_housing_rate"))
      distance_columns <- names(data)[str_detect(names(data), "^nearest_.*_m$")]
      money_columns <- intersect(names(data), c("mean_living_standard", "population_density_km2"))

      if (length(rate_columns) > 0) table <- formatPercentage(table, rate_columns, digits = 1)
      if (length(distance_columns) > 0) table <- formatRound(table, distance_columns, digits = 0)
      if (length(money_columns) > 0) table <- formatRound(table, money_columns, digits = 0)
      table
    }, server = FALSE)

    output$download_full <- downloadHandler(
      filename = function() paste0("paris_", input$dataset, "_", Sys.Date(), ".csv"),
      content = function(file) write_csv(current_data(), file)
    )
  })
}
