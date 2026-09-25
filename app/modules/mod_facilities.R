# Facilities: where cultural facilities are and what they are.

arrondissement_choices <- c(
  "All arrondissements" = "all",
  rlang::set_names(as.character(1:20), paste0(c("1er", paste0(2:20, "e")), " arrondissement"))
)

mod_facilities_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      title = "Filters",
      width = 290,
      checkboxGroupInput(
        ns("categories"),
        "Cultural category",
        choices = facility_categories,
        selected = facility_categories
      ),
      selectInput(ns("arrondissement"), "Arrondissement", choices = arrondissement_choices),
      selectizeInput(
        ns("facility_types"),
        "Facility type",
        choices = NULL,
        multiple = TRUE,
        options = list(placeholder = "All types in selected categories", plugins = list("remove_button"))
      ),
      checkboxInput(ns("cluster"), "Cluster nearby markers", value = TRUE),
      actionButton(ns("reset"), "Reset filters", icon = icon("rotate-left"), class = "btn-outline-secondary btn-sm"),
      hr(),
      uiOutput(ns("selection_summary"))
    ),
    card(
      full_screen = TRUE,
      card_header(
        class = "d-flex justify-content-between align-items-center",
        "Cultural facilities in Paris",
        tags$span(class = "small text-muted", "Click a marker for details")
      ),
      card_body(padding = 0, leafletOutput(ns("map"), height = "100%"))
    )
  )
}

mod_facilities_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    culture <- app_data$culture
    map_ready <- map_ready_signal(input, "map")

    # Facility-type choices follow the selected categories; keep still-valid picks.
    observeEvent(input$categories, {
      types <- culture |>
        filter(cultural_category %in% input$categories) |>
        distinct(facility_type) |>
        drop_na() |>
        arrange(facility_type) |>
        pull(facility_type)

      updateSelectizeInput(
        session, "facility_types",
        choices = types,
        selected = intersect(isolate(input$facility_types), types)
      )
    }, ignoreNULL = FALSE)

    observeEvent(input$reset, {
      updateCheckboxGroupInput(session, "categories", selected = facility_categories)
      updateSelectInput(session, "arrondissement", selected = "all")
      updateSelectizeInput(session, "facility_types", selected = character(0))
      updateCheckboxInput(session, "cluster", value = TRUE)
    })

    filtered_facilities <- reactive({
      selected <- culture |> filter(cultural_category %in% input$categories)

      if (!is.null(input$arrondissement) && input$arrondissement != "all") {
        selected <- selected |> filter(arrondissement_number == as.integer(input$arrondissement))
      }
      if (length(input$facility_types) > 0) {
        selected <- selected |> filter(facility_type %in% input$facility_types)
      }
      selected
    })

    output$map <- renderLeaflet({
      base_leaflet() |>
        addLegend(
          position = "bottomright",
          colors = unname(category_colors),
          labels = names(category_colors),
          title = "Cultural category",
          opacity = 1,
          className = "info legend legend-categories"
        )
    })

    # Redraw only the marker layer when filters change; the base map persists.
    observe({
      req(map_ready())
      facilities <- filtered_facilities()

      proxy <- leafletProxy("map") |>
        clearMarkers() |>
        clearMarkerClusters()

      if (nrow(facilities) == 0) return()

      proxy |>
        addCircleMarkers(
          data = facilities,
          lng = ~longitude,
          lat = ~latitude,
          radius = 6,
          stroke = TRUE,
          color = ui_colors$surface,
          weight = 1.5,
          fillColor = unname(category_colors[facilities$cultural_category]),
          fillOpacity = 0.9,
          popup = create_facility_popup(facilities),
          label = ~name,
          clusterOptions = if (isTRUE(input$cluster)) {
            markerClusterOptions(showCoverageOnHover = FALSE, maxClusterRadius = 45)
          }
        )
    })

    # Zoom to the chosen arrondissement (independent of the other filters).
    observeEvent(input$arrondissement, {
      req(map_ready())
      proxy <- leafletProxy("map")

      if (input$arrondissement == "all") {
        proxy |> setView(paris_view$lng, paris_view$lat, paris_view$zoom)
      } else {
        area <- culture |> filter(arrondissement_number == as.integer(input$arrondissement))
        req(nrow(area) > 0)
        proxy |> fitBounds(
          min(area$longitude), min(area$latitude),
          max(area$longitude), max(area$latitude)
        )
      }
    }, ignoreInit = TRUE)

    output$selection_summary <- renderUI({
      validate(need(length(input$categories) > 0, "Select at least one cultural category."))
      facilities <- filtered_facilities()
      validate(need(nrow(facilities) > 0, "No facilities match the current filters."))

      top_types <- facilities |>
        count(facility_type, sort = TRUE) |>
        slice_head(n = 3)

      tagList(
        stat_list(
          "Facilities shown" = format_count(nrow(facilities)),
          "Categories" = format_count(n_distinct(facilities$cultural_category)),
          "Facility types" = format_count(n_distinct(facilities$facility_type))
        ),
        tags$p(class = "small text-muted mb-1", "Most common types"),
        tags$ul(
          class = "compact-list small",
          map2(top_types$facility_type, top_types$n, \(type, n) tags$li(type, tags$span(class = "text-muted", paste0("(", n, ")"))))
        )
      )
    })
  })
}
