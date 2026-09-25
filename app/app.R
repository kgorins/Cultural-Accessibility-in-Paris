# Cultural Accessibility in Paris — interactive exploration layer.
# Run from the project root with: shiny::runApp("app")

library(shiny)
library(bslib)
library(leaflet)
library(plotly)
library(DT)
library(tidyverse)
library(sf)
library(here)
library(scales)

source("R/helpers.R", local = TRUE)
source("R/load_data.R", local = TRUE)
for (module_file in list.files("modules", pattern = "\\.R$", full.names = TRUE)) {
  source(module_file, local = TRUE)
}

app_data <- load_app_data()

app_theme <- bs_theme(
  version = 5,
  bg = "#fcfcfb",
  fg = "#1f1f1d",
  primary = "#2a78d6",
  secondary = "#6b6a66",
  base_font = font_collection("system-ui", "-apple-system", "Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif"),
  "border-radius" = "0.5rem",
  "card-border-color" = "#e8e7e3"
)

ui <- page_navbar(
  id = "main_nav",
  title = tags$span(class = "app-title", icon("landmark"), "Cultural Accessibility in Paris"),
  window_title = "Cultural Accessibility in Paris",
  theme = app_theme,
  fillable = c("facilities", "accessibility"),
  navbar_options = navbar_options(bg = "#ffffff", theme = "light"),
  header = tags$head(tags$link(rel = "stylesheet", href = "styles.css")),
  nav_panel("Overview", value = "overview", icon = icon("house"), mod_overview_ui("overview")),
  nav_panel("Facilities", value = "facilities", icon = icon("location-dot"), mod_facilities_ui("facilities")),
  nav_panel("Accessibility", value = "accessibility", icon = icon("map"), mod_accessibility_ui("accessibility")),
  nav_panel("Inequality", value = "inequality", icon = icon("scale-balanced"), mod_inequality_ui("inequality")),
  nav_panel("Relationships", value = "relationships", icon = icon("chart-line"), mod_relationships_ui("relationships")),
  nav_panel("Data Explorer", value = "data", icon = icon("table"), mod_data_explorer_ui("data")),
  nav_panel("Methodology", value = "methodology", icon = icon("book-open"), mod_methodology_ui("methodology"))
)

server <- function(input, output, session) {
  mod_overview_server("overview", app_data)
  mod_facilities_server("facilities", app_data)
  mod_accessibility_server("accessibility", app_data)
  mod_inequality_server("inequality", app_data)
  mod_relationships_server("relationships", app_data)
  mod_data_explorer_server("data", app_data)
  mod_methodology_server("methodology")
}

shinyApp(ui, server)
