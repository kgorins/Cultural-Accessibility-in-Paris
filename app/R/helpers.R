# Shared lookups, formatters and small builders used across modules.

# ---- Category lookup ---------------------------------------------------------

# Maps human-readable category labels to the column suffixes produced by the
# pipeline (R/06_category_accessibility.R), e.g. "Cinema" -> nearest_cinema_m.
category_lookup <- tibble::tribble(
  ~label,                    ~suffix,
  "All cultural facilities", "culture",
  "Heritage",                "heritage",
  "Books & reading",         "books_reading",
  "Press & stationery",      "press_stationery",
  "Education & archives",    "education_archives",
  "Performing arts",         "performing_arts",
  "Cinema",                  "cinema",
  "Museum",                  "museum",
  "Parks & gardens",         "parks_gardens",
  "Arts & creation",         "arts_creation"
)

all_culture_label <- "All cultural facilities"
category_choices <- category_lookup$label
facility_categories <- setdiff(category_choices, all_culture_label)

get_category_suffix <- function(category) {
  suffix <- category_lookup$suffix[match(category, category_lookup$label)]
  if (anyNA(suffix)) {
    stop("Unknown cultural category: ", paste(category[is.na(suffix)], collapse = ", "))
  }
  suffix
}

get_distance_column <- function(category) {
  paste0("nearest_", get_category_suffix(category), "_m")
}

get_count_column <- function(category, radius = 1000) {
  paste0(get_category_suffix(category), "_within_", radius, "m")
}

# ---- Accessibility metrics ---------------------------------------------------

metric_lookup <- tibble::tribble(
  ~id,          ~label,                          ~short_label,             ~type,
  "distance",   "Distance to nearest facility",  "Distance (m)",           "distance",
  "count_1000", "Number of facilities within 1 km", "Facilities within 1 km", "count",
  "count_500",  "Number of facilities within 500 m", "Facilities within 500 m", "count"
)

# The pipeline only computed the 500 m count for all facilities combined.
metric_choices_for <- function(category) {
  ids <- if (identical(category, all_culture_label)) metric_lookup$id else c("distance", "count_1000")
  rows <- metric_lookup[match(ids, metric_lookup$id), ]
  rlang::set_names(rows$id, rows$label)
}

# Metrics available for every category (used by the comparison tabs).
comparison_metric_choices <- c(
  "Distance to nearest facility" = "distance",
  "Facilities within 1 km" = "count_1000"
)

get_metric_column <- function(category, metric) {
  switch(metric,
    distance   = get_distance_column(category),
    count_1000 = get_count_column(category, 1000),
    count_500  = get_count_column(category, 500),
    stop("Unknown metric: ", metric)
  )
}

get_metric_type <- function(metric) {
  metric_lookup$type[match(metric, metric_lookup$id)]
}

get_metric_label <- function(metric, short = FALSE) {
  column <- if (short) "short_label" else "label"
  metric_lookup[[column]][match(metric, metric_lookup$id)]
}

# ---- Socioeconomic variables -------------------------------------------------

socio_lookup <- tibble::tribble(
  ~column,                  ~label,                              ~format,
  "mean_living_standard",   "Mean living standard (€/year)",     "euro",
  "poverty_rate",           "Poverty rate",                      "percent",
  "social_housing_rate",    "Social housing indicator (approx.)", "percent",
  "owner_rate",             "Owner-occupier rate",               "percent",
  "single_parent_rate",     "Single-parent household rate",      "percent",
  "population_density_km2", "Population density (residents/km²)", "number"
)

socio_choices <- rlang::set_names(socio_lookup$column, socio_lookup$label)

get_socio_label <- function(column) socio_lookup$label[match(column, socio_lookup$column)]
get_socio_format <- function(column) socio_lookup$format[match(column, socio_lookup$column)]

# ---- Poverty groups ----------------------------------------------------------

poverty_group_labels <- c(
  "Lowest poverty", "Low-medium", "Middle", "High-medium", "Highest poverty"
)

# ---- Colours -----------------------------------------------------------------

# Categorical slots from a colour-vision-deficiency-validated palette, in fixed
# order; the ninth, smallest category (5 facilities) takes a neutral grey rather
# than a generated hue. Colour follows the category everywhere in the app.
category_colors <- c(
  "Heritage"             = "#2a78d6",
  "Press & stationery"   = "#eb6834",
  "Books & reading"      = "#1baf7a",
  "Education & archives" = "#eda100",
  "Performing arts"      = "#e87ba4",
  "Parks & gardens"      = "#008300",
  "Museum"               = "#4a3aa7",
  "Cinema"               = "#e34948",
  "Arts & creation"      = "#8a8984"
)

# Single-hue sequential ramp (light = low, dark = high) for choropleths.
sequential_ramp <- c(
  "#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#256abf", "#184f95", "#0d366b"
)

# Ordinal ramp for the ordered poverty groups (lightest step still clears 2:1).
poverty_group_colors <- rlang::set_names(
  c("#86b6ef", "#5598e7", "#2a78d6", "#1c5cab", "#104281"),
  poverty_group_labels
)

ui_colors <- list(
  text      = "#1f1f1d",
  muted     = "#6b6a66",
  grid      = "#e8e7e3",
  surface   = "#ffffff",
  accent    = "#2a78d6",
  highlight = "#eb6834"
)

# ---- Formatters --------------------------------------------------------------

format_distance <- function(x) {
  ifelse(is.na(x), "–", scales::label_number(accuracy = 1, big.mark = ",", suffix = " m")(x))
}

format_percent <- function(x, accuracy = 0.1) {
  ifelse(is.na(x), "–", scales::label_percent(accuracy = accuracy)(x))
}

format_count <- function(x, accuracy = 1) {
  ifelse(is.na(x), "–", scales::label_number(accuracy = accuracy, big.mark = ",")(x))
}

format_euro <- function(x) {
  ifelse(is.na(x), "–", scales::label_number(accuracy = 1, big.mark = ",", prefix = "€")(x))
}

# Use count_accuracy = 0.1 for averaged counts, which are rarely whole numbers.
format_metric <- function(x, metric, count_accuracy = 1) {
  if (get_metric_type(metric) == "distance") format_distance(x) else format_count(x, count_accuracy)
}

format_socio <- function(x, column) {
  switch(get_socio_format(column),
    euro    = format_euro(x),
    percent = format_percent(x),
    format_count(x)
  )
}

# ---- Statistics --------------------------------------------------------------

# Population-weighted mean that ignores missing values and non-positive weights
# instead of propagating NA or failing on an empty subset.
weighted_mean_safe <- function(x, w) {
  keep <- !is.na(x) & !is.na(w) & w > 0
  if (!any(keep)) return(NA_real_)
  stats::weighted.mean(x[keep], w[keep])
}

# ---- Popups ------------------------------------------------------------------

popup_row <- function(label, value) {
  sprintf(
    "<tr><th>%s</th><td>%s</td></tr>",
    htmltools::htmlEscape(label),
    htmltools::htmlEscape(value)
  )
}

popup_table <- function(title, ...) {
  rows <- purrr::pmap_chr(list(...), function(...) paste0(..., collapse = ""))
  sprintf(
    "<div class='map-popup'><div class='map-popup-title'>%s</div><table>%s</table></div>",
    htmltools::htmlEscape(title),
    rows
  )
}

create_facility_popup <- function(data) {
  popup_table(
    data$name,
    popup_row("Category", data$cultural_category),
    popup_row("Type", data$facility_type),
    popup_row("Arrondissement", data$arrondissement),
    popup_row("Address", data$adresse)
  )
}

create_grid_popup <- function(data, category, metric) {
  metric_column <- get_metric_column(category, metric)
  popup_table(
    "200 m grid cell",
    popup_row("Cell ID", data$idcar_200m),
    popup_row("Population", format_count(data$ind)),
    popup_row("Poverty rate", format_percent(data$poverty_rate)),
    popup_row("Mean living standard", format_euro(data$mean_living_standard)),
    popup_row(
      paste0(get_metric_label(metric), " (", category, ")"),
      format_metric(data[[metric_column]], metric)
    ),
    popup_row(
      "Nearest facility (any category)",
      paste0(data$nearest_facility_name, " · ", format_distance(data$nearest_culture_m))
    )
  )
}

# ---- Maps --------------------------------------------------------------------

paris_view <- list(lng = 2.3425, lat = 48.8589, zoom = 12)

# Keyless basemaps (CARTO tiles now require an API key). The light grey canvas
# keeps attention on the data; OpenStreetMap adds street context on demand.
base_leaflet <- function() {
  leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 10)) |>
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas, group = "Light grey") |>
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OpenStreetMap") |>
    leaflet::addLayersControl(
      baseGroups = c("Light grey", "OpenStreetMap"),
      position = "topright",
      options = leaflet::layersControlOptions(collapsed = TRUE)
    ) |>
    leaflet::setView(paris_view$lng, paris_view$lat, paris_view$zoom)
}

# Quantile bins show spatial contrast in skewed distances; linear keeps the
# true magnitude. Falls back to linear when too many ties (e.g. mostly zeros).
build_palette <- function(values, scale = c("quantile", "linear"), n_bins = 7) {
  scale <- match.arg(scale)
  finite <- values[is.finite(values)]

  if (scale == "quantile") {
    breaks <- stats::quantile(finite, probs = seq(0, 1, length.out = n_bins + 1), names = FALSE)
    breaks <- unique(c(floor(min(finite)), round(breaks[-c(1, length(breaks))]), ceiling(max(finite))))
    if (length(breaks) >= 3) {
      return(leaflet::colorBin(sequential_ramp, domain = finite, bins = breaks, na.color = "transparent"))
    }
  }

  leaflet::colorNumeric(sequential_ramp, domain = range(finite), na.color = "transparent")
}

# Leaflet proxy calls are dropped if the map widget has not rendered yet (maps
# in hidden tabs render lazily), so observers wait for the first bounds event.
map_ready_signal <- function(input, map_id) {
  ready <- shiny::reactiveVal(FALSE)
  shiny::observeEvent(input[[paste0(map_id, "_bounds")]], ready(TRUE), once = TRUE)
  ready
}

# ---- Plotly ------------------------------------------------------------------

style_plotly <- function(p, x_title = NULL, y_title = NULL, xaxis = list(), yaxis = list(),
                         showlegend = FALSE, ...) {
  axis_defaults <- list(
    gridcolor = ui_colors$grid, zeroline = FALSE, linecolor = ui_colors$grid,
    tickfont = list(color = ui_colors$muted), automargin = TRUE
  )
  p |>
    plotly::layout(
      font = list(family = "system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif",
                  size = 12, color = ui_colors$text),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      # An empty title (rather than NULL) stops plotly falling back to the R expression.
      xaxis = utils::modifyList(c(axis_defaults, list(title = list(text = x_title %||% ""))), xaxis),
      yaxis = utils::modifyList(c(axis_defaults, list(title = list(text = y_title %||% ""))), yaxis),
      showlegend = showlegend,
      margin = list(l = 10, r = 10, t = 10, b = 10),
      hoverlabel = list(bgcolor = ui_colors$surface, bordercolor = "#d0cfca",
                        font = list(color = ui_colors$text)),
      ...
    ) |>
    plotly::config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d", "toggleSpikelines")
    )
}

# ---- UI snippets -------------------------------------------------------------

category_select_input <- function(id, label = "Cultural category", selected = all_culture_label) {
  shiny::selectInput(id, label, choices = category_choices, selected = selected)
}

# Label/value pairs, passed as named arguments or as a prebuilt named list.
stat_list <- function(..., .items = list(...)) {
  items <- .items
  htmltools::tags$dl(
    class = "stat-list",
    purrr::imap(items, function(value, label) {
      htmltools::tagList(htmltools::tags$dt(label), htmltools::tags$dd(value))
    })
  )
}
