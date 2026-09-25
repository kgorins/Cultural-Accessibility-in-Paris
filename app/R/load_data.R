# Loads the pipeline's processed outputs once at startup. Nothing here
# recomputes distances: the app only filters, summarises and renders.

processed_path <- function(file) here::here("data", "processed", file)

check_file_exists <- function(path) {
  if (!file.exists(path)) {
    stop(
      "Missing processed dataset: ", path, "\n",
      "Run the analysis pipeline first (source('R/run_all.R') from the project root).",
      call. = FALSE
    )
  }
  invisible(path)
}

check_columns <- function(data, required, dataset_name) {
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop(
      "Dataset '", dataset_name, "' is missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(data)
}

required_facility_columns <- c(
  "name", "facility_type", "cultural_category", "arrondissement_number",
  "arrondissement", "adresse", "latitude", "longitude"
)

required_grid_columns <- c(
  "idcar_200m", "ind", "men", "men_pauv", "poverty_rate", "owner_rate",
  "single_parent_rate", "social_housing_rate", "mean_living_standard",
  "nearest_culture_m", "culture_within_500m", "culture_within_1000m",
  "nearest_facility_name", "nearest_facility_type", "nearest_cultural_category",
  get_distance_column(category_choices),
  get_count_column(category_choices, 1000)
)

load_app_data <- function() {
  facilities_path <- check_file_exists(processed_path("cultural_facilities_paris.csv"))
  grid_path <- check_file_exists(processed_path("population_accessibility_by_category.gpkg"))

  culture <- readr::read_csv(facilities_path, show_col_types = FALSE) |>
    check_columns(required_facility_columns, "cultural_facilities_paris.csv")

  unknown_categories <- setdiff(unique(culture$cultural_category), facility_categories)
  if (length(unknown_categories) > 0) {
    stop("Unexpected cultural categories in facilities data: ",
         paste(unknown_categories, collapse = ", "), call. = FALSE)
  }

  culture_sf <- sf::st_as_sf(
    culture,
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )

  # Same analysis population as R/07_inequality_analysis.R: populated cells,
  # cell-based (not population-weighted) poverty quintiles.
  population_access <- sf::st_read(grid_path, quiet = TRUE) |>
    check_columns(required_grid_columns, "population_accessibility_by_category.gpkg") |>
    dplyr::filter(ind > 0) |>
    dplyr::mutate(
      poverty_quintile = dplyr::ntile(poverty_rate, 5),
      poverty_group = factor(poverty_quintile, levels = 1:5, labels = poverty_group_labels),
      population_density_km2 = ind / 0.04
    )

  population_access_wgs84 <- sf::st_transform(population_access, 4326)

  list(
    culture = culture,
    culture_sf = culture_sf,
    population_access = population_access,
    population_access_wgs84 = population_access_wgs84,
    grid_attributes = sf::st_drop_geometry(population_access)
  )
}
