library(tidyverse)
library(sf)
library(here)

# Load all the data
population_access <- st_read(here("data", "processed", "population_accessibility.gpkg"), quiet = TRUE)

culture <- read_csv(here("data", "processed", "cultural_facilities_paris.csv"), show_col_types = FALSE)
culture_sf <- st_as_sf(culture, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
culture_sf <- st_transform(culture_sf, st_crs(population_access))

# Population centroids
population_points <- st_centroid(population_access)

# Filter by categories
heritage <- culture_sf |> filter(cultural_category == "Heritage")
press <- culture_sf |> filter(cultural_category == "Press & stationery")
books <- culture_sf |> filter(cultural_category == "Books & reading")
education <- culture_sf |> filter(cultural_category == "Education & archive")
perform <- culture_sf |> filter(cultural_category == "Performing arts")
cinemas <- culture_sf |> filter(cultural_category == "Cinema")
parks <- culture_sf |> filter(cultural_category == "Parks & gardens")
museums <- culture_sf |> filter(cultural_category == "Museum")
arts <- culture_sf |> filter(cultural_category == "Arts & creation")

# Count facilities within 1000m
calculate_category_access <- function(population_points, facilities, category) 
{
  category_facilities <- facilities |> filter(cultural_category == category)
  
  stopifnot(nrow(category_facilities) > 0)
  
  nearest_index <- st_nearest_feature(population_points, category_facilities)
  
  # Compute distance to the nearest facility
  nearest_distance_m <- as.numeric(
    st_distance(
      population_points,
      category_facilities[nearest_index, ],
      by_element = TRUE
    )
  )
  
  within_1000m <- lengths(st_is_within_distance(population_points, category_facilities, dist = 1000))
  
  tibble(nearest_distance_m = nearest_distance_m, facilities_within_1000m = within_1000m)
}

# Create safe column names
category_to_column <- function(category) 
{
  category |>
    str_to_lower() |>
    str_replace_all("&", "") |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_remove("_$")
}

# Run the calculate_category_access function for all categories
categories <- culture_sf |>
  st_drop_geometry() |>
  distinct(cultural_category) |>
  pull(cultural_category)


for (category in categories) 
{
  category_access <- calculate_category_access(population_points, culture_sf, category)
  
  column_suffix <- category_to_column(category)
  
  population_access[[paste0("nearest_", column_suffix, "_m")]] <- category_access$nearest_distance_m
  population_access[[paste0(column_suffix, "_within_1000m")]] <- category_access$facilities_within_1000m
}

distance_columns <- population_access |>
  st_drop_geometry() |>
  select(starts_with("nearest_")) |>
  select(ends_with("_m"))

# Distances must be non-negatives
stopifnot(all(map_lgl(distance_columns, ~ all(.x >= 0, na.rm = TRUE))))

# Create a comparison table
category_summary <- tibble(
  category = categories,
  median_distance_m = map_dbl(categories, function(category) 
    {
      suffix <- category_to_column(category)
      median(population_access[[paste0("nearest_", suffix, "_m")]],na.rm = TRUE)
    }
  )
)

category_summary |> arrange(median_distance_m)

# Plot it
category_plot <- category_summary |> ggplot(aes(x = reorder(category, median_distance_m), y = median_distance_m)) +
  geom_col() +
  coord_flip() +
  labs(title = "Median distance to cultural facilities by category", x = NULL, y = "Median distance (m)")

ggsave(here("output", "figures", "median_distance_by_category.png"),
  plot = category_plot,
  width = 8,
  height = 6
)

# Map per category
cinema_map <- ggplot(population_access) +
  geom_sf(aes(fill = nearest_cinema_m), color = NA) +
  labs(title = "Distance to nearest cinema", subtitle = "Paris, 200 m population grid", fill = "Distance (m)")

ggsave(here("output", "figures", "distance_to_nearest_cinema.png"),
  plot = cinema_map,
  width = 9,
  height = 8
)

# Save the dataset
st_write(population_access, here("data", "processed", "population_accessibility_by_category.gpkg"), delete_dsn = TRUE)