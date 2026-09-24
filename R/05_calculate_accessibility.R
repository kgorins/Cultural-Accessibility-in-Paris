library(tidyverse)
library(sf)
library(here)

# Load population grid
population_paris <- st_read(here("data", "processed", "population_grid_paris.gpkg"), quiet = TRUE)

# Load cultural facilities
culture <- read_csv(here("data", "processed", "cultural_facilities_paris.csv"), show_col_types = FALSE)
culture_sf <- st_as_sf(culture, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
culture_sf <- st_transform(culture_sf, st_crs(population_paris)) # transform to the same format

stopifnot

# Represent each population cell by its centre
population_points <- st_centroid(population_paris)

# Find the nearest cultural facility
nearest_index <- st_nearest_feature(population_points, culture_sf)
nearest_distance <- st_distance(population_points, culture_sf[nearest_index, ], by_element = TRUE)
nearest_distance_m <- as.numeric(nearest_distance)

stopifnot(all(population_access$nearest_culture_m >= 0))
stopifnot(!any(is.na(population_access$nearest_culture_m)))

# Add results to population grid
population_access <- population_paris |>
  mutate(nearest_culture_m = nearest_distance_m)

# Store nearest facility
population_access <- population_access |>
  mutate(nearest_facility_name = culture_sf$name[nearest_index],
      nearest_facility_type = culture_sf$facility_type[nearest_index],
      nearest_cultural_category = culture_sf$cultural_category[nearest_index])

# Within 500m
culture_500m <- st_is_within_distance(population_points, culture_sf, dist = 500)
population_access$culture_within_500m <- lengths(culture_500m)

# Within 1km
culture_1000m <- st_is_within_distance(population_points, culture_sf, dist = 1000)
population_access$culture_within_1000m <- lengths(culture_1000m)

stopifnot(all(population_access$culture_within_1000m >= population_access$culture_within_500m))

# Accessibility map
access_map <- ggplot(population_access) +
  geom_sf(aes(fill = nearest_culture_m), color = NA) +
  labs(
    title = "Distance to the nearest cultural facility",
    subtitle = "Paris, 200 m population grid",
    fill = "Distance (m)"
  )

ggsave(
  here("output", "figures", "distance_to_nearest_culture.png"),
  plot = access_map,
  width = 9,
  height = 8
)

# Cultural density map
density_map <- ggplot(population_access) +
  geom_sf(aes(fill = culture_within_1000m), color = NA) +
  labs(
    title = "Cultural facilities within 1 km",
    subtitle = "Paris, 200 m population grid",
    fill = "Facilities"
  )

ggsave(here("output", "figures", "culture_within_1km.png"),
  plot = density_map,
  width = 9,
  height = 8
)

st_write(population_access, here("data", "processed", "population_accessibility.gpkg"),
  delete_dsn = TRUE
)