library(tidyverse)
library(sf)
library(here)

# Load latest dataset
population_access <- st_read(here("data", "processed", "population_accessibility_by_category.gpkg"), quiet = TRUE)

# Check cells
population_access |>
  st_drop_geometry() |>
  summarise(
    n_cells = n(),
    n_zero_population = sum(ind == 0, na.rm = TRUE),
    n_missing_poverty = sum(is.na(poverty_rate)),
    n_missing_income = sum(is.na(mean_living_standard))
  )

analysis <- population_access |> filter(ind > 0)

# Calculate a weighted mean distance
# Aka the average nearest-cultural-facility distance experienced by a resident, weighting cells by population
weighted_mean_distance <- weighted.mean(analysis$nearest_culture_m, w = analysis$ind, na.rm = TRUE)

# Create poverty quintiles
analysis <- analysis |>
  mutate(poverty_quintile = ntile(poverty_rate, 5))

analysis <- analysis |>
  mutate(
    poverty_group = factor(
      poverty_quintile,
      levels = 1:5,
      labels = c(
        "Lowest poverty",
        "Low-medium",
        "Middle",
        "High-medium",
        "Highest poverty"
      )
    )
  )

# Compare general cultural access across poverty groups
poverty_access_summary <- analysis |>
  st_drop_geometry() |>
  group_by(poverty_group) |>
  summarise(cells = n(), population = sum(ind, na.rm = TRUE),
            mean_distance_m = weighted.mean(nearest_culture_m, w = ind, na.rm = TRUE),
            mean_facilities_1km = weighted.mean(culture_within_1000m, w = ind, na.rm = TRUE)
  )

# Plot distance by poverty group
poverty_distance_plot <- poverty_access_summary |>
  ggplot(aes(x = poverty_group, y = mean_distance_m)) +
  geom_col() +
  labs(
    title = "Cultural accessibility by poverty level",
    subtitle = "Population-weighted mean distance to nearest cultural facility",
    x = NULL,
    y = "Mean distance (m)"
  )

ggsave(here("output", "figures", "culture_access_by_poverty.png"),
  plot = poverty_distance_plot,
  width = 8,
  height = 6
)

# Compare density
poverty_density_plot <- poverty_access_summary |>
  ggplot(aes(x = poverty_group, y = mean_facilities_1km)) +
  geom_col() +
  labs(
    title = "Cultural facilities within 1 km by poverty level",
    subtitle = "Population-weighted mean",
    x = NULL,
    y = "Facilities within 1 km"
  )

# Compare by category
cinema_summary <- analysis |>
  st_drop_geometry() |>
  group_by(poverty_group) |>
  summarise(population = sum(ind, na.rm = TRUE),
    
    mean_distance_m = weighted.mean(
      nearest_cinema_m,
      w = ind,
      na.rm = TRUE
    ),
    
    mean_cinemas_1km = weighted.mean(
      cinema_within_1000m,
      w = ind,
      na.rm = TRUE
    )
  )

# Convert into long format
distance_long <- analysis |>
  st_drop_geometry() |>
  select(idcar_200m, ind, poverty_group, starts_with("nearest_") & ends_with("_m")) |>
  pivot_longer(
    cols = starts_with("nearest_") & ends_with("_m"),
    names_to = "category",
    values_to = "distance_m"
  )

# Clean names
distance_long <- distance_long |>
  mutate(
    category = category |>
      str_remove("^nearest_") |>
      str_remove("_m$") |>
      str_replace_all("_", " ") |>
      str_to_title()
  )

# Summarize all categories
category_poverty_summary <- distance_long |>
  group_by(poverty_group, category) |>
  summarise(population = sum(ind, na.rm = TRUE),
    mean_distance_m = weighted.mean(distance_m, w = ind, na.rm = TRUE), .groups = "drop"
  )

# Plot categories against poverty groups
category_poverty_plot <- category_poverty_summary |>
  filter(category != "Culture") |>
  ggplot(aes(x = poverty_group, y = mean_distance_m)) +
  geom_col() +
  facet_wrap(~ category, scales = "free_y") +
  labs(
    title = "Cultural accessibility by poverty level",
    x = NULL,
    y = "Population-weighted mean distance (m)"
  )

# Plot the relationship between living standard and cinema access:
ggplot(
  analysis,
  aes(x = mean_living_standard, y = nearest_cinema_m)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(
    title = "Living standard and cinema accessibility",
    x = "Mean living standard",
    y = "Distance to nearest cinema (m)"
  )

# Compute correlation coefficient
cor.test(analysis$mean_living_standard, analysis$nearest_cinema_m, use = "complete.obs")

# Regression
cinema_model <- lm(nearest_cinema_m ~ poverty_rate + social_housing_rate + ind, data = st_drop_geometry(analysis))
summary(cinema_model)

# Create a cleaner population-density variable
analysis <- analysis |> mutate(population_density_km2 = ind / 0.04)

cinema_model <- lm(
  nearest_cinema_m ~ poverty_rate + social_housing_rate + population_density_km2, data = st_drop_geometry(analysis)
)

# Save tables
write_csv(poverty_access_summary, here("output", "tables", "access_by_poverty_group.csv"))

write_csv(category_poverty_summary, here("output","tables","category_access_by_poverty.csv"))