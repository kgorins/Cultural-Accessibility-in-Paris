library(tidyverse)
library(sf)
library(here)

# Path to INSEE 200m Filosofi GeoPackage
gpkg_path <- here("data", "raw", "carreaux_200m_met.gpkg")

population_grid <- st_read(gpkg_path, layer = "carreaux_200m_met")

# Get Paris data and transform to the same coordinate system
arrondissements <- st_read(here("data", "raw", "arrondissements_fixed.geojson"), quiet = TRUE)
arrondissements <- st_transform(arrondissements, st_crs(population_grid))

paris_boundary <- arrondissements |>  summarise()

# Filter the national grid to Paris
population_paris <- st_crop(population_grid, paris_boundary)

# Only select useful variables
population_paris <- population_paris |>
  select(
    idcar_200m,
    i_est_200,
    lcog_geo,
    ind,
    men,
    men_pauv,
    men_prop,
    men_fmp,
    ind_snv,
    log_soc,
    ind_0_3,
    ind_4_5,
    ind_6_10,
    ind_11_17,
    ind_18_24,
    ind_25_39,
    ind_40_54,
    ind_55_64,
    ind_65_79,
    ind_80p,
    geom
  )

population_paris <- population_paris[st_intersects(population_paris, paris_boundary, sparse = FALSE),]

# Add metrics
population_paris <- population_paris |>
  mutate(
    poverty_rate = if_else(men > 0, men_pauv / men, NA_real_),
    owner_rate = if_else(men > 0, men_prop / men, NA_real_),
    single_parent_rate = if_else(men > 0, men_fmp / men, NA_real_),
    social_housing_rate = if_else(men > 0, log_soc / men, NA_real_)
  )

population_paris <- population_paris |>
  mutate(
    mean_living_standard = if_else(
      ind > 0,
      ind_snv / ind,
      NA_real_
    )
  )

summary(population_paris$poverty_rate)
summary(population_paris$mean_living_standard)

stopifnot(
  all(
    population_paris$poverty_rate >= 0 &
      population_paris$poverty_rate <= 1,
    na.rm = TRUE
  )
)

# Plot the grid
ggplot() +
  geom_sf(data = paris_boundary, fill = NA) +
  geom_sf(data = population_paris)

ggplot(population_paris) +
  geom_sf(aes(fill = ind), color = NA) +
  labs(title = "Population by 200 m grid cell in Paris", fill = "Population")

# Save the Paris subset
st_write(population_paris, here("data", "processed", "population_grid_paris.gpkg"),
  delete_dsn = TRUE)
