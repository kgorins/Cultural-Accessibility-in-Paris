library(tidyverse)
library(sf)
library(here)

# Load clean Paris dataset
culture <- read_csv(here("data", "processed", "cultural_facilities_paris.csv"), show_col_types = FALSE)

# Load the arrondissement polygons.
# The original Paris GeoJSON contains a missing geometry for the 12th arrondissement.
# arrondissements_fixed.geojson patches that geometry using an IGN-derived
# arrondissement boundary from france-geojson v2.0.2.
arrondissements <- st_read(here("data", "raw", "arrondissements_fixed.geojson"))
stopifnot(nrow(arrondissements) == 20) # we expect 20 arrondissements
stopifnot(!any(st_is_empty(arrondissements)))

# Convert coordinates facilities into spatial points.
# 4326 is the EPSG identifier for WGS84.
culture_sf <- st_as_sf(culture, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

# Convert arrondissements to culture_sf coordinate format
arrondissements <- st_transform(arrondissements, st_crs(culture_sf))

# Create and save a map
culture_map <- ggplot() +
  geom_sf(data = arrondissements, fill = "white") +
  geom_sf(data = culture_sf, aes(color = cultural_category), size = 0.8, alpha = 0.6) +
  labs(title = "Cultural facilities in Paris", color = "Category")

ggsave(here("output", "figures", "cultural_facilities_map.png"),
  plot = culture_map,
  width = 9,
  height = 8
)