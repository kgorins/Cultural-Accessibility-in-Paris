library(tidyverse)
library(janitor)
library(here)

# Load the Basilic dataset
culture_raw <- read_csv2(here("data", "raw", "cultural_facilities.csv"))
culture_raw <- culture_raw |> clean_names()

# glimpse(culture_raw)

# Extract only the city of Paris entries
culture_paris <- culture_raw |> filter(departement =="Paris")

# Plot and save types of facilities
facility_counts <- culture_paris |> count(type_equipement_ou_lieu, sort = TRUE)

ggplot(facility_counts, aes(x = reorder(type_equipement_ou_lieu, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Cultural facilities in Paris", x = NULL, y = "Number of facilities")

ggsave(here("output", "figures", "facility_counts.png"), width = 8, height = 6)
