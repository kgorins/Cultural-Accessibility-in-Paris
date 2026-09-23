library(tidyverse)
library(janitor)
library(here)

# Load the Basilic dataset
culture_raw <- read_csv2(here("data", "raw", "cultural_facilities.csv")) |> clean_names()

# Select only useful columns
culture <- culture_raw |>
  select(
    nom,
    adresse,
    code_postal,
    commune,
    code_insee_arrondissement,
    type_equipement_ou_lieu,
    precision_equipement,
    domaine,
    sous_domaine,
    departement,
    latitude,
    longitude
  )

culture <- culture |> filter(departement =="Paris")

# Count rows and columns of facilities in Paris
culture |> summarise(n_arrondissements = n_distinct(commune), n_facilities = n())

# Check coordinates
if (any(is.na(culture$latitude)) || any(is.na(culture$longitude))) 
{
  stop("Expected all cultural facilities to have valid coordinates.")
}

# Check for duplicates
culture |> count(nom, adresse, latitude, longitude, sort = TRUE) |>  filter(n > 1)
culture <- culture |> distinct(nom, adresse, latitude, longitude, .keep_all = TRUE)

# Clean columns
culture <- culture |> mutate(across(where(is.character), str_squish))

# Extract district number into arrondissement_numbera
culture <- culture |> mutate(arrondissement_number = parse_number(commune))

# Reformat and save the processed dataset
culture <- culture |>
  rename(
    name = nom,
    facility_type = type_equipement_ou_lieu,
    facility_detail = precision_equipement,
    domain = domaine,
    subdomain = sous_domaine,
    arrondissement = commune
  )

culture <- culture |>
  select(
    name,
    facility_type,
    facility_detail,
    domain,
    subdomain,
    arrondissement_number,
    arrondissement,
    code_insee_arrondissement,
    adresse,
    code_postal,
    departement,
    latitude,
    longitude
  )

write_csv(culture, here( "data", "processed", "cultural_facilities_paris.csv"))