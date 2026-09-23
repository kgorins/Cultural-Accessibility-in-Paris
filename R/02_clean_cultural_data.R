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

stopifnot(n_distinct(culture$commune) == 20)
stopifnot(setequal(sort(unique(parse_number(culture$commune))), 1:20))

# Check coordinates
if (any(is.na(culture$latitude)) || any(is.na(culture$longitude))) 
{
  stop("Expected all cultural facilities to have valid coordinates.")
}

# Clean columns
culture <- culture |> mutate(across(where(is.character), str_squish))

# Check for duplicates
culture |> count(nom, adresse, latitude, longitude, sort = TRUE) |>  filter(n > 1)
culture <- culture |> distinct(nom, adresse, latitude, longitude, .keep_all = TRUE)

# Extract district number into arrondissement_number
culture <- culture |> mutate(arrondissement_number = parse_number(commune))

# Rename and sort the dataset columns
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

# Group detailed facility types into broader categories
culture <- culture |>
  mutate(
    cultural_category = case_when(
      facility_type %in% c(
        "Monument",
        "Lieu de mémoire",
        "Lieu archéologique",
        "Espace protégé"
      ) ~ "Heritage",
      
      facility_type %in% c(
        "Bibliothèque",
        "Librairie"
      ) ~ "Books & reading",
      
      facility_type %in% c(
        "Théâtre",
        "Opéra",
        "Scène"
      ) ~ "Performing arts",
      
      facility_type == "Cinéma" ~ "Cinema",
      
      facility_type == "Musée" ~ "Museum",
      
      facility_type == "Parc et jardin" ~ "Parks & gardens",
      
      facility_type %in% c(
        "Centre d'art",
        "Centre de création artistique",
        "Centre de création musicale"
      ) ~ "Arts & creation",
      
      facility_type %in% c(
        "Conservatoire",
        "Établissement d'enseignement supérieur",
        "Service d'archives"
      ) ~ "Education & archives",
      
      facility_type == "Papeterie et maisons de la presse" ~ "Press & stationery",

      TRUE ~ "Other"
    )
  )


stopifnot(!any(culture$cultural_category == "Other"))

stopifnot(!any(is.na(culture$cultural_category)))

# Save the dataset
write_csv(culture, here( "data", "processed", "cultural_facilities_paris.csv"))