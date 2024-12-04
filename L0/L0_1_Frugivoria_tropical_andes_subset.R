# Title: IUCN Species List Subset
# Project: Evaluating the Effectiveness of Protected Areas and Community-Managed Lands 
#          in Capturing Multiple Dimensions of Frugivorous Biodiversity in the Tropical Andes
# Author: Beth E. Gerstner
# Collaborators: Phoebe L. Zarnetske
# Overview: This script downloads IUCN species lists for birds and mammals from relevant countries 
#           in the Tropical Andes and subsets them based on species habitat.
# Data Output: 
#   - mam_IUCN_2023.csv
#   - bird_IUCN_2023.csv
# Date: June 20th, 2023

# Libraries
library(taxize)
library(httr)
library(jsonlite)
library(dplyr)
library(rredlist)

# Initialize IUCN API token
token <- 'YOUR_IUCN_API_TOKEN'  # Replace with your API token
api_url <- 'https://apiv3.iucnredlist.org/api/v3/'

# Fetch country codes
country_list <- GET(url = paste0(api_url, 'country/list?token=', token))
countries <- fromJSON(content(country_list, as = 'text'), simplifyDataFrame = TRUE)$result

# Define Tropical Andes countries
andes_countries <- c('CO', 'EC', 'PE', 'BO', 'VE')

# Fetch species lists for Tropical Andes countries
species_lists <- lapply(andes_countries, function(country) {
  rl_sp_country(country, key = token, parse = TRUE)$result
})
all_species_tropical_andes <- do.call(rbind, species_lists)

# Fetch group-level data for birds and mammals
all_birds <- rl_comp_groups(group = 'birds', key = token)$result
all_mammals <- rl_comp_groups(group = 'mammals', key = token)$result

# Convert to data frames
all_birds <- as.data.frame(all_birds)
all_mammals <- as.data.frame(all_mammals)

# Subset Tropical Andes species for birds and mammals
TA_birds <- merge(all_birds, all_species_tropical_andes, by = "taxonid") %>%
  distinct(scientific_name, .keep_all = TRUE)
TA_mammals <- merge(all_mammals, all_species_tropical_andes, by = "taxonid") %>%
  distinct(scientific_name, .keep_all = TRUE)

# Rename columns for consistency
colnames(TA_birds)[which(names(TA_birds) == "scientific_name")] <- "IUCN_species_name"
colnames(TA_mammals)[which(names(TA_mammals) == "scientific_name")] <- "IUCN_species_name"

# Load Frugivoria datasets
frugivoria_mammal <- read.csv("PLACEHOLDER_PATH/Frugivoria_mammal_database.csv")
frugivoria_bird <- read.csv("PLACEHOLDER_PATH/Frugivoria_bird_database.csv")

# Merge with Frugivoria datasets
frugivoria_mam_TA <- frugivoria_mammal %>%
  filter(IUCN_species_name %in% TA_mammals$IUCN_species_name)
frugivoria_bird_TA <- frugivoria_bird %>%
  filter(IUCN_species_name %in% TA_birds$IUCN_species_name)

# Save results
output_dir <- "PLACEHOLDER_PATH/output_directory"
write.csv(frugivoria_mam_TA, file.path(output_dir, "frugivoria_TA_mammal_subset.csv"))
write.csv(frugivoria_bird_TA, file.path(output_dir, "frugivoria_TA_bird_subset.csv"))

# Endemic analysis (example of further refinement)
endemic_mammals <- frugivoria_mammal %>%
  filter(habitat == "1")  # Habitat 1 for endemic
endemic_birds <- frugivoria_bird %>%
  filter(habitat == "1")

write.csv(endemic_mammals, file.path(output_dir, "endemic_mammals.csv"))
write.csv(endemic_birds, file.path(output_dir, "endemic_birds.csv"))

# Summary statistics (optional)
cat("Total Tropical Andes Mammals: ", nrow(TA_mammals), "\n")
cat("Total Tropical Andes Birds: ", nrow(TA_birds), "\n")
