# Title: FUSE Species Distribution Analysis
# Project: Evaluating the Effectiveness of Protected Areas and Community-Managed Lands 
#          in Capturing Multiple Dimensions of Frugivorous Biodiversity in the Tropical Andes
# Author: Beth E. Gerstner
# Collaborators: Phoebe L. Zarnetske
# Overview: 
# This script processes range maps for FUSE species across birds, mammals, 
# and joint metrics. It calculates the sum of range maps and makes a raster, masks by the Tropical Andes region, 
# and computes the average FUSE species per park and per park category.
# Data Output:
#   - Raster files of summed FUSE species distributions masked by the Tropical Andes, raster of FUSE species proportions
#   - CSV files summarizing average/proportion FUSE species per PA and category, and Forest Integrity per PA and category
#   - CSV with counts of unique parks in each category

# Date: August 10th, 2023


## BIRDS
# Calculating average FUSE species per PA/category

# Set the folder path for FUSE species distributions
folder_path <- "PLACEHOLDER_PATH/bird_rasters/AOH_final_bird/"

# Read the parks and reserves shapefile
parks_reserves <- st_read("PLACEHOLDER_PATH/WDPA_shapefile_merged/WDPA_merged_TA.shp")

# Load world map
worldMap <- ne_countries(scale = "medium", type = "countries", returnclass = "sf")

# Define study region for Tropical Andes
study_region_crop <- worldMap %>% 
  filter(sovereignt %in% c("Colombia", "Peru", "Bolivia", "Venezuela", "Ecuador"))

# Read in Tropical Andes shapefile
tropical_andes <- st_read("PLACEHOLDER_PATH/tropical_andes_shp/Tropical_Andes_shape.shp")

# Refine Tropical Andes to study region
TA_refined <- st_intersection(tropical_andes, study_region_crop)

# Read in FUSE species list
fuse_species_list <- read.csv("PLACEHOLDER_PATH/richness/bird_fuse.csv")

# Initialize the summed distribution raster
summed_raster <- raster()

# Get the first distribution raster file path
first_species_name <- fuse_species_list$X[1]
first_species_name_fixed <- gsub(" ", "_", first_species_name)
first_species_name <- fuse_species_list$X[1]
first_species_name_fixed <- gsub(" ", "_", first_species_name)
first_raster_file <- paste0(first_species_name_fixed, "_AOH.tif")
first_distribution_raster <- raster(paste0(folder_path, first_raster_file))

# Initialize the summed distribution raster with the same dimensions and extent as the first distribution raster
first_distribution_raster[is.na(first_distribution_raster)] <- 0
summed_raster <- first_distribution_raster

# Loop through each FUSE species
for (i in 1:nrow(fuse_species_list)) {
  # Get the species name and distribution raster file path
  species_name <- fuse_species_list$X[i]
  species_name_fixed <- gsub(" ", "_", species_name)
  raster_file <- paste0(species_name_fixed, "_AOH.tif")
  
  # Read the distribution raster
  distribution_raster <- raster(paste0(folder_path, raster_file))
  
  # Replace NA values with 0
  distribution_raster[is.na(distribution_raster)] <- 0
  
  # Sum the distribution raster with the existing summed raster
  summed_raster <- summed_raster + distribution_raster
}

# Mask by Tropical Andes
FUSE_species_masked_TA <- mask(summed_raster, TA_refined)
setwd("PLACEHOLDER_PATH/neotropical_diversity/Results/richness/")
writeRaster(FUSE_species_masked_TA, "FUSE_species_masked_TA_birds.tif", format="GTiff", overwrite=T)

# Want to calculate the average number of FUSE species per park and per park category
# Create empty dataframes to store the results
avg_fuse_per_category <- data.frame(Category = character(), Avg_FUSE_Species = numeric(), stringsAsFactors = FALSE)
avg_fuse_per_park <- data.frame(Park = character(), Avg_FUSE_Species = numeric(), Category = numeric(), stringsAsFactors = FALSE)

# Loop through each park category
for (category in unique(parks_reserves$IUCN_CAT)) {
  # Subset the parks shapefile by the current category
  subset_parks <- parks_reserves[parks_reserves$IUCN_CAT == category, ]
  
  # Extract FUSE species values for the subsetted parks
  extracted_values <- exact_extract(FUSE_species_masked_TA, subset_parks, fun = c("mean","variety"), append_cols=c("IUCN_CAT", "NAME", "GOV_TYPE","PARENT_ISO","GIS_AREA"), coverage_area = TRUE)
  extracted_values <- extracted_values[!is.na(extracted_values$mean), ]
  avg_fuse_category <- mean(extracted_values$mean)
  
  
  # Add the result to the category dataframe
  avg_fuse_per_category <- rbind(avg_fuse_per_category, data.frame(Category = category, Avg_FUSE_Species = avg_fuse_category))
  
  # Loop through each park in the current category
  for (park_name in unique(subset_parks$NAME)) {
    # Subset the parks shapefile by the current park
    subset_park <- subset_parks[subset_parks$NAME == park_name, ]
    
    # Extract FUSE species values for the subsetted park
    extracted_park_values <- exact_extract(FUSE_species_masked_TA, subset_park, fun = c("mean","variety"), append_cols=c("IUCN_CAT", "NAME", "GOV_TYPE","PARENT_ISO","GIS_AREA"), coverage_area = TRUE)
    
    # Calculate the average FUSE species per park
    avg_fuse_park <- mean(extracted_park_values$mean)
    
    # Add the result to the park dataframe
    avg_fuse_per_park <- rbind(avg_fuse_per_park, data.frame(Park = park_name, Avg_FUSE_Species = avg_fuse_park, Category = category))
  }
}

# Print the average FUSE species per park category dataframe
print(avg_fuse_per_category)

# Print the average FUSE species per park dataframe
print(avg_fuse_per_park)
avg_fuse_per_park_no_na <- avg_fuse_per_park[!is.na(avg_fuse_per_park$Avg_FUSE_Species), ]

setwd("PLACEHOLDER_PATH//FUSE_results/")

#Write tables to a file
write.csv(avg_fuse_per_category, "avg_fuse_per_category_birds.csv")
write.csv(avg_fuse_per_park_no_na, "avg_fuse_per_park_birds.csv")

## MAMMALS
# Calculating average FUSE species per PA/category
# Set the folder path for FUSE species distributions
folder_path <- "PLACEHOLDER_PATH/mammal_rasters/AOH_final_mam/"

# Read the parks and reserves shapefile
parks_reserves <- st_read("PLACEHOLDER_PATH/WDPA_shapefile_merged/WDPA_merged_TA.shp")

# Read in Tropical Andes shapefile
tropical_andes <- st_read("PLACEHOLDER_PATH/tropical_andes_shp/Tropical_Andes_shape.shp")

# Read in FUSE species list
fuse_species_list <- read.csv("PLACEHOLDER_PATH/richness/mam_fuse.csv")

# Initialize the summed distribution raster
summed_raster <- raster()

# Get the first distribution raster file path
first_species_name <- fuse_species_list$X[1]
first_species_name_fixed <- gsub(" ", "_", first_species_name)
first_species_name <- fuse_species_list$X[1]
first_species_name_fixed <- gsub(" ", "_", first_species_name)
first_raster_file <- paste0(first_species_name_fixed, "_AOH.tif")
first_distribution_raster <- raster(paste0(folder_path, first_raster_file))

# Initialize the summed distribution raster with the same dimensions and extent as the first distribution raster
first_distribution_raster[is.na(first_distribution_raster)] <- 0
summed_raster <- first_distribution_raster

# Loop through each FUSE species
for (i in 1:nrow(fuse_species_list)) {
  # Get the species name and distribution raster file path
  species_name <- fuse_species_list$X[i]
  species_name_fixed <- gsub(" ", "_", species_name)
  raster_file <- paste0(species_name_fixed, "_AOH.tif")
  
  # Read the distribution raster
  distribution_raster <- raster(paste0(folder_path, raster_file))
  
  # Replace NA values with 0
  distribution_raster[is.na(distribution_raster)] <- 0
  distribution_raster <- resample(distribution_raster, first_distribution_raster)
  
  # Sum the distribution raster with the existing summed raster
  summed_raster <- summed_raster + distribution_raster
}

# Mask by Tropical Andes
FUSE_species_masked_TA <- mask(summed_raster, TA_refined)
setwd("PLACEHOLDER_PATH/Results/richness/")
writeRaster(FUSE_species_masked_TA, "FUSE_species_masked_TA_mam.tif", format="GTiff", overwrite=T)

#Want to calculate the average number of FUSE species per park and per park category
# Create empty dataframes to store the results
avg_fuse_per_category <- data.frame(Category = character(), Avg_FUSE_Species = numeric(), stringsAsFactors = FALSE)
avg_fuse_per_park <- data.frame(Park = character(), Avg_FUSE_Species = numeric(), Category = numeric(), stringsAsFactors = FALSE)

# Loop through each park category
for (category in unique(parks_reserves$IUCN_CAT)) {
  # Subset the parks shapefile by the current category
  subset_parks <- parks_reserves[parks_reserves$IUCN_CAT == category, ]
  
  # Extract FUSE species values for the subsetted parks
  extracted_values <- exact_extract(FUSE_species_masked_TA, subset_parks, fun = c("mean","variety"), append_cols=c("IUCN_CAT", "NAME", "GOV_TYPE","PARENT_ISO","GIS_AREA"), coverage_area = TRUE)
  extracted_values <- extracted_values[!is.na(extracted_values$mean), ]
  avg_fuse_category <- mean(extracted_values$mean)
  
  
  # Add the result to the category dataframe
  avg_fuse_per_category <- rbind(avg_fuse_per_category, data.frame(Category = category, Avg_FUSE_Species = avg_fuse_category))
  
  # Loop through each park in the current category
  for (park_name in unique(subset_parks$NAME)) {
    # Subset the parks shapefile by the current park
    subset_park <- subset_parks[subset_parks$NAME == park_name, ]
    
    # Extract FUSE species values for the subsetted park
    extracted_park_values <- exact_extract(FUSE_species_masked_TA, subset_park, fun = c("mean","variety"), append_cols=c("IUCN_CAT", "NAME", "GOV_TYPE","PARENT_ISO","GIS_AREA"), coverage_area = TRUE)
    
    # Calculate the average FUSE species per park
    avg_fuse_park <- mean(extracted_park_values$mean)
    
    # Add the result to the park dataframe
    avg_fuse_per_park <- rbind(avg_fuse_per_park, data.frame(Category= category, Park = park_name, Avg_FUSE_Species = avg_fuse_park))
  }
}

# Print the average FUSE species per park category dataframe
print(avg_fuse_per_category)

# Print the average FUSE species per park dataframe
print(avg_fuse_per_park)
avg_fuse_per_park <- avg_fuse_per_park[!is.na(avg_fuse_per_park$Avg_FUSE_Species), ]

# Set the working directory for FUSE results
setwd("PLACEHOLDER_PATH/Results/richness/FUSE_results/")

# Write tables to a file
write.csv(avg_fuse_per_category, "avg_fuse_per_category_mam.csv")
write.csv(avg_fuse_per_park, "avg_fuse_per_park_mam.csv")

## JOINT (mammal and bird FUSE species distribution)
# Calculating average FUSE species per PA/category

# Set the folder path for FUSE species distributions
folder_path <- "PLACEHOLDER_PATH/Results/rasterized_maps/joint_rasters/joint_rasters_tropical_andes_clip/AOH_final_joint/"

# Read the parks and reserves shapefile
parks_reserves <- st_read("PLACEHOLDER_PATH/datasets/WDPA_shapefile_merged/WDPA_merged_TA.shp")

# Read in Tropical Andes shapefile
tropical_andes <- st_read("PLACEHOLDER_PATH/datasets/tropical_andes_shp/Tropical_Andes_shape.shp")

# Read in FUSE species list 
fuse_species_list <- read.csv("PLACEHOLDER_PATH/Results/richness/joint_FUSE.csv")

## Joined mammals and birds
# Set the path to the folder containing the species raster files
raster_folder_1 <- "PLACEHOLDER_PATH/Results/rasterized_maps/bird_rasters/bird_rasters_tropical_andes_clip/AOH_final_bird"
raster_folder_2 <- "PLACEHOLDER_PATH/Results/rasterized_maps/mammal_rasters/mammal_rasters_tropical_andes_clip/AOH_final_mam"

# Get the list of species raster files
species_rasters_1 <- list.files(raster_folder_1, pattern = ".tif$", full.names = TRUE)
species_rasters_2 <-  list.files(raster_folder_2, pattern = ".tif$", full.names = TRUE)

species_rasters <- c(species_rasters_1, species_rasters_2)


# Initialize the summed distribution raster
summed_raster <- raster()

# Get the first distribution raster file path
first_species_name <- fuse_species_list$X[1]
first_species_name_fixed <- gsub(" ", "_", first_species_name)
first_species_name <- fuse_species_list$X[1]
first_species_name_fixed <- gsub(" ", "_", first_species_name)
first_raster_file <- paste0(first_species_name_fixed, "_AOH.tif")
first_distribution_raster <- raster(species_rasters[1])

# Initialize the summed distribution raster with the same dimensions and extent as the first distribution raster
first_distribution_raster[is.na(first_distribution_raster)] <- 0
summed_raster <- first_distribution_raster

# Loop through each FUSE species
for (i in 1:nrow(fuse_species_list)) {
  # Get the species name and distribution raster file path
  species_name <- fuse_species_list$X[i]
  species_name_fixed <- gsub(" ", "_", species_name)
  
  # Find the matching raster file
  matching_file_path <- species_rasters[grep(species_name_fixed, species_rasters)]
  
  #turn into raster
  distribution_raster <- raster(matching_file_path)
  
  # Replace NA values with 0
  distribution_raster[is.na(distribution_raster)] <- 0
  distribution_raster <- resample(distribution_raster, first_distribution_raster)
  
  # Sum the distribution raster with the existing summed raster
  summed_raster <- summed_raster + distribution_raster
}

# Mask by Tropical Andes
FUSE_species_masked_TA <- mask(summed_raster, TA_refined)
setwd("PLACEHOLDER_PATH/Results/richness/")
writeRaster(FUSE_species_masked_TA, "FUSE_species_masked_TA_joint.tif", format="GTiff", overwrite=T)

# Want to calculate the average number of FUSE species per park and per park category
# Create empty dataframes to store the results
avg_fuse_per_category <- data.frame(Category = character(), Avg_FUSE_Species = numeric(), stringsAsFactors = FALSE)
avg_fuse_per_park <- data.frame(Park = character(), Avg_FUSE_Species = numeric(), Category = numeric(), stringsAsFactors = FALSE)

# Loop through each park category
for (category in unique(parks_reserves$IUCN_CAT)) {
  # Subset the parks shapefile by the current category
  subset_parks <- parks_reserves[parks_reserves$IUCN_CAT == category, ]
  
  # Extract FUSE species values for the subsetted parks
  extracted_values <- exact_extract(FUSE_species_masked_TA, subset_parks, fun = c("mean","variety"), append_cols=c("IUCN_CAT", "NAME", "GOV_TYPE","PARENT_ISO","GIS_AREA"), coverage_area = TRUE)
  extracted_values <- extracted_values[!is.na(extracted_values$mean), ]
  avg_fuse_category <- mean(extracted_values$mean)
  
  
  # Add the result to the category dataframe
  avg_fuse_per_category <- rbind(avg_fuse_per_category, data.frame(Category = category, Avg_FUSE_Species = avg_fuse_category))
  
  # Loop through each park in the current category
  for (park_name in unique(subset_parks$NAME)) {
    # Subset the parks shapefile by the current park
    subset_park <- subset_parks[subset_parks$NAME == park_name, ]
    
    # Extract FUSE species values for the subsetted park
    extracted_park_values <- exact_extract(FUSE_species_masked_TA, subset_park, fun = c("mean","variety"), append_cols=c("IUCN_CAT", "NAME", "GOV_TYPE","PARENT_ISO","GIS_AREA"), coverage_area = TRUE)
    
    # Calculate the average FUSE species per park
    avg_fuse_park <- mean(extracted_park_values$mean)
    
    # Add the result to the park dataframe
    avg_fuse_per_park <- rbind(avg_fuse_per_park, data.frame(Category= category, Park = park_name, Avg_FUSE_Species = avg_fuse_park))
  }
}

# Print the average FUSE species per park category dataframe
print(avg_fuse_per_category)

# Print the average FUSE species per park dataframe
print(avg_fuse_per_park)
avg_fuse_per_park <- avg_fuse_per_park[!is.na(avg_fuse_per_park$Avg_FUSE_Species), ]

setwd("PLACEHOLDER_PATH/Results/richness/FUSE_results/")

#Write tables to a file
write.csv(avg_fuse_per_category, "avg_fuse_per_category_joint.csv")
write.csv(avg_fuse_per_park, "avg_fuse_per_park_joint.csv")


## FOREST INTEGRITY
# Calculating average forest integrity per PA/category

# Read in forest integrity raster
forest_integrity <- raster("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/datasets/flii_SouthAmerica.tif")
# Mask by Tropical Andes
forest_integrity_crop <- crop(forest_integrity, TA_refined)
forest_integrity_TA <- mask(forest_integrity_crop, TA_refined)

# Documentation for forest integrity index says to divide by 1000 for the true index values
forest_integrity_TA <- forest_integrity_TA/1000

# There are -9.9999 for no data available
forest_integrity_TA[forest_integrity_TA == -9.999] <- NA


setwd("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/Results/richness/")
writeRaster(forest_integrity_TA, "forest_integrity_TA.tif", format="GTiff", overwrite=T)

#Want to calculate the average number of FUSE species per park and per park category
# Create empty dataframes to store the results
avg_integrity_per_category <- data.frame(Category = character(), avg_integrity = numeric(), stringsAsFactors = FALSE)
avg_integrity_per_park <- data.frame(Park = character(), avg_integrity = numeric(), Category = numeric(), stringsAsFactors = FALSE)

# Loop through each park category
for (category in unique(parks_reserves$IUCN_CAT)) {
  # Subset the parks shapefile by the current category
  subset_parks <- parks_reserves[parks_reserves$IUCN_CAT == category, ]
  
  # Extract FUSE species values for the subsetted parks
  extracted_values <- exact_extract(forest_integrity_TA, subset_parks, fun = c("mean","variety"), append_cols=c("IUCN_CAT", "NAME", "GOV_TYPE","PARENT_ISO","GIS_AREA"), coverage_area = TRUE)
  extracted_values <- extracted_values[!is.na(extracted_values$mean), ]
  avg_integrity_category <- mean(extracted_values$mean)
  
  # Calculate the standard deviation of FUSE species per park category
  std_dev_integrity_category <- sd(extracted_values$mean)
  
  # Combine mean and standard deviation as a character string
  mean_std_dev_category <- paste(round(avg_integrity_category, 2), "±", round(std_dev_integrity_category, 2))
  
  
  # Add the result to the category dataframe
  avg_integrity_per_category <- rbind(avg_integrity_per_category, data.frame(Category = category, avg_integrity = mean_std_dev_category))
  
  # Loop through each park in the current category
  for (park_name in unique(subset_parks$NAME)) {
    # Subset the parks shapefile by the current park
    subset_park <- subset_parks[subset_parks$NAME == park_name, ]
    
    # Extract FUSE species values for the subsetted park
    extracted_park_values <- exact_extract(forest_integrity_TA, subset_park, fun = c("mean","variety"), append_cols=c("IUCN_CAT", "NAME", "GOV_TYPE","PARENT_ISO","GIS_AREA"), coverage_area = TRUE)
    
    # Calculate the average FUSE species per park
    avg_integrity_park <- mean(extracted_park_values$mean)
    
    # Add the result to the park dataframe
    avg_integrity_per_park <- rbind(avg_integrity_per_park, data.frame(Park = park_name, avg_integrity = avg_integrity_park, Category = category))
  }
}

# Print the average FUSE species per park category dataframe
print(avg_integrity_per_category)

# Print the average FUSE species per park dataframe
#Note that we've removed parks with NA's
print(avg_integrity_per_park)
avg_integrity_per_park_no_na <- avg_integrity_per_park[!is.na(avg_integrity_per_park$avg_integrity), ]

setwd("PLACEHOLDER_PATH/Results/richness/FUSE_results/")

#Write tables to a file
write.csv(avg_integrity_per_category, "avg_integrity_per_category.csv")
write.csv(avg_integrity_per_park_no_na, "avg_integrity_per_park.csv")

## PROPORTIONS OF FUSE SPECIES
# Calculating proportion of FUSE species per PA/category

FUSE_all <- raster("PLACEHOLDER_PATH/richness/all_fuse_species.tif")
species_all <- raster("PLACEHOLDER_PATH/richness/Spec_rich_joint_TA.tif")

# Resample species richness raster of all species (birds and mammals combined)
species_all <- resample(species_all, FUSE_all, method="bilinear")

# Calculate proportion FUSE species
proportion_raster_combined <-(FUSE_all/species_all)

# Set working directory
setwd("PLACEHOLDER_PATH/richness")

# Save the proportion raster
writeRaster(proportion_raster_combined,"FUSE_species_proportion_TA_combined.tif",format="GTiff")

# Extract FUSE proportions for parks and categories
avg_fuse_prop_per_category <- data.frame(Category = character(), Avg_FUSE_Proportion = numeric(), stringsAsFactors = FALSE)
avg_fuse_prop_per_park <- data.frame(Park = character(), Avg_FUSE_Proportion = numeric(), Category = numeric(), stringsAsFactors = FALSE)

# Loop through each park category
for (category in unique(parks_reserves$IUCN_CAT)) {
  # Subset parks by category
  subset_parks <- parks_reserves[parks_reserves$IUCN_CAT == category, ]
  
  # Extract FUSE species proportions for the subsetted parks
  extracted_values <- exact_extract(proportion_raster_combined, subset_parks, fun = "mean", append_cols = c("IUCN_CAT", "NAME"))
  extracted_values <- extracted_values[!is.na(extracted_values$mean), ]
  
  # Calculate the average FUSE proportion for the category
  avg_fuse_prop_category <- mean(extracted_values$mean)
  avg_fuse_prop_per_category <- rbind(avg_fuse_prop_per_category, data.frame(Category = category, Avg_FUSE_Proportion = avg_fuse_prop_category))
  
  # Loop through each park in the current category
  for (park_name in unique(subset_parks$NAME)) {
    # Subset parks by name
    subset_park <- subset_parks[subset_parks$NAME == park_name, ]
    
    # Extract FUSE species proportions for the park
    extracted_park_values <- exact_extract(proportion_raster_combined, subset_park, fun = "mean", append_cols = c("IUCN_CAT", "NAME"))
    
    # Calculate the average FUSE proportion for the park
    avg_fuse_prop_park <- mean(extracted_park_values$mean)
    avg_fuse_prop_per_park <- rbind(avg_fuse_prop_per_park, data.frame(Park = park_name, Avg_FUSE_Proportion = avg_fuse_prop_park, Category = category))
  }
}

# Save results
write.csv(avg_fuse_prop_per_category, "avg_prop_fuse_frug_per_category.csv", row.names = FALSE)
write.csv(avg_fuse_prop_per_park, "avg_prop_fuse_frug_per_park.csv", row.names = FALSE)

## PARK COUNTS

# Count the number of unique parks in each category
pr_dataframe <- as.data.frame(parks_reserves)
park_counts <- pr_dataframe %>%
  group_by(IUCN_CAT) %>%
  summarise(Count = n_distinct(NAME))

write.csv(park_counts, "park_counts_IUCN_cat.csv")

# Print the number of unique parks in each category
print(park_counts)
