# WITHOUT foraging (include the joint FD)



library(raster)
library(sf)
library(exactextractr)
library(rasterVis)
library(raster)
library(dplyr)
library(raster)
library(maps)
library(ggplot2)
library(tmap)
library(rnaturalearth)
library(leaflet)
library(ggsn)
library(rgeos)
library(BAMMtools)
library(viridis)
library(cowplot)
library(gridExtra)


## BIRDS

# Set the folder path for FUSE species distributions
folder_path <- "/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/Results/rasterized_maps/bird_rasters/bird_rasters_tropical_andes_clip/AOH_final_bird/"

# Read the parks and reserves shapefile
parks_reserves <- st_read("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/datasets/WDPA_shapefile_merged/WDPA_merged_TA.shp")
worldMap <- ne_countries(scale = "medium", type = "countries", returnclass = "sf")

study_region_crop <- worldMap %>% filter(sovereignt == "Colombia" | sovereignt == "Peru" | sovereignt == "Bolivia" | sovereignt == "Venezuela"| sovereignt == "Ecuador")
# Read in Tropical andes shapefile
tropical_andes <- st_read("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/datasets/tropical_andes_shp/Tropical_Andes_shape.shp")

TA_refined <- st_intersection(tropical_andes, study_region_crop)

# Read in FUSE species list 
fuse_species_list <- read.csv("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/Results/richness/bird_fuse.csv")


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
setwd("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/Results/richness/")
writeRaster(FUSE_species_masked_TA, "FUSE_species_masked_TA_birds.tif", format="GTiff", overwrite=T)

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
    avg_fuse_per_park <- rbind(avg_fuse_per_park, data.frame(Park = park_name, Avg_FUSE_Species = avg_fuse_park, Category = category))
  }
}

# Print the average FUSE species per park category dataframe
print(avg_fuse_per_category)

# Print the average FUSE species per park dataframe
print(avg_fuse_per_park)
avg_fuse_per_park_no_na <- avg_fuse_per_park[!is.na(avg_fuse_per_park$Avg_FUSE_Species), ]

setwd("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/Results/richness/FUSE_results/")

#Write tables to a file
write.csv(avg_fuse_per_category, "avg_fuse_per_category_birds.csv")
write.csv(avg_fuse_per_park_no_na, "avg_fuse_per_park_birds.csv")

## MAMMALS (FORAGING)
# FUSE species calculations (Foraging)

library(raster)
library(sf)
library(exactextractr)

# Set the folder path for FUSE species distributions
folder_path <- "/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/Results/rasterized_maps/mammal_rasters/mammal_rasters_tropical_andes_clip/AOH_final_mam/"
# Read the parks and reserves shapefile
parks_reserves <- st_read("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/datasets/WDPA_shapefile_merged/WDPA_merged_TA.shp")

# Read in Tropical andes shapefile
tropical_andes <- st_read("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/datasets/tropical_andes_shp/Tropical_Andes_shape.shp")

# Read in FUSE species list 
fuse_species_list <- read.csv("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/Results/richness/mam_fuse.csv")


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
setwd("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/Results/richness/")
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

setwd("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/Results/richness/FUSE_results/")

#Write tables to a file
write.csv(avg_fuse_per_category, "avg_fuse_per_category_mam.csv")
write.csv(avg_fuse_per_park, "avg_fuse_per_park_mam.csv")

## JOINT

# Set the folder path for FUSE species distributions
folder_path <- "/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/Results/rasterized_maps/joint_rasters/joint_rasters_tropical_andes_clip/AOH_final_joint/"
# Read the parks and reserves shapefile
parks_reserves <- st_read("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/datasets/WDPA_shapefile_merged/WDPA_merged_TA.shp")

# Read in Tropical andes shapefile
tropical_andes <- st_read("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/datasets/tropical_andes_shp/Tropical_Andes_shape.shp")

# Read in FUSE species list 
fuse_species_list <- read.csv("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/Results/richness/joint_FUSE.csv")


## Joined mammals and birds
# Set the path to the folder containing the species raster files
raster_folder_1 <- "/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/Results/rasterized_maps/bird_rasters/bird_rasters_tropical_andes_clip/AOH_final_bird"  # Update with the actual path to your folder
raster_folder_2 <- "/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/Results/rasterized_maps/mammal_rasters/mammal_rasters_tropical_andes_clip/AOH_final_mam"  # Update with the actual path to your folder


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
setwd("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/Results/richness/")
writeRaster(FUSE_species_masked_TA, "FUSE_species_masked_TA_joint.tif", format="GTiff", overwrite=T)

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

setwd("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/Results/richness/FUSE_results/")

#Write tables to a file
write.csv(avg_fuse_per_category, "avg_fuse_per_category_joint.csv")
write.csv(avg_fuse_per_park, "avg_fuse_per_park_joint.csv")

## Park count per category



