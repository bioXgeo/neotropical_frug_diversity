# Title: Generating Presence/Absence Matrices and Species Richness Maps

# Project: Evaluating the Effectiveness of Protected Areas and Community-Managed Lands 
#          in Capturing Multiple Dimensions of Frugivorous Biodiversity in the Tropical Andes
# Author: Beth E. Gerstner
# Collaborators: Phoebe L. Zarnetske
# Overview: This script generates presence/absence matrices and species richness maps 
#           for mammals, birds, and their combined datasets. The outputs include 
#           rasterized species richness maps and text-based presence/absence matrices.
# Data output: Rasterized richness maps and presence/absence matrices for mammals, birds, 
#              and combined datasets.

# Date: June 26th, 2023

## Load Libraries
library(raster)
library(letsR)
library(progress)

## Mammals

# Path to the folder containing mammal raster files
raster_folder <- "PLACEHOLDER_PATH/mammal_rasters"

# Get the list of species raster files
species_rasters <- list.files(raster_folder, pattern = ".tif$", full.names = TRUE)

# Initialize containers
coordinates <- list()
species_mam <- character()

# Loop over each species raster file and extract coordinates
for (i in 1:length(species_rasters)) {
  tryCatch({
    species_raster <- raster(species_rasters[i])
    species_points <- rasterToPoints(species_raster)
    xy <- species_points[, 1:2]
    n_coordinates <- nrow(xy)
    species_name <- gsub("_AOH.tif", "", basename(species_rasters[i]))
    species_name <- rep(species_name, n_coordinates)
    species_mam <- c(species_mam, species_name)
    coordinates[[i]] <- xy
  }, error = function(e) {
    cat("Skipping raster:", species_rasters[i], "\n")
  })
}

# Combine coordinates
all_coordinates_mam <- do.call(rbind, coordinates)

# Define resolution
resolution <- c(0.08333333, 0.08333333)  # 10km

# Generate presence-absence matrix
presence_absence_matrix_mam <- lets.presab.points(
  xy = all_coordinates_mam, 
  species = species_mam, 
  resol = resolution, 
  xmn = -91.65305, xmx = -57.49472, ymn = -22.89334, ymx = 12.43166, 
  count = TRUE
)

# View the presence-absence matrix
print(presence_absence_matrix_mam)

# Save outputs
setwd("PLACEHOLDER_PATH/richness/")
writeRaster(presence_absence_matrix_mam$Richness_Raster, filename = "Mammal_richness_aoh_10km.tif", format = "GTiff", overwrite = TRUE)
write.table(presence_absence_matrix_mam$Presence_and_Absence_Matrix, "PAM_mam.txt")

## Birds

# Path to the folder containing bird raster files
raster_folder <- "PLACEHOLDER_PATH/bird_rasters"

# Get the list of species raster files
species_rasters <- list.files(raster_folder, pattern = ".tif$", full.names = TRUE)

# Progress bar
pb <- progress_bar$new(total = length(species_rasters), format = "[:bar] :percent :eta")

# Initialize containers
coordinates <- list()
species_bird <- character()

# Loop over each species raster file and extract coordinates
for (i in 1:length(species_rasters)) {
  tryCatch({
    species_raster <- raster(species_rasters[i])
    species_points <- rasterToPoints(species_raster)
    xy <- species_points[, 1:2]
    n_coordinates <- nrow(xy)
    species_name <- gsub("_AOH.tif", "", basename(species_rasters[i]))
    species_name <- rep(species_name, n_coordinates)
    species_bird <- c(species_bird, species_name)
    coordinates[[i]] <- xy
    pb$tick()
  }, error = function(e) {
    cat("Skipping raster:", species_rasters[i], "\n")
    pb$tick()
  })
}

# Combine coordinates
all_coordinates_bird <- do.call(rbind, coordinates)

# Generate presence-absence matrix
presence_absence_matrix_bird <- lets.presab.points(
  xy = all_coordinates_bird, 
  species = species_bird, 
  resol = resolution, 
  xmn = -91.65305, xmx = -57.49472, ymn = -22.89334, ymx = 12.43166, 
  count = TRUE
)

# View the presence-absence matrix
print(presence_absence_matrix_bird)

# Save outputs
setwd("PLACEHOLDER_PATH/richness/")
writeRaster(presence_absence_matrix_bird$Richness_Raster, filename = "Bird_richness_aoh_10km.tif", format = "GTiff", overwrite = TRUE)
write.table(presence_absence_matrix_bird$Presence_and_Absence_Matrix, "PAM_bird.txt")

## Combined Mammals and Birds

# Paths to folders containing raster files
raster_folder_1 <- "PLACEHOLDER_PATH/bird_rasters"
raster_folder_2 <- "PLACEHOLDER_PATH/mammal_rasters"

# Get the list of species raster files
species_rasters_1 <- list.files(raster_folder_1, pattern = ".tif$", full.names = TRUE)
species_rasters_2 <- list.files(raster_folder_2, pattern = ".tif$", full.names = TRUE)
species_rasters <- c(species_rasters_1, species_rasters_2)

# Progress bar
pb <- progress_bar$new(total = length(species_rasters), format = "[:bar] :percent :eta")

# Initialize containers
coordinates <- list()
species_joint <- character()

# Loop over each species raster file and extract coordinates
for (i in 1:length(species_rasters)) {
  species_raster <- raster(species_rasters[i])
  species_points <- rasterToPoints(species_raster)
  xy <- species_points[, 1:2]
  n_coordinates <- nrow(xy)
  species_name <- gsub("_AOH.tif", "", basename(species_rasters[i]))
  species_name <- rep(species_name, n_coordinates)
  species_joint <- c(species_joint, species_name)
  coordinates[[i]] <- xy
  pb$tick()
}

# Combine coordinates
all_coordinates_joint <- do.call(rbind, coordinates)

# Generate presence-absence matrix
presence_absence_matrix_joint <- lets.presab.points(
  xy = all_coordinates_joint, 
  species = species_joint, 
  resol = resolution, 
  xmn = -91.65305, xmx = -57.49472, ymn = -22.89334, ymx = 12.43166, 
  count = TRUE
)

# View the presence-absence matrix
print(presence_absence_matrix_joint)

# Save outputs
setwd("PLACEHOLDER_PATH/richness/")
writeRaster(presence_absence_matrix_joint$Richness_Raster, filename = "Joint_richness_aoh_10km.tif", format = "GTiff", overwrite = TRUE)
write.table(presence_absence_matrix_joint$Presence_and_Absence_Matrix, "PAM_joint.txt")

# Plot the combined richness raster
plot(presence_absence_matrix_joint$Richness_Raster)
