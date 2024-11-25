#Title: Generating presence/absence matrices and species richness maps

#Project: Functional diversity of frugivores in the Tropical Andes: implications for conservation

#Author: Beth E. Gerstner

#Collaborators: Phoebe L. Zarnetske

#Overview: 

#Data output: 

#Date: June 26th, 2023

#Load libraries
library(raster)
library(letsR)
library(progress)

# Set the path to the folder containing the species raster files
raster_folder <- "/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/Results/rasterized_maps/mammal_rasters/mammal_rasters_tropical_andes_clip/AOH_final_mam"  # Update with the actual path to your folder

# Get the list of species raster files
species_rasters <- list.files(raster_folder, pattern = ".tif$", full.names = TRUE)

# Create an empty list to store the coordinates
coordinates <- list()

# Create an empty vector to store the species names
species_mam <- character()

# Loop over each species raster file and extract the coordinates
for (i in 1:length(species_rasters)) {
  tryCatch({
    # Read the species raster
    species_raster <- raster(species_rasters[i])
    
    # Convert raster to points
    species_points <- rasterToPoints(species_raster)
    
    # Extract the coordinates
    xy <- species_points[, 1:2]
    
    # Get the number of coordinates for this species
    n_coordinates <- nrow(xy)
    
    # Add the species name to the vector, repeated for each coordinate
    species_name <- gsub("_AOH.tif", "", basename(species_rasters[i]))
    species_name <- rep(species_name, n_coordinates)
    
    # Add the species names to the vector
    species_mam <- c(species_mam, species_name)
    
    # Add the coordinates to the list
    coordinates[[i]] <- xy
  }, error = function(e) {
    # Skip the current raster file if it throws an error
    cat("Skipping raster:", species_rasters[i], "\n")
  })
}


# Create a matrix of coordinates
all_coordinates_mam <- do.call(rbind, coordinates)

# Choose resolution (may want to try a few resolutions)
resolution <- c(0.08333333, 0.08333333) #(10km)

# Calculate the presence-absence matrix using lets.presab.points
presence_absence_matrix_mam <- lets.presab.points(xy = all_coordinates_mam, species = species_mam, resol=resolution, xmn=-91.65305, xmx=-57.49472, ymn=-22.89334, ymx=12.43166,count=T)
# Total 391 species (check which are missing)
# View the presence-absence matrix/***
print(presence_absence_matrix_mam)

#Set working directory to where you want richness to be saved
setwd("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/Results/richness/")

# Write taxonomic richness for mammals to file
writeRaster(presence_absence_matrix_mam$Richness_Raster, filename="Mammal_richness_aoh_10km.tif", format="GTiff", overwrite=T)
write.table(presence_absence_matrix_mam$Presence_and_Absence_Matrix, "PAM_mam.txt")

## Birds

# Set the path to the folder containing the species raster files
raster_folder <- "/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/Results/rasterized_maps/bird_rasters/bird_rasters_tropical_andes_clip/AOH_final_bird"  # Update with the actual path to your folder

# Get the list of species raster files
species_rasters <- list.files(raster_folder, pattern = ".tif$", full.names = TRUE)

# Create a progress bar due to how long this takes to run
pb <- progress_bar$new(total = length(species_rasters), format = "[:bar] :percent :eta")

# Create an empty list to store the coordinates
coordinates <- list()

# Create an empty vector to store the species names
species_bird <- character()

# Loop over each species raster file and extract the coordinates
for (i in 1:length(species_rasters)) {
  tryCatch({
    # Read the species raster
    species_raster <- raster(species_rasters[i])
    
    # Convert raster to points
    species_points <- rasterToPoints(species_raster)
    
    # Extract the coordinates
    xy <- species_points[, 1:2]
    
    # Get the number of coordinates for this species
    n_coordinates <- nrow(xy)
    
    # Add the species name to the vector, repeated for each coordinate
    species_name <- gsub("_AOH.tif", "", basename(species_rasters[i]))
    species_name <- rep(species_name, n_coordinates)
    
    # Add the species names to the vector
    species_bird <- c(species_bird, species_name)
    
    # Add the coordinates to the list
    coordinates[[i]] <- xy
    
    # Update the progress bar
    pb$tick()
  }, error = function(e) {
    # Skip the current raster file if it throws an error
    cat("Skipping raster:", species_rasters[i], "\n")
    
    # Update the progress bar
    pb$tick()
  })
}


# Close the progress bar
pb$close()

# Create a matrix of coordinates
all_coordinates_bird <- do.call(rbind, coordinates)

# Choose resolution (may want to try a few resolutions)
resolution <- c(0.08333333, 0.08333333) #(10km)

# Calculate the presence-absence matrix using lets.presab.points
presence_absence_matrix_bird <- lets.presab.points(xy = all_coordinates_bird, species = species_bird, resol=resolution, xmn=-91.65305, xmx=-57.49472, ymn=-22.89334, ymx=12.43166,count=T)

# View the presence-absence matrix/***
print(presence_absence_matrix_bird)

#Set working directory to where you want richness to be saved
setwd("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/Results/richness/")

# Write taxonomic richness for mammals to file
writeRaster(presence_absence_matrix_bird$Richness_Raster, filename="Bird_richness_aoh_10km.tif", format="GTiff",overwrite=T)
write.table(presence_absence_matrix_bird$Presence_and_Absence_Matrix, "PAM_bird.txt")

## Joined mammals and birds
# Set the path to the folder containing the species raster files
raster_folder_1 <- "/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/Results/rasterized_maps/bird_rasters/bird_rasters_tropical_andes_clip/AOH_final_bird"  # Update with the actual path to your folder
raster_folder_2 <- "/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/Results/rasterized_maps/mammal_rasters/mammal_rasters_tropical_andes_clip/AOH_final_mam"  # Update with the actual path to your folder


# Get the list of species raster files
species_rasters_1 <- list.files(raster_folder_1, pattern = ".tif$", full.names = TRUE)
species_rasters_2 <-  list.files(raster_folder_2, pattern = ".tif$", full.names = TRUE)

species_rasters <- c(species_rasters_1, species_rasters_2)


# Create an empty list to store the coordinates
coordinates <- list()

# Create an empty vector to store the species names
species_joint <- character()

library(progress)

# Create a progress bar
pb <- progress_bar$new(total = length(species_rasters), format = "[:bar] :percent :eta")

# Create an empty list to store the coordinates
coordinates <- list()

# Create an empty vector to store the species names
species_joint <- character()

# Loop over each species raster file and extract the coordinates
for (i in 1:length(species_rasters)) {
  # Read the species raster
  species_raster <- raster(species_rasters[i])
  
  # Convert raster to points
  species_points <- rasterToPoints(species_raster)
  
  # Extract the coordinates
  xy <- species_points[, 1:2]
  
  # Get the number of coordinates for this species
  n_coordinates <- nrow(xy)
  
  # Add the species name to the vector, repeated for each coordinate
  species_name <- gsub("_AOH.tif", "", basename(species_rasters[i]))
  species_name <- rep(species_name, n_coordinates)
  
  # Add the species names to the vector
  species_joint <- c(species_joint, species_name)
  
  # Add the coordinates to the list
  coordinates[[i]] <- xy
  
  # Update the progress bar
  pb$tick()
}
# Create a matrix of coordinates
all_coordinates_joint <- do.call(rbind, coordinates)

# Create a matrix that we can write to file
all_coordinates_joint_full_data <- all_coordinates_joint
all_coordinates_joint_full_data$species <- species_joint
#write.csv(all_coordinates_mam_full_data, "coordinate_matrix_mam.csv")

# Choose resolution (may want to try a few resolutions)
resolution <- c(0.08333333, 0.08333333) #(10km)

# Calculate the presence-absence matrix using lets.presab.points
presence_absence_matrix_joint <- lets.presab.points(xy = all_coordinates_joint, species = species_joint, resol=resolution, xmn=-91.65305, xmx=-57.49472, ymn=-22.89334, ymx=12.43166,count=T)

# View the presence-absence matrix/***
print(presence_absence_matrix_joint)

#Set working directory to where you want richness to be saved
setwd("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/Results/richness/")

# Write taxonomic richness for mammals to file
writeRaster(presence_absence_matrix_joint$Richness_Raster, filename="Joint_richness_aoh_10km.tif", format="GTiff")
write.table(presence_absence_matrix_joint$Presence_and_Absence_Matrix, "PAM_joint.txt")

plot(presence_absence_matrix_joint$Richness_Raster)




