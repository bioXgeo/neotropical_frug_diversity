# Title: FD and FUSE Overlap Analysis
# Project: Evaluating the Effectiveness of Protected Areas and Community-Managed Lands 
#          in Capturing Multiple Dimensions of Frugivorous Biodiversity in the Tropical Andes
# Author: Beth E. Gerstner
# Collaborators: Phoebe L. Zarnetske
# Overview: This script calculates the overlap between Functional Diversity (FD) and Functionally Unique, Specialized, and Endangered (FUSE) species distributions for birds and mammals using top 10% thresholds.
# Outputs: 
#   - Bird FUSE Area: [calculated area in km²]
#   - Bird FD-FUSE Overlap Area: [calculated area in km²]
#   - Mammal FUSE Area: [calculated area in km²]
#   - Mammal FD-FUSE Overlap Area: [calculated area in km²]

# Date: August 11th, 2023

# Load Libraries
library(terra)

# Parameters
thresholds <- c(10, 20, 30)  # Threshold percentages
cell_resolution <- 0.911 * 0.911  # Area of each cell in km²

# Placeholder Paths
FUSE_file_birds <- "PLACEHOLDER_PATH/FUSE_species_masked_TA_birds.tif"  # Path to birds' FUSE raster file
FUSE_file_mammals <- "PLACEHOLDER_PATH/FUSE_species_masked_TA_mam.tif"  # Path to mammals' FUSE raster file
fd_file_birds <- "PLACEHOLDER_PATH/FD_birds_foraging_TA.tif"  # Path to birds' FD raster file
fd_file_mammals <- "PLACEHOLDER_PATH/FD_mams_foraging_TA.tif"  # Path to mammals' FD raster file

# Function to Project Raster to UTM
# Projects the raster to the Albers Equal Area projection used for South America
project_to_utm <- function(raster) {
  utm_proj <- "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs"
  projected_raster <- project(raster, utm_proj)
  return(projected_raster)
}

# Birds: FUSE and FD Overlap Calculation
# Read the FUSE raster for birds
FUSE_bird <- rast(FUSE_file_birds)

# Project the FUSE raster to UTM
FUSE_bird_projected <- project_to_utm(FUSE_bird)

# Calculate the top 10% threshold for FUSE species
threshold_bird_fuse <- quantile(values(FUSE_bird_projected), probs = 1 - (10 / 100), na.rm = TRUE)

# Create a binary mask for areas above the threshold
FUSE_bird_projected[FUSE_bird_projected < threshold_bird_fuse] <- 0
FUSE_bird_projected[FUSE_bird_projected >= threshold_bird_fuse] <- 1

# Calculate the area for FUSE species
bird_fuse_cells <- cells(FUSE_bird_projected, c(1))
bird_fuse_count <- length(bird_fuse_cells$FUSE_species_masked_TA_birds_foraging)
bird_fuse_area <- bird_fuse_count * cell_resolution

# Read the FD raster for birds
fd_raster_birds <- rast(fd_file_birds)

# Project the FD raster to UTM and align with the FUSE raster
fd_projected_birds <- project_to_utm(fd_raster_birds)
fd_projected_birds <- project(fd_projected_birds, FUSE_bird_projected)

# Calculate the top 10% threshold for FD
threshold_bird_fd <- quantile(values(fd_projected_birds), probs = 1 - (10 / 100), na.rm = TRUE)

# Create a binary mask for areas above the FD threshold
fd_projected_birds[fd_projected_birds < threshold_bird_fd] <- 0
fd_projected_birds[fd_projected_birds >= threshold_bird_fd] <- 1

# Calculate the area for FD species
bird_fd_cells <- cells(fd_projected_birds, c(1))
bird_fd_count <- length(bird_fd_cells$FD_birds_foraging_TA)
bird_fd_area <- bird_fd_count * cell_resolution

# Calculate the overlap between FD and FUSE species
FD_FUSE_sum_birds <- fd_projected_birds + FUSE_bird_projected
bird_fuse_fd_cells <- cells(FD_FUSE_sum_birds, c(2))
bird_fuse_fd_count <- length(bird_fuse_fd_cells$FD_birds_foraging_TA)
bird_fuse_fd_overlap_area <- bird_fuse_fd_count * cell_resolution

# Mammals: FUSE and FD Overlap Calculation
# Read the FUSE raster for mammals
FUSE_mammal <- rast(FUSE_file_mammals)

# Project the FUSE raster to UTM
FUSE_mammal_projected <- project_to_utm(FUSE_mammal)

# Calculate the top 10% threshold for FUSE species
threshold_mammal_fuse <- quantile(values(FUSE_mammal_projected), probs = 1 - (10 / 100), na.rm = TRUE)

# Create a binary mask for areas above the threshold
FUSE_mammal_projected[FUSE_mammal_projected < threshold_mammal_fuse] <- 0
FUSE_mammal_projected[FUSE_mammal_projected >= threshold_mammal_fuse] <- 1

# Calculate the area for FUSE species
mammal_fuse_cells <- cells(FUSE_mammal_projected, c(1))
mammal_fuse_count <- length(mammal_fuse_cells$FUSE_species_masked_TA_mam_foraging)
mammal_fuse_area <- mammal_fuse_count * cell_resolution

# Read the FD raster for mammals
fd_raster_mammals <- rast(fd_file_mammals)

# Project the FD raster to UTM and align with the FUSE raster
fd_projected_mammals <- project_to_utm(fd_raster_mammals)
fd_projected_mammals <- project(fd_projected_mammals, FUSE_mammal_projected)

# Calculate the top 10% threshold for FD
threshold_mammal_fd <- quantile(values(fd_projected_mammals), probs = 1 - (10 / 100), na.rm = TRUE)

# Create a binary mask for areas above the FD threshold
fd_projected_mammals[fd_projected_mammals < threshold_mammal_fd] <- 0
fd_projected_mammals[fd_projected_mammals >= threshold_mammal_fd] <- 1

# Calculate the area for FD species
mammal_fd_cells <- cells(fd_projected_mammals, c(1))
mammal_fd_count <- length(mammal_fd_cells$FD_mams_foraging_TA)
mammal_fd_area <- mammal_fd_count * cell_resolution

# Calculate the overlap between FD and FUSE species
FD_FUSE_sum_mammals <- fd_projected_mammals + FUSE_mammal_projected
mammal_fuse_fd_cells <- cells(FD_FUSE_sum_mammals, c(2))
mammal_fuse_fd_count <- length(mammal_fuse_fd_cells$FD_mams_foraging_TA)
mammal_fuse_fd_overlap_area <- mammal_fuse_fd_count * cell_resolution

# Print the results
cat("Bird FUSE Area:", bird_fuse_area, "km²\n")
cat("Bird FD Area:", bird_fd_area, "km²\n")
cat("Bird FD-FUSE Overlap Area:", bird_fuse_fd_overlap_area, "km²\n")
cat("Mammal FUSE Area:", mammal_fuse_area, "km²\n")
cat("Mammal FD Area:", mammal_fd_area, "km²\n")
cat("Mammal FD-FUSE Overlap Area:", mammal_fuse_fd_overlap_area, "km²\n")
