# Title: Summing Rasters Across Taxa and Diversity Types
# 
# Project: Evaluating the Effectiveness of Protected Areas and Community-Managed Lands 
#          in Capturing Multiple Dimensions of Frugivorous Biodiversity in the Tropical Andes
#
# Author: Beth E. Gerstner
# Collaborators: Phoebe L. Zarnetske
# Overview: This script sums raster files across taxa and diversity types (e.g., species richness and 
#           functional diversity) for given thresholds. This is to generate joined Frugivore maps including both mammals and birds.
#           Outputs are saved as new raster files.
#
# Data Output: Summed binary raster files for joint diversity across thresholds.
#
# Date: August 1, 2023

# Load necessary library
library(raster)

# Function to sum raster files across taxa and diversity types
sum_across_taxa <- function(base_folder, diversity_list, taxa_list, threshold_list) {
  for (threshold in threshold_list) {
    sum_raster <- NULL  # Initialize sum raster
    
    for (diversity in diversity_list) {
      for (taxa in taxa_list) {
        # Construct file pattern to search for matching rasters
        pattern <- paste0(diversity, "_", taxa, "_", threshold, ".tif")
        raster_files <- list.files(path = base_folder, pattern = pattern, full.names = TRUE)
        
        if (length(raster_files) == 0) {
          cat(paste("No matching raster files found for", diversity, taxa, "at threshold", threshold, ".\n"))
          next
        }
        
        # Load the first raster to set dimensions, extent, and projection
        r <- raster(raster_files[1])
        
        if (is.null(sum_raster)) {
          sum_raster <- raster(matrix(0, nrow = nrow(r), ncol = ncol(r)))
          extent(sum_raster) <- extent(r)
          projection(sum_raster) <- projection(r)
          res(sum_raster) <- res(r)
        }
        
        # Add all matching rasters to the sum raster
        for (raster_file in raster_files) {
          r <- raster(raster_file)
          sum_raster <- sum_raster + r
        }
      }
    }
    
    if (!is.null(sum_raster)) {
      # Define the output folder and save the resulting raster
      output_folder <- "PLACEHOLDER_PATH/richness_rasters_binary"
      output_filename <- file.path(output_folder, paste0("binary_joint_", threshold, ".tif"))
      
      writeRaster(sum_raster, filename = output_filename, format = "GTiff", overwrite = TRUE)
      cat(paste("Sum raster saved for threshold", threshold, ".\n"))
    }
  }
}

# 
base_folder <- "PLACEHOLDER_PATH/richness_rasters_binary"
diversity_list <- c("sr", "fd")  # Diversity types: species richness and functional diversity
taxa_list <- c("bird", "mammal")  # Taxa: birds and mammals
threshold_list <- c(10, 20, 30)  # Thresholds to process

# Run the function
sum_across_taxa(base_folder, diversity_list, taxa_list, threshold_list)

