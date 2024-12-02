library(raster)

sum_across_taxa <- function(base_folder, diversity_list, taxa_list, threshold_list) {
  for (threshold in threshold_list) {
    sum_raster <- NULL
    for (diversity in diversity_list) {
      for (taxa in taxa_list) {
        pattern <- paste0(diversity, "_", taxa, "_", threshold, ".tif")
        raster_files <- list.files(path = base_folder, pattern = pattern, full.names = TRUE)
        
        if (length(raster_files) == 0) {
          cat(paste("No matching raster files found for", diversity, taxa, "at threshold", threshold, ".\n"))
          next
        }
        
        r <- raster(raster_files[1])
        
        if (is.null(sum_raster)) {
          sum_raster <- raster(matrix(0, nrow = nrow(r), ncol = ncol(r)))
          extent(sum_raster) <- extent(r)
          projection(sum_raster) <- projection(r)
          res(sum_raster) <- res(r)
        }
        
        # Sum the matching rasters across taxa and diversity types
        for (raster_file in raster_files) {
          r <- raster(raster_file)
          sum_raster <- sum_raster + r
        }
      }
    }
    
    if (!is.null(sum_raster)) {
      # Save the result to a new raster file
      output_folder <- "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/richness_rasters_binary"
      
      output_filename <- file.path(output_folder, paste("binary_joint_", threshold, ".tif", sep = "_"))
      writeRaster(sum_raster, filename = output_filename, format = "GTiff", overwrite = TRUE)
      
      cat(paste("Sum raster saved for threshold", threshold, ".\n"))
    }
  }
}

# Example usage
base_folder <- "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/richness_rasters_binary"
diversity_list <- c("sr", "fd")
taxa_list <- c("bird", "mammal")
threshold_list <- c(10, 20, 30)

sum_across_taxa(base_folder, diversity_list, taxa_list, threshold_list)

