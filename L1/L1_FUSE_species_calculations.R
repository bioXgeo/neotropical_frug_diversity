# FUSE species calculations

library(raster)
library(sf)

# Set the folder path for FUSE species distributions
folder_path <- "path/to/fuse_species_distributions/"

# Read the parks and reserves shapefile
parks_reserves <- st_read("path/to/parks_reserves.shp")

# Initialize the summed distribution raster
summed_raster <- NULL

# Loop through each FUSE species
for (i in 1:nrow(fuse_species_list)) {
  # Get the species name and distribution raster file path
  species_name <- fuse_species_list$Species[i]
  raster_file <- paste0(species_name, "_", fuse_species_list$RasterFile[i])
  
  # Read the distribution raster
  distribution_raster <- raster(paste0(folder_path, raster_file))
  
  # Sum the distribution raster with the existing summed raster
  if (is.null(summed_raster)) {
    summed_raster <- distribution_raster
  } else {
    summed_raster <- summed_raster + distribution_raster
  }
}

# Calculate the coverage by parks and reserves
overlap_with_parks <- st_intersection(parks_reserves, as(summed_raster, "SpatialPolygons"))
coverage_parks_reserves <- sum(st_area(overlap_with_parks)) / sum(st_area(as(summed_raster, "SpatialPolygons")))

# Calculate the average number of FUSE species protected per category
avg_species_per_category <- aggregate(Species ~ IUCN_park_categories, data = fuse_species_list, FUN = function(x) length(unique(x)) / length(unique(fuse_species_list$Species)))

# Print the results
print(coverage_parks_reserves)
print(avg_species_per_category)
