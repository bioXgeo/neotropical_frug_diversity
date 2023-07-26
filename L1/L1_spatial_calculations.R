# Spatial Overlap
library(terra)
library(sf)

# Pull in world map
worldMap <- ne_countries(scale = "medium", type = "countries", returnclass = "sf")

# Subset world map. In this case we are removing the Galapagos by defining the bounding box around the Ecuador polygon.
study_region <- worldMap %>% filter(continent == "South America" | continent== "North America")
study_region_crop <- worldMap %>% filter(sovereignt == "Colombia" | sovereignt == "Peru" | sovereignt == "Bolivia" | sovereignt == "Venezuela"| sovereignt == "Ecuador")



TA_refined <- st_intersection(TA, study_region_crop)

output_folder <- "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/tropical_subsets"
st_write(TA_refined, dsn = output_folder, layer = "TA_refined", driver = "ESRI Shapefile")


# Function to project raster to UTM
project_to_utm <- function(raster) {
  utm_proj <- "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs"
  projected_raster <- project(raster, utm_proj)
  return(projected_raster)
}

utm_proj <- "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs"
# Set the folder path for the binary rasters
folder_path <- "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/richness_rasters_binary/"

# Set the taxa
taxa <- c("mammal", "bird", "joint")

# Set the threshold percentages
thresholds <- c(10, 20, 30)

# Set the cell resolution
cell_resolution <- 9.115 * 9.115  # Area of each cell in km

# Create an empty data frame to store the results
result_df <- data.frame(
  Taxon = character(),
  Threshold = numeric(),
  FD_Area = numeric(),
  SR_Area = numeric(),
  Overlap_Area = numeric(),
  Percent_full_overlap = numeric(),
  FD_Area_Protected = numeric(),
  SR_Area_Protected = numeric(),
  Overlap_Area_Protected = numeric(),
  Percent_FD_Protected = numeric(),
  Percent_SR_Protected = numeric(),
  Percent_Overlap_Protected = numeric(),
  stringsAsFactors = FALSE
)
#read in shapefile of parks and reserves
parks_reserves <- st_read("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/WDPA_ALL/WDPA_merged_TA.shp")

#crop by TA because some are left for Chile and Argentina, but we are removing

parks_reserves <- st_intersection(parks_reserves, TA_refined)
#Project to match rasters
parks_reserves_project <- st_transform(parks_reserves, crs=utm_proj)

# Loop through each threshold percentage
for (threshold in thresholds) {
  # Loop through each taxon
  for (taxon in taxa) {
    # Get the file names for the current threshold and taxon
    sr_file <- paste0(folder_path, "binary_sr_", taxon, "_", threshold, ".tif")
    fd_file <- paste0(folder_path, "binary_fd_", taxon, "_", threshold, ".tif")
    overlap_file <- paste0(folder_path, "diversity_", taxon, "_sum_", threshold, ".tif")
    
    # Read the SR binary raster
    sr_raster <- rast(sr_file)
    
    # Project the SR raster to UTM
    sr_projected <- project_to_utm(sr_raster)
    
    # Save the projected SR raster
    sr_output_file <- paste0(folder_path, "projected_binary_diversity/binary_sr_", taxon, "_", threshold, "_projected.tif")
    writeRaster(sr_projected, filename = sr_output_file, overwrite = TRUE)
    
    # Count the number of cells with a value of 1 (some weird values)
    sr_projected[sr_projected > .7] <- 1
    sr_projected[sr_projected <= .7] <- 0
    
    sr_cells <-cells(sr_projected, c(1))
    sr_count <-length(sr_cells[[paste0("binary_sr_", taxon, "_", threshold)]])
    # Calculate the overlap area
    sr_area <- sr_count * cell_resolution #233,463 km2
    
    #clip SR by parks
    sr_mask_park <- mask(sr_projected, parks_reserves_project)
    sr_mask_cells <-cells(sr_mask_park, c(1))
    sr_mask_count <-length(sr_mask_cells[[paste0("binary_sr_", taxon, "_", threshold)]])
    # Calculate the overlap area
    sr_park_area <- sr_mask_count * cell_resolution
    percent_sr_protected <- (sr_park_area/sr_area)*100
    
    # Read the FD binary raster
    fd_raster <- rast(fd_file)
    
    # Project the FD raster to UTM
    fd_projected <- project_to_utm(fd_raster)
    
    # Save the projected FD raster
    fd_output_file <- paste0(folder_path, "projected_binary_diversity/binary_fd_", taxon, "_", threshold, "_projected.tif")
    writeRaster(fd_projected, filename = fd_output_file, overwrite = TRUE)
    
    #remove areas distorted by projection
    fd_projected[fd_projected > .7] <- 1
    fd_projected[fd_projected <= .7] <- 0
    
    # Count the number of cells with a value of 1
    fd_cells <- sum(values(fd_projected) == 1)
    fd_cells <-cells(fd_projected, c(1))
    fd_count <-length(fd_cells[[paste0("binary_fd_", taxon, "_", threshold)]])
    # Calculate the overlap area
    fd_area <- fd_count * cell_resolution #174,059km2
    
    #clip FD by parks
    fd_mask_park <- mask(fd_projected, parks_reserves_project)
    fd_mask_cells <-cells(fd_mask_park, c(1))
    fd_mask_count <-length(fd_mask_cells[[paste0("binary_fd_", taxon, "_", threshold)]])
    # Calculate the overlap area
    fd_park_area <- fd_mask_count * cell_resolution 
    percent_fd_protected <- (fd_park_area/fd_area)*100
    
    # Read the overlap binary raster
    overlap_raster <- rast(overlap_file)
    
    # Project the overlap raster to UTM
    overlap_projected <- project_to_utm(overlap_raster)
    
    # Save the projected overlap raster
    overlap_output_file <- paste0(folder_path, "projected_binary_diversity/binary_td_fd_overlap_", taxon, "_", threshold, "_projected.tif")
    writeRaster(overlap_projected, filename = overlap_output_file, overwrite = TRUE)
    overlap_projected[overlap_projected < 1] <- 0
    overlap_projected[overlap_projected >= 1.5] <- 2
    overlap_projected[overlap_projected < 1.5] <- 1
    
    # Count the number of cells with a value of 2 (overlap)
    overlap_cells <-cells(overlap_projected, c(2))
    overlap_count <-length(overlap_cells[[paste0("diversity_", taxon, "_sum_", threshold)]])
    # Calculate the overlap area
    overlap_area <- overlap_count * cell_resolution #21,518.56 km2
    #clip SR by parks
    overlap_mask_park <- mask(overlap_projected, parks_reserves_project)
    overlap_mask_cells <-cells(overlap_mask_park, c(2))
    overlap_mask_count <-length(overlap_mask_cells[[paste0("diversity_", taxon, "_sum_", threshold)]])
    # Calculate the overlap area
    overlap_park_area <- overlap_mask_count * cell_resolution 
    percent_overlap_protected <- (overlap_park_area/overlap_area)*100
    
    #percent calculation
    all_cells_FD_SR <- cells(overlap_projected, c(1,2))
    all_cells_FD_SR_count <-length(all_cells_FD_SR[[paste0("diversity_", taxon, "_sum_", threshold)]])
    # Calculate the overlap area
    full_area <- all_cells_FD_SR_count * cell_resolution #268026.5 km2
    percent_overlap <- (overlap_area/full_area)*100
    
    
    #write result to a file
    result_df <- rbind(result_df, data.frame(
      Taxon = taxon,
      Threshold = threshold,
      FD_Area = fd_area,
      SR_Area = sr_area,
      Overlap_Area = overlap_area,
      Percent_full_overlap = percent_overlap, 
      FD_Area_Protected = fd_park_area,
      SR_Area_Protected = sr_park_area,
      Overlap_Area_Protected = overlap_park_area,
      Percent_FD_Protected = percent_fd_protected,
      Percent_SR_Protected = percent_sr_protected,
      Percent_Overlap_Protected = percent_overlap_protected))
  }
}

# Save the results to a CSV file
output_file <- "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/richness_rasters_binary/projected_binary_diversity/area_results_joint_sum.csv"
write.csv(result_df, file = output_file, row.names = FALSE)
