#permutation analysis

library(raster)
library(sf)

# Set the paths to the species richness rasters, functional diversity rasters, and parks/reserves shapefile
sr_raster_path <- "path/to/species_richness_rasters/"
fd_raster_path <- "path/to/functional_diversity_rasters/"
parks_reserves_path <- "path/to/parks_reserves.shp"

# Define the number of permutations
n_permutations <- 1000

# Create an empty dataframe to store the permutation results
perm_results <- data.frame(
  Permutation = integer(),
  Park_Category = character(),
  Mean_SR_Parks = numeric(),
  Mean_SR_Surrounding = numeric(),
  Mean_FD_Parks = numeric(),
  Mean_FD_Surrounding = numeric(),
  stringsAsFactors = FALSE
)

# Load the parks and reserves shapefile
parks_reserves <- st_read(parks_reserves_path)

# Get the unique park categories
park_categories <- unique(parks_reserves$Park_Category)

# Define a function to extract raster values at specific locations
extract_from_rasters <- function(raster_path, locations) {
  raster_list <- list.files(path = raster_path, pattern = "\\.tif$", full.names = TRUE)
  raster_stack <- stack(raster_list)
  extract(raster_stack, locations)
}

# Loop through each park category
for (category in park_categories) {
  # Subset the parks and reserves by category
  category_parks_reserves <- parks_reserves[parks_reserves$Park_Category == category, ]
  
  # Loop through each permutation
  for (i in 1:n_permutations) {
    # Create random points within the study area
    random_points <- st_sample(st_geometry(category_parks_reserves), n = 1000, type = "random")
    
    # Extract species richness and functional diversity values at random points
    sr_values <- extract_from_rasters(sr_raster_path, random_points)
    fd_values <- extract_from_rasters(fd_raster_path, random_points)
    
    # Calculate mean SR and FD at parks/reserves and surrounding areas
    mean_sr_parks <- mean(sr_values[category_parks_reserves, ])
    mean_sr_surrounding <- mean(sr_values[!category_parks_reserves, ])
    mean_fd_parks <- mean(fd_values[category_parks_reserves, ])
    mean_fd_surrounding <- mean(fd_values[!category_parks_reserves, ])
    
    # Store the permutation results in the dataframe
    perm_results <- rbind(perm_results, data.frame(
      Permutation = i,
      Park_Category = category,
      Mean_SR_Parks = mean_sr_parks,
      Mean_SR_Surrounding = mean_sr_surrounding,
      Mean_FD_Parks = mean_fd_parks,
      Mean_FD_Surrounding = mean_fd_surrounding
    ))
  }
}

# Calculate the observed mean SR and FD at parks/reserves and surrounding areas
observed_means <- data.frame(
  Park_Category = character(),
  Mean_SR_Parks = numeric(),
  Mean_SR_Surrounding = numeric(),
  Mean_FD_Parks = numeric(),
  Mean_FD_Surrounding = numeric(),
  stringsAsFactors = FALSE
)

for (category in park_categories) {
  observed_mean_sr_parks <- mean(extract_from_rasters(sr_raster_path, parks_reserves[parks_reserves$Park_Category == category, ]))
  observed_mean_sr_surrounding <- mean(extract_from_rasters(sr_raster_path, parks_reserves[parks_reserves$Park_Category != category, ]))
  observed_mean_fd_parks <- mean(extract_from_rasters(fd_raster_path, parks_reserves[parks_reserves$Park_Category == category, ]))
  observed_mean_fd_surrounding <- mean(extract_from_rasters(fd_raster_path, parks_reserves[parks_reserves$Park_Category != category, ]))
  
  observed_means <- rbind(observed_means, data.frame(
    Park_Category = category,
    Mean_SR_Parks = observed_mean_sr_parks,
    Mean_SR_Surrounding = observed_mean_sr_surrounding,
    Mean_FD_Parks = observed_mean_fd_parks,
    Mean_FD_Surrounding = observed_mean_fd_surrounding
  ))
}

# Perform the permutation analysis and calculate p-values
p_values <- data.frame(
  Park_Category = character(),
  P_Value_SR_Parks = numeric(),
  P_Value_SR_Surrounding = numeric(),
  P_Value_FD_Parks = numeric(),
  P_Value_FD_Surrounding = numeric(),
  stringsAsFactors = FALSE
)

for (category in park_categories) {
  observed_mean_sr_parks <- observed_means$Mean_SR_Parks[observed_means$Park_Category == category]
  observed_mean_sr_surrounding <- observed_means$Mean_SR_Surrounding[observed_means$Park_Category == category]
  observed_mean_fd_parks <- observed_means$Mean_FD_Parks[observed_means$Park_Category == category]
  observed_mean_fd_surrounding <- observed_means$Mean_FD_Surrounding[observed_means$Park_Category == category]
  
  p_value_sr_parks <- sum(perm_results$Mean_SR_Parks[perm_results$Park_Category == category] >= observed_mean_sr_parks) / n_permutations
  p_value_sr_surrounding <- sum(perm_results$Mean_SR_Surrounding[perm_results$Park_Category == category] >= observed_mean_sr_surrounding) / n_permutations
  p_value_fd_parks <- sum(perm_results$Mean_FD_Parks[perm_results$Park_Category == category] >= observed_mean_fd_parks) / n_permutations
  p_value_fd_surrounding <- sum(perm_results$Mean_FD_Surrounding[perm_results$Park_Category == category] >= observed_mean_fd_surrounding) / n_permutations
  
  p_values <- rbind(p_values, data.frame(
    Park_Category = category,
    P_Value_SR_Parks = p_value_sr_parks,
    P_Value_SR_Surrounding = p_value_sr_surrounding,
    P_Value_FD_Parks = p_value_fd_parks,
    P_Value_FD_Surrounding = p_value_fd_surrounding
  ))
}

# Print the results
print("Mean SR at Parks/Reserves:")
print(observed_means$Mean_SR_Parks)
print("P-values SR at Parks/Reserves:")
print(p_values$P_Value_SR_Parks)

print("Mean SR in Surrounding Areas:")
print(observed_means$Mean_SR_Surrounding)
print("P-values SR in Surrounding Areas:")
print(p_values$P_Value_SR_Surrounding)

print("Mean FD at Parks/Reserves:")
print(observed_means$Mean_FD_Parks)
print("P-values FD at Parks/Reserves:")
print(p_values$P_Value_FD_Parks)

print("Mean FD in Surrounding Areas:")
print(observed_means$Mean_FD_Surrounding)
print("P-values FD in Surrounding Areas:")
print(p_values$P_Value_FD_Surrounding)
