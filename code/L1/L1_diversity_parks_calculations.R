#Diversity calculations per park


library(raster)
library(sf)
library(exactextractr)

# Read the parks and reserves shapefile
parks_reserves <- st_read("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/datasets/WDPA_shapefile_merged/parks_reserves_forest.shp")

# Read in Tropical andes shapefile
tropical_andes <- st_read("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/datasets/tropical_andes_shp/Tropical_Andes_shape.shp")


##BIRDS
# fd

# Create empty dataframes to store the results
avg_fd_per_category <- data.frame(Category = character(), Avg_fd_birds = numeric(), stringsAsFactors = FALSE)
avg_fd_per_park <- data.frame(Park = character(), Avg_fd_birds = numeric(), Category = numeric(), stringsAsFactors = FALSE)
avg_td_per_category <- data.frame(Category = character(), Avg_td_birds = numeric(), stringsAsFactors = FALSE)
avg_td_per_park <- data.frame(Park = character(), Avg_td_birds = numeric(), Category = numeric(), stringsAsFactors = FALSE)


bird_fd <- raster("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/Results/richness/FD_birds_foraging_TA.tif")
bird_td <- raster("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/Results/richness/Spec_rich_birds_TA.tif")
# Loop through each park category
for (category in unique(parks_reserves$IUCN_CAT)) {
  # Subset the parks shapefile by the current category
  subset_parks <- parks_reserves[parks_reserves$IUCN_CAT == category, ]
  
  # Extract fd species values for the subsetted parks
  extracted_values <- exact_extract(bird_fd, subset_parks, fun = c("mean","variety"), append_cols=c("IUCN_CAT", "NAME", "GOV_TYPE","PARENT_ISO","GIS_AREA"), coverage_area = TRUE)
  extracted_values <- extracted_values[!is.na(extracted_values$mean), ]
  avg_fd_category <- mean(extracted_values$mean)
  
  # Add the result to the category dataframe
  avg_fd_per_category <- rbind(avg_fd_per_category, data.frame(Category = category, Avg_fd_birds = avg_fd_category))
  
  # td
  # Extract td species values for the subsetted parks
  extracted_values <- exact_extract(bird_td, subset_parks, fun = c("mean","variety"), append_cols=c("IUCN_CAT", "NAME", "GOV_TYPE","PARENT_ISO","GIS_AREA"), coverage_area = TRUE)
  extracted_values <- extracted_values[!is.na(extracted_values$mean), ]
  avg_td_category <- mean(extracted_values$mean)
  
  
  # Add the result to the category dataframe
  avg_td_per_category <- rbind(avg_td_per_category, data.frame(Category = category, Avg_td_birds = avg_td_category))

  
  # Loop through each park in the current category
  for (park_name in unique(subset_parks$NAME)) {
    # Subset the parks shapefile by the current park
    subset_park <- subset_parks[subset_parks$NAME == park_name, ]
    
    # Extract fd species values for the subsetted park
    extracted_park_values <- exact_extract(bird_fd, subset_park, fun = c("mean","variety"), append_cols=c("IUCN_CAT", "NAME", "GOV_TYPE","PARENT_ISO","GIS_AREA"), coverage_area = TRUE)
    
    # Calculate the average fd species per park
    avg_fd_park <- mean(extracted_park_values$mean)
    
    # Add the result to the park dataframe
    avg_fd_per_park <- rbind(avg_fd_per_park, data.frame(Park = park_name, Avg_fd_birds = avg_fd_park, Category = category))
    
    #td
    # Extract td species values for the subsetted park
    extracted_park_values <- exact_extract(bird_td, subset_park, fun = c("mean","variety"), append_cols=c("IUCN_CAT", "NAME", "GOV_TYPE","PARENT_ISO","GIS_AREA"), coverage_area = TRUE)
    
    # Calculate the average td species per park
    avg_td_park <- mean(extracted_park_values$mean)
    
    # Add the result to the park dataframe
    avg_td_per_park <- rbind(avg_td_per_park, data.frame(Park = park_name, Avg_td_birds = avg_td_park, Category = category))
  }
}
    

# Print the average fd species per park category dataframe
print(avg_fd_per_category)
print(avg_td_per_category)

# Print the average fd species per park dataframe
print(avg_fd_per_park)
print(avg_td_per_park)
avg_fd_per_park_no_na <- avg_fd_per_park[!is.na(avg_fd_per_park$Avg_fd_birds), ]
avg_td_per_park_no_na <- avg_td_per_park[!is.na(avg_td_per_park$Avg_td_birds), ]


setwd("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/Results/richness/")

#Write tables to a file
write.csv(avg_fd_per_category, "avg_fd_per_category_birds_foraging.csv")
write.csv(avg_td_per_park_no_na, "avg_td_per_park_birds_foraging.csv")
write.csv(avg_td_per_category, "avg_td_per_category_birds_foraging.csv")
write.csv(avg_fd_per_park_no_na, "avg_fd_per_park_birds_foraging.csv")


## MAMMALS
#Diversity calculations per park


library(raster)
library(sf)
library(exactextractr)

# Read the parks and reserves shapefile
parks_reserves <- st_read("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/datasets/WDPA_shapefile_merged/parks_reserves_forest.shp")

# Read in Tropical andes shapefile
tropical_andes <- st_read("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/datasets/tropical_andes_shp/Tropical_Andes_shape.shp")


##BIRDS
# fd

# Create empty dataframes to store the results
avg_fd_per_category <- data.frame(Category = character(), Avg_fd_mams = numeric(), stringsAsFactors = FALSE)
avg_fd_per_park <- data.frame(Park = character(), Avg_fd_mams = numeric(), Category = numeric(), stringsAsFactors = FALSE)
avg_td_per_category <- data.frame(Category = character(), Avg_td_mams = numeric(), stringsAsFactors = FALSE)
avg_td_per_park <- data.frame(Park = character(), Avg_td_mams = numeric(), Category = numeric(), stringsAsFactors = FALSE)


mams_fd <- raster("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/Results/richness/FD_mams_foraging_TA.tif")
mams_td <- raster("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/Results/richness/Spec_rich_mam_TA.tif")
# Loop through each park category
for (category in unique(parks_reserves$IUCN_CAT)) {
  # Subset the parks shapefile by the current category
  subset_parks <- parks_reserves[parks_reserves$IUCN_CAT == category, ]
  
  # Extract fd species values for the subsetted parks
  extracted_values <- exact_extract(mams_fd, subset_parks, fun = c("mean","variety"), append_cols=c("IUCN_CAT", "NAME", "GOV_TYPE","PARENT_ISO","GIS_AREA"), coverage_area = TRUE)
  extracted_values <- extracted_values[!is.na(extracted_values$mean), ]
  avg_fd_category <- mean(extracted_values$mean)
  
  # Add the result to the category dataframe
  avg_fd_per_category <- rbind(avg_fd_per_category, data.frame(Category = category, Avg_fd_mams = avg_fd_category))
  
  # td
  # Extract td species values for the subsetted parks
  extracted_values <- exact_extract(mams_td, subset_parks, fun = c("mean","variety"), append_cols=c("IUCN_CAT", "NAME", "GOV_TYPE","PARENT_ISO","GIS_AREA"), coverage_area = TRUE)
  extracted_values <- extracted_values[!is.na(extracted_values$mean), ]
  avg_td_category <- mean(extracted_values$mean)
  
  
  # Add the result to the category dataframe
  avg_td_per_category <- rbind(avg_td_per_category, data.frame(Category = category, Avg_td_mams = avg_td_category))
  
  
  # Loop through each park in the current category
  for (park_name in unique(subset_parks$NAME)) {
    # Subset the parks shapefile by the current park
    subset_park <- subset_parks[subset_parks$NAME == park_name, ]
    
    # Extract fd species values for the subsetted park
    extracted_park_values <- exact_extract(mams_fd, subset_park, fun = c("mean","variety"), append_cols=c("IUCN_CAT", "NAME", "GOV_TYPE","PARENT_ISO","GIS_AREA"), coverage_area = TRUE)
    
    # Calculate the average fd species per park
    avg_fd_park <- mean(extracted_park_values$mean)
    
    # Add the result to the park dataframe
    avg_fd_per_park <- rbind(avg_fd_per_park, data.frame(Park = park_name, Avg_fd_mams = avg_fd_park, Category = category))
    
    #td
    # Extract td species values for the subsetted park
    extracted_park_values <- exact_extract(mams_td, subset_park, fun = c("mean","variety"), append_cols=c("IUCN_CAT", "NAME", "GOV_TYPE","PARENT_ISO","GIS_AREA"), coverage_area = TRUE)
    
    # Calculate the average td species per park
    avg_td_park <- mean(extracted_park_values$mean)
    
    # Add the result to the park dataframe
    avg_td_per_park <- rbind(avg_td_per_park, data.frame(Park = park_name, Avg_td_mams = avg_td_park, Category = category))
  }
}


# Print the average fd species per park category dataframe
print(avg_fd_per_category)
print(avg_td_per_category)

# Print the average fd species per park dataframe
print(avg_fd_per_park)
print(avg_td_per_park)
avg_fd_per_park_no_na <- avg_fd_per_park[!is.na(avg_fd_per_park$Avg_fd_mams), ]
avg_td_per_park_no_na <- avg_td_per_park[!is.na(avg_td_per_park$Avg_td_mams), ]

setwd("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/Results/richness/")

#Write tables to a file
write.csv(avg_fd_per_category, "avg_fd_per_category_mams_foraging.csv")
write.csv(avg_td_per_park_no_na, "avg_td_per_park_mams_foraging.csv")
write.csv(avg_td_per_category, "avg_td_per_category_mams_foraging.csv")
write.csv(avg_fd_per_park_no_na, "avg_fd_per_park_mams_foraging.csv")
