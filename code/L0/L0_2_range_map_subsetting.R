# Title: Extracting and processing range maps for mammals and birds in Tropical Andes

# Project: Evaluating the Effectiveness of Protected Areas and Community-Managed Lands 
#          in Capturing Multiple Dimensions of Frugivorous Biodiversity in the Tropical Andes
# Author: Beth E. Gerstner
# Collaborators: Phoebe L. Zarnetske
# Overview: This script subsets range maps from IUCN and BirdLife International for species in the Tropical Andes. 
# The range map multipolygon for birds was previously subsetted to the Frugivoria dataset (https://github.com/bioXgeo/neotropical_frugivores/blob/master/code/L0/data_package_L0/L0_5_BOTW_processing.R). 
# Further, it removes parts of the range where the species may be extinct and retains only areas where the species is present or inferred to be present.
# Data output: Range maps (as multipolygons) subsetting to species in the Tropical Andes.

#Date: June 20th, 2023

#Load libraries
library(sf)
library(raster)
library(dplyr)
library(fasterize)
library(terra)
library(maps)
library(ggplot2)
library(tmap)
library(rnaturalearth)
library(leaflet)
library(sf)
library(ggsn)
library(rgeos)
library(BAMMtools)


# Read in all range maps from IUCN (2022) for mammals
mam_shp <- st_read("PLACEHOLDER_PATH/datasets/IUCN_mammal_shape/MAMMALS_TERRESTRIAL_ONLY.shp")
# Change species name column to match species list
colnames(mam_shp)[2] <- "IUCN_species_name"

# Read in all range maps for BirdLife International (2022) - subsetted to species of interest in MSU's HPC
bird_shp <-st_read("PLACEHOLDER_PATH/datasets/BOTW_subset.shp")
colnames(bird_shp)[2] <- "IUCN_species_name"

## Tropical Andes subsetting 
#read in Tropical Andes species lists
mammal_list <- read.csv("/PLACEHOLDER_PATH/atasets/frugivoria_TA_mammal_subset_final_elevation.csv")
bird_list <- read.csv("PLACEHOLDER_PATH/datasets/frugivoria_TA_bird_subset_final.csv")

#Subset mammal shapefile to Tropical Andes species list
mam_TA_shp <- mam_shp %>% filter(IUCN_species_name %in% mammal_list$IUCN_species_name)
bird_TA_shp <- bird_shp %>% filter(IUCN_species_name %in% bird_list$IUCN_species_name)
 
nrow(mam_TA_shp)
nrow(bird_TA_shp)

#Check how many distinct species are in each dataset
length(unique(mam_TA_shp$IUCN_species_name))
length(unique(bird_TA_shp$IUCN_species_name)) # Total 1242 species (only 19 missing)

#Write shapefiles to file
st_write(mam_TA_shp, "INSERT FILE PATH")
st_write(bird_TA_shp, "INSERT FILE PATH")

## Subsetting shapefiles to present and inferred presence
# Remove all shapefiles that have a presence code above 3 (this removes parts of the range where the species is extinct or likely extinct; see spatial traits metadata for all codes)
mam_TA_present <- mam_TA_shp[!mam_TA_shp$presence >3,]
mam_TA_introduced<- mam_TA_present[mam_TA_present$origin ==3,] #only part of the range was in Cuba for one C. paca which will be removed
bird_TA_present <- bird_TA_shp[!bird_TA_shp$presenc >3,]

#remove ranges that represent non-native ranges
bird_TA_introduced <- bird_TA_present[bird_TA_present$origin ==3,]

## Mammals
#Join polygons for each species

# Perform union operation based on IUCN_species_name
mammals_union <- mam_TA_present %>% group_by(IUCN_species_name) %>% summarize(geometry = st_union(geometry))

# Check the resulting multipolygon dataset
print(mammals_union)

#create empty raster as a template using SRTM at 1km
r <- rast("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/datasets/srtm_1km.tif")

er <- rast(ext(mammals_union), resolution=res(r))
crs(er) <- crs(r)
er <- raster(er)

# Turn into multipolygon
mammals_union_multi <-st_cast(mammals_union, "GEOMETRY") %>% st_cast("MULTIPOLYGON")

# set working directory for where you want output saved
setwd("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/Results/rasterized_maps/mammal_rasters/")

# fasterize can only handle rasterizing 15 rows at a time
# Define the chunk size
chunk_size <- 15

# Calculate the number of chunks
num_chunks <- ceiling(nrow(mammals_union_multi) / chunk_size)

# Loop through the chunks
for (i in 1:num_chunks) {
  # Define the start and end indices for the current chunk
  start_index <- (i - 1) * chunk_size + 1
  end_index <- min(i * chunk_size, nrow(mammals_union_multi))
  
  # Subset the data for the current chunk
  chunk <- mammals_union_multi[start_index:end_index, ]
  
  # Rasterize the chunk
  mammal_rasters <- fasterize(chunk, er, by = "IUCN_species_name")
  
  # Replace periods with underscores in file names
  file_names <- gsub("\\.", "_", names(mammal_rasters))
  
  # Save rasters with modified file names
  writeRaster(mammal_rasters, filename = file_names, bylayer = TRUE, format = "GTiff", overwrite=TRUE)
}


## BIRDS

# Turn off spherical geometry because it's causing errors
sf_use_s2(FALSE)

# Perform union operation based on IUCN_species_name
birds_union <- bird_TA_present %>% group_by(IUCN_species_name) %>% summarize(geometry = st_union(geometry))

# Check the resulting multipolygon dataset
print(birds_union)

er <- rast(ext(birds_union), resolution=res(r))
crs(er) <- crs(r)
er <- raster(er)

# Turn into multipolygon
birds_union_multi <-st_cast(birds_union, "GEOMETRY") %>% st_cast("MULTIPOLYGON")


setwd("PLACEHOLDER_PATH/Results/rasterized_maps/bird_rasters/")

# fasterize can only handle rasterizing 15 rows at a time
# Define the chunk size
chunk_size <- 15

# Calculate the number of chunks
num_chunks <- ceiling(nrow(birds_union_multi) / chunk_size)

# Loop through the chunks
for (i in 1:num_chunks) {
  # Define the start and end indices for the current chunk
  start_index <- (i - 1) * chunk_size + 1
  end_index <- min(i * chunk_size, nrow(birds_union_multi))
  
  # Subset the data for the current chunk
  chunk <- birds_union_multi[start_index:end_index, ]
  
  # Rasterize the chunk
  bird_rasters <- fasterize(chunk, er, by = "IUCN_species_name")
  
  # Replace periods with underscores in file names
  file_names <- gsub("\\.", "_", names(bird_rasters))
  
  # Save rasters with modified file names
  writeRaster(bird_rasters, filename = file_names, bylayer = TRUE, format = "GTiff", overwrite=TRUE)
}


## Trim range maps to Andean States

# Pull in world map
worldMap <- ne_countries(scale = "medium", type = "countries", returnclass = "sf")

# Subset world map. In this case we are removing the Galapagos by defining the bounding box around the Ecuador polygon.
study_region <- worldMap %>% filter(sovereignt == "Ecuador" | sovereignt == "Colombia"|sovereignt == "Venezuela" | sovereignt == "Peru" | sovereignt == "Bolivia")

# MAMMALS

# Set the paths
input_folder <- "PLACEHOLDER_PATH/Results/rasterized_maps/mammal_rasters"
output_folder <- "PLACEHOLDER_PATH/Results/rasterized_maps/mammal_rasters/mammal_rasters_tropical_andes_clip/"
shapefile <- "study_region.shp"

# Get the list of raster files
raster_files <- list.files(input_folder, pattern = ".tif$", full.names = TRUE)

# Loop over each raster file
for (file in raster_files) {
  # Read the raster
  raster_data <- raster(file)
  
  # Crop the raster using the study region shapefile; mask the smaller file
  cropped_raster <- crop(raster_data, study_region)
  masked_raster <- mask(cropped_raster, study_region)
  
  # Define the output file path
  output_file <- paste0(output_folder, basename(file))
  
  # Write the cropped raster to the output folder
  writeRaster(masked_raster, filename = output_file, format = "GTiff", overwrite=TRUE)
}

#BIRDS

# Set the paths
input_folder <- "PLACEHOLDER_PATH/Results/rasterized_maps/bird_rasters/"
output_folder <- "PLACEHOLDER_PATH/Results/rasterized_maps/bird_rasters/bird_rasters_tropical_andes_clip/"
shapefile <- "study_region.shp"

# Get the list of raster files
raster_files <- list.files(input_folder, pattern = ".tif$", full.names = TRUE)

# Loop over each raster file
for (file in raster_files) {
  # Read the raster
  raster_data <- raster(file)
  
  # Crop the raster using the study region shapefile
  cropped_raster <- crop(raster_data, study_region)
  masked_raster <- mask(cropped_raster, study_region)
  
  # Define the output file path
  output_file <- paste0(output_folder, basename(file))
  
  # Write the cropped raster to the output folder
  writeRaster(masked_raster, filename = output_file, format = "GTiff", overwrite=TRUE)
}

## Elevation range mask

#Define input and output folders
input_folder <- "PLACEHOLDER_PATH/Results/rasterized_maps/mammal_rasters/mammal_rasters_tropical_andes_clip"
output_folder <- "PLACEHOLDER_PATH/Results/rasterized_maps/mammal_rasters/mammal_rasters_tropical_andes_clip/elev_mask"
srtm_filename <- "PLACEHOLDER_PATH/datasets/srtm_1km.tif"

srtm <- raster(srtm_filename)
# Read the mammal traits dataframe
mam_traits <- read.csv("PLACEHOLDER_PATH/datasets/tropical_subsets/frugivoria_TA_mammal_subset_subs_rm.csv")  # Replace with the actual filename and pathnsme


# Loop over each species in the mammal traits dataframe
for (i in 1:nrow(mam_traits)) {
  # Get the species name from the mammal traits dataframe
  species_name <- mam_traits$IUCN_species_name[i]
  
  # Modify the species name to match the raster filename format
  species_name <- gsub(" ", "_", species_name)
  
  # Create the filename for the species raster
  raster_filename <- paste0(input_folder, "/", species_name, ".tif")
  
  # Check if the species range raster file exists
  if (file.exists(raster_filename)) {
  
  # Read the species raster
  species_raster <- raster(raster_filename)
  
  # Crop srtm to smaller extent
  srtm_crop <- crop(srtm, species_raster)
  
  #get extents to match
  srtm_proj <-projectRaster(srtm_crop, species_raster)
  
  # Mask the SRTM elevation raster by the species range raster
  masked_srtm<- srtm_proj$srtm_1km * species_raster
  
  # Get the minimum and maximum elevation for the species
  min_elev <- as.numeric(mam_traits$min_elev[i])
  max_elev <- as.numeric(mam_traits$max_elev[i])
  
  # Mask areas below the minimum and above the maximum elevation
  masked_srtm[masked_srtm< min_elev | masked_srtm > max_elev] <- NA
  
  # Mask species raster by masked elevation
  species_raster_elev_mask <- mask(species_raster, masked_srtm)
  
  # Create the output filename for the refined raster
  output_filename <- paste0(output_folder,"/", species_name, "_elevation_range.tif")
  
  # Write the refined raster to the output folder
  writeRaster(species_raster_elev_mask, filename = output_filename, format = "GTiff", overwrite=T)
  
  } else {
    # Skip the species if the range raster file is not found
    message(paste("Skipping species:", species_name, "- Range raster not found."))
  }
}

# BIRDS

#Define input and output folders
input_folder <- "PLACEHOLDER_PATH/Results/rasterized_maps/bird_rasters/bird_rasters_tropical_andes_clip"
output_folder <- "PLACEHOLDER_PATH/Results/rasterized_maps/bird_rasters/bird_rasters_tropical_andes_clip/elev_mask"
srtm <- raster(srtm_filename)

# Read the bird traits dataframe
bird_traits <- read.csv("PLACEHOLDER_PATH/datasets/frugivoria_TA_bird_subset_final_elevation.csv")  # Replace with the actual filename and path

# Loop over each species in the bird traits dataframe
for (i in 1:nrow(bird_traits)) {
  # Get the species name from the bird traits dataframe
  species_name <- bird_traits$IUCN_species_name[i]
  
  # Modify the species name to match the raster filename format
  species_name <- gsub(" ", "_", species_name)
  
  # Create the filename for the species raster
  raster_filename <- paste0(input_folder, "/", species_name, ".tif")
  
  # Check if the species range raster file exists
  if (file.exists(raster_filename)) {
    
    # Read the species raster
    species_raster <- raster(raster_filename)
    
    # Crop srtm to smaller extent
    srtm_crop <- crop(srtm, species_raster)
    
    #get extents to match
    srtm_proj <-projectRaster(srtm_crop, species_raster)
    
    # Mask the SRTM elevation raster by the species range raster
    masked_srtm<- srtm_proj$srtm_1km * species_raster
    
    # Get the minimum and maximum elevation for the species and add a 100m buffer to account for uncertainty in range boundaries
    min_elev <- as.numeric(mam_traits$min_elev[i]) - 100
    max_elev <- as.numeric(mam_traits$max_elev[i]) + 100
    
    # Mask areas below the minimum and above the maximum elevation
    masked_srtm[masked_srtm< min_elev | masked_srtm > max_elev] <- NA
    
    # Mask species raster by masked elevation
    species_raster_elev_mask <- mask(species_raster, masked_srtm)
    
    # Create the output filename for the refined raster
    output_filename <- paste0(output_folder,"/", species_name, "_elevation_range.tif")
    
    # Write the refined raster to the output folder
    writeRaster(species_raster_elev_mask, filename = output_filename, format = "GTiff", overwrite=T)
    
    
  } else {
    # Skip the species if the range raster file is not found
    message(paste("Skipping species:", species_name, "- Range raster not found.")) 
  }
}

## Habitat masks
# MAMMALS

# Set the paths and filenames
input_folder <-"PLACEHOLDER_PATH/Results/rasterized_maps/mammal_rasters/mammal_rasters_tropical_andes_clip/elev_mask"
output_folder <- "PLACEHOLDER_PATH/Results/rasterized_maps/mammal_rasters/mammal_rasters_tropical_andes_clip/AOH_final_mam"
habitat_folder <- "PLACEHOLDER_PATH/datasets/IUCN_habitat_layers/"
habitat_all_species <- read.csv("PLACEHOLDER_PATH/datasets/tropical_subsets/habitat_all_species.csv")

# Get the list of species raster files
species_rasters <- list.files(input_folder, pattern = ".tif$", full.names = TRUE)

# Loop over each species raster file
for (species_raster_file in species_rasters) {
  # Extract the species name from the file name
  species_name <- gsub("_elevation_range.tif$", "", basename(species_raster_file))
  
  # Create the filename for the species range raster
  range_filename <- paste0(input_folder, "/", species_name, "_elevation_range.tif")
  
  # Check if the species range raster file exists
  if (file.exists(range_filename)) {
    # Read the species range raster
    species_raster <- raster(range_filename)
    
    # Loop over each habitat type associated with the species
    species_name <- gsub("_", " ", species_name)
    habitat_codes <- habitat_all_species$code_alt[habitat_all_species$species == species_name]
    combined_habitat_raster <- NULL
    
    for (code in habitat_codes) {
      # Create the filename for the habitat type raster
      habitat_filename <- paste0(habitat_folder, "iucn_habitatclassification_fraction_lvl2__", code,".tif")
    
      # Check if the habitat type raster file exists
      if (file.exists(habitat_filename)) {
        # Read the habitat type raster
        habitat_raster <- raster(habitat_filename)
        habitat_raster <- crop(habitat_raster, species_raster)
        habitat_raster[is.na(habitat_raster[])] <- 0 
        
        # Add the habitat raster to the combined habitat raster
        if (is.null(combined_habitat_raster)) {
          combined_habitat_raster <- habitat_raster
        } else {
          combined_habitat_raster <- combined_habitat_raster + habitat_raster
        }
      }
    }
  
    # Subset the species range raster to areas with values in the combined habitat raster
    habitat_proj <- projectRaster(combined_habitat_raster, species_raster)
    # Subset the species raster by areas above zero in habitat_proj
    subset_species <- species_raster
    subset_species[habitat_proj == 0] <- NA
    
    # Create the filename for the refined species raster
    species_name <- gsub(" ", "_", species_name)
    refined_species_filename <- paste0(output_folder, "/", species_name, "_AOH.tif")
    
    # Write the refined species raster to the output folder
    writeRaster(subset_species, filename = refined_species_filename, format = "GTiff", overwrite=T)
  }
}

# BIRDS

# Set the paths and filenames
input_folder <-"PLACEHOLDER_PATH/Results/rasterized_maps/bird_rasters/bird_rasters_tropical_andes_clip/elev_mask"
output_folder <- "PLACEHOLDER_PATH/Results/rasterized_maps/bird_rasters/bird_rasters_tropical_andes_clip/AOH_final_bird"
habitat_folder <- "PLACEHOLDER_PATH/datasets/IUCN_habitat_layers/"
habitat_all_species <- read.csv("PLACEHOLDER_PATH/datasets/tropical_subsets/habitat_all_species.csv")

# Get the list of species raster files
species_rasters <- list.files(input_folder, pattern = ".tif$", full.names = TRUE)

# Loop over each species raster file
for (species_raster_file in species_rasters) {
  # Extract the species name from the file name
  species_name <- gsub("_elevation_range.tif$", "", basename(species_raster_file))
  
  # Create the filename for the species range raster
  range_filename <- paste0(input_folder, "/", species_name, "_elevation_range.tif")
  
  # Check if the species range raster file exists
  if (file.exists(range_filename)) {
    # Read the species range raster
    species_raster <- raster(range_filename)
    
    # Loop over each habitat type associated with the species
    species_name <- gsub("_", " ", species_name)
    habitat_codes <- habitat_all_species$code_alt[habitat_all_species$species == species_name]
    combined_habitat_raster <- NULL
    
    for (code in habitat_codes) {
      # Create the filename for the habitat type raster
      habitat_filename <- paste0(habitat_folder, "iucn_habitatclassification_fraction_lvl2__", code,".tif")
      
      # Check if the habitat type raster file exists
      if (file.exists(habitat_filename)) {
        # Read the habitat type raster
        habitat_raster <- raster(habitat_filename)
        habitat_raster <- crop(habitat_raster, species_raster)
        habitat_raster[is.na(habitat_raster[])] <- 0 
        
        # Add the habitat raster to the combined habitat raster
        if (is.null(combined_habitat_raster)) {
          combined_habitat_raster <- habitat_raster
        } else {
          combined_habitat_raster <- combined_habitat_raster + habitat_raster
        }
      }
    }
    
    # Subset the species range raster to areas with values in the combined habitat raster
    habitat_proj <- projectRaster(combined_habitat_raster, species_raster)
    # Subset the species raster by areas above zero in habitat_proj
    subset_species <- species_raster
    subset_species[habitat_proj == 0] <- NA
    
    # Create the filename for the refined species raster
    species_name <- gsub(" ", "_", species_name)
    refined_species_filename <- paste0(output_folder, "/", species_name, "_AOH.tif")
    
    # Write the refined species raster to the output folder
    writeRaster(subset_species, filename = refined_species_filename, format = "GTiff", overwrite=T)
  }
}




