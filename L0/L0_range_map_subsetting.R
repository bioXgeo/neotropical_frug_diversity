#Title: Extracting and processing range maps for mammals and birds in Tropical Andes

#Project: Functional diversity of frugivores in the Tropical Andes: implications for conservation

#Author: Beth E. Gerstner

#Collaborators: Phoebe L. Zarnetske

#Overview: This script subsets range maps from IUCN and BirdLife International for species in the Tropical Andes. The range map multipolygon for birds was previously subsetted to the Frugivoria dataset (https://github.com/bioXgeo/neotropical_frugivores/blob/master/code/L0/data_package_L0/L0_5_BOTW_processing.R). Further, it removes parts of the range where the species may be extinct and retains only areas where the species is present or inferred to be present.

#Data output: Range maps (as multipolygons) subsetting to species in the Tropical Andes.

#Date: June 20th, 2023

#Load libraries
library(sf)
library(raster)
library(dplyr)

# Read in all range maps from IUCN (2022) for mammals
mam_shp <- st_read("C:/Users/bgers/Desktop/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp")
# Change species name column to match species list
colnames(mam_shp)[2] <- "IUCN_species_name"

# Read in all range maps for BirdLife International (2022) - subsetted to species of interest in MSU's HPC
bird_shp <-st_read("C:/Users/bgers/Desktop/frugivoria_range/BOTW_subset.shp")
colnames(bird_shp)[2] <- "IUCN_species_name"

## Tropical Andes subsetting 
#read in Tropical Andes species lists
mammal_list <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/tropical_subsets/frugivoria_TA_mammal_subset.csv")
bird_list <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/tropical_subsets/frugivoria_TA_bird_subset.csv")

#Subset mammal shapefile to Tropical Andes species list
mam_TA_shp <- mam_shp %>% filter(IUCN_species_name %in% mammal_list$IUCN_species_name)
bird_TA_shp <- bird_shp %>% filter(IUCN_species_name %in% bird_list$IUCN_species_name)

nrow(mam_TA_shp)
nrow(bird_TA_shp)

#Check how many distinct species are in each dataset
length(unique(mam_TA_shp$IUCN_species_name))
length(unique(bird_TA_shp$IUCN_species_name)) # Total 1242 species (only 19 missing)

#Write shapefiles to file
st_write(mam_TA_shp, "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/tropical_subsets/mam_TA_shp.shp")
st_write(bird_TA_shp, "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/tropical_subsets/bird_TA_shp.shp")

## Subsetting shapefiles to present and inferred presence
# Remove all shapefiles that have a presence code above 3 (this removes parts of the range where the species is extinct or likely extinct; see spatial traits metadata for all codes)
mam_TA_present <- mam_TA_shp[!mam_TA_shp$presence >3,]
bird_TA_present <- bird_TA_shp[!bird_TA_shp$presenc >3,]
bird_TA_introduced <- bird_TA_shp[!bird_TA_present$origin ==3,]

#Join polygons for each species

# Perform union operation based on IUCN_species_name
mammals_union <- mam_TA_present %>% group_by(IUCN_species_name) %>% summarize(geometry = st_union(geometry))

# Check the resulting multipolygon dataset
print(mammals_union)

library(terra)

#create empty raster as a template using SRTM at 1km
r <- rast("C:/Users/bgers/Desktop/srtm_1km.tif")

er <- rast(ext(mammals_union), resolution=res(r))
crs(er) <- crs(r)
er <- raster(er)

# Turn into multipolygon
library(fasterize)
mammals_union_multi <-st_cast(mammals_union, "GEOMETRY") %>% st_cast("MULTIPOLYGON")

# Turn all polygons into raster (too big, needs to run on HPC)
mammal_rasters <- fasterize(mammals_union_multi, er, by="IUCN_species_name")



