# Title: Mapping and Analyzing FUSE Species Richness and Protected Area Coverage
# Project: Evaluating the Effectiveness of Protected Areas and Community-Managed Lands 
#          in Capturing Multiple Dimensions of Frugivorous Biodiversity in the Tropical Andes
# Author: Beth E. Gerstner
# Collaborators: Phoebe L. Zarnetske
# Overview: This script generates a map of FUSE species richness in the Tropical Andes, including an inset map, and calculates the extent of protected areas in the region based on IUCN categories.
# Outputs: 
#   - FUSE species richness map with an inset
#   - Statistics on protected area coverage by IUCN designation
# Date: September 10, 2023

# Load the necessary libraries

library(raster)
library(dplyr)
library(raster)
library(ggplot2)
library(rnaturalearth)
library(ggsn)
library(viridis)
library(cowplot)
library(sf)

#Parks maps
# Pull in world map
worldMap <- ne_countries(scale = "medium", type = "countries", returnclass = "sf")

# Subset world map. In this case we are removing the Galapagos by defining the bounding box around the Ecuador polygon.
study_region <- worldMap %>% filter(continent == "South America" | continent== "North America")
study_region_crop <- worldMap %>% filter(sovereignt == "Colombia" | sovereignt == "Peru" | sovereignt == "Bolivia" | sovereignt == "Venezuela"| sovereignt == "Ecuador")

TA <- read_sf("PLACEHOLDER_PATHtropical_subsets/Tropical_Andes_shape.shp")

TA_refined <- st_intersection(TA, study_region_crop)

# Crop the study region to the bounding box of interest
study_region_crop <-st_crop(study_region, xmin = -87, xmax = -61, ymin = -25, ymax = 13)

# Density plots
# Load the shapefile of parks (forest only)
parks <- st_read("PLACEHOLDER_PATHWDPA_ALL/parks_reserves_forest.shp")

#parks_IUCN <- parks %>%
  #filter(IUCN_CAT %in% c("Ib", "II", "III", "IV", "V", "VI","Not Reported", "Not Assigned"))

# Remove parks with names starting with "Cuenca hydrographica" <- these are just watersheds
filtered_parks <- parks %>%
  filter(!grepl("^Cuenca", NAME))


# Load the raster of species richness
mam_fuse <- raster("PLACEHOLDER_PATHrichness/FUSE_species_masked_TA_mam_foraging.tif")
bird_fuse <- raster("PLACEHOLDER_PATHrichness/FUSE_species_masked_TA_birds_foraging.tif")

#all fuse overlay

resample_fuse <- resample(mam_fuse, bird_fuse, method="bilinear")
all_fuse <- resample_fuse + bird_fuse

setwd("PLACEHOLDER_PATHrichness")
writeRaster(all_fuse,"all_fuse_species.tif",format="GTiff")



# Convert raster data to a data frame
species_points_fuse <- rasterToPoints(all_fuse)
species_df_fuse <- as.data.frame(species_points_fuse)



# Rename columns for clarity
colnames(species_df_fuse) <- c("Longitude", "Latitude", "FUSE_species")



# Create the species richness map as a raster
FUSE_map <- ggplot() +
  theme_bw() +
  geom_sf(data = study_region_crop, fill = "white") +
  geom_raster(data = species_df_fuse, aes(x = Longitude, y = Latitude, fill = FUSE_species)) +
  scale_fill_viridis_c(name = "# Species") +
  labs(title = "FUSE Species") +
  geom_sf(data = filtered_parks, fill = "transparent", color = "red", size = 0.01, show.legend = "polygon") +
  scalebar(x.min = -83.5, x.max = -76.5, y.min = -24, y.max = -22.5, dist = 400, st.dist = 0.3, st.size = 3.5, 
           height = 0.5, transform = TRUE, dist_unit = "km", model = 'WGS84') +
  north(study_region_crop, location = "bottomleft", scale = 0.065, symbol = 1) +
  theme(
    panel.background = element_rect(fill = "aliceblue", color = "black"),
    legend.key.size = unit(0.6, 'cm'), 
    legend.key.width = unit(0.5, "cm"), 
    legend.justification = c(1, 0), 
    legend.position = c(0.22, 0.20), 
    legend.box.background = element_rect(fill = "white"),    
    plot.title = element_text(size = 14, hjust = 0.5),  
    axis.text = element_text(size = 14),    
    axis.title = element_text(size = 14),                                               
    legend.text = element_text(size = 8 ),  
    legend.title = element_text(size = 10)) +
  ylim(-25, 12) +
  xlab("Longitude") + 
  ylab("Latitude") +
  geom_rect(aes(
    xmin = -79.2, 
    xmax = -77.4, 
    ymin = -2.29, 
    ymax = -0.28),
    fill = NA, 
    colour = "darkorange",
    size = 0.6,
    alpha = 0.8)

#check the way the inset looks
test_inset <- ggplot() + geom_raster(data = species_df_fuse, aes(x = Longitude, y = Latitude, fill = FUSE_species)) + scale_fill_continuous(type = "viridis", name = "# Species") + theme_bw() + xlim(-79.19710282803564, -77.40496798559705) + ylim(-2.2855650361324393,-0.28357305744216) +   geom_sf(data = filtered_parks, fill = "transparent", color = "red", size = 0.01, show.legend = "polygon") 



# Get zoom box and outline study region used for GBIF example
inset_map_box <- 
  test_inset +
  geom_rect(aes(
    xmin = -79.19710282803564, 
    xmax =  -77.40496798559705, 
    ymin = -2.2855650361324393, 
    ymax = -0.28357305744216),
    fill = NA, 
    colour = "white",
    size = 0.6,
    alpha=.8
  )


# Add inset map to the FUSE map
fuse_map_inset <-ggdraw(FUSE_map) +
  draw_plot(
    {
      inset_map_box +
        theme(legend.position = "none", axis.title.x = element_blank(),
              axis.title.y = element_blank(), axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              plot.background = element_blank(),
              panel.grid.major = element_blank())
    },
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = 0.43, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.50,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.43, 
    height = 0.24)

#Protected area stats
sf_use_s2(FALSE)

# Total area of the Tropical Andes
TA_refined$area <- sum(st_area(st_transform(TA_refined, 4326)) / (1000 * 1000)) # in km²

# Load protected areas shapefile
parks_full <- read_sf("PLACEHOLDER_PATHWDPA_ALL/WDPA_merged_TA.shp")

# Calculate area of strongly protected zones (IUCN categories Ia - IV)
strongly_protected <- parks_full %>%
  filter(IUCN_CAT %in% c("Ia", "Ib", "II", "III", "IV"))
strongly_protected_area <- sum(st_area(st_transform(st_union(st_combine(strongly_protected)), 4326)) / (1000 * 1000))

# Calculate area of all designated protected zones (IUCN categories Ia - VI)
all_designated <- parks_full %>%
  filter(IUCN_CAT %in% c("Ia", "Ib", "II", "III", "IV", "V", "VI"))
all_designated_area <- sum(st_area(st_transform(st_union(st_combine(all_designated)), 4326)) / (1000 * 1000))

# Percentage of Tropical Andes under protection
percent_strongly_protected <- (strongly_protected_area / TA_refined$area) * 100
percent_all_designated <- (all_designated_area / TA_refined$area) * 100

# Outputs
# Display the map with the inset
print(fuse_map_inset)

# Print statistics
cat("Total Area of Tropical Andes:", TA_refined$area, "km²\n")
cat("Area of Strongly Protected Zones (Ia - IV):", strongly_protected_area, "km²\n")
cat("Percentage of Strongly Protected Zones:", percent_strongly_protected, "%\n")
cat("Area of All Designated Protected Zones (Ia - VI):", all_designated_area, "km²\n")
cat("Percentage of All Designated Protected Zones:", percent_all_designated, "%\n")
