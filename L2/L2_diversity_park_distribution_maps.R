# Title: Mapping Biodiversity Metrics in Protected Areas
# Project: Evaluating the Effectiveness of Protected Areas and Community-Managed Lands 
#          in Capturing Multiple Dimensions of Frugivorous Biodiversity in the Tropical Andes
# Author: Beth E. Gerstner
# Collaborators: Phoebe L. Zarnetske
# Overview: This script generates density plots and maps for functional and taxonomic diversity (FD and TD) 
#           of mammals and birds within protected areas. It includes data extraction, raster masking, 
#           and visualization using circle maps and rasters for biodiversity metrics.
# Outputs: 
#   - Density plots for FD and TD across regions
#   - Maps visualizing FD and TD for birds and mammals in protected areas

# Date: September 1, 2023

# Required Packages
library(ggplot2)
library(sf)
library(raster)
library(dplyr)
library(cowplot)
library(patchwork)
library(exactextractr)
library(rnaturalearth)

# Pull in world map
worldMap <- ne_countries(scale = "medium", type = "countries", returnclass = "sf")

# Subset world map. In this case we are removing the Galapagos by defining the bounding box around the Ecuador polygon.
study_region <- worldMap %>% filter(continent == "South America" | continent== "North America")
 study_region_crop <- worldMap %>% filter(sovereignt == "Colombia" | sovereignt == "Peru" | sovereignt == "Bolivia" | sovereignt == "Venezuela"| sovereignt == "Ecuador")

# Read in Tropical Andes Shapefile
TA <- read_sf("PLACEHOLDER_PATH/tropical_subsets/Tropical_Andes_shape.shp")

# Crop TA by countries of interest
TA_refined <- st_intersection(TA, study_region_crop)

# Crop the study region to the bounding box of interest
study_region_crop <-st_crop(study_region, xmin = -87, xmax = -61, ymin = -29, ymax = 13)

## Generate density plots
# Load the shapefile of parks (forest only)
parks <- st_read("PLACEHOLDER_PATH/WDPA_ALL/parks_reserves_forest.shp")

# Load the raster of species richness
species_raster_TD_mam <- raster("PLACEHOLDER_PATH/richness/Spec_rich_mam_TA.tif")

# Mask raster by park 
species_mask_park_TD_mam <- mask(species_raster_TD_mam, parks)

# Add mean lines
raster_points_full_TD_mam <- as.data.frame(rasterToPoints(species_raster_TD_mam))
raster_points_park_TD_mam <- as.data.frame(rasterToPoints(species_mask_park_TD_mam))


mam_td_density <- ggplot() + 
  geom_histogram(aes(x = raster_points_full_TD_mam$Spec_rich_mam_TA, y = ..ncount.., 
                     fill = "Spec_rich_mam_TA", color = "Spec_rich_mam_TA"), alpha = 0.7, binwidth = 4) + 
  geom_histogram(aes(x = raster_points_park_TD_mam$Spec_rich_mam_TA, y = ..ncount.., 
                     fill = "Extracted Values", color = "Extracted Values"), alpha = 0.4, binwidth=4)+
  geom_vline(xintercept = 71, col = "red", linetype = "dashed") +  # Add a red dashed vertical line
  
  labs(x = "Taxonomic Diversity", y = "Frequency") +
  scale_fill_manual(name = "TD Comparison", 
                    values = c("darkblue", "lightblue"), 
                    labels = c("TD in PAs", "TD mammals in all area")) +
  scale_color_manual(name = "TD Comparison", 
                     values = c("darkturquoise", "red"),  # Switched the colors here
                     labels = c("TD in PAs", "TD mammals in all area")) +
  theme( panel.background = element_rect(fill = "white", color = "black", size = 1),
         plot.background = element_rect(fill = "white"),
         legend.position = c(.95, .95),
         legend.justification = c("right", "top"),
         legend.box.just = "right",
         legend.margin = margin(6, 6, 6, 6),
         legend.key.size = unit(0.2, "cm"),
         legend.text = element_text(size = 9),
         legend.title = element_text(size = 10),
         legend.background = element_rect(fill = "transparent"))



## FD
# Load the shapefile of parks (forest only)
parks <- st_read("PLACEHOLDER_PATH/WDPA_ALL/parks_reserves_forest.shp")

# Load the raster of species richness 
species_raster_FD_mam <- raster("PLACEHOLDER_PATH/richness/FD_mams_foraging_TA.tif")

# Mask raster by park 
species_mask_park_FD_mam <- mask(species_raster_FD_mam, parks)

# Add mean lines
raster_points_full_FD_mam <- as.data.frame(rasterToPoints(species_raster_FD_mam))
raster_points_park_FD_mam <- as.data.frame(rasterToPoints(species_mask_park_FD_mam))


mam_fd_density <- ggplot() + 
  geom_histogram(aes(x = raster_points_full_FD_mam$FD_mams_foraging_TA, y = ..ncount.., 
                     fill = "FD_mams_foraging_TA", color = "FD_mams_foraging_TA"), alpha = 0.7, binwidth = .0125) +
  geom_histogram(aes(x = raster_points_park_FD_mam$FD_mams_foraging_TA, y = ..ncount.., 
                     fill = "Extracted Mean", color = "Extracted Mean"), alpha = 0.4, binwidth=.0125) +
  geom_vline(xintercept = 0.69, col = "red", linetype = "dashed") +  # Add a red dashed vertical line
  labs(x = "Functional Diversity", y = "Frequency")  +
  scale_fill_manual(name = "FD Comparison", 
                    values = c("darkblue", "lightblue"), 
                    labels = c("FD in PAs", "FD mammals in all area")) + 
  scale_color_manual(name = "FD Comparison", 
                     values = c("darkturquoise", "red"),  # Switched the colors
                     labels = c("FD in PAs", "FD mammals in all area")) +
  theme( panel.background = element_rect(fill = "white", color = "black", size = 1),
         plot.background = element_rect(fill = "white"),
         legend.position = c(.45, .95),
         legend.justification = c("right", "top"),
         legend.box.just = "right",
         legend.margin = margin(6, 6, 6, 6),
         legend.key.size = unit(0.4, "cm"),
         legend.text = element_text(size = 9),
         legend.title = element_text(size = 10),
         legend.background = element_rect(fill = "transparent"))
)


plot_grid(mam_fd_density, bird_fd_density, mam_td_density, bird_td_density, ncol=2, align=c("hv"))

#Mapping
# Load the shapefile of parks (forest only)
parks <- st_read("PLACEHOLDER_PATH/WDPA_ALL/parks_reserves_forest.shp")

# Load the raster of species richness
species_raster_bird_td <- raster("PLACEHOLDER_PATH/richness/Spec_rich_birds_TA.tif")

# Extract the values of species richness at the centroid locations
extracted_values_bird_td <- exact_extract(species_raster_bird_td, parks, fun = c("mean"), append_cols = c("IUCN_CAT", "NAME", "GOV_TYPE", "PARENT_ISO", "GIS_AREA"), coverage_area = TRUE)
extracted_values_subset_bird_td <-as.data.frame(extracted_values_bird_td[,c("mean")])


# Load the raster of species richness
species_raster_bird_TD <- raster("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/Spec_rich_birds_TA.tif")

# Mask raster by park 
species_mask_park_TD <- mask(species_raster_bird_TD, parks)

# Add mean lines
raster_points_full_TD <- as.data.frame(rasterToPoints(species_raster_bird_TD))
raster_points_park_TD <- as.data.frame(rasterToPoints(species_mask_park_TD))


bird_td_density <- ggplot() + 
  geom_histogram(aes(x = raster_points_full_TD$Spec_rich_birds_TA, y = ..ncount.., 
                     fill = "Spec_rich_birds_TA", color = "Spec_rich_birds_TA"), alpha = 0.7, binwidth = 6) + 
  geom_histogram(aes(x = raster_points_park_TD$Spec_rich_birds_TA, y = ..ncount.., 
                     fill = "Extracted Values", color = "Extracted Values"), alpha = 0.4, binwidth=6) +
  geom_vline(xintercept = 140, col = "red", linetype = "dashed") +  # Add a red dashed vertical line
  
  labs(x = "Taxonomic Diversity", y = "Frequency") +
  scale_fill_manual(name = "TD Comparison", 
                    values = c("darkblue", "lightblue"), 
                    labels = c("TD in PAs", "TD birds in all area")) +
  scale_color_manual(name = "TD Comparison", 
                     values = c("darkturquoise", "red"),  # Switched colors for outlines
                     labels = c("TD in PAs", "TD birds in all area")) +
  theme( panel.background = element_rect(fill = "white", color = "black", size = 1),
         plot.background = element_rect(fill = "white"),
         legend.position = c(.95, .95),
         legend.justification = c("right", "top"),
         legend.box.just = "right",
         legend.margin = margin(6, 6, 6, 6),
         legend.key.size
         


## FD

# Load the shapefile of parks (forest only)
parks <- st_read("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/WDPA_ALL/parks_reserves_forest.shp")

# Load the raster of species richness
species_raster_FD <- raster("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/FD_birds_foraging_TA.tif")

# Mask raster by park 
species_mask_park_FD <- mask(species_raster_FD, parks)

# Add mean lines
raster_points_full_FD <- as.data.frame(rasterToPoints(species_raster_FD))
raster_points_park_FD <- as.data.frame(rasterToPoints(species_mask_park_FD))

# Generate bird density plot
bird_td_density <- ggplot() + 
  geom_histogram(aes(x = raster_points_full_TD$Spec_rich_birds_TA, y = ..ncount.., 
                     fill = "Spec_rich_birds_TA", color = "Spec_rich_birds_TA"), alpha = 0.7, binwidth = 6) + 
  geom_histogram(aes(x = raster_points_park_TD$Spec_rich_birds_TA, y = ..ncount.., 
                     fill = "Extracted Values", color = "Extracted Values"), alpha = 0.4, binwidth=6) +
  geom_vline(xintercept = 140, col = "red", linetype = "dashed") +  # Add a red dashed vertical line
  
  labs(x = "Taxonomic Diversity", y = "Frequency") +
  scale_fill_manual(name = "TD Comparison", 
                    values = c("darkblue", "lightblue"), 
                    labels = c("TD in PAs", "TD birds in all area")) +
  scale_color_manual(name = "TD Comparison", 
                     values = c("darkturquoise", "red"),  # Switched colors for outlines
                     labels = c("TD in PAs", "TD birds in all area")) +
  theme( panel.background = element_rect(fill = "white", color = "black", size = 1),
         plot.background = element_rect(fill = "white"),
         legend.position = c(.97, .95),
         legend.justification = c("right", "top"),
         legend.box.just = "right",
         legend.margin = margin(6, 6, 6, 6),
         legend.key.size = unit(0.2, "cm"),
         legend.text = element_text(size = 9),
         legend.title = element_text(size = 10),
         legend.background = element_rect(fill = "transparent"))


# Show the plot
TD_bird <- as.numeric(extracted_values_bird_td$mean)  # Convert to numeric

# Turn off spherical geometry
sf_use_s2(FALSE)

# Calculate the centroids of the parks
parks_centroids_bird <- st_centroid(parks)
parks_centroids_bird$TD_bird <- TD_bird

# Create the map with circles and species richness labels
circle_map_birds_TD <- ggplot() +
  geom_sf(data = study_region_crop, fill = "white") +
  geom_sf(data= TA_refined, fill="lightgray")+
  geom_sf(data = parks_centroids_bird, aes(fill = TD_bird)) +
  geom_point(data = parks_centroids_bird, aes(x = st_coordinates(parks_centroids_bird)[, 1], y = st_coordinates(parks_centroids_bird)[, 2], fill = TD_bird), shape = 21, size = 1.8) +
  labs(title = "Bird TD in Protected Areas") +
  scale_fill_continuous(type = "viridis", name="TD") +
  scalebar(x.min = -83.5, x.max = -76.5, y.min = -27, y.max = -25.5, dist=400, st.dist=.3, st.size=2.5, height=.5, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop,  location="bottomleft", scale=.065, symbol=1) +   theme(panel.background = element_rect(fill = "aliceblue", color="black"), legend.key.size = unit(.3, 'cm'), legend.key.width = unit(.4, "cm"), legend.justification = c(1, 0), legend.position = c(.30, .14), legend.box.background = element_rect(fill = "white"),    plot.title = element_text(size = 12, hjust = 0.5),  axis.text = element_text(size = 12),    axis.title = element_text(size=12), legend.text = element_text(size = 9 ),  legend.title = element_text(size = 10)) + ylim(-28,12) +xlab("Longitude")+ ylab("Latitude")


# Convert raster data to a data frame
species_points_bird_td <- rasterToPoints(species_raster_bird_TD)
species_df_bird_td <- as.data.frame(species_points_bird_td)

# Rename columns for clarity
colnames(species_df_bird_td) <- c("Longitude", "Latitude", "TD_bird")

# Create the species richness map as a raster
richness_map_birds_TD <- ggplot() +
  theme_bw() +
  geom_sf(data = study_region_crop, fill = "white") +
  geom_sf(data= TA_refined, fill="lightgray")+ 
  geom_raster(data = species_df_bird_td, aes(x = Longitude, y = Latitude, fill =  TD_bird)) +
  scale_fill_continuous(type = "viridis", name="TD") + labs(title = "Bird TD") + 
  scalebar(x.min = -83.5, x.max = -76.5, y.min = -27, y.max = -25.5, dist=400, st.dist=.3, st.size=2.5, height=.5, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop,  location="bottomleft", scale=.065, symbol=1) +   theme(panel.background = element_rect(fill = "aliceblue", color="black"), legend.key.size = unit(.3, 'cm'), legend.key.width = unit(.4, "cm"), legend.justification = c(1, 0), legend.position = c(.30, .14), legend.box.background = element_rect(fill = "white"),    plot.title = element_text(size = 12, hjust = 0.5),  axis.text = element_text(size = 12),    axis.title = element_text(size=12),                                                legend.text = element_text(size = ),  legend.title = element_text(size = 10)) + ylim(-28,12) +xlab("Longitude")+ ylab("Latitude")
                                                                                                                                                                                                                                                        
# Arrange the plots side by side using patchwork
bird_td <-plot_grid(richness_map_birds_TD,circle_map_birds_TD, bird_td_density, ncol=3, align= c("h"))

#FD birds
# Load the raster of species richness
species_raster_bird_fd <- raster("PLACEHOLDER_PATH/richness/FD_birds_foraging_TA.tif")
# Extract the values of species richness at the centroid locations
extracted_values_bird_fd <- exact_extract(species_raster_bird_fd, parks, fun = c("mean"), append_cols = c("IUCN_CAT", "NAME", "GOV_TYPE", "PARENT_ISO", "GIS_AREA"), coverage_area = TRUE)

FD_bird <- as.numeric(extracted_values_bird_fd$mean)  # Convert to numeric

# Calculate the centroids of the parks
parks_centroids_bird_fd <- st_centroid(parks)
parks_centroids_bird_fd$FD_bird <- FD_bird

# Create the map with circles and species richness labels
circle_map_bird_FD <- ggplot() +
  geom_sf(data = study_region_crop, fill = "white") +
  geom_sf(data= TA_refined, fill="lightgray")+
  geom_sf(data = parks_centroids_bird_fd, aes(fill = FD_bird)) +
  geom_point(data = parks_centroids_bird_fd, aes(x = st_coordinates(parks_centroids_bird_fd)[, 1], y = st_coordinates(parks_centroids_bird_fd)[, 2], fill = FD_bird),
             shape = 21, size = 1.8) +
  labs(title = "Bird FD in Protected Areas") + 
  scale_fill_continuous(type = "viridis", name="FD") +
  scalebar(x.min = -83.5, x.max = -76.5, y.min = -27, y.max = -25.5, dist=400, st.dist=.3, st.size=2.5, height=.5, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop,  location="bottomleft", scale=.065, symbol=1) +   theme(panel.background = element_rect(fill = "aliceblue", color="black"), legend.key.size = unit(.3, 'cm'), legend.key.width = unit(.4, "cm"), legend.justification = c(1, 0), legend.position = c(.30, .14), legend.box.background = element_rect(fill = "white"),    plot.title = element_text(size = 12, hjust = 0.5),  axis.text = element_text(size = 12),    axis.title = element_text(size=12),                                                legend.text = element_text(size = ),  legend.title = element_text(size = 10)) + ylim(-28,12) +xlab("Longitude")+ ylab("Latitude")

# Convert raster data to a data frame
species_points_bird_fd <- rasterToPoints(species_raster_bird_fd)
species_df_bird_fd <- as.data.frame(species_points_bird_fd)

# Rename columns for clarity
colnames(species_df_bird_fd) <- c("Longitude", "Latitude", "FD_bird")

# Create the species richness map as a raster
richness_map_bird_FD <- ggplot() +
  theme_bw() +
  geom_sf(data = study_region_crop, fill = "white") +
  geom_sf(data= TA_refined, fill="lightgray")+ 
  geom_raster(data = species_df_bird_fd, aes(x = Longitude, y = Latitude, fill =  FD_bird)) +
  scale_fill_continuous(type = "viridis", name= "FD") + labs(title = "Bird FD") + 
  scalebar(x.min = -83.5, x.max = -76.5, y.min = -27, y.max = -25.5, dist=400, st.dist=.3, st.size=2.5, height=.5, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop,  location="bottomleft", scale=.065, symbol=1) +   theme(panel.background = element_rect(fill = "aliceblue", color="black"), legend.key.size = unit(.3, 'cm'), legend.key.width = unit(.4, "cm"), legend.justification = c(1, 0), legend.position = c(.30, .14), legend.box.background = element_rect(fill = "white"),    plot.title = element_text(size = 12, hjust = 0.5),  axis.text = element_text(size = 12),    axis.title = element_text(size=12),                                                legend.text = element_text(size = ),  legend.title = element_text(size = 10)) + ylim(-28,12) +xlab("Longitude")+ ylab("Latitude")
                                                                                                                                                                                                                                                         
plot_grid(richness_map_bird_FD,circle_map_bird_FD, ncol=2, align= "hv")

### TD mammals

# Load the raster of species richness
species_raster_mam_td <- raster("PLACEHOLDER_PATH/richness/Spec_rich_mam_TA.tif")
# Extract the values of species richness at the centroid locations
extracted_values_mam_td <- exact_extract(species_raster_mam_td, parks, fun = c("mean"), append_cols = c("IUCN_CAT", "NAME", "GOV_TYPE", "PARENT_ISO", "GIS_AREA"), coverage_area = TRUE)

TD_mam <- as.numeric(extracted_values_mam_td$mean)  # Convert to numeric

# Calculate the centroids of the parks
parks_centroids_mam_td <- st_centroid(parks)
parks_centroids_mam_td$TD_mam <- TD_mam

# Create the map with circles and species richness labels
circle_map_mam_TD <- ggplot() +
  geom_sf(data = study_region_crop, fill = "white") +
  geom_sf(data= TA_refined, fill="lightgray")+
  geom_sf(data = parks_centroids_mam_td, aes(fill = TD_mam)) +
  geom_point(data = parks_centroids_mam_td, aes(x = st_coordinates(parks_centroids_mam_td)[, 1], y = st_coordinates(parks_centroids_mam_td)[, 2], fill = TD_mam),
             shape = 21, size = 1.8) +
  labs(title = "Mammal TD in Protected Areas") +
  scale_fill_continuous(type = "viridis", name="TD") +
  scalebar(x.min = -83.5, x.max = -76.5, y.min = -27, y.max = -25.5, dist=400, st.dist=.3, st.size=2.5, height=.5, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop,  location="bottomleft", scale=.065, symbol=1) +   theme(panel.background = element_rect(fill = "aliceblue", color="black"), legend.key.size = unit(.3, 'cm'), legend.key.width = unit(.4, "cm"), legend.justification = c(1, 0), legend.position = c(.30, .14), legend.box.background = element_rect(fill = "white"),    plot.title = element_text(size = 12, hjust = 0.5),  axis.text = element_text(size = 12),    axis.title = element_text(size=12),                                                legend.text = element_text(size = ),  legend.title = element_text(size = 10)) + ylim(-28,12) +xlab("Longitude")+ ylab("Latitude")

# Convert raster data to a data frame
species_points_mam_td <- rasterToPoints(species_raster_mam_td)
species_df_mam_td <- as.data.frame(species_points_mam_td)

# Rename columns for clarity
colnames(species_df_mam_td) <- c("Longitude", "Latitude", "TD_mam")

# Create the species richness map as a raster
richness_map_mam_TD <- ggplot() +
  theme_bw() +
  geom_sf(data = study_region_crop, fill = "white") +
  geom_sf(data= TA_refined, fill="lightgray")+ 
  geom_raster(data = species_df_mam_td, aes(x = Longitude, y = Latitude, fill =  TD_mam)) +
  scale_fill_continuous(type = "viridis", name="TD") + labs(title = "Mammal TD") +
  scalebar(x.min = -83.5, x.max = -76.5, y.min = -27, y.max = -25.5, dist=400, st.dist=.3, st.size=2.5, height=.5, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop,  location="bottomleft", scale=.065, symbol=1) +   theme(panel.background = element_rect(fill = "aliceblue", color="black"), legend.key.size = unit(.3, 'cm'), legend.key.width = unit(.4, "cm"), legend.justification = c(1, 0), legend.position = c(.30, .14), legend.box.background = element_rect(fill = "white"),    plot.title = element_text(size = 12, hjust = 0.5),  axis.text = element_text(size = 12),    axis.title = element_text(size=12),                                                legend.text = element_text(size = ),  legend.title = element_text(size = 10)) + ylim(-28,12) +xlab("Longitude")+ ylab("Latitude")

mammal_td <-plot_grid(richness_map_mam_TD, circle_map_mam_TD, ncol=2,align="h")

# Create desired layout
layout <-plot_layout(
  ncol=3,
  nrow=2,
  widths = c(7, 7), heights = unit(c(10, 10), c('cm', 'cm')))

#FD mammals
species_raster_mam_fd <- raster("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/FD_mams_foraging_TA.tif")
# Extract the values of species richness at the centroid locations
extracted_values_mam_fd <- exact_extract(species_raster_mam_fd, parks, fun = c("mean"), append_cols = c("IUCN_CAT", "NAME", "GOV_TYPE", "PARENT_ISO", "GIS_AREA"), coverage_area = TRUE)

FD_mam<- as.numeric(extracted_values_mam_fd$mean)  # Convert to numeric

# Calculate the centroids of the parks
parks_centroids_mam_fd <- st_centroid(parks)

parks_centroids_mam_fd$FD_mam <- FD_mam

# Create the map with circles and species richness labels
circle_map_mam_FD <- ggplot() +
  geom_sf(data = study_region_crop, fill = "white") +
  geom_sf(data= TA_refined, fill="lightgray")+
  geom_sf(data = parks_centroids_mam_fd, aes(fill = FD_mam)) +
  geom_point(data = parks_centroids_mam_fd, aes(x = st_coordinates(parks_centroids_mam_fd)[, 1], y = st_coordinates(parks_centroids_mam_fd)[, 2], fill = FD_mam), shape = 21, size = 1.8) +
  scale_fill_continuous(type = "viridis", name="FD")+
  labs(title = "Mammal FD in Protected Areas") +
  scalebar(x.min = -83.5, x.max = -76.5, y.min = -27, y.max = -25.5, dist=400, st.dist=.3, st.size=2.5, height=.5, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop,  location="bottomleft", scale=.065, symbol=1) +   theme(panel.background = element_rect(fill = "aliceblue", color="black"), legend.key.size = unit(.3, 'cm'), legend.key.width = unit(.4, "cm"), legend.justification = c(1, 0), legend.position = c(.30, .14), legend.box.background = element_rect(fill = "white"),    plot.title = element_text(size = 12, hjust = 0.5),  axis.text = element_text(size = 12),    axis.title = element_text(size=12),                                                legend.text = element_text(size = ),  legend.title = element_text(size = 10)) + ylim(-28,12) +xlab("Longitude")+ ylab("Latitude")


# Convert raster data to a data frame
species_points_mam_fd <- rasterToPoints(species_raster_mam_fd)
species_df_mam_fd <- as.data.frame(species_points_mam_fd)

# Rename columns for clarity
colnames(species_df_mam_fd) <- c("Longitude", "Latitude", "FD_mam")

# Create the species richness map as a raster
richness_map_mam_FD <- ggplot() +
  theme_bw() +
  geom_sf(data = study_region_crop, fill = "white") +
  geom_sf(data= TA_refined, fill="lightgray")+ 
  geom_raster(data = species_df_mam_fd, aes(x = Longitude, y = Latitude, fill =  FD_mam)) +
  scale_fill_continuous(type = "viridis", name="FD") + labs(title = "Mammal FD") +
  scalebar(x.min = -83.5, x.max = -76.5, y.min = -27, y.max = -25.5, dist=400, st.dist=.3, st.size=2.5, height=.5, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop,  location="bottomleft", scale=.065, symbol=1) +   theme(panel.background = element_rect(fill = "aliceblue", color="black"), legend.key.size = unit(.3, 'cm'), legend.key.width = unit(.4, "cm"), legend.justification = c(1, 0), legend.position = c(.30, .14), legend.box.background = element_rect(fill = "white"),    plot.title = element_text(size = 12, hjust = 0.5),  axis.text = element_text(size = 12),    axis.title = element_text(size=12),                                                legend.text = element_text(size = ),  legend.title = element_text(size = 10)) + ylim(-28,12) +xlab("Longitude")+ ylab("Latitude")
                                                                                                                                                                                                                                                        
plot_grid(richness_map_mam_FD, circle_map_mam_FD, ncol=2,align="hv")

FD <-richness_map_mam_FD + circle_map_mam_FD + mam_fd_density + richness_map_bird_FD +circle_map_bird_FD + bird_fd_density + layout

TD <- richness_map_mam_TD + circle_map_mam_TD + mam_td_density + richness_map_birds_TD +circle_map_birds_TD + bird_td_density + layout
