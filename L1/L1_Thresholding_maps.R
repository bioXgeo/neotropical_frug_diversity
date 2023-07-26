#thresholding maps

# Load the necessary libraries
library(raster)
library(rasterVis)
library(raster)
library(dplyr)
library(raster)
library(maps)
library(ggplot2)
library(tmap)
library(rnaturalearth)
library(leaflet)
library(ggsn)
library(rgeos)
library(BAMMtools)
library(viridis)
library(cowplot)
library(gridExtra)

# Load the raster files of mammal functional diversity, mammal species richness, bird functional diversity, and bird species richness
raster_fd_mammal <- raster("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/FD_mams_foraging_TA.tif")
raster_sr_mammal <- raster("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/Spec_rich_mam_TA.tif")
raster_fd_bird <- raster("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/FD_birds_foraging_TA.tif")
raster_sr_bird <- raster("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/Spec_rich_birds_TA.tif")

# MAP

# Pull in world map
worldMap <- ne_countries(scale = "medium", type = "countries", returnclass = "sf")

# Subset world map. In this case we are removing the Galapagos by defining the bounding box around the Ecuador polygon.
study_region <- worldMap %>% filter(continent == "South America" | continent== "North America")


# Crop the study region to the bounding box of interest
study_region_crop <-st_crop(study_region, xmin = -87, xmax = -61, ymin = -25, ymax = 13)


# Define the upper percentage values
upper_percentages <- c(30, 20, 10)  # Desired upper percentage values

# Create empty lists to store the panels for mammals and birds
panels_mammal <- list()
panels_bird <- list()
panels_joint_sum <-list()

#Set wd to where you want the rasters to save
output_dir <-"C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/richness_rasters_binary/"

 # Loop through each upper percentage value and create the panels for mammals
    for (percentage in upper_percentages) {
      # Calculate the threshold value based on the upper percentage
      threshold_fd <- quantile(values(raster_fd_mammal), probs = 1 - (percentage / 100), na.rm = TRUE)
      
      # Create a binary raster by thresholding mammal functional diversity
      binary_fd_mammal <- raster_fd_mammal
      binary_fd_mammal[binary_fd_mammal < threshold_fd] <- 0
      binary_fd_mammal[binary_fd_mammal >= threshold_fd] <- 1
      
      # Save the binary FD mammal raster
      binary_fd_mammal_file <- paste0(output_dir, "binary_fd_mammal_", percentage, ".tif")
      writeRaster(binary_fd_mammal, filename = binary_fd_mammal_file, format = "GTiff", overwrite = TRUE)
      
      
      threshold_sr <- quantile(values(raster_sr_mammal), probs = 1 - (percentage / 100), na.rm = TRUE)
      
      # Create a binary raster by thresholding mammal species richness
      binary_sr_mammal <- raster_sr_mammal
      binary_sr_mammal[binary_sr_mammal < threshold_sr] <- 0
      binary_sr_mammal[binary_sr_mammal >= threshold_sr] <- 1
      
      # Save the binary SR mammal raster
      binary_sr_mammal_file <- paste0(output_dir, "binary_sr_mammal_", percentage, ".tif")
      writeRaster(binary_sr_mammal, filename = binary_sr_mammal_file, format = "GTiff", overwrite = TRUE)
      
      # Sum them up
      diversity_mammal_sum <- binary_sr_mammal + binary_fd_mammal
      
      # Raster to points
      # Convert the raster data to data frames
      df_fd_mammal <- as.data.frame(rasterToPoints(binary_fd_mammal, spatial = TRUE))
      df_sr_mammal <- as.data.frame(rasterToPoints(binary_sr_mammal, spatial = TRUE))
      df_diversity_mammal <- as.data.frame(rasterToPoints(diversity_mammal_sum, spatial = TRUE))
      
      # Save the diversity mammal sum raster
      diversity_mammal_sum_file <- paste0(output_dir, "diversity_mammal_sum_", percentage, ".tif")
      writeRaster(diversity_mammal_sum, filename = diversity_mammal_sum_file, format = "GTiff", overwrite = TRUE)

      
      # Create a ggplot-based plot for each panel
      # Convert layer column to factor
      df_fd_mammal$FD_mams_foraging_TA <- as.factor(df_fd_mammal$FD_mams_foraging_TA)
      
      panel_fd_mammal <- ggplot() +
        theme_bw() +
        geom_sf(data = study_region_crop, fill = "white") +
        geom_raster(data = df_fd_mammal, aes(x = x, y = y, fill = FD_mams_foraging_TA)) +
        scale_fill_manual(  values = c("1" = "skyblue","0" = "darkblue"),
          labels = c("All FD areas", "FD above threshold"),
          guide = guide_legend(override.aes = list(shape = c(0, 1))),
          name = "Functional Diversity"
        ) + labs(title = paste("Mammals - Top", percentage, "% - Functional Diversity")) + ylab("Latitude") + xlab("Longitude") + scalebar(x.min = -83, x.max = -79.5, y.min =-25, y.max = -23,dist = 200, st.dist=.3, st.size=2.5, height=.4, transform = TRUE, dist_unit = "km", model = 'WGS84') +north(study_region_crop,  location="bottomleft", scale=.065, symbol=1) +   theme(panel.background = element_rect(fill = "aliceblue"),  axis.title.x = element_blank(),  axis.ticks.x = element_blank(), axis.text.x = element_blank(), legend.key.size = unit(.7, 'cm'), legend.justification = c(1, 0), legend.position = c(.50, .14), legend.box.background = element_rect(fill = "white"),plot.title = element_text(size=18, hjust = 0.5), axis.text = element_text(size = 16), legend.text = element_text(size = 16), legend.title = element_text(size=16))
      
      # Convert layer column to factor
      df_sr_mammal$Spec_rich_mam_TA<- as.factor(df_sr_mammal$Spec_rich_mam_TA)
      
      
      panel_sr_mammal <- ggplot() +
        theme_bw() +
        geom_sf(data = study_region_crop, fill = "white") +
        geom_raster(data = df_sr_mammal, aes(x = x, y = y, fill = Spec_rich_mam_TA)) +
        scale_fill_manual(
          values = c("1" = "orchid1","0" = "mediumorchid4"),
          labels = c("All TD areas", "TD above threshold"),
          guide = guide_legend(override.aes = list(shape = c(0, 1))),
          name = "Taxonomic Diversity"
        ) + labs(title = paste("Top ", percentage, "% - Taxonomic Diversity")) + ylab("Latitude") + xlab("Longitude") + scalebar(x.min = -83, x.max = -79.5, y.min =-25, y.max = -23,dist = 200, st.dist=.3, st.size=2.5, height=.4, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop,  location="bottomleft", scale=.065, symbol=1) +   theme(panel.background = element_rect(fill = "aliceblue"),  axis.title.x = element_blank(),  axis.ticks.x = element_blank(),axis.text.x = element_blank(), axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank(), legend.key.size = unit(.7, 'cm'), legend.justification = c(1, 0), legend.position = c(.50, .14), legend.box.background = element_rect(fill = "white"),plot.title = element_text(size=18, hjust = 0.5), axis.text = element_text(size = 16), legend.text = element_text(size = 16), legend.title = element_text(size=16))
      
      df_diversity_mammal$layer<- as.factor(df_diversity_mammal$layer)
      
      panel_diversity_mammal <-  ggplot() +
        theme_bw() +
        geom_sf(data = study_region_crop, fill = "white") +
        geom_raster(data = df_diversity_mammal, aes(x = x, y = y, fill = layer)) +
        scale_fill_manual(
          values = c("2" = "seagreen1", "1"= "darkgreen","0" = "black"),
          labels = c("All areas", "TD or FD above threshold","TD and FD overlap at threshold"),
          guide = guide_legend(override.aes = list(shape = c(0, 1,2)), size=14),
          name = "Mammal Diversity"
        ) + labs(title = paste("Top ", percentage, "% - Taxonomic and Functional Diversity")) + ylab("Latitude") + xlab("Longitude") + scalebar(x.min = -83, x.max = -79.5, y.min =-25, y.max = -23,dist = 200, st.dist=.3, st.size=2.5, height=.4, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop,  location="bottomleft", scale=.065, symbol=1) +   theme(panel.background = element_rect(fill = "aliceblue"), legend.key.size = unit(.3, 'cm'), axis.title.x = element_blank(),  axis.ticks.x = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank(), axis.title.y = element_blank(),  axis.ticks.y = element_blank(), legend.justification = c(1, 0), legend.position = c(.54, .14), legend.box.background = element_rect(fill = "white"),plot.title = element_text(size=18, hjust = 0.5), axis.text = element_text(size = 16), legend.text = element_text(size = 16), legend.title = element_text(size=16))
      
    
      # Add the panels to the list
      panels_mammal[[as.character(percentage)]] <- list(
        panel_fd_mammal,
        panel_sr_mammal,
        panel_diversity_mammal
      )
    }
  
## Birds
# Loop through each upper percentage value and create the panels for birdmals
for (percentage in upper_percentages) {
  # Calculate the threshold value based on the upper percentage
  threshold_fd <- quantile(values(raster_fd_bird), probs = 1 - (percentage / 100), na.rm = TRUE)
  
  # Create a binary raster by thresholding bird functional diversity
  binary_fd_bird <- raster_fd_bird
  binary_fd_bird[binary_fd_bird < threshold_fd] <- 0
  binary_fd_bird[binary_fd_bird >= threshold_fd] <- 1
  
  
  # Save the binary FD mammal raster
  binary_fd_bird_file <- paste0(output_dir, "binary_fd_bird_", percentage, ".tif")
  writeRaster(binary_fd_bird, filename = binary_fd_bird_file, format = "GTiff", overwrite = TRUE)
  
  threshold_sr <- quantile(values(raster_sr_bird), probs = 1 - (percentage / 100), na.rm = TRUE)
  
  # Create a binary raster by thresholding bird species richness
  binary_sr_bird <- raster_sr_bird
  binary_sr_bird[binary_sr_bird < threshold_sr] <- 0
  binary_sr_bird[binary_sr_bird >= threshold_sr] <- 1
  
  # Save the binary SR bird raster
  binary_sr_bird_file <- paste0(output_dir, "binary_sr_bird_", percentage, ".tif")
  writeRaster(binary_sr_bird, filename = binary_sr_bird_file, format = "GTiff", overwrite = TRUE)
  
  # Sum them up
  diversity_bird_sum <- binary_sr_bird + binary_fd_bird
  
  # Save the diversity mammal sum raster
  diversity_bird_sum_file <- paste0(output_dir, "diversity_bird_sum_", percentage, ".tif")
  writeRaster(diversity_bird_sum, filename = diversity_bird_sum_file, format = "GTiff", overwrite = TRUE)
  
  
  # Raster to points
  # Convert the raster data to data frames
  df_fd_bird <- as.data.frame(rasterToPoints(binary_fd_bird, spatial = TRUE))
  df_sr_bird <- as.data.frame(rasterToPoints(binary_sr_bird, spatial = TRUE))
  df_diversity_bird <- as.data.frame(rasterToPoints(diversity_bird_sum, spatial = TRUE))
  
  # Create a ggplot-based plot for each panel
  # Convert layer column to factor
  df_fd_bird$FD_birds_foraging_TA <- as.factor(df_fd_bird$FD_birds_foraging_TA)
  
  panel_fd_bird <- ggplot() +
    theme_bw() +
    geom_sf(data = study_region_crop, fill = "white") +
    geom_raster(data = df_fd_bird, aes(x = x, y = y, fill = FD_birds_foraging_TA)) +
    scale_fill_manual(
      values = c("1" = "skyblue","0" = "darkblue"),
      labels = c("All FD areas", "FD above threshold"),
      guide = guide_legend(override.aes = list(shape = c(0, 1))),
      name = "Functional Diversity"
    ) + labs(title = paste("Birds - Top ", percentage, "% - Functional Diversity")) + ylab("Latitude") + xlab("Longitude") + scalebar(x.min = -83, x.max = -79.5, y.min =-25, y.max = -23,dist = 200, st.dist=.3, st.size=2.5, height=.4, transform = TRUE, dist_unit = "km", model = 'WGS84') +north(study_region_crop,  location="bottomleft", scale=.065, symbol=1) +   theme(panel.background = element_rect(fill = "aliceblue"),  axis.title.x = element_blank(),  axis.ticks.x = element_blank(), axis.text.x = element_blank(), legend.key.size = unit(.7, 'cm'), legend.justification = c(1, 0), legend.position = c(.50, .14), legend.box.background = element_rect(fill = "white"),plot.title = element_text(size=18, hjust = 0.5), axis.text = element_text(size = 16), legend.text = element_text(size = 16), legend.title = element_text(size=16))
  
  
  # Convert layer column to factor
  df_sr_bird$Spec_rich_birds_TA<- as.factor(df_sr_bird$Spec_rich_birds_TA)
  
  
  panel_sr_bird <- ggplot() +
    theme_bw() +
    geom_sf(data = study_region_crop, fill = "white") +
    geom_raster(data = df_sr_bird, aes(x = x, y = y, fill = Spec_rich_birds_TA)) +
    scale_fill_manual(
      values = c("1" = "orchid1","0" = "mediumorchid4"),
      labels = c("All TD areas", "TD above threshold"),
      guide = guide_legend(override.aes = list(shape = c(0, 1))),
      name = "Taxonomic Diversity"
    ) + labs(title = paste("Top ", percentage, "% - Taxonomic Diversity")) + ylab("Latitude") + xlab("Longitude") + scalebar(x.min = -83, x.max = -79.5, y.min =-25, y.max = -23,dist = 200, st.dist=.3, st.size=2.5, height=.4, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop,  location="bottomleft", scale=.065, symbol=1) +   theme(panel.background = element_rect(fill = "aliceblue"),  axis.title.x = element_blank(),  axis.ticks.x = element_blank(),axis.text.x = element_blank(), axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank(), legend.key.size = unit(.7, 'cm'), legend.justification = c(1, 0), legend.position = c(.50, .14), legend.box.background = element_rect(fill = "white"),plot.title = element_text(size=18, hjust = 0.5), axis.text = element_text(size = 16), legend.text = element_text(size = 16), legend.title = element_text(size=16))
  
  
  df_diversity_bird$layer<- as.factor(df_diversity_bird$layer)
  
  panel_diversity_bird <-  ggplot() +
    theme_bw() +
    geom_sf(data = study_region_crop, fill = "white") +
    geom_raster(data = df_diversity_bird, aes(x = x, y = y, fill = layer)) +
    scale_fill_manual(
      values = c("2" = "seagreen1", "1"= "darkgreen","0" = "black"),
      labels = c("All areas", "TD or FD above threshold","TD and FD overlap at threshold"),
      guide = guide_legend(override.aes = list(shape = c(0, 1,2))),
      name = "Bird Diversity"
    ) + labs(title = paste("Top ", percentage, "% - Taxonomic and Functional Diversity")) + ylab("Latitude") + xlab("Longitude") + scalebar(x.min = -83, x.max = -79.5, y.min =-25, y.max = -23,dist = 200, st.dist=.3, st.size=2.5, height=.4, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop,  location="bottomleft", scale=.065, symbol=1) +   theme(panel.background = element_rect(fill = "aliceblue"), legend.key.size = unit(.3, 'cm'), axis.title.x = element_blank(),  axis.ticks.x = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank(), axis.title.y = element_blank(),  axis.ticks.y = element_blank(), legend.justification = c(1, 0), legend.position = c(.54, .14), legend.box.background = element_rect(fill = "white"),plot.title = element_text(size=18, hjust = 0.5), axis.text = element_text(size = 16), legend.text = element_text(size = 16), legend.title = element_text(size=16))
  
  # Add the panels to the list
  panels_bird[[as.character(percentage)]] <- list(
    panel_fd_bird,
    panel_sr_bird,
    panel_diversity_bird
  )
}


#Joint addition instead of modeling togetherfor (percentage in upper_percentages) {
# Loop through each upper percentage value and create the panels for joint with addition (not modeled)
for (percentage in upper_percentages) {
# Create a binary raster by thresholding joint functional diversity
raster_fd_joint <- (raster_fd_bird/2) + (raster_fd_mammal/2)

# Calculate the threshold value based on the upper percentage
threshold_fd <- quantile(values(raster_fd_joint), probs = 1 - (percentage / 100), na.rm = TRUE)

binary_fd_joint <- raster_fd_joint
binary_fd_joint[binary_fd_joint < threshold_fd] <- 0
binary_fd_joint[binary_fd_joint >= threshold_fd] <- 1


# Save the binary FD mammal raster
binary_fd_joint_file <- paste0(output_dir, "binary_fd_joint_", percentage, ".tif")
writeRaster(binary_fd_joint, filename = binary_fd_joint_file, format = "GTiff", overwrite = TRUE)

raster_sr_joint <- raster_sr_bird + raster_sr_mammal
threshold_sr <- quantile(values(raster_sr_joint), probs = 1 - (percentage / 100), na.rm = TRUE)

# Create a binary raster by thresholding joint species richness
binary_sr_joint <- raster_sr_joint
binary_sr_joint[binary_sr_joint < threshold_sr] <- 0
binary_sr_joint[binary_sr_joint >= threshold_sr] <- 1

# Save the binary SR mammal raster
binary_sr_joint_file <- paste0(output_dir, "binary_sr_joint_", percentage, ".tif")
writeRaster(binary_sr_joint, filename = binary_sr_joint_file, format = "GTiff", overwrite = TRUE)

# Sum them up
diversity_joint_sum <- binary_sr_joint + binary_fd_joint

# Save the diversity joint sum raster
diversity_joint_sum_file <- paste0(output_dir, "diversity_joint_sum_", percentage, ".tif")
writeRaster(diversity_joint_sum, filename = diversity_joint_sum_file, format = "GTiff", overwrite = TRUE)


# Raster to points
# Convert the raster data to data frames
df_fd_joint <- as.data.frame(rasterToPoints(binary_fd_joint, spatial = TRUE))
df_sr_joint <- as.data.frame(rasterToPoints(binary_sr_joint, spatial = TRUE))
df_diversity_joint <- as.data.frame(rasterToPoints(diversity_joint_sum, spatial = TRUE))

# Create a ggplot-based plot for each panel
# Convert layer column to factor
df_fd_joint$layer <- as.factor(df_fd_joint$layer)

panel_fd_joint <- ggplot() +
  theme_bw() +
  geom_sf(data = study_region_crop, fill = "white") +
  geom_raster(data = df_fd_joint, aes(x = x, y = y, fill = layer)) +
  scale_fill_manual(
    values = c("1" = "skyblue","0" = "darkblue"),
    labels = c("All FD areas", "FD above threshold"),
    guide = guide_legend(override.aes = list(shape = c(0, 1))),
    name = "Functional Diversity"
  ) + labs(title = paste("Frugivores - Top ", percentage, "% - Functional Diversity")) + ylab("Latitude") + xlab("Longitude") + scalebar(x.min = -83, x.max = -79.5, y.min =-25, y.max = -23,dist = 200, st.dist=.3, st.size=2.5, height=.4, transform = TRUE, dist_unit = "km", model = 'WGS84') +north(study_region_crop,  location="bottomleft", scale=.065, symbol=1) +   theme(panel.background = element_rect(fill = "aliceblue"),  axis.title.x = element_blank(),  axis.ticks.x = element_blank(), axis.text.x = element_blank(), legend.key.size = unit(.7, 'cm'), legend.justification = c(1, 0), legend.position = c(.50, .14), legend.box.background = element_rect(fill = "white"),plot.title = element_text(size=18, hjust = 0.5), axis.text = element_text(size = 16), legend.text = element_text(size = 16), legend.title = element_text(size=16))


# Convert layer column to factor
df_sr_joint$layer<- as.factor(df_sr_joint$layer)


panel_sr_joint <- ggplot() +
  theme_bw() +
  geom_sf(data = study_region_crop, fill = "white") +
  geom_raster(data = df_sr_joint, aes(x = x, y = y, fill =  layer)) +
  scale_fill_manual(
    values = c("1" = "orchid1","0" = "mediumorchid4"),
    labels = c("All TD areas", "TD above threshold"),
    guide = guide_legend(override.aes = list(shape = c(0, 1))),
    name = "Taxonomic Diversity"
  ) + labs(title = paste("Top ", percentage, "% - Taxonomic Diversity")) + ylab("Latitude") + xlab("Longitude") + scalebar(x.min = -83, x.max = -79.5, y.min =-25, y.max = -23,dist = 200, st.dist=.3, st.size=2.5, height=.4, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop,  location="bottomleft", scale=.065, symbol=1) +   theme(panel.background = element_rect(fill = "aliceblue"),  axis.title.x = element_blank(),  axis.ticks.x = element_blank(),axis.text.x = element_blank(), axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank(), legend.key.size = unit(.7, 'cm'), legend.justification = c(1, 0), legend.position = c(.50, .14), legend.box.background = element_rect(fill = "white"),plot.title = element_text(size=18, hjust = 0.5), axis.text = element_text(size = 16), legend.text = element_text(size = 16), legend.title = element_text(size=16))


df_diversity_joint$layer<- as.factor(df_diversity_joint$layer)

panel_diversity_joint <-  ggplot() +
  theme_bw() +
  geom_sf(data = study_region_crop, fill = "white") +
  geom_raster(data = df_diversity_joint, aes(x = x, y = y, fill = layer)) +
  scale_fill_manual(
    values = c("2" = "seagreen1", "1"= "darkgreen","0" = "black"),
    labels = c("All areas", "TD or FD above threshold","TD and FD overlap at threshold"),
    guide = guide_legend(override.aes = list(shape = c(0, 1,2))),
    name = "Frugivore Diversity"
  ) + labs(title = paste("Top ", percentage, "% - Taxonomic and Functional Diversity")) + ylab("Latitude") + xlab("Longitude") + scalebar(x.min = -83, x.max = -79.5, y.min =-25, y.max = -23,dist = 200, st.dist=.3, st.size=2.5, height=.4, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop,  location="bottomleft", scale=.065, symbol=1) +   theme(panel.background = element_rect(fill = "aliceblue"), legend.key.size = unit(.3, 'cm'), axis.title.x = element_blank(),  axis.ticks.x = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank(), axis.title.y = element_blank(),  axis.ticks.y = element_blank(), legend.justification = c(1, 0), legend.position = c(.54, .14), legend.box.background = element_rect(fill = "white"),plot.title = element_text(size=18, hjust = 0.5), axis.text = element_text(size = 16), legend.text = element_text(size = 16), legend.title = element_text(size=16))
# Add the panels to the list
panels_joint_sum[[as.character(percentage)]] <- list(
  panel_fd_joint,
  panel_sr_joint,
  panel_diversity_joint
)
}

## JOINT overlap of birds and mammals (already thresholded)
#Joint addition instead of modeling togetherfor (percentage in upper_percentages) {
# Loop through each upper percentage value and create the panels for joint with addition (not modeled)
  # Set the directory where your rasters are located
  raster_directory <- "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/richness_rasters_binary/joint_rasters/"
  
  # Define the percentage thresholds you want to loop through
  upper_percentages <- c("10", "20", "30")
  
  # Loop through each percentage threshold
  for (percentage in upper_percentages) {
    # Create the file pattern to match rasters with the specific percentage
    pattern <- paste0("_", percentage, ".tif$")  # Assuming your raster files have the ".tif" extension
    
    # List files in the directory that match the pattern
    raster_files <- list.files(path = raster_directory, pattern = pattern, full.names = TRUE)
  
  # Raster to points
  # Convert the raster data to data frames
  df_fd_joint <- as.data.frame(rasterToPoints(raster(raster_files[1]), spatial = TRUE))
  df_sr_joint <- as.data.frame(rasterToPoints(raster(raster_files[2]), spatial = TRUE))
  df_diversity_joint <- as.data.frame(rasterToPoints(raster(raster_files[3]), spatial = TRUE))
  
  # Create a ggplot-based plot for each panel
  # Convert layer column to factor
  col_name <- paste0("binary_fd_joint_", percentage)
  df_fd_joint[[col_name]] <- as.factor(df_fd_joint[[col_name]])
  
  
  panel_fd_joint <- ggplot() +
    theme_bw() +
    geom_sf(data = study_region_crop, fill = "white") +
    geom_raster(data = df_fd_joint, aes(x = x, y = y, fill = !!sym(col_name))) +
    scale_fill_manual(
      values = c("1" = "cadetblue","0" = "darkblue", "2"="skyblue"),
      labels = c("All areas", "FD above threshold for (m) or (b)","FD above threshold for both"),
      guide = guide_legend(override.aes = list(shape = c(0,1, 2))),
      name = "Functional Diversity"
    ) + labs(title = paste("Frugivores - Top ", percentage, "% - Functional Diversity")) + ylab("Latitude") + xlab("Longitude") + scalebar(x.min = -83, x.max = -79.5, y.min =-25, y.max = -23,dist = 200, st.dist=.3, st.size=2.5, height=.4, transform = TRUE, dist_unit = "km", model = 'WGS84') +north(study_region_crop,  location="bottomleft", scale=.065, symbol=1) +   theme(panel.background = element_rect(fill = "aliceblue"),  axis.title.x = element_blank(),  axis.ticks.x = element_blank(), axis.text.x = element_blank(), legend.key.size = unit(.7, 'cm'), legend.justification = c(1, 0), legend.position = c(.50, .14), legend.box.background = element_rect(fill = "white"),plot.title = element_text(size=18, hjust = 0.5), axis.text = element_text(size = 16), legend.text = element_text(size = 16), legend.title = element_text(size=16))
  
  
  # Convert layer column to factor
  col_name <- paste0("binary_sr_joint_", percentage)
  df_sr_joint[[col_name]] <- as.factor(df_sr_joint[[col_name]])
  
  
  panel_sr_joint <- ggplot() +
    theme_bw() +
    geom_sf(data = study_region_crop, fill = "white") +
    geom_raster(data = df_sr_joint, aes(x = x, y = y, fill =  !!sym(col_name))) +
    scale_fill_manual(
      values = c("1" = "orchid1","0" = "mediumorchid4", "2"= "lightpink"),
      labels = c("All areas", "TD above thresh for (m) or (b)", "TD above thresh for both"),
      guide = guide_legend(override.aes = list(shape = c(0, 1, 2))),
      name = "Taxonomic Diversity"
    ) + labs(title = paste("Top ", percentage, "% - Taxonomic Diversity")) + ylab("Latitude") + xlab("Longitude") + scalebar(x.min = -83, x.max = -79.5, y.min =-25, y.max = -23,dist = 200, st.dist=.3, st.size=2.5, height=.4, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop,  location="bottomleft", scale=.065, symbol=1) +   theme(panel.background = element_rect(fill = "aliceblue"),  axis.title.x = element_blank(),  axis.ticks.x = element_blank(),axis.text.x = element_blank(), axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank(), legend.key.size = unit(.7, 'cm'), legend.justification = c(1, 0), legend.position = c(.50, .14), legend.box.background = element_rect(fill = "white"),plot.title = element_text(size=18, hjust = 0.5), axis.text = element_text(size = 16), legend.text = element_text(size = 16), legend.title = element_text(size=16))
   
  col_name <- paste0("diversity_joint_sum_", percentage)
  df_diversity_joint[[col_name]] <- as.factor(df_diversity_joint[[col_name]])
  
  panel_diversity_joint <-  ggplot() +
    theme_bw() +
    geom_sf(data = study_region_crop, fill = "white") +
    geom_raster(data = df_diversity_joint, aes(x = x, y = y, fill = !!sym(col_name))) +
    scale_fill_manual(
      values = c("2" = "seagreen1", "1"= "darkgreen","0" = "black"),
      labels = c("All areas", "TD or FD above threshold for (m) or (b)","TD and FD overlap at thresh for both"),
      guide = guide_legend(override.aes = list(shape = c(0, 1,2))),
      name = "Frugivore Diversity"
    ) + labs(title = paste("Top ", percentage, "% - Taxonomic and Functional Diversity")) + ylab("Latitude") + xlab("Longitude") + scalebar(x.min = -83, x.max = -79.5, y.min =-25, y.max = -23,dist = 200, st.dist=.3, st.size=2.5, height=.4, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop,  location="bottomleft", scale=.065, symbol=1) +   theme(panel.background = element_rect(fill = "aliceblue"), legend.key.size = unit(.3, 'cm'), axis.title.x = element_blank(),  axis.ticks.x = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank(), axis.title.y = element_blank(),  axis.ticks.y = element_blank(), legend.justification = c(1, 0), legend.position = c(.54, .14), legend.box.background = element_rect(fill = "white"),plot.title = element_text(size=18, hjust = 0.5), axis.text = element_text(size = 16), legend.text = element_text(size = 16), legend.title = element_text(size=16))
  
 
   # Add the panels to the list
  panels_joint_sum[[as.character(percentage)]] <- list(
    panel_fd_joint,
    panel_sr_joint,
    panel_diversity_joint
  )
  }
  
  
  #had to seperately fix the 10% overlap because there is no value of 2 (replaced in the list). Changed the percentage value to the first in the percentage list and inserted that into the list after the fact
 panel_diversity_joint_fixed <-  ggplot() +
  theme_bw() +
   geom_sf(data = study_region_crop, fill = "white") +
  geom_raster(data = df_diversity_joint, aes(x = x, y = y, fill = !!sym(col_name))) +
   scale_fill_manual(
     values = c("1"= "darkgreen","0" = "black"),
    labels = c("All areas", "TD or FD above threshold for (m) or (b)","TD and FD overlap at thresh for both"),
     guide = guide_legend(override.aes = list(shape = c(0, 1))),
     name = "Frugivore Diversity"
 ) + labs(title = paste("Top ", upper_percentages[1], "% - Taxonomic and Functional Diversity")) + ylab("Latitude") + xlab("Longitude") + scalebar(x.min = -83, x.max = -79.5, y.min =-25, y.max = -23,dist = 200, st.dist=.3, st.size=2.5, height=.4, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop,  location="bottomleft", scale=.065, symbol=1) +   theme(panel.background = element_rect(fill = "aliceblue"), legend.key.size = unit(.3, 'cm'), axis.title.x = element_blank(),  axis.ticks.x = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank(), axis.title.y = element_blank(),  axis.ticks.y = element_blank(), legend.justification = c(1, 0), legend.position = c(.54, .14), legend.box.background = element_rect(fill = "white"),plot.title = element_text(size=18, hjust = 0.5), axis.text = element_text(size = 16), legend.text = element_text(size = 16), legend.title = element_text(size=16))

 panels_joint_sum[["10"]][[3]] <- panel_diversity_joint_fixed
 
library(gridExtra)
library(cowplot)


#Mammal plots
# Arrange the grobs using grid.arrange
plot_mammal_3 <- do.call(plot_grid, c(panels_mammal[['30']], ncol = 3,align=c("v")))
plot_mammal_2 <- do.call(plot_grid, c(panels_mammal[['20']], ncol = 3,align=c("v")))
plot_mammal_1 <- do.call(plot_grid, c(panels_mammal[['10']], ncol = 3,align=c("v")))
#mammal plot
mammal_plot <-plot_grid(plot_mammal_3, plot_mammal_2, plot_mammal_1, nrow=3, align=c("hv"),  labels = "AUTO" )


#bird plots
# Create the plots for mammals and birds
plot_bird_3 <- do.call(plot_grid, c(panels_bird[['30']], ncol = 3,align=c("v")))
plot_bird_2 <- do.call(plot_grid, c(panels_bird[['20']], ncol = 3,align=c("v")))
plot_bird_1 <- do.call(plot_grid, c(panels_bird[['10']], ncol = 3,align=c("v")))
bird_plot <-plot_grid(plot_bird_3, plot_bird_2, plot_bird_1, nrow=3, align=c("hv"),  labels = "AUTO" )
(plot_joint_3, plot_joint_2, plot_joint_1, nrow=3, align=c("hv"),  labels = "AUTO" )

#frugivores plots with addition
# Create the plots for mammals and birds
plot_sum_3 <- do.call(plot_grid, c(panels_joint_sum[['30']], ncol = 3,align=c("v")))
plot_sum_2 <- do.call(plot_grid, c(panels_joint_sum[['20']], ncol = 3,align=c("v")))
plot_sum_1 <- do.call(plot_grid, c(panels_joint_sum[['10']], ncol = 3,align=c("v")))
frugivore_plot <-plot_grid(plot_sum_3, plot_sum_2, plot_sum_1, nrow=3, align=c("hv"),  labels = "AUTO" )

