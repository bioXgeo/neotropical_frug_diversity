# Title: Thresholding and Visualizing Functional and Taxonomic Diversity Maps
# Project: Evaluating the Effectiveness of Protected Areas and Community-Managed Lands 
#          in Capturing Multiple Dimensions of Frugivorous Biodiversity in the Tropical Andes
# Author: Beth E. Gerstner
# Collaborators: Phoebe L. Zarnetske
# Overview: This script thresholds functional and taxonomic diversity maps for mammals, birds, 
#           and their combined metrics. Outputs include binary rasters and visualization panels 
#           for diversity metrics across top percentages.
# Data Output: Binary rasters for top percentages and associated diversity panels.
# Date: August 3rd, 2023

# Load required libraries
library(raster)
library(dplyr)
library(sf)
library(ggplot2)
library(ggsn)
library(viridis)
library(cowplot)
library(gridExtra)
library(rnaturalearth)

# Define file paths
output_dir <- "PLACEHOLDER_PATH/richness_rasters_binary/"
raster_directory <- "PLACEHOLDER_PATH/richness_rasters_binary/joint_rasters/"

# Load raster data
raster_fd_mammal <- raster("PLACEHOLDER_PATH/FD_mams_foraging_TA.tif")
raster_sr_mammal <- raster("PLACEHOLDER_PATH/Spec_rich_mam_TA.tif")
raster_fd_bird <- raster("PLACEHOLDER_PATH/FD_birds_foraging_TA.tif")
raster_sr_bird <- raster("PLACEHOLDER_PATH/Spec_rich_birds_TA.tif")

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
output_dir <-"PLACEHOLDER_PATH/richness_rasters_binary/"

# Loop through each upper percentage value and create the panels for mammals
for (percentage in upper_percentages) {
  # Calculate the threshold value based on the upper percentage
  threshold_fd <- quantile(values(raster_fd_mammal), probs = 1 - (percentage / 100), na.rm = TRUE)
  
  # Create a binary raster by thresholding mammal functional diversity
  binary_fd_mammal <- raster_fd_mammal
  binary_fd_mammal[binary_fd_mammal < threshold_fd] <- 0
  binary_fd_mammal[binary_fd_mammal >= threshold_fd] <- 1
  
  # Save the binary FD mammal raster
  # binary_fd_mammal_file <- paste0(output_dir, "binary_fd_mammal_", percentage, ".tif")
  # writeRaster(binary_fd_mammal, filename = binary_fd_mammal_file, format = "GTiff", overwrite = TRUE)
  
  
  threshold_sr <- quantile(values(raster_sr_mammal), probs = 1 - (percentage / 100), na.rm = TRUE)
  
  # Create a binary raster by thresholding mammal species richness
  binary_sr_mammal <- raster_sr_mammal
  binary_sr_mammal[binary_sr_mammal < threshold_sr] <- 0
  binary_sr_mammal[binary_sr_mammal >= threshold_sr] <- 1
  
  # Save the binary SR mammal raster
  #   binary_sr_mammal_file <- paste0(output_dir, "binary_sr_mammal_", percentage, ".tif")
  #   writeRaster(binary_sr_mammal, filename = binary_sr_mammal_file, format = "GTiff", overwrite = TRUE)
  
  # Sum them up
  diversity_mammal_sum <- binary_sr_mammal + binary_fd_mammal
  
  # Raster to points
  # Convert the raster data to data frames
  df_fd_mammal <- as.data.frame(rasterToPoints(binary_fd_mammal, spatial = TRUE))
  df_sr_mammal <- as.data.frame(rasterToPoints(binary_sr_mammal, spatial = TRUE))
  df_diversity_mammal <- as.data.frame(rasterToPoints(diversity_mammal_sum, spatial = TRUE))
  
  
  # Create a ggplot-based plot for each panel
  # Convert layer column to factor
  df_fd_mammal$FD_mams_foraging_TA <- as.factor(df_fd_mammal$FD_mams_foraging_TA)
  
  panel_fd_mammal <- ggplot() +
    theme_bw() +
    geom_sf(data = study_region_crop, fill = "white") +
    geom_raster(data = df_fd_mammal, aes(x = x, y = y, fill = FD_mams_foraging_TA)) +
    scale_fill_manual(  values = c("1" = "darkblue","0" = "cyan4"),
                        labels = c("All FD areas", "FD above threshold"),
                        guide = guide_legend(override.aes = list(shape = c(0, 1))),
                        name = "Functional Diversity"
    ) + labs(title = paste("Mammals - Top", percentage, "% - Functional Diversity")) + ylab("Latitude") + xlab("Longitude") + scalebar(x.min = -83.5, x.max = -76.5, y.min = -24, y.max = -22.5, dist=400, st.dist=.3, st.size=3.5, height=.5, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop,  location="bottomleft", scale=.065, symbol=1) +   theme(panel.background = element_rect(fill = "aliceblue", color="black"), legend.key.size = unit(1, 'cm'), legend.key.width = unit(1.2, "cm"), legend.justification = c(1.8, 0), legend.position = c(.50, .20), legend.box.background = element_rect(fill = "white"),    plot.title = element_text(size = 20, hjust = 0.5),  axis.text = element_text(size = 20),    axis.title = element_text(size=20),                                                legend.text = element_text(size = 14 ),  legend.title = element_text(size = 14)) + ylim(-25,12) +xlab("Longitude")+ ylab("Latitude")
  # Convert layer column to factor
  df_sr_mammal$Spec_rich_mam_TA<- as.factor(df_sr_mammal$Spec_rich_mam_TA)
  
  
  panel_sr_mammal <- ggplot() +
    theme_bw() +
    geom_sf(data = study_region_crop, fill = "white") +
    geom_raster(data = df_sr_mammal, aes(x = x, y = y, fill = Spec_rich_mam_TA)) +
    scale_fill_manual(
      values = c("1" = "mediumorchid4","0" = "orchid1"),
      labels = c("All TD areas", "TD above threshold"),
      guide = guide_legend(override.aes = list(shape = c(0, 1))),
      name = "Taxonomic Diversity"
    ) + labs(title = paste("Top ", percentage, "% - Taxonomic Diversity")) + ylab("Latitude") + xlab("Longitude") + scalebar(x.min = -83.5, x.max = -76.5, y.min = -24, y.max = -22.5, dist=400, st.dist=.3, st.size=3.5, height=.5, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop,  location="bottomleft", scale=.065, symbol=1) +   theme(panel.background = element_rect(fill = "aliceblue", color="black"), legend.key.size = unit(1, 'cm'), legend.key.width = unit(1.2, "cm"), legend.justification = c(1.8, 0), legend.position = c(.50, .20), legend.box.background = element_rect(fill = "white"),    plot.title = element_text(size = 20, hjust = 0.5),  axis.text = element_text(size = 20),    axis.title = element_text(size=20),                                                legend.text = element_text(size = 14 ),  legend.title = element_text(size = 14)) + ylim(-25,12) +xlab("Longitude")+ ylab("Latitude")
  df_diversity_mammal$layer<- as.factor(df_diversity_mammal$layer)
  
  panel_diversity_mammal <-  ggplot() +
    theme_bw() +
    geom_sf(data = study_region_crop, fill = "white") +
    geom_raster(data = df_diversity_mammal, aes(x = x, y = y, fill = layer)) +
    scale_fill_manual(
      values = c("2" = "darkorange4", "1"= "darkorange","0" = "burlywood1"),
      labels = c("All areas", "TD or FD above threshold","TD and FD overlap at threshold"),
      guide = guide_legend(override.aes = list(shape = c(0, 1,2)), size=14),
      name = "Mammal Diversity"
    ) + labs(title = paste("Top ", percentage, "% - Taxonomic and Functional Diversity")) + ylab("Latitude") + xlab("Longitude") + scalebar(x.min = -83.5, x.max = -76.5, y.min = -24, y.max = -22.5, dist=400, st.dist=.3, st.size=3.5, height=.5, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop,  location="bottomleft", scale=.065, symbol=1) +   theme(panel.background = element_rect(fill = "aliceblue", color="black"), legend.key.size = unit(1, 'cm'), legend.key.width = unit(1.2, "cm"), legend.justification = c(1.3, 0), legend.position = c(.69, .15), legend.box.background = element_rect(fill = "white"),    plot.title = element_text(size = 24, hjust = 0.5),  axis.text = element_text(size = 20),    axis.title = element_text(size=24),                                                legend.text = element_text(size = 20 ),  legend.title = element_text(size = 20)) + ylim(-25,12) +xlab("Longitude")+ ylab("Latitude")
  
  # Add the panels to the list
  panels_mammal[[as.character(percentage)]] <- list(
    panel_fd_mammal,
    panel_sr_mammal,
    panel_diversity_mammal
  )
}

## Birds
# Loop through each upper percentage value and create the panels for birds
for (percentage in upper_percentages) {
  # Calculate the threshold value based on the upper percentage
  threshold_fd <- quantile(values(raster_fd_bird), probs = 1 - (percentage / 100), na.rm = TRUE)
  
  # Create a binary raster by thresholding bird functional diversity
  binary_fd_bird <- raster_fd_bird
  binary_fd_bird[binary_fd_bird < threshold_fd] <- 0
  binary_fd_bird[binary_fd_bird >= threshold_fd] <- 1
  
  
  # Save the binary FD mammal raster
  #  binary_fd_bird_file <- paste0(output_dir, "binary_fd_bird_", percentage, ".tif")
  # writeRaster(binary_fd_bird, filename = binary_fd_bird_file, format = "GTiff", overwrite = TRUE)
  
  threshold_sr <- quantile(values(raster_sr_bird), probs = 1 - (percentage / 100), na.rm = TRUE)
  
  # Create a binary raster by thresholding bird species richness
  binary_sr_bird <- raster_sr_bird
  binary_sr_bird[binary_sr_bird < threshold_sr] <- 0
  binary_sr_bird[binary_sr_bird >= threshold_sr] <- 1
  
  # Save the binary SR bird raster
  # binary_sr_bird_file <- paste0(output_dir, "binary_sr_bird_", percentage, ".tif")
  # writeRaster(binary_sr_bird, filename = binary_sr_bird_file, format = "GTiff", overwrite = TRUE)
  
  # Sum them up
  diversity_bird_sum <- binary_sr_bird + binary_fd_bird
  
  # Save the diversity mammal sum raster
  ##  diversity_bird_sum_file <- paste0(output_dir, "diversity_bird_sum_", percentage, ".tif")
  # writeRaster(diversity_bird_sum, filename = diversity_bird_sum_file, format = "GTiff", overwrite = TRUE)
  
  
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
      values = c("1" = "darkblue","0" = "cyan4"),
      labels = c("All FD areas", "FD above threshold"),
      guide = guide_legend(override.aes = list(shape = c(0, 1))),
      name = "Functional Diversity"
    ) + labs(title = paste("Birds - Top ", percentage, "% - Functional Diversity")) + ylab("Latitude") + xlab("Longitude") + scalebar(x.min = -83.5, x.max = -76.5, y.min = -24, y.max = -22.5, dist=400, st.dist=.3, st.size=3.5, height=.5, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop,  location="bottomleft", scale=.065, symbol=1) +   theme(panel.background = element_rect(fill = "aliceblue", color="black"), legend.key.size = unit(1, 'cm'), legend.key.width = unit(1.2, "cm"), legend.justification = c(1.8, 0), legend.position = c(.50, .20), legend.box.background = element_rect(fill = "white"),    plot.title = element_text(size = 20, hjust = 0.5),  axis.text = element_text(size = 20),    axis.title = element_text(size=20),                                                legend.text = element_text(size = 14 ),  legend.title = element_text(size = 14)) + ylim(-25,12) +xlab("Longitude")+ ylab("Latitude")
  
  
  # Convert layer column to factor
  df_sr_bird$Spec_rich_birds_TA<- as.factor(df_sr_bird$Spec_rich_birds_TA)
  
  
  panel_sr_bird <- ggplot() +
    theme_bw() +
    geom_sf(data = study_region_crop, fill = "white") +
    geom_raster(data = df_sr_bird, aes(x = x, y = y, fill = Spec_rich_birds_TA)) +
    scale_fill_manual(
      values = c("1" = "mediumorchid4","0" = "orchid1"),
      labels = c("All TD areas", "TD above threshold"),
      guide = guide_legend(override.aes = list(shape = c(0, 1))),
      name = "Taxonomic Diversity"
    ) + labs(title = paste("Top ", percentage, "% - Taxonomic Diversity")) + ylab("Latitude") + xlab("Longitude") + scalebar(x.min = -83.5, x.max = -76.5, y.min = -24, y.max = -22.5, dist=400, st.dist=.3, st.size=3.5, height=.5, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop,  location="bottomleft", scale=.065, symbol=1) +   theme(panel.background = element_rect(fill = "aliceblue", color="black"), legend.key.size = unit(1, 'cm'), legend.key.width = unit(1.2, "cm"), legend.justification = c(1.8, 0), legend.position = c(.50, .20), legend.box.background = element_rect(fill = "white"),    plot.title = element_text(size = 20, hjust = 0.5),  axis.text = element_text(size = 20),    axis.title = element_text(size=20),                                                legend.text = element_text(size = 14 ),  legend.title = element_text(size = 14)) + ylim(-25,12) +xlab("Longitude")+ ylab("Latitude")
  
  
  df_diversity_bird$layer<- as.factor(df_diversity_bird$layer)
  
  panel_diversity_bird <-  ggplot() +
    theme_bw() +
    geom_sf(data = study_region_crop, fill = "white") +
    geom_raster(data = df_diversity_bird, aes(x = x, y = y, fill = layer)) +
    scale_fill_manual(
      values = c("2" = "darkorange4", "1"= "darkorange","0" = "burlywood1"),
      labels = c("All areas", "TD or FD above threshold","TD and FD overlap at threshold"),
      guide = guide_legend(override.aes = list(shape = c(0, 1,2))),
      name = "Bird Diversity"
    ) + labs(title = paste("Top ", percentage, "% - Taxonomic and Functional Diversity")) + ylab("Latitude") + xlab("Longitude") + scalebar(x.min = -83.5, x.max = -76.5, y.min = -24, y.max = -22.5, dist=400, st.dist=.3, st.size=3.5, height=.5, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop,  location="bottomleft", scale=.065, symbol=1) +   theme(panel.background = element_rect(fill = "aliceblue", color="black"), legend.key.size = unit(1, 'cm'), legend.key.width = unit(1.2, "cm"), legend.justification = c(1.3, 0), legend.position = c(.69, .15), legend.box.background = element_rect(fill = "white"),    plot.title = element_text(size = 24, hjust = 0.5),  axis.text = element_text(size = 20),    axis.title = element_text(size=24),                                                legend.text = element_text(size = 20 ),  legend.title = element_text(size = 20)) + ylim(-25,12) +xlab("Longitude")+ ylab("Latitude")
  
  
  # Add the panels to the list
  panels_bird[[as.character(percentage)]] <- list(
    panel_fd_bird,
    panel_sr_bird,
    panel_diversity_bird
  )
}

# Define paths
base_directory <- "PLACEHOLDER_PATH" # Replace with your base directory
raster_directory <- file.path(base_directory, "richness_rasters_binary/joint_rasters/")
fixed_raster_10_path <- file.path(base_directory, "richness_rasters_binary/joint_rasters/diversity_joint_sum_10.tif")

# Define the percentage thresholds
upper_percentages <- c("10", "20", "30")

# Initialize a list to store plots
panels_joint_sum <- list()

# Loop through each percentage threshold
for (percentage in upper_percentages) {
  # List rasters for the current percentage
  pattern <- paste0("_", percentage, ".tif$")
  raster_files <- list.files(path = raster_directory, pattern = pattern, full.names = TRUE)
  
  # Load rasters and convert to data frames
  df_fd_joint <- as.data.frame(rasterToPoints(raster(raster_files[1]), spatial = TRUE))
  df_sr_joint <- as.data.frame(rasterToPoints(raster(raster_files[2]), spatial = TRUE))
  df_diversity_joint <- as.data.frame(rasterToPoints(raster(raster_files[3]), spatial = TRUE))
  
  # Create and store functional diversity plot
  panel_fd_joint <- ggplot() +
    theme_bw() +
    geom_sf(data = study_region_crop, fill = "white") +
    geom_raster(data = df_fd_joint, aes(x = x, y = y, fill = !!sym(paste0("binary_fd_joint_", percentage)))) +
    scale_fill_manual(
      values = c("1" = "lightblue", "0" = "cyan4", "2" = "darkblue"),
      labels = c("All areas", "FD above threshold (m or b)", "FD above threshold (both)"),
      guide = guide_legend(override.aes = list(shape = c(0, 1, 2))),
      name = "Functional Diversity"
    ) +
    labs(title = paste("Frugivores - Top", percentage, "% - Functional Diversity"), x = "Longitude", y = "Latitude") +
    theme_minimal()
  
  # Create and store taxonomic diversity plot
  panel_sr_joint <- ggplot() +
    theme_bw() +
    geom_sf(data = study_region_crop, fill = "white") +
    geom_raster(data = df_sr_joint, aes(x = x, y = y, fill = !!sym(paste0("binary_sr_joint_", percentage)))) +
    scale_fill_manual(
      values = c("1" = "hotpink", "0" = "rosybrown1", "2" = "mediumorchid4"),
      labels = c("All areas", "TD above threshold (m or b)", "TD above threshold (both)"),
      guide = guide_legend(override.aes = list(shape = c(0, 1, 2))),
      name = "Taxonomic Diversity"
    ) +
    labs(title = paste("Top", percentage, "% - Taxonomic Diversity"), x = "Longitude", y = "Latitude") +
    theme_minimal()
  
  # Create and store combined diversity plot
  panel_diversity_joint <- ggplot() +
    theme_bw() +
    geom_sf(data = study_region_crop, fill = "white") +
    geom_raster(data = df_diversity_joint, aes(x = x, y = y, fill = !!sym(paste0("diversity_joint_sum_", percentage)))) +
    scale_fill_manual(
      values = c("2" = "darkorange4", "1" = "darkorange", "0" = "burlywood1"),
      labels = c("All areas", "TD or FD (m or b)", "TD and FD overlap"),
      guide = guide_legend(override.aes = list(shape = c(0, 1, 2))),
      name = "Frugivore Diversity"
    ) +
    labs(title = paste("Top", percentage, "% - Combined Diversity"), x = "Longitude", y = "Latitude") +
    theme_minimal()
  
  # Add plots to the list
  panels_joint_sum[[percentage]] <- list(panel_fd_joint, panel_sr_joint, panel_diversity_joint)
}

# Handle the 10% overlap correction
fixed_joint_10 <- raster(fixed_raster_10_path)
fixed_joint_10_df <- as.data.frame(rasterToPoints(fixed_joint_10, spatial = TRUE))
fixed_joint_10_df$diversity_joint_sum_10 <- as.factor(fixed_joint_10_df$diversity_joint_sum_10)

panel_diversity_joint_fixed <- ggplot() +
  theme_bw() +
  geom_sf(data = study_region_crop, fill = "white") +
  geom_raster(data = fixed_joint_10_df, aes(x = x, y = y, fill = diversity_joint_sum_10)) +
  scale_fill_manual(
    values = c("1" = "darkorange", "0" = "burlywood1"),
    labels = c("TD or FD (m or b)", "TD and FD overlap"),
    guide = guide_legend(override.aes = list(shape = c(0, 1))),
    name = "Frugivore Diversity"
  ) +
  labs(title = "Top 10% - Combined Diversity", x = "Longitude", y = "Latitude") +
  theme_minimal()

panels_joint_sum[["10"]][[3]] <- panel_diversity_joint_fixed


# Mammal plots
plot_mammal_3 <- do.call(plot_grid, c(panels_mammal[['30']], ncol = 3,align=c("v")))
plot_mammal_2 <- do.call(plot_grid, c(panels_mammal[['20']], ncol = 3,align=c("v")))
plot_mammal_1 <- do.call(plot_grid, c(panels_mammal[['10']], ncol = 3,align=c("v")))
#mammal plot
mammal_plot <-plot_grid(plot_mammal_3, plot_mammal_1, nrow=2, align=c("hv"),  labels = "AUTO" )


# Bird plots
plot_bird_3 <- do.call(plot_grid, c(panels_bird[['30']], ncol = 3,align=c("v")))
plot_bird_2 <- do.call(plot_grid, c(panels_bird[['20']], ncol = 3,align=c("v")))
plot_bird_1 <- do.call(plot_grid, c(panels_bird[['10']], ncol = 3,align=c("v")))
bird_plot <-plot_grid(plot_bird_3, plot_bird_1, nrow=2, align=c("hv"),  labels = "AUTO" )
(plot_joint_3, plot_joint_1, nrow=2, align=c("hv"),  labels = "AUTO" )

# Frugivore plots 
plot_sum_3 <- do.call(plot_grid, c(panels_joint_sum[['30']], ncol = 3,align=c("v")))
plot_sum_2 <- do.call(plot_grid, c(panels_joint_sum[['20']], ncol = 3,align=c("v")))
plot_sum_1 <- do.call(plot_grid, c(panels_joint_sum[['10']], ncol = 3,align=c("v")))
frugivore_plot <-plot_grid(plot_sum_3, plot_sum_1, nrow=2, align=c("hv"),  labels = "AUTO" )

taxa_diversity_overlap <- plot_grid(panels_bird[["10"]][[3]], panels_mammal[["10"]][[3]],panels_joint_sum[["10"]][[3]], ncol=3, align=c("hv"))

