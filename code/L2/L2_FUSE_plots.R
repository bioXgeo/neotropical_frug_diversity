# Title: Lollipop and Bar Plot of FUSE Species and Forest Integrity in Protected Areas
# Project: Evaluating the Effectiveness of Protected Areas and Community-Managed Lands 
#          in Capturing Multiple Dimensions of Frugivorous Biodiversity in the Tropical Andes
# Author: Beth E. Gerstner
# Collaborators: Phoebe L. Zarnetske
# Overview: This script processes biodiversity metrics (FUSE, FD, TD) and forest integrity for protected areas 
#           in the Tropical Andes. It calculates averages and standard deviations for biodiversity metrics 
#           and forest integrity across IUCN protected area categories. The script also generates visualizations 
#           to compare biodiversity metrics and forest integrity scores by category.
# Outputs: 
#   - Combined statistics table with biodiversity metrics and forest integrity values
#   - Visualization of average FUSE species counts with error bars
#   - Visualization of forest integrity scores with error bars

# Date: September 1, 2023


# Load the necessary libraries
library(raster)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(patchwork)
library(exactextractr)

# Load Tropical Andes subsets (generated in L0_0)
frugivoria_mam_TA <- read.csv("PLACEHOLDER_PATH/frugivoria_TA_mammal_subset.csv")
frugivoria_bird_TA <- read.csv("PLACEHOLDER_PATH/frugivoria_TA_bird_subset.csv")

# Load FUSE species files 
FUSE_species_mam <- read.csv("PLACEHOLDER_PATH/mam_fuse_foraging.csv")
FUSE_species_bird <- read.csv("PLACEHOLDER_PATH/bird_fuse_foraging.csv")

# Identify montane-only species (habitat = 1)
endemic_mammals <- frugivoria_mam_TA %>%
  filter(habitat == "1") %>%
  pull(IUCN_species_name)

endemic_birds <- frugivoria_bird_TA %>%
  filter(habitat == "1") %>%
  pull(IUCN_species_name)

# Ensure `Tremarctos ornatus` and `Pauxi unicornis` are included/minor habitat correction in Frugivoria dataset
manual_additions_mammals <- c("Tremarctos ornatus")
manual_additions_birds <- c("Pauxi unicornis")

# Remove `Lagothrix lagothricha` from mammals
endemic_mammals <- endemic_mammals[!endemic_mammals %in% "Lagothrix lagothricha"]
endemic_mammals <- unique(c(endemic_mammals, manual_additions_mammals))
endemic_birds <- unique(c(endemic_birds, manual_additions_birds))

# Filter FUSE species for endemic species
mam_endemic_fuse <- FUSE_species_mam %>%
  filter(X %in% endemic_mammals) %>%
  arrange(desc(FUSE))

bird_endemic_fuse <- FUSE_species_bird %>%
  filter(X %in% endemic_birds) %>%
  arrange(desc(FUSE))

# Write endemic FUSE subsets to files
write.csv(mam_endemic_fuse, "mam_endemic_fuse.csv", row.names = FALSE)
write.csv(bird_endemic_fuse, "bird_endemic_fuse.csv", row.names = FALSE)


#Function to create the lollipop plot of endemic species
create_ggdotchart <- function(FUSE_species_file, IUCN_status_file) {
  library(ggplot2)
  library(ggpubr)
  
  # Read the FUSE_species data and sort it to get the top ten species
  FUSE_species <- read.csv(FUSE_species_file)
  FUSE_species_sort <- head(arrange(FUSE_species, desc(FUSE)), 10)
  
  # Rename the column "X" to "IUCN_species_name"
  colnames(FUSE_species_sort)[which(names(FUSE_species_sort) == "X")] <- "IUCN_species_name"
  
  # Make the "IUCN_species_name" column the first column in the data frame
  FUSE_species_sort <- cbind(IUCN_species_name = FUSE_species_sort$IUCN_species_name, FUSE_species_sort[-which(names(FUSE_species_sort) == "IUCN_species_name")])
  
  # Read the IUCN_status data
  IUCN_status <- read.csv(IUCN_status_file)
  
  # Merge FUSE_species_sort with IUCN_status by "IUCN_species_name"
  FUSE_species_status <- merge(FUSE_species_sort, IUCN_status, by = "IUCN_species_name")
  
  # Convert "IUCN_category" to factor (in case it's not already a factor)
  FUSE_species_status$IUCN_category <- as.factor(FUSE_species_status$IUCN_category)
  
  # Convert "IUCN_species_name" to factor
  FUSE_species_status$IUCN_species_name <- as.factor(FUSE_species_status$IUCN_species_name)
  
  # Define colors for threat status
  threat_colors <- c("EN" = "#00AFBB", "VU" = "#E7B800", "CR" = "#FC4E07", "LC" = "green", "NT" = "darkgray")
  
  # Create the ggdotchart
  dotchart <- ggdotchart(FUSE_species_status, x = "IUCN_species_name", y = "FUSE",
                         color = "IUCN_category",
                         palette = threat_colors,
                         sorting = "descending",
                         add = "segments",
                         rotate = TRUE,
                         add.params = list(color = "lightgray", size = 2),
                         dot.size = 9,                                 # Large dot size
                         label = round(FUSE_species_status$FUSE,2),             # Add mpg values as dot labels
                         font.label = list(color = "white", size = 9, 
                                           vjust = 0.5),               # Adjust label parameters
                         ggtheme = theme_pubr()   +                     # ggplot2 theme )+
                           theme(
                             panel.grid.major.y = element_blank(),
                             plot.margin = unit(c(.1, .1, .1, .1), "cm"),
                             text = element_text(size = 12),
                             legend.title = element_blank(),
                             legend.text = element_text(size = 12),
                             axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),  # Set angle and alignment of x-axis labels
                             axis.title.y = element_text(size = 16))) +  xlab("Species") + ylab("FUSE score")
  # Return the ggdotchart
  return(dotchart)
}

# Mammals

FUSE_species_file <- "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/tropical_subsets/mam_endemic_fuse.csv"
IUCN_status_file <- "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/IUCN_statuses.csv"
dotchart_mammals <- create_ggdotchart(FUSE_species_file, IUCN_status_file)
print(dotchart_mammals)

# Birds
FUSE_species_file <- "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/tropical_subsets/bird_endemic_fuse.csv"
dotchart_birds <- create_ggdotchart(FUSE_species_file, IUCN_status_file)
print(dotchart_birds)

plot_grid(dotchart_mammals, dotchart_birds, ncol=2, align=c("hv"))

#FUSE bar plots

# Initialize the final stats table
# Initialize the final stats table
combined_stats <- data.frame(
  Category = character(),
  X..parks.in.category = numeric(),
  Taxa = character(),
  Metric = character(),
  count_avg = numeric(),
  count_sd = numeric(),
  stringsAsFactors = FALSE
)

# Forest integrity statistics
forest_stats <- data.frame(
  Category = character(),
  X..parks.in.category = numeric(),
  Forest_integrity_avg = numeric(),
  Forest_integrity_sd = numeric(),
  stringsAsFactors = FALSE
)

# Define file paths
parks_reserves_path <- "C:/Users/bgers/Desktop/testing/WDPA/WDPA_merged_TA.shp"
tropical_andes_path <- "C:/Users/bgers/Desktop/testing/TA/Tropical_Andes_shape.shp"
forest_integrity_path <- "C:/Users/bgers/Desktop/testing/forest_integrity_TA.tif"

# Read shapefiles and forest integrity raster
parks_reserves <- st_read(parks_reserves_path)
tropical_andes <- st_read(tropical_andes_path)
forest_integrity_raster <- raster(forest_integrity_path)

# Define biodiversity metrics and their raster paths
metrics <- c("FUSE", "FD", "TD")
metric_paths <- list(
  FUSE = list(
    mammals = "C:/Users/bgers/Desktop/testing/FUSE/FUSE_species_masked_TA_mam_foraging.tif",
    birds = "C:/Users/bgers/Desktop/testing/FUSE/FUSE_species_masked_TA_birds_foraging.tif"
  ),
  FD = list(
    mammals = "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/FD_mams_foraging_TA.tif",
    birds = "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/FD_birds_foraging_TA.tif"
  ),
  TD = list(
    mammals = "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/Spec_rich_mam_TA.tif",
    birds = "C:/Users/bgers/Desktop/testing/FUSE/FUSE_species_masked_TA_birds_foraging.tif"
  )
)

# Calculate biodiversity metrics
for (metric in metrics) {
  for (taxa in c("mammals", "birds")) {
    # Load the raster for the current metric and taxa
    metric_raster <- raster(metric_paths[[metric]][[taxa]])
    
    for (category in unique(parks_reserves$IUCN_CAT)) {
      # Subset parks by category
      subset_parks <- parks_reserves[parks_reserves$IUCN_CAT == category, ]
      
      # Extract metric values
      metric_values <- exact_extract(metric_raster, subset_parks, fun = "mean")
      metric_values <- metric_values[!is.na(metric_values)]
      
      # Calculate biodiversity metric statistics
      count_avg <- if (length(metric_values) > 0) mean(metric_values) else NA
      count_sd <- if (length(metric_values) > 0) sd(metric_values) else NA
      
      # Count parks in this category
      num_parks <- nrow(subset_parks)
      
      # Add biodiversity metric stats to the table
      combined_stats <- rbind(
        combined_stats,
        data.frame(
          Category = category,
          X..parks.in.category = num_parks,
          Taxa = taxa,
          Metric = metric,
          count_avg = round(count_avg, 2),
          count_sd = round(count_sd, 2)
        )
      )
    }
  }
}

# Calculate forest integrity statistics
for (category in unique(parks_reserves$IUCN_CAT)) {
  # Subset parks by category
  subset_parks <- parks_reserves[parks_reserves$IUCN_CAT == category, ]
  
  # Extract forest integrity values
  integrity_values <- exact_extract(forest_integrity_raster, subset_parks, fun = "mean")
  integrity_values <- integrity_values[!is.na(integrity_values)]
  
  # Calculate forest integrity statistics
  avg_forest_integrity <- if (length(integrity_values) > 0) mean(integrity_values) else NA
  sd_forest_integrity <- if (length(integrity_values) > 0) sd(integrity_values) else NA
  
  # Count parks in this category
  num_parks <- nrow(subset_parks)
  
  # Add forest integrity stats to the table
  forest_stats <- rbind(
    forest_stats,
    data.frame(
      Category = category,
      X..parks.in.category = num_parks,
      Forest_integrity_avg = round(avg_forest_integrity, 2),
      Forest_integrity_sd = round(sd_forest_integrity, 2)
    )
  )
}

# Transform biodiversity metrics to wide format
metrics_stats <- combined_stats %>%
  pivot_wider(
    names_from = Metric,
    values_from = c(count_avg, count_sd),
    names_sep = "_"
  )

# Combine biodiversity metrics and forest integrity stats
combined_stats <- metrics_stats %>%
  left_join(forest_stats, by = c("Category", "X..parks.in.category")) %>%
  mutate(Category = factor(Category, levels = c("Ib", "II", "III", "IV", "V", "VI", "Not Applicable", "Not Assigned", "Not Reported"))) %>%
  na.omit()

# Save the final table to a CSV file
write.csv(combined_stats, "combined_stats_with_forest_integrity.csv", row.names = FALSE)


# Ensure fuse_count_avg and fuse_count_sd are numeric
combined_stats$count_avg_FUSE<- as.numeric(combined_stats$count_avg_FUSE)
combined_stats$count_sd_FUSE <- as.numeric(combined_stats$count_sd_FUSE)

# Calculate the ymin values for the error bars (taking the maximum of 0 and avg - sd)
combined_stats$ymin <- pmax(combined_stats$count_avg_FUSE - combined_stats$count_sd_FUSE, 0)

# Plot the data
plot_1 <-ggplot(combined_stats, aes(x = Category, y = count_avg_FUSE, fill = Taxa)) +
  geom_col(position = position_dodge()) +
  scale_fill_manual(values = c("lightcoral", "lightseagreen")) +
  geom_errorbar(aes(ymin = ymin,
                    ymax = count_avg_FUSE + as.numeric(count_sd_FUSE)),
                width = 0.2,
                position = position_dodge(0.9),
                color = "black") +
  # geom_text(aes(label = X..protected areas.in.category), position = position_dodge(0), vjust = -2, size = 5, color = "darkblue") +
  
  theme(
    panel.background = element_rect(fill = "white", color = "black"), 
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y =  element_text(size = 12),
  )+ ylab("Average FUSE species count") + xlab("IUCN Protected Area Category") + ggtitle("Average FUSE Species Per Protected Area Category")

#protected area type and forest integrity


combined_stats$Forest_integrity_avg <- round(as.numeric(combined_stats$Forest_integrity_avg), 2)
combined_stats$Forest_integrity_sd <- round(as.numeric(combined_stats$Forest_integrity_sd), 2)

# Calculate the ymin values for the error bars (taking the maximum of 0 and avg - sd)
combined_stats$ymin <- pmax(combined_stats$Forest_integrity_avg - combined_stats$Forest_integrity_sd, 0)

# Pl
# Plot the data
plot_2 <-ggplot(combined_stats, aes(x = Category, y = Forest_integrity_avg, fill = Category)) + scale_fill_manual(values = c("Ib"="Firebrick", "II"="red", "III"= "darkorange3", "IV"="darkorange","V"="goldenrod","VI"="gold","Not Applicable" = "grey51", "Not Assigned" = "grey", "Not Reported" = "lightgrey")) +
  geom_col(position = position_dodge()) +
  # geom_text(aes(label = X..protected areas.in.category), position = position_dodge(.9), vjust = 25, size = 5, color = "darkblue") +
  geom_errorbar(aes(ymin = ymin,
                    ymax = Forest_integrity_avg + as.numeric(Forest_integrity_sd)),
                width = 0.2,
                position = position_dodge(0.9),
                color = "black") +
  # geom_text(aes(label = labels.minor), position = position_dodge(width = 0.9), vjust = -0.5, size = 4, color = "black") +
  theme(
    panel.background = element_rect(fill = "white", color = "black"), 
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title.x =  element_text(size = 12),
    axis.title.y= element_text(size = 12)
  )+  ylab("Average forest integrity") +
  xlab("IUCN protected area category") +
  ggtitle("Forest Integrity Per Protected Area Category") +
  labs(fill = "PA Category") #+

# Assume you have two ggplot objects: plot1 and plot2
# Create the layout
layout <- plot_layout(
  ncol = 2,
  widths = c(2, 1),  # The first plot takes up more space than the second column
  heights = c(2, 1)   # The second column is split into two rows with equal heights
)

plot_1 / plot_2 + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(size = 10, hjust = -3, vjust = 3))

