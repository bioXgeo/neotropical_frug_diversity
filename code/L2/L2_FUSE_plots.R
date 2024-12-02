
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
library(sf)


# Pull in world map
worldMap <- ne_countries(scale = "medium", type = "countries", returnclass = "sf")

# Subset world map. In this case we are removing the Galapagos by defining the bounding box around the Ecuador polygon.
study_region <- worldMap %>% filter(continent == "South America" | continent== "North America")
study_region_crop <- worldMap %>% filter(sovereignt == "Colombia" | sovereignt == "Peru" | sovereignt == "Bolivia" | sovereignt == "Venezuela"| sovereignt == "Ecuador")

TA <- read_sf("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/tropical_subsets/Tropical_Andes_shape.shp")

TA_refined <- st_intersection(TA, study_region_crop)

# Crop the study region to the bounding box of interest
study_region_crop <-st_crop(study_region, xmin = -87, xmax = -61, ymin = -29, ymax = 13)




create_lollipop_plot <- function(FUSE_species_file, IUCN_status_file) {
  library(ggplot2)
  
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
  
  # Define colors for threat status
  threat_colors <- c("EN" = "red", "VU" = "orange", "CR" = "darkred", "LC" = "green")
  
  # Create the lollipop graph with larger text and smaller plot size
  lollipop_plot <- ggplot(FUSE_species_status, aes(x = FUSE, y = reorder(IUCN_species_name, FUSE))) +
    geom_segment(aes(xend = 0, yend = IUCN_species_name), color = "gray50") +
    geom_point(size = 4, aes(color = IUCN_category, shape = IUCN_category)) +
    scale_color_manual(values = threat_colors) +
    scale_shape_manual(values = c(16, 17, 15, 18)) +
    labs(x = "FUSE Score", y = NULL, title = "Top Ten Species by FUSE Score",
         subtitle = "Threat Status indicated by color and shape") +
    theme_minimal() +
    theme(axis.title.y = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.spacing = unit(0.2, "cm"),
          legend.text = element_text(size = 12),  # Increase legend text size
          panel.grid.major.y = element_blank(),
          text = element_text(size = 14),  # Increase all other text size
          plot.margin = unit(c(1, 1, 1, 1), "cm"))  # Set smaller plot margins
  
  # Return the lollipop plot
  return(lollipop_plot)
}

# Mammals
FUSE_species_file <- "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/mam_fuse_foraging.csv"
IUCN_status_file <- "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/IUCN_statuses.csv"
lollipop_plot <- create_lollipop_plot(FUSE_species_file, IUCN_status_file)
print(lollipop_plot)

# Birds
FUSE_species_file <- "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/bird_fuse_foraging.csv"
IUCN_status_file <- "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/IUCN_statuses.csv"
lollipop_plot <- create_lollipop_plot(FUSE_species_file, IUCN_status_file)
print(lollipop_plot)

#GGDOT
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
FUSE_species_file <- "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/mam_fuse_foraging.csv"
FUSE_species_file <- "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/tropical_subsets/mam_endemic_fuse_fixed.csv"
IUCN_status_file <- "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/IUCN_statuses.csv"
dotchart_mammals <- create_ggdotchart(FUSE_species_file, IUCN_status_file)
print(dotchart_mammals)

FUSE_all <-read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/mam_fuse_foraging.csv")
FUSE_species_sort <- head(arrange(FUSE_all, desc(FUSE)), 86)
FUSE_names <- FUSE_species_sort$X
FUSE_species_sort$IUCN_species_name <- FUSE_names
frugivoria_mammal <- read.csv("G:/Shared drives/SpaCE_Lab_neotropical_frugivores/Manuscripts/Database_Manuscript/Database_paper/EDI_resubmission_2023/databases_2023/Frugivoria_mammal_database_2023_full.csv")
frugivoria_mammal_montane <- frugivoria_mammal[frugivoria_mammal$habitat==1 | frugivoria_mammal$habitat==3, ]

FUSE_subset <-merge(FUSE_species_sort, frugivoria_mammal_montane, by="IUCN_species_name")
FUSE_subset <- FUSE_subset[,c("IUCN_species_name","FUSE")]
FUSE_subset_organized_mammals <- head(arrange(FUSE_subset, desc(FUSE)), 43)
setwd("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness")
write.csv(FUSE_subset_organized_mammals,"FUSE_species_organized_mammals.csv")

FUSE_species_file <- "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/FUSE_species_organized_mammals.csv"
dotchart_mammals <- create_ggdotchart(FUSE_species_file, IUCN_status_file)
print(dotchart_mammals)



# Birds
FUSE_species_file <- "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/bird_fuse_foraging.csv"
IUCN_status_file <- "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/IUCN_statuses.csv"
FUSE_species_file <- "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/tropical_subsets/bird_endemic_fuse.csv"
frugivoria_bird <- read.csv("G:/Shared drives/SpaCE_Lab_neotropical_frugivores/Manuscripts/Database_Manuscript/Database_paper/EDI_resubmission_2023/databases_2023/Frugivoria_bird_database_2023_full_1.csv")

FUSE_all <-read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/bird_fuse_foraging.csv")
FUSE_species_sort <- head(arrange(FUSE_all, desc(FUSE)), 86)
FUSE_names <- FUSE_species_sort$X
FUSE_species_sort$IUCN_species_name <- FUSE_names

frugivoria_bird_montane <- frugivoria_bird[frugivoria_bird$habitat==1 | frugivoria_bird$habitat==3, ]

FUSE_subset <-merge(FUSE_species_sort, frugivoria_bird_montane, by="IUCN_species_name")
FUSE_subset <- FUSE_subset[,c("IUCN_species_name","FUSE")]
FUSE_subset_organized_bird <- head(arrange(FUSE_subset, desc(FUSE)), 60)
setwd("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness")
write.csv(FUSE_subset_organized_bird,"FUSE_species_organized_birds.csv")
FUSE_species_file <- "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/FUSE_species_organized_birds.csv"


dotchart_birds <- create_ggdotchart(FUSE_species_file, IUCN_status_file)
print(dotchart_birds)

plot_grid(dotchart_mammals, dotchart_birds, ncol=2, align=c("hv"))

#FUSE bar plots

fuse_stats <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/FUSE_stats_format.csv")
fuse_stats$Category <- as.factor(fuse_stats$Category)

ggplot(fuse_stats, aes(x = Category, y = fuse_count_avg, fill = taxa)) +
  geom_col(position = position_dodge()) +
  scale_fill_manual(values = c("deepskyblue2", "coral2")) +
  # geom_text(aes(label = fuse_count_avg), position = position_dodge(0.9), vjust = 1, size = 4, color = "black") +
  geom_errorbar(aes(ymin = as.numeric(fuse_count_avg) - as.numeric(fuse_count_sd),
                    ymax = as.numeric(fuse_count_avg) + as.numeric(fuse_count_sd)),
                width = 0.1,
                position = position_dodge(0.9), color="black") +
  labs(y = "Average FUSE Species Count") + xlab("IUCN protected area Category") # Update y-axis label


library(ggplot2)

# Read the CSV data
fuse_stats <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/FUSE_stats_format.csv")

# Convert Category to a factor
fuse_stats$Category <- factor(fuse_stats$Category, levels = c("Ib","II","III","IV","V","VI","Not Applicable","Not Assigned","Not Reported"))


# Round the average FUSE species count and standard deviation to two decimal places
fuse_stats$fuse_count_avg <- round(as.numeric(fuse_stats$fuse_count_avg), 2)
fuse_stats$fuse_count_sd <- round(as.numeric(fuse_stats$fuse_count_sd), 2)

# Calculate the ymin values for the error bars (taking the maximum of 0 and avg - sd)
fuse_stats$ymin <- pmax(fuse_stats$fuse_count_avg - fuse_stats$fuse_count_sd, 0)

# Plot the data
plot_1 <-ggplot(fuse_stats, aes(x = Category, y = fuse_count_avg, fill = Taxa)) +
  geom_col(position = position_dodge()) +
  scale_fill_manual(values = c("lightcoral", "lightseagreen")) +
  geom_errorbar(aes(ymin = ymin,
                    ymax = fuse_count_avg + as.numeric(fuse_count_sd)),
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


fuse_stats$Forest_integrity_index <- round(as.numeric(fuse_stats$Forest_integrity_index), 2)
fuse_stats$FII_sd <- round(as.numeric(fuse_stats$FII_sd), 2)

# Calculate the ymin values for the error bars (taking the maximum of 0 and avg - sd)
fuse_stats$ymin <- pmax(fuse_stats$Forest_integrity_index - fuse_stats$FII_sd, 0)

# Pl

library(RColorBrewer)
# Plot the data
plot_2 <-ggplot(fuse_stats, aes(x = Category, y = Forest_integrity_index, fill = Category)) + scale_fill_manual(values = c("Ib"="Firebrick", "II"="red", "III"= "darkorange3", "IV"="darkorange","V"="goldenrod","VI"="gold","Not Applicable" = "grey51", "Not Assigned" = "grey", "Not Reported" = "lightgrey")) +
  geom_col(position = position_dodge()) +
  # geom_text(aes(label = X..protected areas.in.category), position = position_dodge(.9), vjust = 25, size = 5, color = "darkblue") +
  geom_errorbar(aes(ymin = ymin,
                    ymax = Forest_integrity_index + as.numeric(FII_sd)),
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




fuse_stats$perc_makeup_FUSE_species <- round(as.numeric(fuse_stats$perc_makeup_FUSE_species), 2)
fuse_stats$FII_sd <- round(as.numeric(fuse_stats$sd_mean_prop_fuse), 2)

# Calculate the ymin values for the error bars (taking the maximum of 0 and avg - sd)
fuse_stats$ymin <- pmax(fuse_stats$perc_makeup_FUSE_species - fuse_stats$sd_mean_prop_fuse, 0)


# number protected areas per category
# Plot the data
plot_3 <-ggplot(fuse_stats, aes(x = Category, y = perc_makeup_FUSE_species, fill = Category)) + scale_fill_manual(values = c("Ib"="Firebrick", "II"="red", "III"= "darkorange3", "IV"="darkorange","V"="goldenrod","VI"="gold","Not Applicable" = "grey51", "Not Assigned" = "grey", "Not Reported" = "lightgrey")) +
  geom_col(position = position_dodge()) +
  # geom_text(aes(label = X..protected areas.in.category), position = position_dodge(.9), vjust = 25, size = 5, color = "darkblue") +
  geom_errorbar(aes(ymin = ymin,
                    ymax = perc_makeup_FUSE_species + as.numeric(sd_mean_prop_fuse)),
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
  )+  ylab("% Composition Fuse Species") +
  xlab("IUCN protected area category") +
  ggtitle("% Composition FUSE Species Per Protected Area Category") +
  labs(fill = "PA Category") #+



library(ggplot2)
library(patchwork)

# Assume you have two ggplot objects: plot1 and plot2

# Create the layout
layout <- plot_layout(
  ncol = 2,
  widths = c(2, 1),  # The first plot takes up more space than the second column
  heights = c(2, 1)   # The second column is split into two rows with equal heights
)

plot_1 / plot_2 / plot_3 + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(size = 10, hjust = -3, vjust = 3))


# FUSE proportion
library(raster)
FUSE_all <- raster("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/all_fuse_species.tif")
species_all <- raster("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/Spec_rich_joint_TA.tif")

species_all <- resample(species_all, FUSE_all, method="bilinear")

proportion_fuse <-(FUSE_all/species_all)*100

setwd("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness")
writeRaster(proportion_fuse,"proportion_fuse.tif",format="GTiff")

