
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
  FUSE_species_sort <- head(arrange(FUSE_species, desc(FUSE)), 20)
  
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
                         dot.size = 6,                                 # Large dot size
                         label = round(FUSE_species_status$FUSE,1),             # Add mpg values as dot labels
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
IUCN_status_file <- "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/IUCN_statuses.csv"
dotchart_mammals <- create_ggdotchart(FUSE_species_file, IUCN_status_file)
print(dotchart_mammals)

# Birds
FUSE_species_file <- "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/bird_fuse_foraging.csv"
IUCN_status_file <- "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/IUCN_statuses.csv"
dotchart_birds <- create_ggdotchart(FUSE_species_file, IUCN_status_file)
print(dotchart_birds)

plot_grid(dotchart_mammals, dotchart_birds, ncol=2, align=c("hv"))

