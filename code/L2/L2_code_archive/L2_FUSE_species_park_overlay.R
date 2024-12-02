
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

#Parks maps
# Pull in world map
worldMap <- ne_countries(scale = "medium", type = "countries", returnclass = "sf")

# Subset world map. In this case we are removing the Galapagos by defining the bounding box around the Ecuador polygon.
study_region <- worldMap %>% filter(continent == "South America" | continent== "North America")
study_region_crop <- worldMap %>% filter(sovereignt == "Colombia" | sovereignt == "Peru" | sovereignt == "Bolivia" | sovereignt == "Venezuela"| sovereignt == "Ecuador")

TA <- read_sf("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/tropical_subsets/Tropical_Andes_shape.shp")

TA_refined <- st_intersection(TA, study_region_crop)

# Crop the study region to the bounding box of interest
study_region_crop <-st_crop(study_region, xmin = -87, xmax = -61, ymin = -25, ymax = 13)

# Density plots
# Load the shapefile of parks (forest only)
parks <- st_read("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/WDPA_ALL/parks_reserves_forest.shp")

#parks_IUCN <- parks %>%
  #filter(IUCN_CAT %in% c("Ib", "II", "III", "IV", "V", "VI","Not Reported", "Not Assigned"))

# Remove parks with names starting with "Cuenca hydrographica" <- these are just watersheds
filtered_parks <- parks %>%
  filter(!grepl("^Cuenca", NAME))


# Load the raster of species richness
mam_fuse <- raster("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/FUSE_species_masked_TA_mam_foraging.tif")
bird_fuse <- raster("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/FUSE_species_masked_TA_birds_foraging.tif")

#all fuse overlay

resample_fuse <- resample(mam_fuse, bird_fuse, method="bilinear")
all_fuse <- resample_fuse + bird_fuse

setwd("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness")
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
  scale_fill_continuous(type = "viridis", name = "# Species") +
  labs(title = "FUSE Species") +
  geom_sf(data = filtered_parks, fill = "transparent", color = "red", size = 0.01, show.legend = "polygon") +  # Specify the outline and fill color
  scalebar(x.min = -83.5, x.max = -76.5, y.min = -24, y.max = -22.5, dist=400, st.dist=.3, st.size=3.5, height=.5, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop,  location="bottomleft", scale=.065, symbol=1) +   theme(panel.background = element_rect(fill = "aliceblue", color="black"), legend.key.size = unit(.6, 'cm'), legend.key.width = unit(.5, "cm"), legend.justification = c(1, 0), legend.position = c(.22, .20), legend.box.background = element_rect(fill = "white"),    plot.title = element_text(size = 14, hjust = 0.5),  axis.text = element_text(size = 14),    axis.title = element_text(size=14),                                                legend.text = element_text(size = 8 ),  legend.title = element_text(size = 10)) + ylim(-25,12) +xlab("Longitude")+ ylab("Latitude") + geom_rect(aes(
    xmin = -79.19710282803564, 
    xmax =   -77.40496798559705, 
    ymin = -2.2855650361324393, 
    ymax = -0.28357305744216),
    fill = NA, 
    colour = "darkorange",
    size = 0.6,
    alpha=.8
  )

#
library(sf)

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


# Add inset map to bird occurrence map
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

# transform and sum across all polygons
TA_refined$area <- sum(st_area(st_transform(TA_refined, 4326))/(1000*1000)) #1537095
parks_full <- read_sf("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/WDPA_ALL/WDPA_merged_TA.shp")

parks_full_comb <- st_combine(parks_full)
parks_full_union <- st_union(parks_full_comb)

parks_full_union$area <- sum(st_area(st_transform(parks_full_union, 4326))/(1000*1000)) #447146.4 
#29% with some degree of protection

#How much area are strongly protected?
filtered_parks <- parks_full %>%
  filter(IUCN_CAT %in% c("Ia", "Ib", "II", "III", "IV"))
parks_filter_comb <- st_combine(filtered_parks)
parks_filter_union <- st_union(parks_filter_comb)
parks_filter_union$area <- sum(st_area(st_transform(parks_filter_union, 4326))/(1000*1000)) # 116985.3

# Ia - IV because there is less human interaction and higher degree of protection
116985.3/1537095 #7.6%


#How much area with IUCN designation:
filtered_parks <- parks_full %>%
  filter(IUCN_CAT %in% c("Ia", "Ib", "II", "III", "IV","V","VI"))
parks_filter_comb <- st_combine(filtered_parks)
parks_filter_union <- st_union(parks_filter_comb)
parks_filter_union$area <- sum(st_area(st_transform(parks_filter_union, 4326))/(1000*1000)) # 238421.9

# Ia - VI for all designated areas
238421.9/1537095 #15.5%
