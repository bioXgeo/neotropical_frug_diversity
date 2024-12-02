# Title: Calculating Functional Diversity (Functional Dispersion) for birds in the Tropical Andes

# Project: Evaluating the Effectiveness of Protected Areas and Community-Managed Lands 
#          in Capturing Multiple Dimensions of Frugivorous Biodiversity in the Tropical Andes
# Author: Beth E. Gerstner
# Collaborators: Phoebe L. Zarnetske
# Overview: This script calculates functional diversity indices for birds in the Tropical Andes, 
#           including functional dispersion (FD), species richness, and FUSE scores. Outputs 
#           include rasterized maps of FD and species richness. Adds foraging prevalence as a fuzzy trait.
# Data Output: Functional dispersion raster, species richness raster, and FUSE score outputs.

# Date: September 30, 2023


# Load libraries
library(raster)
library(mFD)
library(letsR)
library(dplyr)
library(sf)

# Read in bird Presence-Absence Matrix (PAM)
bird_PAM <- read.table("PLACEHOLDER_PATH/PAM_bird.txt")

# Read in bird traits dataset
bird_traits_df <- read.csv("PLACEHOLDER_PATH/frugivoria_TA_bird_subset_impute.csv")

# Remove coordinates outside of the Tropical Andes from the PAM
# Read the Tropical Andes shapefile
TA_shapefile <- st_read("PLACEHOLDER_PATH/Tropical_Andes_shape.shp") 

#Location key to keep things straight for later on
coordinates_df_bird <-as.data.frame(bird_PAM[,1:2])

# Convert the dataframe to an sf object
coordinates_sf_bird <- st_as_sf(coordinates_df_bird, coords = c("Longitude.x.", "Latitude.y."), crs = st_crs(TA_shapefile))

# Perform the spatial intersection
intersections_bird_TA <- st_intersection(coordinates_sf_bird, TA_shapefile)

# Convert the intersected sf object back to a dataframe
coordinates_df_subset_bird <- as.data.frame(intersections_bird_TA)

# Print the resulting subset of coordinates
print(coordinates_df_subset_bird)

# Convert the point geometry to a dataframe of longitude and latitude
subset_bird <- as.data.frame(st_coordinates(intersections_bird_TA))


# Match the coordinates to the original dataframe
#merge the subset and full dataframe together to get final TA dataset
bird_PAM_filter <- merge(bird_PAM, subset_bird, by.x = c("Longitude.x.", "Latitude.y."), by.y = c("X", "Y"))


# Turn PAM into matrix
bird_PAM <- as.matrix(bird_PAM_filter)

# Add rownames
# Get the number of rows in the matrix
num_rows <- nrow(bird_PAM)

# Generate unique row names
row_names <- paste0("cell", 1:num_rows)

# Assign row names to the matrix
rownames(bird_PAM) <- row_names

# Print the matrix with row names
print(bird_PAM)
column_names <- colnames(bird_PAM)
clean_column_names <- gsub("_", " ", column_names)

#Insert clean column names
colnames(bird_PAM) <- clean_column_names


#rename bird diet cat column to match mammals
colnames(bird_traits_df)[2] <- "IUCN_species_name"
bird_traits_df$X <- NULL
colnames(bird_traits_df)[2] <- "diet_cat"


#turn data into correct data types for inputs into the trait categories dataframe
bird_traits_df$diet_cat <- as.factor(bird_traits_df$diet_cat)
bird_traits_df$diet_breadth <- as.numeric(bird_traits_df$diet_breadth)
bird_traits_df$body_mass_e <- as.numeric(bird_traits_df$body_mass_e)
bird_traits_df$generation_time<- as.numeric(bird_traits_df$generation_time)
bird_traits_df$habitat_breadth <- factor(bird_traits_df$habitat_breadth, ordered = TRUE)
bird_traits_df$for_strat_water <- as.numeric(bird_traits_df$for_strat_water)
bird_traits_df$for_strat_ground_e <-as.numeric(bird_traits_df$for_strat_ground_e)
bird_traits_df$for_strat_understory_e <- as.numeric(bird_traits_df$for_strat_understory_e)
bird_traits_df$for_strat_midhigh_e <- as.numeric(bird_traits_df$for_strat_midhigh_e)
bird_traits_df$for_strat_canopy_e <- as.numeric(bird_traits_df$for_strat_canopy_e)
bird_traits_df$for_strat_aerial_e <- as.numeric(bird_traits_df$for_strat_aerial_e)
# Remove the species from PAM that have no occurrences anymore after subsetting to TA
# Remove columns with sum equal to zero
PAM_bird_site_final <- bird_PAM[, colSums(bird_PAM) != 0]

#Save coordinates for later
site_loc_key_bird <- PAM_bird_site_final[,1:2]


columns_to_remove <- c(1,2)
PAM_bird_site_final <- PAM_bird_site_final[,-columns_to_remove]

colnames <- colnames(PAM_bird_site_final)

# Remove species names from trait matrix not in the PAM
bird_traits_df_subset <- bird_traits_df %>% filter(IUCN_species_name %in% colnames)


# Turn trait dataframe into a matrix
bird_traits_matrix <- as.matrix(bird_traits_df_subset)


# Define row names as species names
row_names <- bird_traits_matrix[,1]

# Assign row names to the matrix
rownames(bird_traits_matrix) <- row_names

# Turn back into dataframe
bird_traits_df <-as.data.frame(bird_traits_matrix)
bird_traits_df$IUCN_species_name <-NULL
bird_traits_df_final <- bird_traits_df %>% select("diet_cat", "diet_breadth", "body_mass_e", "generation_time", "habitat_breadth", "for_strat_water","for_strat_ground_e","for_strat_understory_e","for_strat_midhigh_e","for_strat_canopy_e","for_strat_aerial_e")

# Assign the trait types (habitat breadth would be ordinal because it is based on IUCN habitat categories)
trait_name <- c("diet_cat", "diet_breadth", "body_mass_e", "generation_time", "habitat_breadth", "for_strat_water","for_strat_ground_e","for_strat_understory_e","for_strat_midhigh_e","for_strat_canopy_e","for_strat_aerial_e")
trait_type <- c("N","Q","Q","Q","O","F","F","F","F","F","F")
fuzzy_name <- c(NA,NA,NA,NA,NA,"foraging_strata","foraging_strata","foraging_strata","foraging_strata","foraging_strata","foraging_strata")

# Create the trait type dataframe
bird_traits_cat <- as.data.frame(cbind(trait_name, trait_type, fuzzy_name))

# View the trait type table
knitr::kable(head(bird_traits_cat), 
             caption = "Traits types based on **frugivore traits** dataset")

bird_traits_df_final$diet_cat <- as.factor(bird_traits_df_final$diet_cat)
bird_traits_df_final$diet_breadth <- as.numeric(bird_traits_df_final$diet_breadth)
bird_traits_df_final$body_mass_e <- as.numeric(bird_traits_df_final$body_mass_e)
bird_traits_df_final$generation_time<- as.numeric(bird_traits_df_final$generation_time)
bird_traits_df_final$habitat_breadth <- factor(bird_traits_df_final$habitat_breadth, ordered = TRUE)
bird_traits_df_final$for_strat_water <- as.numeric(bird_traits_df_final$for_strat_water)
bird_traits_df_final$for_strat_ground_e <-as.numeric(bird_traits_df_final$for_strat_ground_e)
bird_traits_df_final$for_strat_understory_e <- as.numeric(bird_traits_df_final$for_strat_understory_e)
bird_traits_df_final$for_strat_midhigh_e <- as.numeric(bird_traits_df_final$for_strat_midhigh_e)
bird_traits_df_final$for_strat_canopy_e <- as.numeric(bird_traits_df_final$for_strat_canopy_e)
bird_traits_df_final$for_strat_aerial_e <- as.numeric(bird_traits_df_final$for_strat_aerial_e)


# Species traits summary:
bird_traits_summ <- mFD::sp.tr.summary(
  tr_cat     = bird_traits_cat,   
  sp_tr      = bird_traits_df_final, 
  stop_if_NA = TRUE)

# View the trait types
bird_traits_summ$"tr_types"  

# View the traits types for non-continuous traits
bird_traits_summ$"mod_list"         


bird_traits_summ$"tr_summary_fuzzy_list"

# Summary of the assemblages * species dataframe:
asb_sp_bird_summ <- mFD::asb.sp.summary(asb_sp_w = PAM_bird_site_final)

# Occurences 
asb_sp_bird_occ <- asb_sp_bird_summ$"asb_sp_occ"

#richness per cell
asb_sp_bird_summ$"asb_sp_richn"           # Species richness per assemblage

# Names of species present in random assemblage
asb_sp_bird_summ$"asb_sp_nm"[[200]]   

## FD 
# scaled triats to center (x' = x - mean(x)), all traits have the same weight, gower distance because of mixed traits.
# This will then be used to build PCOAs describing functional space. The PCOAs collapse the traits into dimensions that describe
# most of the dissimilarity. PCoA is a dimensionality reduction technique that aims to summarize the dissimilarity matrix into a lower-dimensional space while preserving the relative distances between species. 
#It transforms the dissimilarities into coordinates along the principal axes that capture the most significant variation. 
#However, the focus is on capturing overall dissimilarity patterns rather than explicitly separating and representing individual traits.
sp_dist_bird <- mFD::funct.dist(
  sp_tr         = bird_traits_df_final,
  tr_cat        = bird_traits_cat,
  metric        = "gower",
  scale_euclid  = "center",
  ordinal_var   = "classic",
  weight_type   = "equal",
  stop_if_NA    = TRUE)

# Choose number of output digits
round(sp_dist_bird, 3)                 

#Assessing quality using PCOA (values are coordinates in PCOA space)
#Compute a Principal Coordinates Analysis (PCoA) using functional distance between species. Then the function evaluates the quality of spaces built using an increasing number of principal components. 
#Quality is evaluated as the (absolute or squared) deviation between trait-based distance (input) and distance in the PCoA-based space (raw Euclidean distance or scaled distance according to its maximum value and maximum of trait-based distance). Option to compute a functional dendrogram and its quality. This function is based on the framework presented in Maire et al. (2015).
#For birds the mad is lowest 5 dimensions. But 4 is close and can therefore go forward with 4 dimensions as this can decrease
# computation time and still closely represent the dissimilary matrix. May be able to skip the removal of species less than 4 per assemblage.
fspaces_quality_bird<- mFD::quality.fspaces(
  sp_dist             = sp_dist_bird,
  maxdim_pcoa         = 10,
  deviation_weighting = "absolute",
  fdist_scaling       = FALSE,
  fdendro             = "average")

# Look at the quality spaces only (MAD index looks at mean absolute deviation from the dissimilary matrix; want the deviation
# to be low, meaning that the true distances have been retained in the PCA)
round(fspaces_quality_bird$"quality_fspaces", 3)            

# Plot the quality spaces (chose to look at 3D, 4D, and 5D since they had the lowest MAD). Will go with 4 dimensions.
mFD::quality.fspaces.plot(
  fspaces_quality            = fspaces_quality_bird,
  quality_metric             = "mad",
  fspaces_plot               = c("pcoa_4d", 
                                 "pcoa_5d"),
  name_file                  = NULL,
  range_dist                 = NULL,
  range_dev                  = NULL,
  range_qdev                 = NULL,
  gradient_deviation         = c(neg = "darkblue", nul = "grey80", pos = "darkred"),
  gradient_deviation_quality = c(low = "yellow", high = "red"),
  x_lab                      = "Trait-based distance")


#testing correlation between functional axes and traits
sp_faxes_coord_bird <- fspaces_quality_bird$"details_fspaces"$"sp_pc_coord"

# View the components of the PCA axes *Remember the firt few components explain the most variation in dissimilarity. Clusters into groups
# Computes linear model for continuous traits and Kruskall-Wallis tests for other types. 
bird_tr_faxes <- mFD::traits.faxes.cor(
  sp_tr          = bird_traits_df_final, 
  sp_faxes_coord = sp_faxes_coord_bird[ , c("PC1", "PC2", "PC3", "PC4")], 
  plot           = F)

# Print traits with significant effect:
bird_tr_faxes$"tr_faxes_stat"[which(bird_tr_faxes$"tr_faxes_stat"$"p.value" < 0.05), ]

# Return plots:
bird_tr_faxes$"tr_faxes_plot"

#plotting functional space
sp_faxes_coord_bird <- fspaces_quality_bird$"details_fspaces"$"sp_pc_coord"

big_plot <- mFD::funct.space.plot(
  sp_faxes_coord  = sp_faxes_coord_bird[ , c("PC1", "PC2", "PC3", "PC4")],
  faxes           = c("PC1", "PC2", "PC3", "PC4"),
  name_file       = NULL,
  faxes_nm        = NULL,
  range_faxes     = c(NA, NA),
  color_bg        = "grey95",
  color_pool      = "darkgreen",
  fill_pool       = "white",
  shape_pool      = 21,
  size_pool       = 1,
  plot_ch         = TRUE,
  color_ch        = "black",
  fill_ch         = "white",
  alpha_ch        = 0.5,
  plot_vertices   = TRUE,
  color_vert      = "blueviolet",
  fill_vert       = "blueviolet",
  shape_vert      = 6,
  size_vert       = 1,
  plot_sp_nm      = NULL,
  nm_size         = 3,
  nm_color        = "black",
  nm_fontface     = "plain",
  check_input     = TRUE)

#Need to remove parts of the PAM that have values less than or equal to the number of dimensions (4)
# Calculate row sums
row_sums <- rowSums(PAM_bird_site_final)
subset_matrix <- PAM_bird_site_final[row_sums >= 5, ]


#computing FD
# The number of species per assemblage has to be higher or equal to the number of traits
alpha_fd_indices_bird <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_bird[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = subset_matrix,
  ind_vect         = c("fide", "fdis", "fmpd", "fnnd", "feve", "fori", "fspe"), #did not run fdiv or fric (slows it down)
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)


details_list_bird <- alpha_fd_indices_bird$"details"

#plot
plots_alpha <- mFD::alpha.multidim.plot(
  output_alpha_fd_multidim = alpha_fd_indices_bird,
  plot_asb_nm              = c("cell40", "cell200"),
  ind_nm                   = c("fdis", "fide", "fnnd"
                        , "fori", "fspe"),
  faxes                    = NULL,
  faxes_nm                 = NULL,
  range_faxes              = c(NA, NA),
  color_bg                 = "grey95",
  shape_sp                 = c(pool = 3, asb1 = 21, asb2 = 21),
  size_sp                  = c(pool = 0.7, asb1 = 1, asb2 = 1),
  color_sp                 = c(pool = "grey50", asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  color_vert               = c(pool = "grey50", asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  fill_sp                  = c(pool = NA, asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  fill_vert                = c(pool = NA, asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  color_ch          
  = c(pool = NA, asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  fill_ch                  = c(pool = "white", asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  alpha_ch                 = c(pool = 1, asb1 = 0.3, asb2 = 0.3),
  shape_centroid_fdis      = c(asb1 = 22,  asb2 = 24),
  shape_centroid_fdiv      = c(asb1 = 22,  asb2 = 24),
  shape_centroid_fspe      = 23,
  color_centroid_fspe      = "black",
  size_sp_nm               = 3, 
  color_sp_nm              = "black",
  plot_sp_nm               = NULL,
  fontface_sp_nm           = "plain",
  save_file                = FALSE,
  check_input              = TRUE) 

plots_alpha$"fric"$"patchwork"  #comparing convex hulls of different hulls

plots_alpha$"fdiv"$"patchwork"

plots_alpha$"fspe"$"patchwork"

plots_alpha$"fdis"$"patchwork"

plots_alpha$"fori"$"patchwork"

## Turning the FD values into a raster

#Need to subset the coordinates made at the top to the same set of the assemblages (removed values less than 5 so these coords need to be removed to do this correctly)

## Create an empty raster with desired resolution and extent
resolution <- c(0.08333333, 0.08333333)  # Set the desired resolution
extent <- c(-91.65305, -57.48638, -22.90167, 12.43166)  # Set the desired extent
empty_raster <- raster(resolution = resolution, xmn = extent[1], xmx = extent[2], ymn = extent[3], ymx = extent[4])

## Generate coordinates
subset_coords_bird <- site_loc_key_bird[rowSums(PAM_bird_site_final) >= 5, ]
subset_coords_bird_sp <- subset_coords_bird[, 1:2]

## Get functional dispersion
fdis_bird <- alpha_fd_indices_bird$functional_diversity_indices$fdis
bird_fd_sp <- data.frame(subset_coords_bird_sp, fdis_bird)
setwd("PLACEHOLDER_PATH/Results/richness/")
write.csv(bird_fd_sp, "bird_fd_sp.csv")

# Convert the dataframe to sf format
spatial_fdis_bird <- st_as_sf(bird_fd_sp, coords = c("Longitude.x.", "Latitude.y."))

# Rasterize the sf data to create the FD raster
fd_raster_bird <- rasterize(spatial_fdis_bird, empty_raster)
writeRaster(fd_raster_bird$fdis_bird, "FD_birds_foraging_TA.tif", format = "GTiff", overwrite = TRUE)

# Write ID raster for analysis
writeRaster(fd_raster_bird$ID, "FD_birds_TA_ID.tif", format = "GTiff", overwrite = TRUE)

## Species richness
spec_rich_bird <- alpha_fd_indices_bird$functional_diversity_indices$sp_richn
bird_spec_rich_sp <- data.frame(subset_coords_bird_sp, spec_rich_bird)

# Convert the dataframe to sf format
spatial_spec_rich_bird <- st_as_sf(bird_spec_rich_sp, coords = c("Longitude.x.", "Latitude.y."))

# Rasterize the sf data to create the richness raster
spec_rich_raster_bird <- rasterize(spatial_spec_rich_bird, empty_raster)
setwd("PLACEHOLDER_PATH/Results/richness/")
writeRaster(spec_rich_raster_bird$spec_rich_bird, "Spec_rich_birds_TA.tif", format = "GTiff", overwrite = TRUE)

## FUSE
bird_traits_IUCN <- read.csv("PLACEHOLDER_PATH/frugivoria_TA_bird_subset_impute.csv")
bird_traits_IUCN_final <- bird_traits_IUCN %>% filter(IUCN_species_name %in% bird_traits_df_subset$X)

IUCN_status_birds <- bird_traits_IUCN_final$IUCN_category
IUCN_index <- lets.iucncont(IUCN_status_birds, dd = 0.5, ne = NA)
bird_fuse <- fuse(sp_dist_bird, sp_faxes_coord_bird, GE = IUCN_index, standGE = FALSE)
write.csv(bird_fuse, "bird_fuse.csv")
