#Calculating Functional Diversity (Functional Dispersion - not weighted by abundance)
# 1205 species total (790 birds and 415 mammals)

library(mFD)
library(letsR)
library(dplyr)
library(sf)

#Read in mam PAM
mam_PAM <- read.table("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/Results/richness/PAM_mam.txt")

# Read in mammal and mam traits
mam_traits_df <- read.csv("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/datasets/frugivoria_TA_mammal_subset_impute.csv")

#Remove coordinates outside of Tropical Andes from the PAM
# Read the shapefile
TA_shapefile <- st_read("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/datasets/tropical_andes_shp/Tropical_Andes_shape.shp")  # Replace with the path to your shapefile

#Location key to keep things straight for later on
coordinates_df_mam <-as.data.frame(mam_PAM[,1:2])

# Convert the dataframe to an sf object
coordinates_sf_mam <- st_as_sf(coordinates_df_mam, coords = c("Longitude.x.", "Latitude.y."), crs = st_crs(TA_shapefile))

# Perform the spatial intersection
intersections_mam_TA <- st_intersection(coordinates_sf_mam, TA_shapefile)

# Convert the intersected sf object back to a dataframe
coordinates_df_subset_mam <- as.data.frame(intersections_mam_TA)

# Print the resulting subset of coordinates
print(coordinates_df_subset_mam)

# Convert the point geometry to a dataframe of longitude and latitude
subset_mam <- as.data.frame(st_coordinates(intersections_mam_TA))

mam_PAM <- as.data.frame(mam_PAM)

#merge the subset and full dataframe together to get final TA dataset
mam_PAM_filter <- merge(mam_PAM, subset_mam, by.x = c("Longitude.x.", "Latitude.y."), by.y = c("X", "Y"))


# Turn PAM into matrix
mam_PAM <- as.matrix(mam_PAM_filter)

# Add rownames
# Get the number of rows in the matrix
num_rows <- nrow(mam_PAM_filter)

# Generate unique row names
row_names <- paste0("cell", 1:num_rows)

# Assign row names to the matrix
rownames(mam_PAM_filter) <- row_names

# Print the matrix with row names
print(mam_PAM_filter)
column_names <- colnames(mam_PAM_filter)
clean_column_names <- gsub("_", " ", column_names)

#Insert clean column names
colnames(mam_PAM_filter) <- clean_column_names

#turn data into correct data types for inputs into the trait categories dataframe
mam_traits_df$diet_cat <- as.factor(mam_traits_df$diet_cat)
mam_traits_df$diet_breadth <- as.numeric(mam_traits_df$diet_breadth)
mam_traits_df$body_mass_e <- as.numeric(mam_traits_df$body_mass_e)
mam_traits_df$generation_time<- as.numeric(mam_traits_df$generation_time)
mam_traits_df$habitat_breadth <- factor(mam_traits_df$habitat_breadth, ordered = TRUE)
mam_traits_df$for_strat_value_e <- as.factor(mam_traits_df$for_strat_value_e)
#names(mam_traits_df)[1] <- ""


# Remove the species from PAM that have no occurrences anymore after subsetting to TA
# Remove columns with sum equal to zero
PAM_mam_site_final <- mam_PAM_filter[, colSums(mam_PAM_filter) != 0]

#Save coordinates for later
site_loc_key_mam <- PAM_mam_site_final[,1:2]


columns_to_remove <- c(1,2)
PAM_mam_site_final <- PAM_mam_site_final[,-columns_to_remove]

colnames <- colnames(PAM_mam_site_final)

# Remove species names from trait matrix not in the PAM
mam_traits_df_subset <- mam_traits_df %>% filter(IUCN_species_name %in% colnames)
mam_traits_df_subset$X <- NULL


# Turn trait dataframe into a matrix
class(mam_traits_df)
mam_traits_matrix <- as.matrix(mam_traits_df_subset)


# Define row names as species names
row_names <- mam_traits_matrix[,1]

# Assign row names to the matrix
rownames(mam_traits_matrix) <- row_names

# Turn back into dataframe
mam_traits_df <-as.data.frame(mam_traits_matrix)
mam_traits_df$IUCN_species_name <-NULL


#fix types
mam_traits_df$diet_cat <- factor(mam_traits_df$diet_cat)
mam_traits_df$diet_breadth <- as.numeric(mam_traits_df$diet_breadth)
mam_traits_df$body_mass_e <- as.numeric(mam_traits_df$body_mass_e)
mam_traits_df$generation_time<- as.numeric(mam_traits_df$generation_time)
mam_traits_df$habitat_breadth <- factor(mam_traits_df$habitat_breadth, ordered = TRUE)
mam_traits_df$for_strat_value_e <- factor(mam_traits_df$for_strat_value_e)
mam_traits_df$IUCN_category <- NULL

# Assign the trait types (habitat breadth would be ordinal because it is based on IUCN habitat categories)
trait_name <- c("diet_cat", "diet_breadth", "body_mass_e", "generation_time", "habitat_breadth","for_strat_value_e")
trait_type <- c("N","Q","Q","Q","O","N")

# Create the trait type dataframe
mam_traits_cat <- as.data.frame(cbind(trait_name, trait_type))

# View the trait type table
knitr::kable(head(mam_traits_cat), 
             caption = "Traits types based on **frugivore traits** dataset")

# Species traits summary:
mam_traits_summ <- mFD::sp.tr.summary(
  tr_cat     = mam_traits_cat,   
  sp_tr      = mam_traits_df, 
  stop_if_NA = TRUE)

# View the trait types
mam_traits_summ$"tr_types"  

# View the traits types for non-continuous traits
mam_traits_summ$"mod_list"                   

#Turn to matrix
PAM_mam_site_final <- as.matrix(PAM_mam_site_final)
# Summary of the assemblages * species dataframe:
asb_sp_mam_summ <- mFD::asb.sp.summary(asb_sp_w = PAM_mam_site_final)

# Occurences 
asb_sp_mam_occ <- asb_sp_mam_summ$"asb_sp_occ"

#richness per cell
asb_sp_mam_summ$"asb_sp_richn"           # Species richness per assemblage

# Names of species present in random assemblage
asb_sp_mam_summ$"asb_sp_nm"[[200]]   

#functional entities
#FE_mam <-mFD::sp.to.fe(
#  sp_tr       = mam_traits_df, 
#  tr_cat      = mam_traits_cat, 
#  fe_nm_type  = "fe_rank", 
#  check_input = TRUE) 
#fe_tr <- FE_mam$"fe_tr"
#species per entity
#fe_nb_sp <- FE_mam$"fe_nb_sp"

##FD 
# scaled triats to center (x' = x - mean(x)), all traits have the same weight, gower distance because of mixed traits.
# This will then be used to build PCOAs describing functional space. The PCOAs collapse the traits into dimensions that describe
# most of the dissimilarity. PCoA is a dimensionality reduction technique that aims to summarize the dissimilarity matrix into a lower-dimensional space while preserving the relative distances between species. 
#It transforms the dissimilarities into coordinates along the principal axes that capture the most significant variation. 
#However, the focus is on capturing overall dissimilarity patterns rather than explicitly separating and representing individual traits.
sp_dist_mam <- mFD::funct.dist(
  sp_tr         = mam_traits_df,
  tr_cat        = mam_traits_cat,
  metric        = "gower",
  scale_euclid  = "center",
  ordinal_var   = "classic",
  weight_type   = "equal",
  stop_if_NA    = TRUE)

# Choose number of output digits
round(sp_dist_mam, 3)                 

#Assessing quality using PCOA (values are coordinates in PCOA space)
#Compute a Principal Coordinates Analysis (PCoA) using functional distance between species. Then the function evaluates the quality of spaces built using an increasing number of principal components. 
#Quality is evaluated as the (absolute or squared) deviation between trait-based distance (input) and distance in the PCoA-based space (raw Euclidean distance or scaled distance according to its maximum value and maximum of trait-based distance). Option to compute a functional dendrogram and its quality. This function is based on the framework presented in Maire et al. (2015).
#For mams the mad is lowest 4 dimensions. We can therefore go forward with 4 dimensions as this can decrease
# computation time and still closely represent the dissimilary matrix. May be able to skip the removal of species less than 4 per assemblage.
fspaces_quality_mam<- mFD::quality.fspaces(
  sp_dist             = sp_dist_mam,
  maxdim_pcoa         = 10,
  deviation_weighting = "absolute",
  fdist_scaling       = FALSE,
  fdendro             = "average")

# Look at the quality spaces only (MAD index looks at mean absolute deviation from the dissimilary matrix; want the deviation
# to be low, meaning that the true distances have been retained in the PCA)
round(fspaces_quality_mam$"quality_fspaces", 3)            

# Plot the quality spaces (chose to look at 3D, 4D, and 5D since they had the lowest MAD). Will go with 4 dimensions.
mFD::quality.fspaces.plot(
  fspaces_quality            = fspaces_quality_mam,
  quality_metric             = "mad",
  fspaces_plot               = c("pcoa_3d", "pcoa_4d", 
                                 "pcoa_5d"),
  name_file                  = NULL,
  range_dist                 = NULL,
  range_dev                  = NULL,
  range_qdev                 = NULL,
  gradient_deviation         = c(neg = "darkblue", nul = "grey80", pos = "darkred"),
  gradient_deviation_quality = c(low = "yellow", high = "red"),
  x_lab                      = "Trait-based distance")


#testing correlation between functional axes and traits
sp_faxes_coord_mam <- fspaces_quality_mam$"details_fspaces"$"sp_pc_coord"

# View the components of the PCA axes *Remember the firt few components explain the most variation in dissimilarity. Clusters into groups
# Computes linear model for continuous traits and Kruskall-Wallis tests for other types. 
mam_tr_faxes <- mFD::traits.faxes.cor(
  sp_tr          = mam_traits_df, 
  sp_faxes_coord = sp_faxes_coord_mam[ , c("PC1", "PC2", "PC3", "PC4")], 
  plot           = TRUE)

# Print traits with significant effect:
mam_tr_faxes$"tr_faxes_stat"[which(mam_tr_faxes$"tr_faxes_stat"$"p.value" < 0.05), ]

# Return plots:
mam_tr_faxes$"tr_faxes_plot"

#plotting functional space
sp_faxes_coord_mam <- fspaces_quality_mam$"details_fspaces"$"sp_pc_coord"

big_plot <- mFD::funct.space.plot(
  sp_faxes_coord  = sp_faxes_coord_mam[ , c("PC1", "PC2", "PC3", "PC4")],
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
row_sums <- rowSums(PAM_mam_site_final)
subset_matrix <- PAM_mam_site_final[row_sums >= 5, ]


#computing FD
# The number of species per assemblage has to be higher or equal to the number of traits
alpha_fd_indices_mam <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_mam[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = subset_matrix,
  ind_vect         = c("fide", "fdis", "fmpd", "fnnd", "feve", "fori", "fspe"), #did not run fdiv or fric (slows it down)
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)


details_list_mam <- alpha_fd_indices_mam$"details"

#plot
plots_alpha <- mFD::alpha.multidim.plot(
  output_alpha_fd_multidim = alpha_fd_indices_mam,
  plot_asb_nm              = c("cell40", "cell200"),
  ind_nm                   = c("fide", "fdis"),
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

#Have coordinates and associated values

library(raster)

# Create an empty raster with desired resolution and extent
resolution <- c(0.08333333, 0.08333333)  # Set the desired resolution
extent <- c(-91.65305, -57.48638, -22.90167, 12.43166)  # Set the desired extent
empty_raster <- raster(resolution = resolution, xmn = extent[1], xmx = extent[2], ymn = extent[3], ymx = extent[4])

# Generate coordinates
subset_coords_mam <- site_loc_key_mam[rowSums(PAM_mam_site_final) >= 5, ]
subset_coords_mam_sp <-subset_coords_mam[,1:2 ]

#Get functional dispersion
fdis_mam <- alpha_fd_indices_mam$functional_diversity_indices$fdis

mam_fd_sp <- data.frame(subset_coords_mam_sp, fdis_mam)

# Convert the dataframe to sf format
spatial_fdis_mam <- st_as_sf(mam_fd_sp, coords = c("Longitude.x.", "Latitude.y."))

# Rasterize the sf data to create the FD raster
fd_raster_mam <- rasterize(spatial_fdis_mam, empty_raster)
setwd("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/Results/richness/")
writeRaster(fd_raster_mam$fdis_mam, "FD_mams_foraging_TA.tif", format="GTiff",overwrite=T)

#Species richness
spec_rich_mam <- alpha_fd_indices_mam$functional_diversity_indices$sp_richn

mam_spec_rich_sp <- data.frame(subset_coords_mam_sp, spec_rich_mam)

# Convert the dataframe to sf format
spatial_spec_rich_mam <- st_as_sf(mam_spec_rich_sp, coords = c("Longitude.x.", "Latitude.y."))

# Rasterize the sf data to create the FD raster
spec_rich_raster_mam <- rasterize(spatial_spec_rich_mam, empty_raster)
setwd("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/Results/richness/")
writeRaster(spec_rich_raster_mam$spec_rich_mam, "Spec_rich_TA.tif", format="GTiff",overwrite=T)


# Understand relationship between species richness and functional diversity
library(ggplot2)

# Diversity metrics
mam_diversity <-  data.frame(spec_rich_mam, fdis_mam)

# Create a scatter plot
ggplot(mam_diversity, aes(x = spec_rich_mam, y = fdis_mam)) +
  geom_point() +
  xlab("Species Richness") +
  ylab("Functional Diversity")

# Calculate correlation coefficient
#correlation_mam_div <- cor(mam_diversity$spec_rich_mam, mam_diversity$fdis_mam)

# Print the correlation coefficient
#print(correlation)

# Regression

# Perform linear regression
#reg_model <- lm(fdis_mam ~ spec_rich_mam, data = mam_diversity)

# Print the regression summary
#summary(reg_model)

#plot mam diversity as a function of species richness ()
#ggplot(mam_diversity, aes(x = spec_rich_mam, y = fdis_mam)) +
#  geom_point() +
#  geom_smooth(method = "lm", se = FALSE, color = "blue") +
#  xlab("Species Richness (spec_rich_mam)") +
#  ylab("Functional Diversity (fdis_mam)") +
#  ggtitle("Linear Regression: fdis_mam ~ spec_rich_mam")

#FUSE

mam_traits_IUCN <- read.csv("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/datasets/IUCN_statuses.csv")

mam_traits_IUCN_final<- mam_traits_IUCN %>% filter(IUCN_species_name %in% mam_traits_df_subset$IUCN_species_name)


IUCN_status_mams <- mam_traits_IUCN_final$IUCN_category
IUCN_index <- lets.iucncont(IUCN_status_mams, dd = .5, ne = NA)
mam_fuse <- fuse(sp_dist_mam, sp_faxes_coord_mam, GE= IUCN_index,standGE = FALSE) # need numerical vector of IUCN statuses 
write.csv(mam_fuse, "mam_fuse_foraging.csv")

# MAP

study_region <- as.data.frame(study_region)
# Need the raster to be a dataframe for mapping. Convert to points to do this.
spec_rich_raster_point <- rasterToPoints(spec_rich_raster_mam, spatial = TRUE)
spec_mam_raster_point <- rasterToPoints(fd_raster_mam, spatial = TRUE)

# Convert to a 'conventional' dataframe
spec_rich_raster_point <- data.frame(spec_rich_raster_point)
fd_raster_point <- data.frame(spec_mam_raster_point)

# Create a blank map of the Tropical Andes region
test <- ggplot() + theme_bw() + geom_sf(data = study_region, fill = "white") + geom_raster(data=spec_rich_raster_point, aes(x=x, y=y, fill=spec_rich_mam))+ scale_fill_viridis()
test_2 <- ggplot() + theme_bw() + geom_sf(data = study_region, fill = "white") + geom_raster(data=fd_raster_point, aes(x=x, y=y, fill=fdis_mam))+ scale_fill_viridis()
