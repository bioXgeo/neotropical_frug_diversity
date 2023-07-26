#Calculating Functional Diversity (Functional Dispersion - not weighted by abundance)

library(mFD)
library(letsR)
library(dplyr)

## Mammals

#Edit presence absence matrix so there's is one value for site
# Assuming your matrix is called 'presence_absence_matrix'
n_sites <- dim(presence_absence_matrix_mam$Presence_and_Absence_Matrix)[1]
PAM_mam <- presence_absence_matrix_mam$Presence_and_Absence_Matrix
site_column <- matrix(seq_len(n_sites), ncol = 1)

# Add the site column to the matrix
PAM_mam_site <- cbind(site_column, PAM_mam)

#Specify the columns to remove (latitude and longitude), but we'll need those coordinates alter
columns_to_remove <- c(1, 2, 3)

# Remove the specified columns
PAM_mam_site_final <- PAM_mam_site[, -columns_to_remove]
head(PAM_mam_site_final)


#Fix column names in PAM
# Get the current column names
current_colnames <- colnames(PAM_mam_site_final)

# Remove underscores from the column names
new_colnames <- gsub("_", " ", current_colnames)

# Set the modified column names back to the matrix
colnames(PAM_mam_site_final) <- new_colnames


#Location key to keep things straight for later on
site_loc_key_mam <-PAM_mam_site[,1:3]

# Read in full Frugivoria database for mammals
mam_traits <- read.csv("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/datasets/frugivoria_TA_mammal_subset_impute.csv")

# Subset to traits of interest
mam_subset <- mam_traits %>% select(IUCN_species_name,diet_cat, diet_breadth, body_mass_e, generation_time, habitat_breadth)

# How many species missing generation time
#nrow(mam_subset[mam_subset$generation_time=="NA",]) #none!
#mam_missing <- mam_subset[is.na(mam_subset$generation_time), ]

# Subset to species with generation time values
#mam_subset_na_rm <- mam_subset[!is.na(mam_subset$generation_time), ]

# Some species didn't have shapefiles so removed from dataset
mam_shp <- st_read("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/datasets/IUCN_mammal_shape/MAMMALS_TERRESTRIAL_ONLY.shp")

# subset traits to species with spatial data
mam_traits_final<- mam_subset %>% filter(IUCN_species_name %in% mam_shp$sci_name)

#Find what species is missing from the PAM 
# Extract the column names and remove the underscores
column_names <- colnames(PAM_mam_site_final)
clean_column_names <- as.data.frame(gsub("_", " ", column_names))

#Remove first column
PAM_mam_site_final[,-1]


mam_traits_proc <- mam_traits_final %>% filter(IUCN_species_name %in% clean_column_names$`gsub("_", " ", column_names)`)

write.csv(clean_column_names, "PAM_names.csv")
write.csv(mam_traits_proc, "mam_traits_species_names.csv")

#Functional Diversity 

library(mFD)
# Show table of mammal traits
knitr::kable(head(mam_traits_proc),
             caption = "Species x traits data frame")

# Turn trait dataframe into a matrix
class(mam_traits_proc)
mam_traits <- as.matrix(mam_traits_proc)


# Define row names as species names
row_names <- mam_traits_proc$IUCN_species_name

# Assign row names to the matrix
rownames(mam_traits) <- row_names

# Print the matrix with row names
print(mam_traits)

# Turn back into dataframe
mam_traits_df <-as.data.frame(mam_traits)

# Remove species name from trait row
mam_traits_df <-mam_traits_df[,-1]

#turn data into correct data types for inputs into the trait categories dataframe
mam_traits_df$diet_cat <- as.factor(mam_traits_df$diet_cat)
mam_traits_df$diet_breadth <- as.numeric(mam_traits_df$diet_breadth)
mam_traits_df$body_mass_e <- as.numeric(mam_traits_df$body_mass_e)
mam_traits_df$generation_time<- as.numeric(mam_traits_df$generation_time)
mam_traits_df$habitat_breadth <- factor(mam_traits_df$habitat_breadth, ordered = TRUE)

# Load presence absence table
knitr::kable(as.data.frame(PAM_mam_site_final), 
             centering = TRUE,
             caption = "Species x assemblages matrix based on the **mammal** dataset")


# Assign the trait types (habitat breadth would be ordinal because it is based on IUCN habitat categories)
trait_name <- c("diet_cat", "diet_breadth", "body_mass_e", "generation_time", "habitat_breadth")
trait_type <- c("N","Q","Q","Q","O")

# Create the trait type dataframe
mam_traits_cat <- as.data.frame(cbind(trait_name, trait_type))

# View the trait type table
knitr::kable(head(mam_traits_cat), 
             caption = "Traits types based on **mam traits** dataset")

# Species traits summary:
mam_traits_summ <- mFD::sp.tr.summary(
  tr_cat     = mam_traits_cat,   
  sp_tr      = mam_traits_df, 
  stop_if_NA = TRUE)

# View the trait types
mam_traits_summ$"tr_types"  

# View the traits types for non-continuous traits
mam_traits_summ$"mod_list"                   


# FIX ROWNAMES for the presence absence matrix. Each row needs to have a distinct name (Can't just be a number)
# Get the number of rows in the matrix
num_rows <- nrow(PAM_mam_site_final)

# Generate unique row names
row_names <- paste0("cell", 1:num_rows)

# Assign row names to the matrix
rownames(PAM_mam_site_final) <- row_names

# Print the matrix with row names
print(PAM_mam_site_final)

#remove assemblages with less than 5 species because of issues with creating convux hulls with less than 5 species
# Calculate row sums
row_sums <- rowSums(PAM_mam_site_final)

# Subset the matrix based on the condition
subset_matrix <- PAM_mam_site_final[row_sums >= 5, ]

# Print the subsetted matrix
print(subset_matrix)

# Summary of the assemblages * species dataframe:
asb_sp_mam_summ <- mFD::asb.sp.summary(asb_sp_w = PAM_mam_site_final )

# Occurences 
asb_sp_mam_occ <- asb_sp_mam_summ$"asb_sp_occ"

#richness per cell
asb_sp_mam_summ$"asb_sp_richn"           # Species richness per assemblage

# Names of species present in random assemblage
asb_sp_mam_summ$"asb_sp_nm"[[200]]   

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
#For mammals the mad is lowest at 3 and 5 dimensions. We can therefore go forward with 3 dimensions as this can decrease
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

#computing FD
# The number of species per assemblage has to be higher or equal to the number of traits
alpha_fd_indices_mam <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_mam[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = subset_matrix,
  ind_vect         = c("fide", "fdis", "fmpd", "fnnd", "feve", "fric", "fdiv", "fori", "fspe"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)
)

details_list_mam <- alpha_fd_indices_mam$"details"

#plot
plots_alpha <- mFD::alpha.multidim.plot(
  output_alpha_fd_multidim = alpha_fd_indices_mam,
  plot_asb_nm              = c("cell40", "cell200"),
  ind_nm                   = c("fdis", "fide", "fnnd", 
                               "fdiv", "fori", "fspe"),
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
library(fasterize)

# Create an empty raster with desired resolution and extent
resolution <- c(0.08333333, 0.08333333)  # Set the desired resolution
extent <- c(-91.65305, -57.48638, -22.90167, 12.43166)  # Set the desired extent
empty_raster <- raster(resolution = resolution, xmn = extent[1], xmx = extent[2], ymn = extent[3], ymx = extent[4])

# Generate coordinates
subset_coords_mam <- PAM_mam_site[rowSums(PAM_mam_site_final) >= 5, ]
subset_coords_mam_sp <-subset_coords_mam[,2:3 ]

#Get functional dispersion
fdis_mam <- alpha_fd_indices_mam$functional_diversity_indices$fdis

mam_fd_sp <- data.frame(subset_coords_mam_sp, fdis_mam)

# Convert the dataframe to sf format
spatial_fdis_mam <- st_as_sf(mam_fd_sp, coords = c("Longitude.x.", "Latitude.y."))

# Rasterize the sf data to create the FD raster
fd_raster_mam <- rasterize(spatial_fdis_mam, empty_raster)
setwd("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/Results/richness/")
writeRaster(fd_raster_mam$fdis_mam, "FD_mammals.tif", format="GTiff",overwrite=T)

#Species richness
spec_rich_mam <- alpha_fd_indices_mam$functional_diversity_indices$sp_richn

mam_spec_rich_sp <- data.frame(subset_coords_mam_sp, spec_rich_mam)

# Convert the dataframe to sf format
spatial_spec_rich_mam <- st_as_sf(mam_spec_rich_sp, coords = c("Longitude.x.", "Latitude.y."))

# Rasterize the sf data to create the FD raster
spec_rich_raster_mam <- rasterize(spatial_spec_rich_mam, empty_raster)
setwd("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/Results/richness/")
writeRaster(spec_rich_raster_mam$spec_rich_mam, "Spec_rich_mammals_mFD_pkg.tif", format="GTiff",overwrite=T)


# Understand relationship between species richness and functional diversity
library(ggplot2)

# Diversity metrics
mam_diversity <-  data.frame(spec_rich_mam, fdis_mam)

# Create a scatter plot
ggplot(mam_diversity, aes(x = spec_rich_mam, y = fdis_mam)) +
  geom_line() +
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

#plot mammal diversity as a function of species richness ()
#ggplot(mam_diversity, aes(x = spec_rich_mam, y = fdis_mam)) +
#  geom_point() +
#  geom_smooth(method = "lm", se = FALSE, color = "blue") +
#  xlab("Species Richness (spec_rich_mam)") +
#  ylab("Functional Diversity (fdis_mam)") +
#  ggtitle("Linear Regression: fdis_mam ~ spec_rich_mam")

#FUSE

mam_traits_IUCN <- read.csv("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_diversity/datasets/frugivoria_TA_mammal_subset_impute.csv")
mam_traits_IUCN_final<- mam_traits_IUCN %>% filter(IUCN_species_name %in% mam_traits_proc$IUCN_species_name)


IUCN_status_mammals <- mam_traits_IUCN_final$IUCN_category
IUCN_index <- lets.iucncont(IUCN_status_mammals, dd = .5, ne = NA)
mam_fuse <- fuse(sp_dist_mam, sp_faxes_coord_mam, GE= IUCN_index,standGE = FALSE) # need numerical vector of IUCN statuses 



