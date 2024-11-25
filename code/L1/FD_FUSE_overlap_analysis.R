# FD and FUSE overlap

# Set the threshold percentages
thresholds <- c(10, 20, 30)

# Set the cell resolution
cell_resolution <- .911 * .911  # Area of each cell in km

FUSE_file_birds <- "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/FUSE_species_masked_TA_birds_foraging.tif"
FUSE_file_mammals <-"C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/FUSE_species_masked_TA_mam_foraging.tif"
fd_file_birds <- "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/FD_birds_foraging_TA.tif"
fd_file_mammals <- "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/FD_mams_foraging_TA.tif"

# Function to project raster to UTM
project_to_utm <- function(raster) {
  utm_proj <- "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs"
  projected_raster <- project(raster, utm_proj)
  return(projected_raster)
}
# Read the SR binary raster
FUSE_bird <- rast(FUSE_file_birds)

# Project the SR raster to UTM
FUSE_bird_projected <- project_to_utm(FUSE_bird)

# Calculate the threshold for the top 10% values
threshold_bird_fuse <- quantile(values(FUSE_bird_projected), probs = 1 - (10 / 100), na.rm = TRUE)
# Create a binary mask where values above the threshold are TRUE

FUSE_bird_projected[FUSE_bird_projected < threshold_bird_fuse] <- 0
FUSE_bird_projected[FUSE_bird_projected >= threshold_bird_fuse] <- 1

bird_fuse_cells <-cells(FUSE_bird_projected, c(1))
bird_fuse_count <-length(bird_fuse_cells$FUSE_species_masked_TA_birds_foraging)
# Calculate the overlap area
bird_fuse_area <- bird_fuse_count * (cell_resolution) #144660km

# Read the FD binary raster
fd_raster <- rast(fd_file_birds)

# Project the FD raster to UTM
fd_projected <- project_to_utm(fd_raster)
fd_projected <-project(fd_projected, FUSE_bird_projected)
# Calculate the threshold for the top 10% values
threshold_bird_fd <- quantile(values(fd_projected), probs = 1 - (10 / 100), na.rm = TRUE)
# Create a binary mask where values above the threshold are TRUE

fd_projected[fd_projected < threshold_bird_fd] <- 0
fd_projected[fd_projected>= threshold_bird_fd] <- 1

# Count the number of cells with a value of 1
bird_fd_cells <-cells(fd_projected, c(1))
bird_fuse_count <-length(bird_fd_cells$FD_birds_foraging_TA)
# Calculate the overlap area
bird_fuse_area <- bird_fuse_count * (cell_resolution) #91,731km

# Area of overlap
FD_FUSE_sum <- fd_projected + FUSE_bird_projected
bird_fuse_fd_cells <-cells(FD_FUSE_sum, c(2))
bird_fuse_fd_count <-length(bird_fuse_fd_cells$FD_birds_foraging_TA)
# Calculate the overlap area
bird_fuse_fd_overlap_area <- bird_fuse_fd_count * (cell_resolution) # 16,414km

# all areas above 10% threshold
FD_FUSE_sum <- fd_projected + FUSE_bird_projected
bird_fuse_fd_cells <-cells(FD_FUSE_sum, c(1,2))
bird_fuse_fd_count <-length(bird_fuse_fd_cells$FD_birds_foraging_TA)
# Calculate the overlap area
all_bird_fuse_fd_area <- bird_fuse_fd_count * (cell_resolution) # 208,712

# percent overlapping out of all areas above  
# Read the SR binary raster
FUSE_mam <- rast(FUSE_file_mammals)

# Project the SR raster to UTM
FUSE_mam_projected <- project_to_utm(FUSE_mam)

# Calculate the threshold for the top 10% values
threshold_mam_fuse <- quantile(values(FUSE_mam_projected), probs = 1 - (10 / 100), na.rm = TRUE)
# Create a binary mask where values above the threshold are TRUE

FUSE_mam_projected[FUSE_mam_projected < threshold_mam_fuse] <- 0
FUSE_mam_projected[FUSE_mam_projected >= threshold_mam_fuse] <- 1

mam_fuse_cells <-cells(FUSE_mam_projected, c(1))
mam_fuse_count <-length(mam_fuse_cells$FUSE_species_masked_TA_mam_foraging)
# Calculate the overlap area
mam_fuse_area <- mam_fuse_count * (cell_resolution) #128,895km

# Read the FD binary raster
fd_raster <- rast(fd_file_mammals)

# Project the FD raster to UTM
fd_projected <- project_to_utm(fd_raster)
fd_projected <-project(fd_projected, FUSE_mam_projected)
# Calculate the threshold for the top 10% values
threshold_mam_fd <- quantile(values(fd_projected), probs = 1 - (10 / 100), na.rm = TRUE)
# Create a binary mask where values above the threshold are TRUE

fd_projected[fd_projected < threshold_mam_fd] <- 0
fd_projected[fd_projected>= threshold_mam_fd] <- 1

# Count the number of cells with a value of 1
mam_fd_cells <-cells(fd_projected, c(1))
mam_fuse_count <-length(mam_fd_cells$FD_mams_foraging_TA)
# Calculate the overlap area
mam_fuse_area <- mam_fuse_count * (cell_resolution) #87,567km

# Area of overlap
FD_FUSE_sum <- fd_projected + FUSE_mam_projected
mam_fuse_fd_cells <-cells(FD_FUSE_sum, c(2))
mam_fuse_fd_count <-length(mam_fuse_fd_cells$FD_mams_foraging_TA)
# Calculate the overlap area
mam_fuse_fd_overlap_area <- mam_fuse_fd_count * (cell_resolution) # 11,303km

# all areas above 10% threshold
FD_FUSE_sum <- fd_projected + FUSE_mam_projected
mam_fuse_fd_cells <-cells(FD_FUSE_sum, c(1,2))
mam_fuse_fd_count <-length(mam_fuse_fd_cells$FD_mams_foraging_TA)
# Calculate the overlap area
all_mam_fuse_fd_area <- mam_fuse_fd_count * (cell_resolution) # 193,980

# percent overlapping out of all areas above 10% 
mam_fuse_fd_overlap_area/all_mam_fuse_fd_area #5.8% of all the areas of high FD and high FUSE species this is what overlaps
