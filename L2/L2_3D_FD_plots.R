#3d maps

# Load required packages
library(plotly)
library(elevatr)

# Set the coordinates of the points (replace with your own coordinates)
points <-bird_fd_sp[,1:2] 
coordinates(points)= ~ Longitude.x.+ Latitude.y.
srtm <- raster("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/10km_srtm.tif")

# Extract elevation values from the data
elevation <- extract(srtm, points )

bird_fd_sp$elevation <- elevation
bird_fd_sp_rm <- bird_fd_sp[!bird_fd_sp$elevation <0,]

# Load required packages
library(plotly)

# Create data for the contour plot (replace with your own data)
longitude <- bird_fd_sp_rm $Longitude.x.
latitude <- bird_fd_sp_rm$Latitude.y.
elevation <- bird_fd_sp_rm$elevation
functional_diversity <- bird_fd_sp_rm$fdis_bird

# Get the range of functional diversity values
fdiv_min <- min(functional_diversity)
fdiv_max <- max(functional_diversity)
fdiv_range <- seq(fdiv_min, fdiv_max, length.out = 5)  # Adjust the number of ticks as desired

# Create the 3D scatter plot
scatter_plot <- plot_ly(
  x = longitude, y = latitude, z = elevation,
  type = "scatter3d",
  mode = "markers",
  marker = list(
    size = 5,
    color = functional_diversity,
    colorscale = "Viridis",
    showscale = TRUE,
    colorbar = list(title = "Fdis", titleside = "top", len=.6)
  )
)

# Create the 3D surface plot
surface_plot <- plot_ly(
  x = longitude, y = latitude, z = elevation,
  type = "surface",
  showscale = FALSE
)

# Combine the scatter plot and surface plot
fig <- subplot(
  scatter_plot, surface_plot,
  nrows = 2, heights = c(0.6, 0.4),
  margin = 0.04
)

# Customize the plot appearance (optional)
fig <- fig %>% layout(
  scene = list(
    xaxis = list(title = "Longitude"),
    yaxis = list(title = "Latitude"),
    zaxis = list(title = "Elevation"),
    aspectratio = list(x = .7, y = 1, z = .7)  # Adjust the aspect ratio values
  )
)

# Display the plot
fig

## MAMMALS

# Set the coordinates of the points (replace with your own coordinates)
points <-mam_fd_sp[,1:2] 
coordinates(points)= ~ Longitude.x.+ Latitude.y.
srtm <- raster("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/10km_srtm.tif")

# Extract elevation values from the data
elevation <- extract(srtm, points )

mam_fd_sp$elevation <- elevation
mam_fd_sp_rm <- mam_fd_sp[!mam_fd_sp$elevation <0,]

# Load required packages
library(plotly)

# Create data for the contour plot (replace with your own data)
longitude <- mam_fd_sp_rm$Longitude.x.
latitude <- mam_fd_sp_rm$Latitude.y.
elevation <- mam_fd_sp_rm$elevation
functional_diversity <- mam_fd_sp_rm$fdis_mam


# Get the range of functional diversity values
fdiv_min <- min(functional_diversity)
fdiv_max <- max(functional_diversity)
fdiv_range <- seq(fdiv_min, fdiv_max, length.out = 5)  # Adjust the number of ticks as desired

# Create the 3D scatter plot
scatter_plot <- plot_ly(
  x = longitude, y = latitude, z = elevation,
  type = "scatter3d",
  mode = "markers",
  marker = list(
    size = 3,
    color = functional_diversity,
    colorscale = "Viridis",
    showscale = TRUE,
    colorbar = list(title = "Fdis", titleside = "top",  len = 0.6)
  )
)

# Create the 3D surface plot
surface_plot <- plot_ly(
  x = longitude, y = latitude, z = elevation,
  type = "surface",
  showscale = FALSE
)

# Combine the scatter plot and surface plot
fig <- subplot(
  scatter_plot, surface_plot,
  nrows = 2, heights = c(0.6, 0.4),
  margin = 0.04
)

# Customize the plot appearance (optional)
fig <- fig %>% layout(
  scene = list(
    xaxis = list(title = "Longitude"),
    yaxis = list(title = "Latitude"),
    zaxis = list(title = "Elevation"),
    aspectratio = list(x = .7, y = 1, z = .7)  # Adjust the aspect ratio values
  )
)

# Display the plot
fig
 
mam_fd_sp_rm$elevation
#elevation
library(ggpmisc)
ggplot(mam_fd_sp_rm, aes(x = elevation, y = fdis_mam)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., p.value.label,sep = "~~~~")),
    formula = y ~ x,
    parse = TRUE,
    label.x = "left",
    label.y = "bottom"
  ) +
  xlab("Elevation (m)") +
  ylab("Functional Diversity (fdis)") +
  ggtitle("Linear Regression: FD ~ elevation")

#birds

#elevation
library(ggpmisc)
ggplot(bird_fd_sp_rm, aes(x = elevation, y = fdis_bird)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label..,  p.value.label,sep = "~~~~")),
    formula = y ~ x,
    parse = TRUE,
    label.x = "left",
    label.y = "bottom"
  ) +
  xlab("Elevation (m)") +
  ylab("Functional Diversity (fdis)") +
  ggtitle("Linear Regression: FD ~ elevation")
