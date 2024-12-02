# Title: 3D Visualization and Analysis of Elevation and Functional Diversity in Birds and Mammals
# Project: Project: Evaluating the Effectiveness of Protected Areas and Community-Managed Lands 
#          in Capturing Multiple Dimensions of Frugivorous Biodiversity in the Tropical Andes
# Author: Beth E. Gerstner
# Collaborators: Phoebe L. Zarnetske
# Overview: This script generates 3D maps to visualize the relationship between elevation and functional diversity (FD)
#           for bird and mammal species across the Tropical Andes. It also performs linear regression to assess the 
#           relationship between FD and elevation.
# Outputs:
#   - 3D scatter and surface plots for birds and mammals
#   - Linear regression plots of FD vs. elevation for birds and mammals
#   - Summary of elevation and FD correlations
# Date: September 1, 2023


# Load Required Packages
library(plotly)
library(elevatr)
library(ggpmisc)
library(ggplot2)
library(sp)
library(raster)

# Load Elevation Raster
srtm <- raster("PLACEHOLDER_PATH/richness/10km_srtm.tif")


# Birds: 3D Visualization
# Prepare Data: Extract Elevation
points <- bird_fd_sp[, 1:2]
coordinates(points) <- ~ Longitude.x. + Latitude.y.
elevation <- extract(srtm, points)
bird_fd_sp$elevation <- elevation
bird_fd_sp_rm <- bird_fd_sp[!bird_fd_sp$elevation < 0, ]

# Define Variables for Plot
longitude <- bird_fd_sp_rm$Longitude.x.
latitude <- bird_fd_sp_rm$Latitude.y.
elevation <- bird_fd_sp_rm$elevation
functional_diversity <- bird_fd_sp_rm$fdis_bird

# Create 3D Scatter Plot
scatter_plot <- plot_ly(
  x = longitude, y = latitude, z = elevation,
  type = "scatter3d",
  mode = "markers",
  marker = list(
    size = 5,
    color = functional_diversity,
    colorscale = "Viridis",
    showscale = TRUE,
    colorbar = list(title = "Fdis", titleside = "top", len = 0.6)
  )
)

# Create 3D Surface Plot
surface_plot <- plot_ly(
  x = longitude, y = latitude, z = elevation,
  type = "surface",
  showscale = FALSE
)

# Combine Scatter and Surface Plots
fig <- subplot(
  scatter_plot, surface_plot,
  nrows = 2, heights = c(0.6, 0.4),
  margin = 0.04
)

# Customize and Display the Plot
fig <- fig %>% layout(
  scene = list(
    xaxis = list(title = "Longitude"),
    yaxis = list(title = "Latitude"),
    zaxis = list(title = "Elevation"),
    aspectratio = list(x = 0.7, y = 1, z = 0.7)
  )
)
fig


# Mammals: 3D Visualization
# Prepare Data: Extract Elevation
points <- mam_fd_sp[, 1:2]
coordinates(points) <- ~ Longitude.x. + Latitude.y.
elevation <- extract(srtm, points)
mam_fd_sp$elevation <- elevation
mam_fd_sp_rm <- mam_fd_sp[!mam_fd_sp$elevation < 0, ]

# Define Variables for Plot
longitude <- mam_fd_sp_rm$Longitude.x.
latitude <- mam_fd_sp_rm$Latitude.y.
elevation <- mam_fd_sp_rm$elevation
functional_diversity <- mam_fd_sp_rm$fdis_mam

# Create 3D Scatter Plot
scatter_plot <- plot_ly(
  x = longitude, y = latitude, z = elevation,
  type = "scatter3d",
  mode = "markers",
  marker = list(
    size = 3,
    color = functional_diversity,
    colorscale = "Viridis",
    showscale = TRUE,
    colorbar = list(title = "Fdis", titleside = "top", len = 0.6)
  )
)

# Create 3D Surface Plot
surface_plot <- plot_ly(
  x = longitude, y = latitude, z = elevation,
  type = "surface",
  showscale = FALSE
)

# Combine Scatter and Surface Plots
fig <- subplot(
  scatter_plot, surface_plot,
  nrows = 2, heights = c(0.6, 0.4),
  margin = 0.04
)

# Customize and Display the Plot
fig <- fig %>% layout(
  scene = list(
    xaxis = list(title = "Longitude"),
    yaxis = list(title = "Latitude"),
    zaxis = list(title = "Elevation"),
    aspectratio = list(x = 0.7, y = 1, z = 0.7)
  )
)

# Linear Regression Analysis

# Mammals: Functional Diversity vs. Elevation
ggplot(mam_fd_sp_rm, aes(x = elevation, y = fdis_mam)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., p.value.label, sep = "~~~~")),
    formula = y ~ x,
    parse = TRUE,
    label.x = "left",
    label.y = "bottom"
  ) +
  xlab("Elevation (m)") +
  ylab("Functional Diversity (Fdis)") +
  ggtitle("Linear Regression: FD ~ Elevation (Mammals)")

# Birds: Functional Diversity vs. Elevation
ggplot(bird_fd_sp_rm, aes(x = elevation, y = fdis_bird)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., p.value.label, sep = "~~~~")),
    formula = y ~ x,
    parse = TRUE,
    label.x = "left",
    label.y = "bottom"
  ) +
  xlab("Elevation (m)") +
  ylab("Functional Diversity (Fdis)") +
  ggtitle("Linear Regression: FD ~ Elevation (Birds)")
