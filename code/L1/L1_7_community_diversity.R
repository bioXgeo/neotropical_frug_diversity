# Title: Park comparisons and community managed lands
# Project: Evaluating the Effectiveness of Protected Areas and Community-Managed Lands 
#          in Capturing Multiple Dimensions of Frugivorous Biodiversity in the Tropical Andes
# Author: Beth E. Gerstner
# Collaborators: Phoebe L. Zarnetske
# Overview: This script performs Mann-Whitney U tests, Kruskal-Wallis tests, and post-hoc Dunn's tests (if applicable) 
#           to evaluate the differences in biodiversity metrics (FUSE, FD, TD, and forest integrity) across park categories and community managed lands.
# Outputs: 
#   - Mann-Whitney U test results for community-managed lands vs. other areas
#   - Comparisons between biodiversity comparisons between strict and less strict areas

# Date: August 15, 2023


# Load Libraries
library(sf)
library(dplyr)
library(stats)

# Load protected area shapefile
parks <- st_read("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/WDPA_ALL/parks_reserves_forest.shp")

# Subset community managed lands
local <- parks[parks$GOV_TYPE == c("Local communities"),]

# Pull in proportion of FUSE
park_prop_FUSE <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_prop_FUSE_frug_per_park.csv")

# Filter FUSE data for community-managed lands
# Subsets the `park_prop_FUSE` dataframe to include only rows corresponding to parks managed by local communities.
local_prop_FUSE <- park_prop_FUSE %>% filter(Park %in% local$NAME)

# Compare FUSE data between community-managed lands and other areas
# Generate subset of `not_local_prop_FUSE` which are not governed by local communities.
not_local_prop_FUSE <- park_prop_FUSE[park_prop_FUSE$Category == c("VI"),]
not_local_prop_FUSE <- not_local_prop_FUSE %>% filter(!Park %in% local$NAME)


# Perform the Mann-Whitney U test
local_fuse_result <- wilcox.test(local_prop_FUSE$avg_prop_fuse, not_local_prop_FUSE$avg_prop_fuse)

# Check if the p-value is less than your chosen significance level (e.g., 0.05)
if (local_fuse_result$p.value < 0.05) {
  cat("The difference in FUSE between community managed and non-community managed lands is statistically significant.\n")
} else {
  cat("There is no statistically significant difference in FUSE between community managed and non-community managed lands.\n")
}


# MAMMALS
# Compare FUSE mammals between community-managed lands and other areas
# Generate subset of `not_local_prop_FUSE` which are not governed by local communities.
mam_count_FUSE <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_fuse_per_park_mam_foraging.csv")
local_mam_count <- mam_count_FUSE %>% filter(Park %in% local$NAME)


# Compare FUSE mammal data between community-managed lands and other areas
# Generate subset of `not_local_prop_FUSE` which are not governed by local communities.
not_local_count_FUSE <- mam_count_FUSE[mam_count_FUSE$Category == "VI",]
not_local_count_FUSE_test <- not_local_count_FUSE %>% filter(!Park %in% local$NAME)


local_mam_fuse_result <- wilcox.test(local_mam_count$Avg_FUSE_Species, not_local_count_FUSE_test$Avg_FUSE_Species) #significant higher outside reserve
# Check if the p-value is less than your chosen significance level (e.g., 0.05)
if (local_mam_fuse_result$p.value < 0.05) {
  cat("The difference in FUSE mammals between community managed and non-community managed lands is statistically significant.\n")
} else {
  cat("There is no statistically significant difference in FUSE mammals between community managed and non-community managed lands.\n")
}


# BIRDS
# Compare FUSE birds between community-managed lands and other areas
bird_count_FUSE <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_fuse_per_park_birds_foraging.csv")
local_bird_count <- bird_count_FUSE %>% filter(Park %in% local$NAME)

# Compare FUSE bird data between community-managed lands and other areas
# Generate subset of `not_local_prop_FUSE` which are not governed by local communities.
not_local_count_FUSE <- bird_count_FUSE[bird_count_FUSE$Category == "VI",]
not_local_count_FUSE_test <- not_local_count_FUSE %>% filter(!Park %in% local$NAME) # not significant

local_bird_fuse_result <- wilcox.test(local_bird_count$Avg_FUSE_Species, not_local_count_FUSE_test$Avg_FUSE_Species)
# Check if the p-value is less than your chosen significance level (e.g., 0.05)
if (local_bird_fuse_result$p.value < 0.05) {
  cat("The difference in FUSE birds between community managed and non-community managed lands is statistically significant.\n")
} else {
  cat("There is no statistically significant difference in FUSE birds between community managed and non-community managed lands.\n")
}


## Diversity comparisons in strict vs less strict PAs
# Birds
bird_count_FUSE <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_fuse_per_park_birds_foraging.csv")

# Non-strict PAs
parks_non_strict <-bird_count_FUSE[bird_count_FUSE$Category == c("VI", "V"),]

# Strict PAs
parks_strict <-bird_count_FUSE[bird_count_FUSE$Category == c("I","II","III", "IV"),]

# Subset to avg FUSE species 
FUSE_non_strict <- parks_non_strict$Avg_FUSE_Species
FUSE_strict <- parks_strict$Avg_FUSE_Species

# Perform Mann-Whitney U test
bird_fuse_result <- wilcox.test(FUSE_strict, FUSE_non_strict)

# Check if the p-value is less than your chosen significance level (e.g., 0.05)
if (bird_fuse_result <-bird_fuse_result$p.value < 0.05) {
  cat("The difference in FD between strict and non-strict categories is statistically significant./n")
} else {
  cat("There is no statistically significant difference in FD between strict and non-strict categories./n")
}

## FD and TD comparisons inside and outside community managed lands
# FD, birds

# Load the shapefile of parks (forest only)
parks <- st_read("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/WDPA_ALL/parks_reserves_forest.shp")
local <- parks[parks$GOV_TYPE == c("Local communities"),]
park_FD_birds <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_fd_per_park_birds_foraging.csv")
local_prop_FD <- park_FD_birds %>% filter(Park %in% local$NAME)

#  Comparisons of bird FD inside and outside community managed lands
not_local_prop_FD <- park_FD_birds[park_FD_birds$Category == c("VI"),]
not_local_FD <- not_local_prop_FD  %>% filter(!Park %in% local$NAME)

# Perform Mann-Whitney U test
bird_FD_result <- wilcox.test(local_prop_FD$Avg_fd_birds, not_local_FD$Avg_fd_birds) # higher in community managed

# Check if the p-value is less than your chosen significance level (e.g., 0.05)
if (bird_FD_result <-bird_FD_result$p.value < 0.05) {
    cat("The difference in birds FD between community managed and non-community managed lands is statistically significant.\n")
  } else {
    cat("There is no statistically significant difference in bird FD between community managed and non-community managed lands.\n")
  }

####
# FOREST INTEGRITY COMPARISONS
forest_integrity <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_integrity_per_park.csv")
local_forest <- forest_integrity %>% filter(Park %in% local$NAME)
not_local_forest <- forest_integrity[forest_integrity$Category == c("VI"),]
not_local_forest <- not_local_forest %>% filter(!Park %in% local$NAME)
FI_result <- wilcox.test(local_forest$avg_integrity, not_local_forest$avg_integrity)
if (FI_result <-FI_result$p.value < 0.05) {
  cat("The difference in Forest Integrity between community managed and non-community managed lands is statistically significant.\n")
} else {
  cat("There is no statistically significant difference in Forest Integrity between community managed and non-community managed lands.\n")
}

# MAMMALS
# FD

# Load the shapefile of parks (forest only)
park_FD_mams <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_fd_per_park_mams_foraging.csv")
local_prop_FD <- park_FD_mams %>% filter(Park %in% local$NAME)


# Comparisons of mammal FD inside and outside community managed lands
not_local_prop_FD <- park_FD_mams[park_FD_mams$Category == c("VI"),]
not_local_prop_FD <- not_local_prop_FD %>% filter(!Park %in% local$NAME)

# Perform Mann-Whitney U test
mam_FD_result <- wilcox.test(local_prop_FD$Avg_fd_mams, not_local_prop_FD$Avg_fd_mams)#significantly higher in community ~5% increase

# Check if the p-value is less than your chosen significance level (e.g., 0.05)
if (mam_FD_result <-mam_FD_result$p.value < 0.05) {
  cat("The difference in mammal FD between community managed and non-community managed lands is statistically significant.\n")
} else {
  cat("There is no statistically significant difference in mammal FD between community managed and non-community managed lands.\n")
}

## Birds
# TD

# Load in avg bird TD per park
park_TD_birds <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_TD_per_park_birds_foraging.csv")
local_prop_TD <- park_TD_birds %>% filter(Park %in% local$NAME)

# Comparisons of bird TD inside and outside community managed lands
not_local_prop_TD <- park_TD_birds[park_TD_birds$Category == c("VI"),]
not_local_prop_TD <- not_local_prop_TD %>% filter(!Park %in% local$NAME)

# Perform Mann-Whitney U test
bird_TD_result <- wilcox.test(local_prop_TD$Avg_td_birds, not_local_prop_TD$Avg_td_birds)

# Check if the p-value is less than your chosen significance level (e.g., 0.05)
if (bird_TD_result <-bird_TD_result$p.value < 0.05) {
  cat("The difference in bird TD between community managed and non-community managed lands is statistically significant.\n")
} else {
  cat("There is no statistically significant difference in bird TD between community managed and non-community managed lands.\n")
}


## Mammals
# TD

# Load the shapefile of parks (forest only)
park_TD_mams <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_TD_per_park_mams_foraging.csv")
local_prop_TD <- park_TD_mams %>% filter(Park %in% local$NAME)

# Comparisons of bird TD inside and outside community managed lands
not_local_prop_TD <- park_TD_mams[park_TD_mams$Category == c("VI"),]
not_local_prop_TD <- not_local_prop_TD %>% filter(!Park %in% local$NAME)

# Perform Mann-Whitney U test
mam_TD_result <- wilcox.test(local_prop_TD$Avg_td_mams, not_local_prop_TD$Avg_td_mams) #significantly different outside community

# Check if the p-value is less than your chosen significance level (e.g., 0.05)
if (mam_TD_result <-mam_TD_result$p.value < 0.05) {
  cat("The difference in mammal TD between community managed and non-community managed lands is statistically significant.\n")
} else {
  cat("There is no statistically significant difference in mammal TD between community managed and non-community managed lands.\n")
}


# Mammals
# Diversity comparisons in strict vs less strict PAs

mam_count_FUSE <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_fuse_per_park_mam_foraging.csv")

# Less strict
parks_non_strict <-mam_count_FUSE[mam_count_FUSE$Category == c("VI", "V"),]

# Strict
parks_strict <-mam_count_FUSE[mam_count_FUSE$Category == c("I","II","III", "IV"),]
FUSE_non_strict <- parks_non_strict$Avg_FUSE_Species
FUSE_strict <- parks_strict$Avg_FUSE_Species

# Perform Mann-Whitney U test
mam_fuse_result <- wilcox.test(FUSE_strict, FUSE_non_strict)
if (mam_fuse_result <-mam_fuse_result$p.value < 0.05) {
  cat("The difference in FUSE mammals between strict and non-strict categories is statistically significant./n")
} else {
  cat("There is no statistically significant difference in FUSE mammals between strict and non-strict categories./n")
}

# Mammal
# FD
mam_FD <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_fd_per_park_mams_foraging.csv")

# Less strict
parks_non_strict <-mam_FD[mam_FD$Category == c("VI", "V"),]

# Strict
parks_strict <-mam_FD[mam_FD$Category == c("I","II","III", "IV"),]
FD_non_strict <- parks_non_strict$Avg_fd_mams
FD_strict <- parks_strict$Avg_fd_mams
# Perform Mann-Whitney U test
mam_FD_result <- wilcox.test(FD_strict, FD_non_strict)

# Check if the p-value is less than your chosen significance level (e.g., 0.05)
if (mam_FD_result <-mam_FD_result$p.value < 0.05) {
  cat("The difference in mammal FD between strict and non-strict categories is statistically significant./n")
} else {
  cat("There is no statistically significant difference in mammal FD between strict and non-strict categories./n")
}


# Bird
# FD
bird_FD <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_fd_per_park_birds_foraging.csv")

# Less strict
parks_non_strict <-bird_FD[bird_FD$Category == c("VI", "V"),]

# Strict
parks_strict <-bird_FD[bird_FD$Category == c("I","II","III", "IV"),]
FD_non_strict <- parks_non_strict$Avg_fd_birds
FD_strict <- parks_strict$Avg_fd_birds

# Perform Mann-Whitney U test
bird_FD_result <- wilcox.test(FD_strict, FD_non_strict)

# Check if the p-value is less than your chosen significance level (e.g., 0.05)
if (bird_FD_result <-bird_FD_result$p.value < 0.05) {
  cat("The difference in bird FD between strict and non-strict categories is statistically significant./n")
} else {
  cat("There is no statistically significant difference in bird FD between strict and non-strict categories./n")
}

# Mammal
# TD
mam_TD <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_td_per_park_mams_foraging.csv")

# Less Strict
parks_non_strict <-mam_TD[mam_TD$Category == c("VI", "V"),]

# Strict
parks_strict <-mam_TD[mam_TD$Category == c("I","II","III", "IV"),]
TD_non_strict <- parks_non_strict$Avg_td_mams
TD_strict <- parks_strict$Avg_td_mams

# Perform Mann-Whitney U test
mam_TD_result <- wilcox.test(TD_strict, TD_non_strict)

# Check if the p-value is less than your chosen significance level (e.g., 0.05)
if (mam_TD_result <-mam_TD_result$p.value < 0.05) {
  cat("The difference in mammal TD between strict and non-strict categories is statistically significant./n")
} else {
  cat("There is no statistically significant difference in mammal TD between strict and non-strict categories./n")
}


# Bird
# TD
bird_TD <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_td_per_park_birds_foraging.csv")

# Less strict
parks_non_strict <-bird_TD[bird_TD$Category == c("VI", "V"),]

# Strict
parks_strict <-bird_TD[bird_TD$Category == c("I","II","III", "IV"),]
TD_non_strict <- parks_non_strict$Avg_td_birds
TD_strict <- parks_strict$Avg_td_birds

# Perform Mann-Whitney U test
bird_TD_result <- wilcox.test(TD_strict, TD_non_strict)

# Check if the p-value is less than your chosen significance level (e.g., 0.05)
if (bird_TD_result <-bird_TD_result$p.value < 0.05) {
  cat("The difference in bird TD between strict and non-strict categories is statistically significant./n")
} else {
  cat("There is no statistically significant difference in bird TD between strict and non-strict categories./n")
}

# Forest Intregrity between strict and less strict
integrity <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_integrity_per_park.csv")
parks_non_strict <-integrity[integrity$Category == c("VI", "V"),]
parks_strict <-integrity[integrity$Category == c("I","II","III", "IV"),]
integrity_non_strict <- parks_non_strict$avg_integrity
integrity_strict <- parks_strict$avg_integrity

# Perform Mann-Whitney U test
integrity_result <- wilcox.test(integrity_strict, integrity_non_strict)

# Check if the p-value is less than your chosen significance level (e.g., 0.05)
if (integrity_result <-integrity_result$p.value < 0.05) {
  cat("The difference in forest integrity between strict and non-strict categories is statistically significant./n")
} else {
  cat("There is no statistically significant difference in forest integrity between strict and non-strict categories./n")
}

# Proportion of FUSE species in strict vs less strict

prop <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_prop_fuse_frug_per_park.csv")

# Less strict
parks_non_strict <-prop[prop$Category == c("VI", "V"),]

# Strict
parks_strict <-prop[prop$Category == c("I","II","III", "IV"),]

prop_non_strict <- parks_non_strict$avg_prop_fuse
prop_strict <- parks_strict$avg_prop_fuse
# Perform Mann-Whitney U test
prop_result <- wilcox.test(prop_strict, prop_non_strict)

# Check if the p-value is less than your chosen significance level (e.g., 0.05)
if (prop_result <-prop_result$p.value < 0.05) {
  cat("The difference in proportion of FUSE species between strict and non-strict categories is statistically significant./n")
} else {
  cat("There is no statistically significant difference in proportion of FUSE species between strict and non-strict categories./n")
}
