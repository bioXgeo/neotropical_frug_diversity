# Community managed lands

local <- parks[parks$GOV_TYPE == c("Local communities"),]
park_prop_FUSE <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_prop_fuse_frug_per_park.csv")
library(dplyr)
local_prop_FUSE <- park_prop_FUSE %>% filter(Park %in% local$NAME)

forest_integrity <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_integrity_per_park.csv")
local_forest <- forest_integrity %>% filter(Park %in% local$NAME)

forest_integrity_man <- mean(local_forest$avg_integrity)
local_prop_fuse <- mean(local_prop_FUSE$avg_prop_fuse)

#is this higher than other areas?
not_local_prop_FUSE <- park_prop_FUSE %>% filter(!Park %in% local$NAME)
not_local_prop_fuse <- mean(not_local_prop_FUSE$avg_prop_fuse)

not_local_forest <- forest_integrity %>% filter(!Park %in% local$NAME)
not_local_forest_integrity_man <- mean(not_local_forest$avg_integrity)

#within VI
parks_VI <-parks[parks$IUCN_CAT == c("VI"),]
VI_prop_FUSE <- park_prop_FUSE %>% filter(Park %in% parks_VI$NAME)
VI_prop_fuse <- mean(VI_prop_FUSE$avg_prop_fuse)

VI_forest <- forest_integrity %>% filter(Park %in% parks_VI$NAME)
VI_forest_mean <- mean(VI_forest$avg_integrity)

# MAMMALS

local <- parks[parks$GOV_TYPE == c("Local communities"),]
mam_count_FUSE <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_fuse_per_park_mam_foraging.csv")
library(dplyr)
local_mam_count <- mam_count_FUSE %>% filter(Park %in% local$NAME)

forest_integrity <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_integrity_per_park.csv")
local_forest <- forest_integrity %>% filter(Park %in% local$NAME)

forest_integrity_man <- mean(local_forest$avg_integrity)
local_mam_count_fuse<- mean(local_mam_count$Avg_FUSE_Species)

#is this higher than other areas?
not_local_mam_count_FUSE <- mam_count_FUSE %>% filter(!Park %in% local$NAME)
not_local_mam_count_mean <- mean(not_local_prop_FUSE$avg_prop_fuse)

not_local_forest <- forest_integrity %>% filter(!Park %in% local$NAME)
not_local_forest_integrity_man <- mean(not_local_forest$avg_integrity)

#within VI
parks_VI <-parks[parks$IUCN_CAT == c("VI"),]
VI_prop_FUSE <- mam_count_FUSE %>% filter(Park %in% parks_VI$NAME)
VI_prop_fuse <- mean(VI_prop_FUSE$Avg_FUSE_Species)

VI_forest <- forest_integrity %>% filter(Park %in% parks_VI$NAME)
VI_forest_mean <- mean(VI_forest$avg_integrity)



# BIRDS
local <- parks[parks$GOV_TYPE == c("Local communities"),]
bird_count_FUSE <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_fuse_per_park_birds_foraging.csv")
library(dplyr)
local_bird_count <- bird_count_FUSE %>% filter(Park %in% local$NAME)

forest_integrity <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_integrity_per_park.csv")
local_forest <- forest_integrity %>% filter(Park %in% local$NAME)

forest_integrity_man <- mean(local_forest$avg_integrity)
local_bird_count_fuse<- mean(local_bird_count$Avg_FUSE_Species)

#is this higher than other areas?
not_local_mam_count_FUSE <- mam_count_FUSE %>% filter(!Park %in% local$NAME)
not_local_mam_count_mean <- mean(not_local_prop_FUSE$avg_prop_fuse)

not_local_forest <- forest_integrity %>% filter(!Park %in% local$NAME)
not_local_forest_integrity_man <- mean(not_local_forest$avg_integrity)

#within VI
parks_VI <-parks[parks$IUCN_CAT == c("VI"),]
VI_prop_FUSE <- bird_count_FUSE %>% filter(Park %in% parks_VI$NAME)
VI_prop_fuse <- mean(VI_prop_FUSE$Avg_FUSE_Species)

VI_forest <- forest_integrity %>% filter(Park %in% parks_VI$NAME)
VI_forest_mean <- mean(VI_forest$avg_integrity)






