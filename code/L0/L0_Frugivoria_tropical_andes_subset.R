#Title: IUCN species list subset

#Project: Functional diversity of frugivores in the Tropical Andes: implications for conservation

#Author: Beth E. Gerstner

#Collaborators: Phoebe L. Zarnetske

#Overview: This script obtains the species list of birds and mammals for relevant countries from the IUCN and subsets by species habitat.

#Data output: IUCN species lists for countries of interest in tropical/subtropical moist montane and tropical/subtropical moist lowland forest - mam_IUCN_2023.csv, bird_IUCN_2023.csv

#Date: June 20th, 2023

# Libraries
library(taxize)
library(httr)
library(jsonlite)
library(dplyr)
library(rredlist)

## Token for accessing IUCN data through their API
## User must obtain a token (https://apiv3.iucnredlist.org/api/v3/token) before using the 'rredlist package'.
token <- '3b3db1c753a7cad0616e16146363ec88eead97d185cb3304070d7343123516fd'
api_url <- 'https://apiv3.iucnredlist.org/api/v3/'

# To obtain country codes of interest:
all_countries <- GET(url = paste0(api_url, 'country/list?token=',token))
all_countries_1 <- fromJSON(content(all_countries, as = 'text'), simplifyDataFrame = TRUE, flatten = TRUE)$result

# Extract all species in the IUCN database by country
# In this example, we are downloading species lists for countries with significant mountain ranges in Central and South America

## South America

#Colombia
colombia_species <- rl_sp_country('CO', key = token, parse = FALSE)
c1 <- colombia_species$result[1]
#Ecuador
ecuador_species <- rl_sp_country("EC", key = token, parse = TRUE)
c2 <- ecuador_species$result[1]
#Peru
peru_species <- rl_sp_country("PE", key = token, parse = TRUE)
c3 <- peru_species$result[1]
#Bolivia
bolivia_species <- rl_sp_country("BO", key = token, parse = TRUE)
c4 <-  bolivia_species$result[1]
#Venezuela
venezuela_species <- rl_sp_country("VE", key = token, parse = TRUE)
c5 <- venezuela_species$result[1]


# Generate a list of all species in the main Tropical Andes
#There will be repeats within these objects because the same species may be present in multiple countries. These should be removed later.
all_species_tropical_andes <- rbind(c1, c2, c3, c4, c5)


## Extract taxa of interest

# Get codes for the different species groups
grp_codes <- GET(url = paste0(api_url, 'comp-group/list?token=', token))
code <-fromJSON(content(grp_codes, as = 'text'), simplifyDataFrame = TRUE, flatten = TRUE)$result

# Get species list of all birds across all countries. 
all_birds <-rl_comp_groups(group = c('birds'), key = token)

# Get species list of all mammals across all countries
all_mammals <-rl_comp_groups(group = c('mammals'), key = token)

## Extract mammals and birds occurring in the Tropical Andes

# Mammals

# Turn the list into a dataframe so the merge can be completed
# This subsets the entire species list to only the mammals included in our country list of species
all_mammals <- as.data.frame(all_mammals$result)
TA_mammals <- merge(all_mammals, all_species_tropical_andes, by="taxonid")
TA_mammals <- distinct(TA_mammals, scientific_name, .keep_all = TRUE)
colnames(TA_mammals)[which(names(TA_mammals) == "scientific_name")] <- "IUCN_species_name"



# Birds
all_birds<- as.data.frame(all_birds$result)
TA_birds <- merge(all_birds, all_species_tropical_andes, by="taxonid")
TA_birds <- distinct(TA_birds, scientific_name, .keep_all = TRUE)
colnames(TA_birds)[which(names(TA_birds) == "scientific_name")] <- "IUCN_species_name"

# Pull in Frugivoria dataset
frugivoria_mammal <- read.csv("G:/Shared drives/SpaCE_Lab_neotropical_frugivores/Manuscripts/Database_Manuscript/Database_paper/EDI_resubmission_2023/databases_2023/Frugivoria_mammal_database_2023_full.csv")
frugivoria_bird <- read.csv("G:/Shared drives/SpaCE_Lab_neotropical_frugivores/Manuscripts/Database_Manuscript/Database_paper/EDI_resubmission_2023/databases_2023/Frugivoria_bird_database_2023_full_1.csv")

# Merge bird and mammal IUCN datasets with Frugivoria datasets to get final species list/dataset for Tropical Andes (retaining only those species that are in Frugivoria)
# 1261 species of frugivore
frugivoria_mam_TA <- frugivoria_mammal%>%
  filter(IUCN_species_name %in% TA_mammals$IUCN_species_name)

frugivoria_bird_TA <- frugivoria_bird%>%
  filter(IUCN_species_name %in% TA_birds$IUCN_species_name)

# Write subsets to file
setwd("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/tropical_subsets")
write.csv(frugivoria_mam_TA, "frugivoria_TA_mammal_subset.csv")
write.csv(frugivoria_bird_TA, "frugivoria_TA_bird_subset.csv")

# For the final run before publication run with this subset
test <- frugivoria_bird[frugivoria_bird$habitat == "1" | frugivoria_bird$habitat == "3", ]
nrow(test)
test_2 <- frugivoria_mammal[frugivoria_mammal$habitat == "1" | frugivoria_mammal$habitat == "3", ]
nrow(test_2)

FUSE_species_mam <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/mam_fuse_foraging.csv")

# Birds
FUSE_species_bird <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/bird_fuse_foraging.csv")

#What species are in the FUSE dataset but not in the correctly subsetted dataset
missing_mam <- FUSE_species_mam %>% filter(!X %in% test_2$IUCN_species_name)#28
missing_bird <- FUSE_species_bird %>% filter(!X %in% test$IUCN_species_name) #32

missing_mam <- missing_mam[1:28,]
missing_bird <- missing_bird[1:32,]

species_to_remove <- rbind(missing_mam, missing_bird)
write.csv(species_to_remove,"FUSE_species_to_remove.csv")
write.csv(missing_bird,"FUSE_bird_to_remove.csv")
write.csv(missing_mam,"FUSE_mam_to_remove.csv")

# Endemic
# For the final run before publication run with this subset
test <- frugivoria_bird[frugivoria_bird$habitat == "1", ] 
nrow(test)
test_2 <- frugivoria_mammal[frugivoria_mammal$habitat == "1", ]
nrow(test_2)
mam_in <- FUSE_species_mam %>% filter(X %in% test_2$IUCN_species_name)#42
bird_in <- FUSE_species_bird %>% filter(X %in% test$IUCN_species_name) #41
mam_endemic_fuse <- mam_in[1:42,]
bird_endemic_fuse <- bird_in[1:41,]

FUSE_species_mam_to <- FUSE_species_mam[1,]
FUSE_species_bird_po <- FUSE_species_bird[1,]

mam_endemic_fuse <- rbind(mam_endemic_fuse, FUSE_species_mam_to) %>%  arrange(desc(FUSE))
bird_endemic_fuse <- rbind(bird_endemic_fuse, FUSE_species_bird_po)  %>%   arrange(desc(FUSE))

write.csv(mam_endemic_fuse, "mam_endemic_fuse.csv")
write.csv(bird_endemic_fuse, "bird_endemic_fuse.csv")

#endemic
84/(86+110)

