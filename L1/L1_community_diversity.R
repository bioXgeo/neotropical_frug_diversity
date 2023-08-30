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

##Community
local_prop_fuse <- local_prop_FUSE$avg_prop_fuse
not_local <- not_local_prop_FUSE$avg_prop_fuse
# Perform Mann-Whitney U test
local_fuse_result <- wilcox.test(local_prop_fuse, not_local)

# Check if the p-value is less than your chosen significance level (e.g., 0.05)
if (bird_fuse_result <-bird_fuse_result$p.value < 0.05) {
  cat("The difference in FD between strict and non-strict categories is statistically significant./n")
} else {
  cat("There is no statistically significant difference in FD between strict and non-strict categories./n")
}

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

# Diversity comparison
bird_count_FUSE <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_fuse_per_park_birds_foraging.csv")
parks_non_strict <-bird_count_FUSE[bird_count_FUSE$Category == c("VI", "V"),]
parks_strict <-bird_count_FUSE[bird_count_FUSE$Category == c("I","II","III", "IV"),]

# Load necessary library
library(stats)

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

# Mammals
# Diversity comparison

mam_count_FUSE <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_fuse_per_park_mam_foraging.csv"
)
parks_non_strict <-mam_count_FUSE[mam_count_FUSE$Category == c("VI", "V"),]
parks_strict <-mam_count_FUSE[mam_count_FUSE$Category == c("I","II","III", "IV"),]

# Load necessary library
library(stats)

FUSE_non_strict <- parks_non_strict$Avg_FUSE_Species
FUSE_strict <- parks_strict$Avg_FUSE_Species
# Perform Mann-Whitney U test
mam_fuse_result <- wilcox.test(FUSE_strict, FUSE_non_strict)

#FD
mam_FD <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_fd_per_park_mams_foraging.csv")
parks_non_strict <-mam_FD[mam_FD$Category == c("VI", "V"),]
parks_strict <-mam_FD[mam_FD$Category == c("I","II","III", "IV"),]

# Load necessary library
library(stats)

FD_non_strict <- parks_non_strict$Avg_fd_mams
FD_strict <- parks_strict$Avg_fd_mams
# Perform Mann-Whitney U test
mam_FD_result <- wilcox.test(FD_strict, FD_non_strict)

# Check if the p-value is less than your chosen significance level (e.g., 0.05)
if (mam_FD_result <-mam_FD_result$p.value < 0.05) {
  cat("The difference in FD between strict and non-strict categories is statistically significant./n")
} else {
  cat("There is no statistically significant difference in FD between strict and non-strict categories./n")
}


#FD
bird_FD <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_fd_per_park_birds_foraging.csv")
parks_non_strict <-bird_FD[bird_FD$Category == c("VI", "V"),]
parks_strict <-bird_FD[bird_FD$Category == c("I","II","III", "IV"),]

# Load necessary library
library(stats)

FD_non_strict <- parks_non_strict$Avg_fd_birds
FD_strict <- parks_strict$Avg_fd_birds
# Perform Mann-Whitney U test
bird_FD_result <- wilcox.test(FD_strict, FD_non_strict)

# Check if the p-value is less than your chosen significance level (e.g., 0.05)
if (bird_FD_result <-bird_FD_result$p.value < 0.05) {
  cat("The difference in FD between strict and non-strict categories is statistically significant./n")
} else {
  cat("There is no statistically significant difference in FD between strict and non-strict categories./n")
}

#TD

mam_TD <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_td_per_park_mams_foraging.csv")
parks_non_strict <-mam_TD[mam_TD$Category == c("VI", "V"),]
parks_strict <-mam_TD[mam_TD$Category == c("I","II","III", "IV"),]

# Load necessary library
library(stats)

TD_non_strict <- parks_non_strict$Avg_td_mams
TD_strict <- parks_strict$Avg_td_mams
# Perform Mann-Whitney U test
mam_TD_result <- wilcox.test(TD_strict, TD_non_strict)

# Check if the p-value is less than your chosen significance level (e.g., 0.05)
if (mam_TD_result <-mam_TD_result$p.value < 0.05) {
  cat("The difference in FD between strict and non-strict categories is statistically significant./n")
} else {
  cat("There is no statistically significant difference in FD between strict and non-strict categories./n")
}


#TD
bird_TD <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_td_per_park_birds_foraging.csv")
parks_non_strict <-bird_TD[bird_TD$Category == c("VI", "V"),]
parks_strict <-bird_TD[bird_TD$Category == c("I","II","III", "IV"),]

# Load necessary library
library(stats)

TD_non_strict <- parks_non_strict$Avg_td_birds
TD_strict <- parks_strict$Avg_td_birds
# Perform Mann-Whitney U test
bird_TD_result <- wilcox.test(TD_strict, TD_non_strict)

# Check if the p-value is less than your chosen significance level (e.g., 0.05)
if (bird_TD_result <-bird_TD_result$p.value < 0.05) {
  cat("The difference in FD between strict and non-strict categories is statistically significant./n")
} else {
  cat("There is no statistically significant difference in FD between strict and non-strict categories./n")
}

#integrity
integrity <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_integrity_per_park.csv")
parks_non_strict <-integrity[integrity$Category == c("VI", "V"),]
parks_strict <-integrity[integrity$Category == c("I","II","III", "IV"),]

# Load necessary library
library(stats)

integrity_non_strict <- parks_non_strict$avg_integrity
integrity_strict <- parks_strict$avg_integrity
# Perform Mann-Whitney U test
integrity_result <- wilcox.test(integrity_strict, integrity_non_strict)

# Check if the p-value is less than your chosen significance level (e.g., 0.05)
if (integrity_result <-integrity_result$p.value < 0.05) {
  cat("The difference in FD between strict and non-strict categories is statistically significant./n")
} else {
  cat("There is no statistically significant difference in FD between strict and non-strict categories./n")
}


#FUSE prop
# Load necessary library
library(stats)

prop <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_prop_fuse_frug_per_park.csv")
parks_non_strict <-prop[prop$Category == c("VI", "V"),]
parks_strict <-prop[prop$Category == c("I","II","III", "IV"),]

prop_non_strict <- parks_non_strict$avg_prop_fuse
prop_strict <- parks_strict$avg_prop_fuse
# Perform Mann-Whitney U test
prop_result <- wilcox.test(prop_strict, prop_non_strict)

# Check if the p-value is less than your chosen significance level (e.g., 0.05)
if (integrity_result <-integrity_result$p.value < 0.05) {
  cat("The difference in FD between strict and non-strict categories is statistically significant./n")
} else {
  cat("There is no statistically significant difference in FD between strict and non-strict categories./n")
}

# Compare categories separately 

## FUSE

# Diversity comparison
bird_count_FUSE <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_fuse_per_park_birds_foraging.csv")

# Load necessary library
library(stats)

# Create an empty matrix to store p-values
num_categories <- length(unique(bird_count_FUSE$Category))
p_value_matrix <- matrix(NA, nrow = num_categories, ncol = num_categories)
rownames(p_value_matrix) <- colnames(p_value_matrix) <- unique(bird_count_FUSE$Category)

# Iterate through each combination of park categories
categories <- unique(bird_count_FUSE$Category)
for (i in 1:length(categories)) {
  for (j in 1:length(categories)) {
    category_i <- categories[i]
    category_j <- categories[j]
    
    # Subset data for the two categories being compared
    parks_i <- bird_count_FUSE[bird_count_FUSE$Category == category_i,]
    parks_j <- bird_count_FUSE[bird_count_FUSE$Category == category_j,]
    
    # Extract FUSE values for each category
    FUSE_i <- parks_i$Avg_FUSE_Species
    FUSE_j <- parks_j$Avg_FUSE_Species
    
    # Perform Mann-Whitney U test
    bird_fuse_result <- wilcox.test(FUSE_i, FUSE_j)
    
    # Store the p-value in the matrix
    p_value_matrix[i, j] <- bird_fuse_result$p.value
  }
}

# Print the matrix of p-values
print("Matrix of p-values:")
print(p_value_matrix)

# Create a matrix to indicate significance based on p-value threshold (e.g., 0.05)
significant_matrix_bird_FUSE <- p_value_matrix < 0.05
print("Significant comparisons:")
print(significant_matrix)

#mammals 
mam_count_FUSE <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_fuse_per_park_mam_foraging.csv")

# Load necessary library
library(stats)

# Create an empty matrix to store p-values
num_categories <- length(unique(mam_count_FUSE$Category))
p_value_matrix <- matrix(NA, nrow = num_categories, ncol = num_categories)
rownames(p_value_matrix) <- colnames(p_value_matrix) <- unique(mam_count_FUSE$Category)

# Iterate through each combination of park categories
categories <- unique(mam_count_FUSE$Category)
for (i in 1:length(categories)) {
  for (j in 1:length(categories)) {
    category_i <- categories[i]
    category_j <- categories[j]
    
    # Subset data for the two categories being compared
    parks_i <- mam_count_FUSE[mam_count_FUSE$Category == category_i,]
    parks_j <- mam_count_FUSE[mam_count_FUSE$Category == category_j,]
    
    # Extract FUSE values for each category
    FUSE_i <- parks_i$Avg_FUSE_Species
    FUSE_j <- parks_j$Avg_FUSE_Species
    
    # Perform Mann-Whitney U test
    mam_fuse_result <- wilcox.test(FUSE_i, FUSE_j)
    
    # Store the p-value in the matrix
    p_value_matrix[i, j] <- mam_fuse_result$p.value
  }
}

# Print the matrix of p-values
print("Matrix of p-values:")
print(p_value_matrix)

# Create a matrix to indicate significance based on p-value threshold (e.g., 0.05)
significant_matrix_mam_FUSE <- p_value_matrix < 0.05
print("Significant comparisons:")
print(significant_matrix_mam)

## FD
bird_FD <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_fd_per_park_birds_foraging.csv")

# Create an empty matrix to store p-values
num_categories <- length(unique(bird_FD$Category))
p_value_matrix <- matrix(NA, nrow = num_categories, ncol = num_categories)
rownames(p_value_matrix) <- colnames(p_value_matrix) <- unique(bird_FD$Category)

# Iterate through each combination of park categories
categories <- unique(bird_FD$Category)
for (i in 1:length(categories)) {
  for (j in 1:length(categories)) {
    category_i <- categories[i]
    category_j <- categories[j]
    
    # Subset data for the two categories being compared
    parks_i <- bird_FD[bird_FD$Category == category_i,]
    parks_j <- bird_FD[bird_FD$Category == category_j,]
    
    # Extract FUSE values for each category
    FUSE_i <- parks_i$Avg_fd_bird
    FUSE_j <- parks_j$Avg_fd_bird
    
    # Perform Mann-Whitney U test
    bird_fd_result <- wilcox.test(FUSE_i, FUSE_j)
    
    # Store the p-value in the matrix
    p_value_matrix[i, j] <- bird_fd_result$p.value
  }
}

# Print the matrix of p-values
print("Matrix of p-values:")
print(p_value_matrix)

# Create a matrix to indicate significance based on p-value threshold (e.g., 0.05)
significant_matrix_bird_fd <- p_value_matrix < 0.05
print("Significant comparisons:")
print(significant_matrix_mam)

#mammals

mam_FD <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_fd_per_park_mams_foraging.csv")
# Create an empty matrix to store p-values
num_categories <- length(unique(mam_FD$Category))
p_value_matrix <- matrix(NA, nrow = num_categories, ncol = num_categories)
rownames(p_value_matrix) <- colnames(p_value_matrix) <- unique(mam_FD$Category)

# Iterate through each combination of park categories
categories <- unique(mam_FD$Category)
for (i in 1:length(categories)) {
  for (j in 1:length(categories)) {
    category_i <- categories[i]
    category_j <- categories[j]
    
    # Subset data for the two categories being compared
    parks_i <- mam_FD[mam_FD$Category == category_i,]
    parks_j <- mam_FD[mam_FD$Category == category_j,]
    
    # Extract FUSE values for each category
    FUSE_i <- parks_i$Avg_fd_mams
    FUSE_j <- parks_j$Avg_fd_mams
    
    # Perform Mann-Whitney U test
    mam_fd_result <- wilcox.test(FUSE_i, FUSE_j)
    
    # Store the p-value in the matrix
    p_value_matrix[i, j] <- mam_fd_result$p.value
  }
}

# Print the matrix of p-values
print("Matrix of p-values:")
print(p_value_matrix)

# Create a matrix to indicate significance based on p-value threshold (e.g., 0.05)
significant_matrix_mam_fd <- p_value_matrix < 0.05
print("Significant comparisons:")
print(significant_matrix_mam_fd)



## TD
mam_TD <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_td_per_park_mams_foraging.csv")
# Create an empty matrix to store p-values
num_categories <- length(unique(mam_TD$Category))
p_value_matrix <- matrix(NA, nrow = num_categories, ncol = num_categories)
rownames(p_value_matrix) <- colnames(p_value_matrix) <- unique(mam_TD$Category)

# Iterate through each combination of park categories
categories <- unique(mam_TD$Category)
for (i in 1:length(categories)) {
  for (j in 1:length(categories)) {
    category_i <- categories[i]
    category_j <- categories[j]
    
    # Subset data for the two categories being compared
    parks_i <- mam_TD[mam_TD$Category == category_i,]
    parks_j <- mam_TD[mam_TD$Category == category_j,]
    
    # Extract FUSE values for each category
    FUSE_i <- parks_i$Avg_td_mams
    FUSE_j <- parks_j$Avg_td_mams
    
    # Perform Mann-Whitney U test
    mam_td_result <- wilcox.test(FUSE_i, FUSE_j)
    
    # Store the p-value in the matrix
    p_value_matrix[i, j] <- mam_td_result$p.value
  }
}

# Print the matrix of p-values
print("Matrix of p-values:")
print(p_value_matrix)

# Create a matrix to indicate significance based on p-value threshold (e.g., 0.05)
significant_matrix_mam_TD <- p_value_matrix < 0.05
print("Significant comparisons:")
print(significant_matrix_mam_TD)


#birds 
bird_TD <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_td_per_park_birds_foraging.csv")
# Create an empty matrix to store p-values
num_categories <- length(unique(bird_TD$Category))
p_value_matrix <- matrix(NA, nrow = num_categories, ncol = num_categories)
rownames(p_value_matrix) <- colnames(p_value_matrix) <- unique(bird_TD$Category)

# Iterate through each combination of park categories
categories <- unique(bird_TD$Category)
for (i in 1:length(categories)) {
  for (j in 1:length(categories)) {
    category_i <- categories[i]
    category_j <- categories[j]
    
    # Subset data for the two categories being compared
    parks_i <- bird_TD[bird_TD$Category == category_i,]
    parks_j <- bird_TD[bird_TD$Category == category_j,]
    
    # Extract FUSE values for each category
    FUSE_i <- parks_i$Avg_td_birds
    FUSE_j <- parks_j$Avg_td_birds
    
    # Perform Mann-Whitney U test
    bird_td_result <- wilcox.test(FUSE_i, FUSE_j)
    
    # Store the p-value in the matrix
    p_value_matrix[i, j] <- bird_td_result$p.value
  }
}

# Print the matrix of p-values
print("Matrix of p-values:")
print(p_value_matrix)

# Create a matrix to indicate significance based on p-value threshold (e.g., 0.05)
significant_matrix_bird_TD <- p_value_matrix < 0.05
print("Significant comparisons:")
print(significant_matrix_bird_TD)




## Prop FUSE
prop <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_prop_fuse_frug_per_park.csv")
num_categories <- length(unique(prop$Category))
p_value_matrix <- matrix(NA, nrow = num_categories, ncol = num_categories)
rownames(p_value_matrix) <- colnames(p_value_matrix) <- unique(prop$Category)

# Iterate through each combination of park categories
categories <- unique(prop$Category)
for (i in 1:length(categories)) {
  for (j in 1:length(categories)) {
    category_i <- categories[i]
    category_j <- categories[j]
    
    # Subset data for the two categories being compared
    parks_i <- prop[prop$Category == category_i,]
    parks_j <- prop[prop$Category == category_j,]
    
    # Extract FUSE values for each category
    FUSE_i <- parks_i$avg_prop_fuse
    FUSE_j <- parks_j$avg_prop_fuse
    
    # Perform Mann-Whitney U test
    prop_result <- wilcox.test(FUSE_i, FUSE_j)
    
    # Store the p-value in the matrix
    p_value_matrix[i, j] <- prop_result$p.value
  }
}

# Print the matrix of p-values
print("Matrix of p-values:")
print(p_value_matrix)

# Create a matrix to indicate significance based on p-value threshold (e.g., 0.05)
significant_matrix_prop_FUSE <- p_value_matrix < 0.05
print("Significant comparisons:")
print(significant_matrix_prop_FUSE)

#integrity
#integrity
integrity <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_integrity_per_park.csv")

num_categories <- length(unique(integrity$Category))
p_value_matrix <- matrix(NA, nrow = num_categories, ncol = num_categories)
rownames(p_value_matrix) <- colnames(p_value_matrix) <- unique(integrity$Category)

# Iterate through each combination of park categories
categories <- unique(integrity$Category)
for (i in 1:length(categories)) {
  for (j in 1:length(categories)) {
    category_i <- categories[i]
    category_j <- categories[j]
    
    # Subset data for the two categories being compared
    parks_i <- integrity[integrity$Category == category_i,]
    parks_j <- integrity[integrity$Category == category_j,]
    
    # Extract FUSE values for each category
    FUSE_i <- parks_i$avg_integrity
    FUSE_j <- parks_j$avg_integrity
    
    # Perform Mann-Whitney U test
    integrity_result <- wilcox.test(FUSE_i, FUSE_j)
    
    # Store the p-value in the matrix
    p_value_matrix[i, j] <- integrity_result$p.value
  }
}



# Create a matrix to indicate significance based on p-value threshold (e.g., 0.05)
significant_matrix_integrity <- p_value_matrix < 0.05
print("Significant comparisons:")
print(significant_matrix_integrity)

#calculate median of all categories
# Calculate median of Avg_FUSE_Species for all park categories
median_per_category_FUSE_birds <- aggregate(Avg_FUSE_Species ~ Category, data = bird_count_FUSE, median)
median_per_category_FUSE_mammals <- aggregate(Avg_FUSE_Species ~ Category, data = mam_count_FUSE, median)
median_per_category_fd_mammals <- aggregate(Avg_fd_mams ~ Category, data = mam_FD, median)
median_per_category_fd_birds<- aggregate(Avg_fd_birds ~ Category, data = bird_FD, median)

# Print median values for all park categories
cat("Median values for all park categories:\n")
print(median_per_category)


## KRUSKALL WALLACE
# Load required libraries
library(dunn.test)

# Assuming you have your functional diversity (FD) dataset loaded as 'data' dataframe
# with 'Category' being the IUCN park category and 'FD' being the functional diversity values
integrity <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_integrity_per_park.csv"

# Perform Kruskal-Wallis test
KW_integrity <- kruskal.test(avg_integrity ~ Category,
             data = integrity)

# Perform Dunn's post-hoc test if the Kruskal-Wallis test is significant
library(FSA)

DT_integrity = dunnTest(avg_integrity ~ Category,
              data=integrity,
              method="bh") 

# Perform Dunn's post-hoc test if the Kruskal-Wallis test is significant
  DT_integrity <- dunnTest(avg_integrity ~ Category, data = integrity, method = "bh")
  
  # Add a new column "Significance" with value "significant" where P.adj < 0.05
  DT_integrity$res$Significance <- ifelse(DT_integrity$res$P.adj < 0.05, "significant", "not significant")
  
  print(DT_integrity$res)


