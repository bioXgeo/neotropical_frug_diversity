# Title: Kruskal-Wallis Tests on Biodiversity Metrics
# Project: Evaluating the Effectiveness of Protected Areas and Community-Managed Lands 
#          in Capturing Multiple Dimensions of Frugivorous Biodiversity in the Tropical Andes
# Author: Beth E. Gerstner
# Collaborators: Phoebe L. Zarnetske
# Overview: This script performs Kruskal-Wallis tests and post-hoc Dunn's tests (if applicable) 
#           to evaluate the differences in biodiversity metrics (FUSE, FD, TD) and Forest Integrity across park categories.
# Outputs: 
#   - Kruskal-Wallis test results
#   - Dunn's post-hoc test results (if significant)
# Date: August 15, 2023

# Load Libraries
library(FSA)

# Load Datasets
# Placeholder paths for input datasets
bird_count_FUSE <- read.csv("PLACEHOLDER_PATH/avg_fuse_per_park_birds.csv")
mam_count_FUSE <- read.csv("PLACEHOLDER_PATH/avg_fuse_per_park_mam.csv")
prop_FUSE <- read.csv("PLACEHOLDER_PATH/avg_prop_fuse_frug_per_park.csv")
bird_FD <- read.csv("PLACEHOLDER_PATH/avg_fd_per_park_bird.csv")
mam_FD <- read.csv("PLACEHOLDER_PATH/avg_fd_per_park_mams.csv")
mam_TD <- read.csv("PLACEHOLDER_PATH/avg_td_per_park_mams.csv")
bird_TD <- read.csv("PLACEHOLDER_PATH/avg_td_per_park_birds.csv")
avg_integrity <- read.csv("PLACEHOLDER_PATH/avg_integrity_per_park.csv")

# Function to Perform Kruskal-Wallis Test and Dunn's Test
perform_tests <- function(dataset, column_name) {
  cat("\nPerforming tests for:", column_name, "\n")
  
  # Perform Kruskal-Wallis test
  KW_result <- kruskal.test(get(column_name) ~ Category, data = dataset)
  print(KW_result)
  
  # Perform Dunn's test if Kruskal-Wallis is significant
  if (KW_result$p.value < 0.05) {
    cat("\nKruskal-Wallis test significant (p < 0.05), proceeding with Dunn's post-hoc test...\n")
    DT_result <- dunnTest(as.formula(paste(column_name, "~ Category")), data = dataset, method = "bh")
    
    # Add significance labels to Dunn's test results
    DT_result$res$Significance <- ifelse(DT_result$res$P.adj < 0.05, "significant", "not significant")
    print(DT_result$res)
  } else {
    cat("\nKruskal-Wallis test not significant, no post-hoc test performed.\n")
  }
}

# Perform Tests for Each Metric and Dataset
perform_tests(bird_count_FUSE, "Avg_FUSE_Species")
perform_tests(mam_count_FUSE, "Avg_FUSE_Species")
perform_tests(prop_FUSE, "avg_prop_fuse")

perform_tests(bird_FD, "Avg_fd_birds")
perform_tests(mam_FD, "Avg_fd_mams") # Note: Mammal FD not significant

perform_tests(mam_TD, "Avg_td_mams")
perform_tests(bird_TD, "Avg_td_birds")
perform_tests(avg_integrity, "Avg_forest_integrity")


                      
                      
