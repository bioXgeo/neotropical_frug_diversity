# Title: Imputation of Missing Data for Mammals and Birds

# Project: Evaluating the Effectiveness of Protected Areas and Community-Managed Lands 
#          in Capturing Multiple Dimensions of Frugivorous Biodiversity in the Tropical Andes
# Author: Beth E. Gerstner
# Collaborators: Phoebe L. Zarnetske
# Overview: This script introduces random missing values, performs imputation using 
#           different methods (PMM, RF, CART), evaluates their performance, and applies 
#           the best method (CART) for imputation on mammals and birds datasets.
# Data output: Imputed datasets for mammals and birds with missing values filled 
#              using the CART method.

# Date: June 25, 2023

## Load Libraries
library(mice)
library(dplyr)

## Introduce Random Missing Values

# Set the seed for reproducibility
set.seed(123)

# Load the dataset
df_real <- read.csv("PLACEHOLDER_PATH/mammal_subset.csv")
df_real <- df_real %>%
  select(diet_cat, diet_breadth, body_mass_e, generation_time, habitat_breadth, IUCN_category, for_strat_value_e)

# Function to introduce random missing values in a column
introduce_missing <- function(column, missing_percentage) {
  n_missing <- round(length(column) * missing_percentage)
  missing_indices <- sample(length(column), n_missing)
  column[missing_indices] <- NA
  return(column)
}

# Specify the missing percentage
missing_percentage <- 0.10  # 10% missing values

## Evaluation of Imputation Methods

# Define the number of iterations, methods, and initialize containers
n_iterations <- 1000
methods <- c("pmm", "rf", "cart")
standard_deviations <- list()
mean_standard_deviations <- numeric(length = 3)
sd_se <- numeric(length = 3)
average_differences <- numeric(length = 3)

# Perform imputation and evaluate performance
for (method in methods) {
  method_standard_deviations <- numeric(n_iterations)
  method_average_differences <- numeric(n_iterations)
  
  for (i in 1:n_iterations) {
    # Copy the real dataset and introduce missing values
    mam_subset_test <- df_real
    mam_subset_test$generation_time <- introduce_missing(mam_subset_test$generation_time, missing_percentage)
    
    # Convert relevant columns to appropriate data types
    mam_subset_test <- mam_subset_test %>%
      mutate(
        generation_time = as.numeric(generation_time),
        diet_breadth = as.numeric(diet_breadth),
        body_mass_e = as.numeric(body_mass_e),
        habitat_breadth = as.numeric(habitat_breadth),
        diet_cat = as.character(diet_cat)
      )
    
    # Perform imputation
    imp_model <- mice(mam_subset_test, method = method, maxit = 20)
    imputed_data <- complete(imp_model)
    
    # Calculate differences and standard deviation
    differences <- imputed_data$generation_time[is.na(mam_subset_test$generation_time)] -
      df_real$generation_time[is.na(mam_subset_test$generation_time)]
    method_standard_deviations[i] <- sd(abs(differences), na.rm = TRUE)
    method_average_differences[i] <- mean(abs(differences), na.rm = TRUE)
  }
  
  # Store results for the current method
  standard_deviations[[method]] <- method_standard_deviations
  mean_standard_deviations[which(methods == method)] <- mean(method_standard_deviations)
  average_differences[which(methods == method)] <- mean(method_average_differences)
  sd_se[which(methods == method)] <- sd(method_average_differences)
}

## Compare Imputation Methods

method_names <- c("Mean Matching", "Random Forest", "CART")
for (i in 1:length(standard_deviations)) {
  cat(paste0(method_names[i], " - Mean Standard Deviation: ", mean_standard_deviations[i], "\n"))
  cat(paste0(method_names[i], " - Standard Error of Standard Deviation: ", sd_se[i], "\n"))
  cat(paste0(method_names[i], " - Average Difference: ", average_differences[i], "\n\n"))
}

## Final Imputation for Mammals

df_real <- read.csv("PLACEHOLDER_PATH/mammal_subset_subs_rm.csv")
df_real <- df_real %>%
  select(IUCN_species_name, diet_cat, diet_breadth, body_mass_e, generation_time, habitat_breadth, IUCN_category, for_strat_value_e) %>%
  mutate(
    generation_time = as.numeric(generation_time),
    diet_breadth = as.numeric(diet_breadth),
    body_mass_e = as.numeric(body_mass_e),
    habitat_breadth = as.numeric(habitat_breadth),
    diet_cat = as.character(diet_cat)
  )

imp_model <- mice(df_real, method = "cart", maxit = 20)
imputed_data <- complete(imp_model)

# Plot imputation results
plot(imp_model)
stripplot(imp_model, generation_time ~ .imp, pch = 20, cex = 2)

# Update and save the dataset
df_real$generation_time <- imputed_data$generation_time
write.csv(df_real, "PLACEHOLDER_PATH/mammal_subset_impute.csv")

## Final Imputation for Birds

df_real <- read.csv("PLACEHOLDER_PATH/bird_subset_final_elevation.csv")
df_real <- df_real %>%
  select(IUCN_species_name, diet_cat_e, diet_breadth, body_mass_e, generation_time, habitat_breadth, IUCN_category) %>%
  mutate(
    generation_time = as.numeric(generation_time),
    diet_breadth = as.numeric(diet_breadth),
    body_mass_e = as.numeric(body_mass_e),
    habitat_breadth = as.numeric(habitat_breadth),
    diet_cat_e = as.character(diet_cat_e)
  )

imp_model <- mice(df_real, method = "cart", maxit = 20)
imputed_data <- complete(imp_model)

# Plot imputation results
plot(imp_model)
stripplot(imp_model, generation_time ~ .imp, pch = 20, cex = 2)

# Update and save the dataset
df_real$generation_time <- imputed_data$generation_time
write.csv(df_real, "PLACEHOLDER_PATH/bird_subset_impute.csv")
