

#Load libraries
library(mice)
library(dplyr)


# Set the seed for reproducibility
set.seed(123)

# Specify the dataframe with missing values
df_real <- read.csv("C:/Users/bgers/Desktop/frugivoria_TA_mammal_subset_subs_rm.csv")
df_real <- df_real %>% select(diet_cat, diet_breadth, body_mass_e, generation_time, habitat_breadth, IUCN_category, for_strat_value_e)

# Function to introduce random missing values in a column
introduce_missing <- function(column, missing_percentage) {
  n_missing <- round(length(column) * missing_percentage)
  missing_indices <- sample(length(column), n_missing)
  column[missing_indices] <- NA
  return(column)
}

# Specify the missing percentage for each column
missing_percentage <- 0.10  # 10% missing values

# Define the number of random iterations
n_iterations <- 1000

# Define the methods
methods <- c("pmm", "rf", "cart")

# Initialize a list to store the standard deviations for each method
standard_deviations <- list()

# Initialize a vector to store the mean standard deviations for each method
mean_standard_deviations <- numeric(length = 3)

# Initialize a vector to store the average differences for each method
average_differences <- numeric(length = 3)

# Perform the imputation and comparison across random iterations (using 3 different methods: PMM, RF, CART)
for (method in methods) {
  # Initialize a vector to store the standard deviations for the current method
  method_standard_deviations <- numeric(n_iterations)
  
  # Initialize a vector to store the average differences for the current method
  method_average_differences <- numeric(n_iterations)
  
  for (i in 1:n_iterations) {
    # Make a copy of the real dataset
    mam_subset_test <- df_real
    
    # Introduce random missing values in the test dataset
    mam_subset_test$generation_time <- introduce_missing(mam_subset_test$generation_time, missing_percentage)
    
    # Convert relevant columns to appropriate data types
    mam_subset_test$generation_time <- as.numeric(mam_subset_test$generation_time)
    mam_subset_test$diet_breadth <- as.numeric(mam_subset_test$diet_breadth)
    mam_subset_test$body_mass_e <- as.numeric(mam_subset_test$body_mass_e)
    mam_subset_test$habitat_breadth <- as.numeric(mam_subset_test$habitat_breadth)
    mam_subset_test$diet_cat <- as.character(mam_subset_test$diet_cat)
    
    # Perform the imputation
    imp_model <- mice(mam_subset_test, method = method, maxit = 20)
    
    # Complete the imputation
    imputed_data <- complete(imp_model)
    
    # Extract the imputed "generation_time" variable
    missing_gen_time <- mam_subset_test$generation_time
    real_gen_time <- df_real$generation_time
    imputed_gen_time <- imputed_data$generation_time
    
    # Calculate the differences between real and imputed values for missing_gen_time
    differences <- imputed_gen_time[is.na(missing_gen_time)] - real_gen_time[is.na(missing_gen_time)]
    
    # Calculate the standard deviation of differences
    standard_deviation <- sd(differences, na.rm = TRUE)
    
    # Store the standard deviation in the vector
    method_standard_deviations[i] <- standard_deviation
    
    # Calculate the average difference for the current method
    method_average_differences[i] <- mean(differences, na.rm = TRUE)
  }
  
  # Store the method-specific standard deviations in the list
  standard_deviations[[method]] <- method_standard_deviations
  
  # Calculate the mean standard deviation for the current method
  mean_standard_deviations[which(methods == method)] <- mean(method_standard_deviations)
  
  # Calculate the average difference for the current method
  average_differences[which(methods == method)] <- mean(method_average_differences)
}

# Compare the standard deviations, mean standard deviations, and average differences across imputation methods
method_names <- c("Mean Matching", "Random Forest", "CART")
for (i in 1:length(standard_deviations)) {
  method_name <- method_names[i]
  method_sd <- standard_deviations[[method_name]]
  
  mean_sd <- mean_standard_deviations[i]
  sd_sd <- sd(method_sd)
  
  mean_diff <- average_differences[i]
  
  cat(paste0(method_name, " - Mean Standard Deviation: ", mean_sd, "/n"))
  cat(paste0(method_name, " - Standard Deviation of Standard Deviation: ", sd_sd, "/n"))
  cat(paste0(method_name, " - Average Difference: ", mean_diff, "/n/n"))
}

## Final imputation for mammals

df_real <- read.csv("C:/Users/bgers/Desktop/frugivoria_TA_mammal_subset_subs_rm.csv")
df_real <- df_real %>% select(IUCN_species_name, diet_cat, diet_breadth, body_mass_e, generation_time, habitat_breadth, IUCN_category, for_strat_value_e)
  
  # Convert relevant columns to appropriate data types
  df_real$generation_time <- as.numeric(df_real$generation_time)
  df_real$diet_breadth <- as.numeric(df_real$diet_breadth)
  df_real$body_mass_e <- as.numeric(df_real$body_mass_e)
  df_real$habitat_breadth <- as.numeric(df_real$habitat_breadth)
  df_real$diet_cat <- as.character(df_real$diet_cat)
  
  # Perform the imputation
  imp_model <- mice(df_real, method = "cart", maxit = 20)
  
  # Complete the imputation
  imputed_data <- complete(imp_model)
  
  #plot imp_model
  plot(imp_model)
  
  #check the distribution of the imputed values
  stripplot(imp_model, generation_time~.imp, pch=20, cex=2)
  
  #Substitute new generation time column into original dataset
  df_real$generation_time <- imputed_data$generation_time
  
  write.csv(df_real, "C:/Users/bgers/Desktop/frugivoria_TA_mammal_subset_impute.csv")
  

  #BIRDS
  df_real <- read.csv("C:/Users/bgers/Desktop/frugivoria_TA_bird_subset_final_elevation.csv")
  df_real <- df_real %>% select(IUCN_species_name, diet_cat_e, diet_breadth, body_mass_e, generation_time, habitat_breadth, IUCN_category)
  
  # Convert relevant columns to appropriate data types
  df_real$generation_time <- as.numeric(df_real$generation_time)
  df_real$diet_breadth <- as.numeric(df_real$diet_breadth)
  df_real$body_mass_e <- as.numeric(df_real$body_mass_e)
  df_real$habitat_breadth <- as.numeric(df_real$habitat_breadth)
  df_real$diet_cat_e <- as.character(df_real$diet_cat_e)
  
  # Perform the imputation
  imp_model <- mice(df_real, method = "cart", maxit = 20)
  
  #plot imp_model
  plot(imp_model)
  
  #check the distribution of the imputed values
  stripplot(imp_model, generation_time~.imp, pch=20, cex=2)

  
  # Complete the imputation
  imputed_data <- complete(imp_model)
  
  #Substitute new generation time column into original dataset
  df_real$generation_time <- imputed_data$generation_time
  
  write.csv(df_real, "C:/Users/bgers/Desktop/frugivoria_TA_bird_subset_impute.csv")

elev <- raster("C:/Users/bgers/Desktop/Joint_richness_aoh_10km.tif")
