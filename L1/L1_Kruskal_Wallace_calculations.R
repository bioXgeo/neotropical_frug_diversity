#Kruskall_Wallace_tests
library(FSA)

# FUSE dataset
bird_count_FUSE <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_fuse_per_park_birds_foraging.csv")
mam_count_FUSE <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_fuse_per_park_mam_foraging.csv")
prop_FUSE <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_prop_fuse_frug_per_park.csv")

# FD dataset
bird_FD <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_fd_per_park_birds_foraging.csv")
mam_FD <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_fd_per_park_mams_foraging.csv")

# TD dataset
mam_TD <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_td_per_park_mams_foraging.csv")
bird_TD <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/avg_td_per_park_birds_foraging.csv")

# Define a function to perform Kruskal-Wallis test and Dunn's test
# Load required libraries
library(FSA)

# Load required libraries
library(FSA)

# Define a function to perform Kruskal-Wallis test and Dunn's test
perform_tests <- function(dataset, column_name) {
  KW_result <- kruskal.test(get(column_name) ~ Category, data = dataset)
  
  if (KW_result$p.value < 0.05) {
    DT_result <- dunnTest(as.formula(paste(column_name, "~ Category")), data = dataset, method = "bh")
    
    DT_result$res$Significance <- ifelse(DT_result$res$P.adj < 0.05, "significant", "not significant")
    
    print(DT_result$res)
  } else {
    print("Kruskal-Wallis test not significant, no post-hoc test performed.")
  }
}

# Call the function for each dataset and column
perform_tests(bird_count_FUSE, "Avg_FUSE_Species")
perform_tests(mam_count_FUSE, "Avg_FUSE_Species")
perform_tests(prop_FUSE, "avg_prop_fuse")

perform_tests(bird_FD, "Avg_fd_birds")
perform_tests(mam_FD, "Avg_fd_mams") # not significant ... stricter protected areas do not protect higher levels of mammal FD

perform_tests(mam_TD, "Avg_td_mams")
perform_tests(bird_TD, "Avg_td_birds")
