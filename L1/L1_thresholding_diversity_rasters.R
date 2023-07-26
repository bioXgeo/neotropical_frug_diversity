#sum some rasters

binary_fd_30 <-raster("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/richness_rasters_binary/thresholded_binary_maps/binary_fd_joint_30.tif")
binary_fd_30[binary_fd_30 < 2] <- 0
binary_fd_30[binary_fd_30 == 2] <- 1


binary_fd_20 <-raster("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/richness_rasters_binary/thresholded_binary_maps/binary_fd_joint_20.tif")
binary_fd_20[binary_fd_20 < 2] <- 0
binary_fd_20[binary_fd_20 == 2] <-1


binary_fd_10 <-raster("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/richness_rasters_binary/thresholded_binary_maps/binary_fd_joint_10.tif")
binary_fd_10[binary_fd_10 < 2] <- 0
binary_fd_10[binary_fd_10 == 2] <- 1


binary_sr_30 <-raster("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/richness_rasters_binary/thresholded_binary_maps/binary_sr_joint_30.tif")
binary_sr_30[binary_sr_30 < 2] <- 0
binary_sr_30[binary_sr_30 == 2] <- 1

binary_sr_20 <-raster("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/richness_rasters_binary/thresholded_binary_maps/binary_sr_joint_20.tif")
binary_sr_20[binary_sr_20 < 2] <- 0
binary_sr_20[binary_sr_20 == 2] <- 1

binary_sr_10 <-raster("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/richness_rasters_binary/thresholded_binary_maps/binary_sr_joint_10.tif")
binary_sr_10[binary_sr_10 < 2] <- 0
binary_sr_10[binary_sr_10 == 2] <- 1


#only retain values of 3 or 4 and replace with 0 and 1 ### 1 is fd or sr not BOTH in this case
binary_fd_sr_10 <- binary_sr_10 + binary_fd_10
#missing 2 because no overlap


binary_fd_sr_20 <- binary_sr_20 + binary_fd_20



binary_fd_sr_30 <- binary_sr_30 + binary_fd_30




setwd("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_3/richness/richness_rasters_binary")

writeRaster(binary_fd_sr_30, filename = "diversity_joint_sum_30.tif", format = "GTiff", overwrite = TRUE)
writeRaster(binary_fd_sr_20, filename = "diversity_joint_sum_20.tif", format = "GTiff", overwrite = TRUE)
writeRaster(binary_fd_sr_10, filename = "diversity_joint_sum_10.tif", format = "GTiff", overwrite = TRUE) #max val of 1 (no overlap)
writeRaster(binary_fd_10, filename = "binary_fd_joint_10.tif", format = "GTiff", overwrite = TRUE) #max val of 1 (no overlap)
writeRaster(binary_fd_20, filename = "binary_fd_joint_20.tif", format = "GTiff", overwrite = TRUE) #max val of 1 (no overlap)
writeRaster(binary_fd_30, filename = "binary_fd_joint_30.tif", format = "GTiff", overwrite = TRUE) #max val of 1 (no overlap)
writeRaster(binary_sr_10, filename = "binary_sr_joint_10.tif", format = "GTiff", overwrite = TRUE) #max val of 1 (no overlap)
writeRaster(binary_sr_20, filename = "binary_sr_joint_20.tif", format = "GTiff", overwrite = TRUE) #max val of 1 (no overlap)
writeRaster(binary_sr_30, filename = "binary_sr_joint_30.tif", format = "GTiff", overwrite = TRUE) #max val of 1 (no overlap)






