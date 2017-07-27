library(sits)
library(raster)
library(dtwSat)

data_folder = system.file("lucc_MT/data/", package = "dtwSat")
dir(data_folder)

doy <- raster::brick(paste(data_folder,"doy.tif", sep = ""))

dates <- scan(paste(data_folder,"timeline", sep = "/"), what = "dates")

doy1 <- doy[[1]]

doy1[1,1]
