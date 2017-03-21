# https://geoscripting-wur.github.io/IntroToRaster/
# http://neondataskills.org/R/Raster-Data-In-R/
# https://journal.r-project.org/archive/2011-1/RJournal_2011-1_Murrell.pdf

library (raster)
data_folder <-  "/Users/gilberto/Dropbox/brickBuilder/"
dir (data_folder)
AguaBoa_evi.file <- paste(data_folder, "AguaBoa_evi.tif", sep = "")
AguaBoa_ndvi.file <- paste(data_folder, "AguaBoa_ndvi.tif", sep = "")

AguaBoa_evi <- raster::brick (AguaBoa_evi.file)
AguaBoa_ndvi <- raster::brick (AguaBoa_ndvi.file)

plot(AguaBoa_evi, 1, col = gray.colors(10, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL))

e <- drawExtent(show=TRUE)

AguaBoa_evi_sub1 <- crop(AguaBoa_evi, e)
AguaBoa_evi_sub1 <- AguaBoa_evi_sub1/10000

AguaBoa_ndvi_sub1 <- crop(AguaBoa_ndvi, e)
AguaBoa_ndvi_sub1 <- AguaBoa_ndvi_sub1/10000

writeRaster(AguaBoa_evi_sub1, system.file("extdata/raster/agua_boa_evi_sub2.tif", package="sits"))

writeRaster(AguaBoa_ndvi_sub1, system.file("extdata/raster/agua_boa_ndvi_sub2.tif", package="sits"))

json_file <- paste (data_folder, "mod13q1_512.json", sep ="")

modis_info <- jsonlite::fromJSON (json_file)

timeline <- modis_info$MOD13Q1$tiles$h09v07



