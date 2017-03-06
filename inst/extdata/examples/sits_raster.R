# https://geoscripting-wur.github.io/IntroToRaster/
# http://neondataskills.org/R/Raster-Data-In-R/

library (raster)
data_folder <-  "/Users/gilbertocamara/Dropbox/brickBuilder/"
dir (data_folder)
AguaBoa_evi.file <- paste(data_folder, "AguaBoa_evi.tif", sep = "")

AguaBoa_evi <- raster::brick (AguaBoa_evi.file)

plot(AguaBoa_evi,1)
e <- drawExtent(show=TRUE)

AguaBoa_evi_sub1 <- crop(AguaBoa_evi, e)

AguaBoa_evi_sub1 <- AguaBoa_evi_sub1/10000

writeRaster(AguaBoa_evi_sub1, "./inst/extdata/raster/agua_boa_evi_sub1.tiff")
