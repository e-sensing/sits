# https://geoscripting-wur.github.io/IntroToRaster/
# http://neondataskills.org/R/Raster-Data-In-R/

library (raster)
data_folder <-  "/Users/gilberto/Dropbox/brickBuilder/"
dir (data_folder)
AguaBoa_evi.file <- paste(data_folder, "AguaBoa_evi.tif", sep = "")

AguaBoa_evi <- raster::brick (AguaBoa_evi.file)
