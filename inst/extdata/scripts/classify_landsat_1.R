# Classification of LANDSAT data

l8m_222068_evi_file <- paste0("/users/gilberto/Dropbox/RasterData/LC8MOD-2015-08-29_evi.tif")
l8m_222068_ndvi_file <- paste0("/users/gilberto/Dropbox/RasterData/LC8MOD-2015-08-29_ndvi.tif")
l8m_222068_nir_file <- paste0("/users/gilberto/Dropbox/RasterData/LC8MOD-2015-08-29_nir.tif")

files <- c(l8m_222068_ndvi_file, l8m_222068_evi_file, l8m_222068_nir_file)

# define the timeline

timeline <- timeline_2000_2017 [timeline_2000_2017 >= lubridate::as_date("2015-08-29")]
timeline <- timeline[1:23]

# create a raster metadata file based on the information about the files
raster.tb <- sits_coverage(service = "RASTER", name = "L8MOD-228068_2015-2016",
                           timeline = timeline, bands = c("ndvi", "evi", "nir"), files = files)

# retrieve the samples from EMBRAPA (used as training sets for classification)

load(file = "/users/gilberto/Dropbox/TWDTWAmazoniaCerrado/Samples/MODIS-col6/cerrado_13_classes_col6_adj.rda")

#select the bands for classification
samples2.tb  <- sits_select(samples.tb, bands = c("ndvi", "evi", "nir"))


samples3.tb <- dplyr::filter(samples2.tb, !(label %in% c("Cerrado_Campo", "Corn_Cotton",
                                                       "Cerrado_Rupestre",
                                                       "Millet_Cotton")) )

# classify the raster image
raster.class.tb <- sits_classify_raster(file = "./L8_MOD_226068-class", raster.tb, samples2.tb,
                     ml_method = sits_svm(),
                     blocksize = 1000000, multicores = 2)


