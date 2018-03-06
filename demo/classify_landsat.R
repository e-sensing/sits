# Classification of LANDSAT data
# these are the symbolic links for the files at dropbox

l8m_222068_evi_file <- paste0("/viscurl/https://www.dropbox.com/s/xukj97v987a50bg/LC8MOD-2015-08-29_evi.tif?raw=1")
l8m_222068_ndvi_file <- paste0("/vsicurl/https://www.dropbox.com/s/p3whp36fghbag5j/LC8MOD-2015-08-29_ndvi.tif?raw=1")
l8m_222068_nir_file <- paste0("/vsicul/https://www.dropbox.com/s/z7gnj6moly12gam/LC8MOD-2015-08-29_nir.tif?raw=1")
# read the files to a local director

files <- c(l8m_222068_ndvi_file, l8m_222068_evi_file, l8m_222068_nir_file)

# define the timeline

timeline <- timeline_2000_2017 [timeline_2000_2017 >= lubridate::as_date("2015-08-29")]
timeline <- timeline1[1:23]

# create a raster metadata file based on the information about the files
raster.tb <- sits_coverage(service = "RASTER", name = "L8MOD-221_65_2013-2014",
                           timeline = timeline, bands = c("ndvi", "evi", "nir"), files = files)

# retrieve the samples from EMBRAPA (used as training sets for classification)

samples <- paste0("https://www.dropbox.com/s/p1fhzokfwdoum2m/cerrado_13_classes_col6_adj.rda?raw=1")

download.file(samples, destfile = "./cerrado_13classes_col6_adj.rda")
load(file = "./cerrado_13classes_col6_adj.rda")

#select the bands for classification
samples.tb <- sits_select(samples.tb, bands = c("ndvi", "evi", "nir"))

# classify the raster image
sits_classify_raster(file = "./L8_MOD_221-065-class", raster.tb, samples.tb,
                     ml_method = sits_svm(),
                     blocksize = 725900, multicores = 16)


