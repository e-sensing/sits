# Classification of LANDSAT data
# these are the symbolic links for the files at dropbox

message("Processing of a mixed Landsat 8 - MODIS data set")
message("Please ensure that you have enough memory available")
l8m_222068_evi_file <- paste0("/vsicurl/https://www.dropbox.com/s/oy0vyx2pfruqzz4/LC8MOD_222068_2015-08-29_evi.tif?raw=1")
l8m_222068_ndvi_file <- paste0("/vsicurl/https://www.dropbox.com/s/40hoe5v5hbqcltd/LC8MOD_222068_2015-08-29_ndvi.tif?raw=1")
l8m_222068_nir_file <- paste0("/vsicurl/https://www.dropbox.com/s/1196riyxlwds8hl/LC8MOD_222068_2015-08-29_nir.tif?raw=1")

files <- c(l8m_222068_ndvi_file, l8m_222068_evi_file, l8m_222068_nir_file)

# define the timeline

timeline <- timeline_2000_2017 [timeline_2000_2017 >= lubridate::as_date("2015-08-29")]
timeline <- timeline1[1:23]

# create a raster metadata file based on the information about the files
raster.tb <- sits_coverage(service = "RASTER", name = "L8MOD-222_68_2015-2016",
                           timeline = timeline, bands = c("ndvi", "evi", "nir"), files = files)

# retrieve the samples from EMBRAPA (used as training sets for classification)

samples <- paste0("https://www.dropbox.com/s/p1fhzokfwdoum2m/cerrado_13_classes_col6_adj.rda?raw=1")

download.file(samples, destfile = "./cerrado_13classes_col6_adj.rda")
load(file = "./cerrado_13classes_col6_adj.rda")

#select the bands for classification
samples.tb <- sits_select(samples.tb, bands = c("ndvi", "evi", "nir"))

message("Please select a blocksize (tipical values 10000 - 1000000)")
message("blocksize = (free memory available)/20*nbands*ntimes")
blocksize <- as.integer(readline(prompt = "Enter a blocksize value: "))

message("Please select number of cores (tipical values 1 - 32)")
multicores <- as.integer(readline(prompt = "Enter number of cores: "))


# classify the raster image
sits_classify_raster(file = "./L8_MOD_222-068-class", raster.tb, samples.tb,
                     ml_method = sits_svm(),
                     blocksize = 20000000, multicores = 24)


