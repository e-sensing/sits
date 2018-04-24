# Classification of LANDSAT data
# these are the symbolic links for the files at dropbox

message("Processing of a mixed Landsat 8 - MODIS data set")
message("Processing takes a while, please be patient")
l8m_222068_evi_file <- paste0("/vsicurl/https://www.dropbox.com/s/51d20d1hci55brj/LC8MOD_evi.tif?raw=1")
l8m_222068_ndvi_file <- paste0("/vsicurl/https://www.dropbox.com/s/wn6q55kvth5r3y3/LC8MOD_ndvi.tif?raw=1")
l8m_222068_nir_file <- paste0("/vsicurl/https://www.dropbox.com/s/2r4od7s4zlh383r/LC8MOD_nir.tif?raw=1")

files <- c(l8m_222068_ndvi_file, l8m_222068_evi_file, l8m_222068_nir_file)

# define the timeline

timeline <- timeline_2000_2017 [timeline_2000_2017 >= lubridate::as_date("2015-08-29")]
timeline <- timeline[1:23]

# create a raster metadata file based on the information about the files
raster.tb <- sits_coverage(service = "RASTER", name = "L8MOD-222_68_2015-2016",
                           timeline = timeline, bands = c("ndvi", "evi", "nir"), files = files)

# retrieve the samples from EMBRAPA (used as training sets for classification)

# select a file with 61,000 samples
samples_file <- paste0("https://www.dropbox.com/s/5llfs8w371u6lng/samples_Cerrado_01042018.rda?raw=1")
download.file(samples_file, destfile = "./samples_Cerrado_01042018.rda")
load(file = "./samples_Cerrado_01042018.rda")

# select only the ndvi and evi bands
samples.tb <- sits_select_bands(samples_Cerrado_01042018.tb, bands = c("ndvi", "evi", "nir"))

# create an SVM model
svm_model <- sits_train(samples.tb, ml_method = sits_svm(normalize = TRUE))

message("Please select memory size avaliable for processing in GB (tipical values 1 - 100)")
memsize <- as.integer(readline(prompt = "Enter a memsize value: "))

message("Please select number of cores (tipical values 1 - 32)")
multicores <- as.integer(readline(prompt = "Enter number of cores: "))


# classify the raster image
raster_class.tb <- sits_classify_raster(file = "./L8_MOD_222-068-class", raster.tb, ml_model = svm_model,
                     memsize = memsize, multicores = multicores)

sits_plot_raster(raster_class.tb[1,], title = "LANDSAT-MODIS-222-068-2015-2016")

