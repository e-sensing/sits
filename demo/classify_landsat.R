# Classification of LANDSAT data
# these are the symbolic links for the files at dropbox

message("Processing of a mixed Landsat 8 - MODIS data set")
message("Processing takes a while, please be patient")
evi_file <- paste0("/vsicurl/https://www.dropbox.com/s/dykv1o4ut1d09ok/LC8MODIS_222_068_2015_evi.tif?raw=1")
ndvi_file <- paste0("/vsicurl/https://www.dropbox.com/s/p7z69cjo87xgveu/LC8MODIS_222_068_2015_ndvi.tif?raw=1")
nir_file <- paste0("/vsicurl/https://www.dropbox.com/s/e8lonfuxn6a205d/LC8MODIS_222_068_2015_nir.tif?raw=1")
red_file <- paste0("/vsicurl/https://www.dropbox.com/s/7aig90ikflgyuba/LC8MODIS_222_068_2015_red.tif?raw=1")
mir_file <- paste0("/vsicurl/https://www.dropbox.com/s/wvp7y95gy2y1e4n/LC8MODIS_222_068_2015_swir2.tif?raw=1")

files <- c(ndvi_file, evi_file, red_file, nir_file, mir_file)
bands <- c("ndvi", "evi", "red", "nir", "mir")

# define the timeline

timeline <- timeline_2000_2017 [timeline_2000_2017 >= lubridate::as_date("2015-08-29")]
timeline <- timeline[1:23]

# create a raster metadata file based on the information about the files
raster.tb <- sits_coverage(service = "RASTER", name = "L8MOD-222_068_2015-2016",
                           missing_values = c(0.0, 0.0, 0.0, 0.0, 0.0),
                           minimum_values = c(0.0, 0.0, 0.0, 0.0, 0.0),
                           scale_factors  = c(0.0001, 0.0001, 0.0001, 0.001, 0.0001),
                           timeline = timeline, bands = bands, files = files)

# retrieve a pre-built SVM model
model_file <- paste0("https://www.dropbox.com/s/n4ps99ilmngaqgf/ml_model_Cerrado_25042018_5bands.rda?raw=1")
dest_file  <- paste0(tempdir(),"/ml_model_Cerrado_25042018_5bands.rda")
download.file(model_file, destfile = dest_file)
load(dest_file)

message("Please select memory size avaliable for processing in GB (tipical values 1 - 100)")
memsize <- as.integer(readline(prompt = "Enter a memsize value: "))

# classify the raster image
raster_class.tb <- sits_classify_raster(file = paste0(tempdir(),"/L8_MOD_222-068-class"), raster.tb,
                                        ml_model = svm_model, memsize = memsize)

sits_plot_raster(raster_class.tb[1,], title = "LANDSAT-MODIS-222-068-2015-2016")

