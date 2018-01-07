# load sits library
library(sits)


# data access: sits_getdata() function

# WTSS service
URL <- "http://www.dpi.inpe.br/tws/wtss"
wtss_inpe <- sits_infoWTSS(URL)

# get information about a specific coverage
coverage <- "mod13q1_512"
coverage.tb <- sits_coverageWTSS(URL, coverage)


# select a point in the Mato Grosso
longitude <- -55.5027
latitude  <- -11.7181

# choose a coverage
bands <-  c("ndvi", "evi", "nir", "mir")

# retrieve the time series associated with the point from the WTSS server
point.tb <- sits_getdata(latitude = latitude, longitude = longitude,
                         bands = bands,
                         URL = URL, coverage = coverage)
# show the data
point.tb
point.tb$time_series

# plot the series (only the ndvi and evi bands)
point.tb <- sits_select(point.tb, bands = c("ndvi", "evi"))
sits_plot(point.tb)



# raster data source
# select the files for processing
files <- c(system.file("extdata/raster/mod13q1/sinop_ndvi_sample.tif", package = "sits"))
files

# select the bands
bands <- c("ndvi")

# define the scale factors
scale_factors <- c(0.0001)

# create a raster metadata file based on the information about the files
raster.tb <- sits_STRaster(files, timeline, bands, scale_factors)

# show the data
raster.tb

# select a location to read
longitude <- -55.5027
latitude  <- -11.7181

# test the data by reading a point
point.tb <- sits_getdata(raster.tb = raster.tb,
                         longitude = longitude, latitude = latitude)

# plot the time series for the point
sits_plot(point.tb)

# show image
raster::plot(raster.tb$r_obj[[1]])
raster::plot(raster.tb$r_obj[[1]][[1]])
sf::st_point(c(longitude, latitude)) %>%
    sf::st_sfc(crs = "+init=epsg:4326") %>%
    sf::st_transform(crs = raster.tb$crs) %>%
    sf::st_coordinates() %>%
    points()




# retrieve the samples from EMBRAPA (used as training sets for classification)
samples.tb <- readRDS(system.file("extdata/time_series/embrapa_mt.rds", package = "sits"))
samples.tb
sits_labels(samples.tb)

#select the bands for classification
sits_bands(samples.tb)
samples.tb <- sits_select(samples.tb, bands = c("ndvi"))

# plot samples
sits_plot(samples.tb)

# how can we produce a representative time series pattern for each class?
# define the patterns from data with GAM
patterns.tb <- sits_patterns(samples.tb)

# plot the patterns
sits_plot(patterns.tb)

# classification using machine learning
sits_plot(point.tb)

# SVM
classification.tb <-
    sits_classify(point.tb, samples.tb,
                  sits_svm(cost = 1000, kernel = "radial", tolerance = 0.001, epsilon = 0.1))
sits_plot(classification.tb)

# Generalised Linear Models
classification.tb <-
    sits_classify(point.tb, samples.tb, sits_glm())
sits_plot(classification.tb)

# Generalised Linear Models
classification.tb <-
    sits_classify(point.tb, samples.tb, sits_rfor())
sits_plot(classification.tb)

# classify the raster image
system.time({sits_classify_raster(file = "~/Downloads/embrapa-class",
                                  raster.tb, samples.tb,
                                  sits_svm(cost = 1000, kernel = "radial", tolerance = 0.001, epsilon = 0.1),
                                  blocksize = 300000, multicores = 1)})

# Download a larger image
# these are the symbolic links for the files at dropbox
ndvi <- paste0("https://www.dropbox.com/s/epqfo5vdu1cox6i/Sinop_ndvi.tif?raw=1")
evi <- paste0("https://www.dropbox.com/s/xb9embetftxyr6w/Sinop_evi.tif?raw=1")

# read the files to a local directory
download.file(ndvi, dest="~/Downloads/sinop_ndvi.tif")
download.file(evi, dest ="~/Downloads/sinop_evi.tif")

# Classification
