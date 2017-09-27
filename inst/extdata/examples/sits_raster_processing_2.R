files <- c(system.file("extdata/raster/sinop/sinop_crop_ndvi.tif",package = "sits"),
           system.file("extdata/raster/sinop/sinop_crop_evi.tif", package = "sits"))

bands <- c("ndvi", "evi")

scale_factors <- c(0.0001, 0.0001)

timeline <- read.csv(system.file("extdata/raster/sinop/mod13Q1-timeline",package = "sits"),
                     header = FALSE)
timeline <- lubridate::as_date (timeline$V1)

raster.tb <- sits_STRaster (files, timeline, bands, scale_factors)

embrapa.tb <- readRDS(system.file("extdata/time_series/embrapa_mt.rds", package = "sits"))

patterns.tb  <- sits_patterns_from_data(embrapa.tb)

interval <- "12 month"
# produce the breaks used to generate the output rasters
subset_dates.lst <- sits_match_dates(timeline, patterns.tb[1,]$start_date, patterns.tb[1,]$end_date, interval)

file <- "./inst/extdata/raster/classified.tif"

# create the raster objects and their respective filenames
raster_class.tb <- .sits_create_classified_raster(raster.tb, patterns.tb, subset_dates.lst, file)

# get the labels of the data
labels <- sits_labels(patterns.tb)$label

# recover the input data by blocks for efficiency
bs <- raster::blockSize (raster.tb[1,]$r_obj[[1]])

layer.lst <- list()
for (i in 1:nrow(raster_class.tb)) {
    layer <-  raster_class.tb[i,]$r_obj[[1]]
    r_out  <- raster::writeStart(layer, layer@file@name, overwrite = TRUE)
    layer.lst[[length(layer.lst) +  1]] <- r_out
}
for (i in 1:bs$n){
    # find out the size of the block in pixels
    row   <-  bs$row[i]
    print (row)
    nrows <-  bs$nrows[i]
    size  <-  nrows * raster_class.tb[1,]$ncols

    for (j in 1:nrow(raster_class.tb)){
        # start writing
        values <- vector()
        for (i in 1:size)
            values[length(values) + 1] <- 5
        layer.lst[[j]] <- raster::writeValues(layer.lst[[j]], values, row)
    }
}
for (j in 1:nrow(raster_class.tb))
    layer.lst[[j]] <- raster::writeStop(layer.lst[[j]])


