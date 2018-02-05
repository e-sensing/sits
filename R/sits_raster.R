#' @title Classify a set of spatio-temporal raster bricks using machine learning models
#' @name sits_classify_raster
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a set of spatio-temporal raster bricks, whose metadata is
#'              described by tibble (created by \code{\link[sits]{sits_coverage}}),
#'              a set of samples used for training a classification model,
#'              a prediction model (created by \code{\link[sits]{sits_train}}),
#'              and produces a classified set of RasterLayers. This function is similar to
#'               \code{\link[sits]{sits_classify}} which is applied to time series stored in a SITS tibble.
#'
#'
#' @param  file            a set of file names to store the output (one file per classified year)
#' @param  raster.tb       a tibble with information about a set of space-time raster bricks
#' @param  samples.tb      The samples used for training the classification model
#' @param  ml_method       a model trained by \code{\link[sits]{sits_train}}
#' @param  adj_fun         Adjustment function to be applied to the data
#' @param  interval        The interval between two sucessive classification
#' @param  blocksize       Default size of the block (rows * cols) (see function .sits_raster_block_size)
#' @param  multicores      Number of threads to process the time series.
#' @param  ...             other parameters to be passed to the distance function
#' @return raster_class.tb a SITS tibble with the metadata for the set of RasterLayers
#'
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(samples_MT_ndvi)
#'
#' # read a raster file and put it into a vector
#' files  <- c(system.file ("extdata/raster/mod13q1/sinop-crop-ndvi.tif", package = "sits"))
#'
#' # define the timeline
#' data(timeline_mod13q1)
#' timeline <- lubridate::as_date (timeline_mod13q1$V1)
#'
#' # create a raster metadata file based on the information about the files
#' raster.tb <- sits_coverageRaster(product = "MOD13Q1", coverage = "Sinop-Crop",
#'              timeline = timeline, bands = c("ndvi"), files = files)
#'
#' # classify the raster file
#' raster_class.tb <- sits_classify_raster (file = "./raster-class", raster.tb, samples_MT_ndvi,
#'    ml_method = sits_svm(), blocksize = 300000, multicores = 1)
#' }
#'
#' @export
sits_classify_raster <- function(file = NULL,
                                 raster.tb,
                                 samples.tb,
                                 ml_method  = sits_svm(),
                                 adj_fun    = sits_adjust(),
                                 interval   = "12 month",
                                 blocksize  = 250000,
                                 multicores = 2){

    # ensure metadata tibble exists
    ensurer::ensure_that(raster.tb, NROW(.) > 0,
                         err_desc = "sits_classify_raster: need a valid metadata for coverage")

    # ensure patterns tibble exits
    .sits_test_tibble(samples.tb)

    # ensure that file name and prediction model are provided
    ensurer::ensure_that(file, !purrr::is_null(.),
                         err_desc = "sits-classify-raster: please provide name of output file")

    # set up the ML model
    ml_model <- sits_train(samples.tb, ml_method = ml_method, adj_fun = adj_fun)

    # create the raster objects and their respective filenames
    raster_class.tb <- .sits_create_classified_raster(raster.tb, samples.tb, file, interval)

    # define the classification info parameters
    class_info.tb <- .sits_class_info(raster.tb, samples.tb, interval)

    # get the labels of the data
    labels <- sits_labels(samples.tb)$label

    # create a named vector with integers match the class labels
    int_labels <- c(1:length(labels))
    names(int_labels) <- labels

    layers.lst <- raster_class.tb$r_objs[[1]]

    #initiate writing
    layers.lst <- layers.lst %>%
        purrr::map(function (layer){
            layer <- raster::writeStart(layer, layer@file@name, overwrite = TRUE)
            return (layer)
        })

    # recover the input data by blocks for efficiency
    bs <- .sits_raster_block_size(raster_class.tb[1,], blocksize)

    # read the input raster in blocks

    for (i in 1:bs$n) {

        # extract time series from the block of RasterBrick rows
        data.mx <- .sits_data_from_block(raster.tb, row = bs$row[i], nrows = bs$nrows[i], adj_fun = adj_fun)

        # classify the time series that are part of the block
        pred.lst <- .sits_classify_block(data.mx, class_info.tb, ml_model, multicores = multicores)

        # write the block back
        layers.lst <- .sits_block_from_data(pred.lst, layers.lst, int_labels, bs$row[i])
    }
    # finish writing
    layers.lst %>%
        purrr::map(function (layer){
            layer <- raster::writeStop(layer)
            return (layer)
        })
    return(raster_class.tb)
}

