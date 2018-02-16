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
#' @param  read_lines      Number of lines of to be read (see function .sits_raster_block_size)
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
#'    ml_method = sits_svm(), read_lines = 50, multicores = 1)
#' }
#'
#' @export
sits_classify_raster <- function(file = NULL,
                                 raster.tb,
                                 samples.tb,
                                 ml_method  = sits_svm(),
                                 adj_fun    = sits_adjust(),
                                 interval   = "12 month",
                                 read_lines = 200,
                                 multicores = 2){

    # ensure metadata tibble exists
    ensurer::ensure_that(raster.tb, NROW(.) > 0,
                         err_desc = "sits_classify_raster: need a valid metadata for coverage")

    # ensure patterns tibble exits
    .sits_test_tibble(samples.tb)

    # ensure that file name and prediction model are provided
    ensurer::ensure_that(file, !purrr::is_null(.),
                         err_desc = "sits-classify-raster: please provide name of output file")

    # estimate the amount of memory required
    blocksize <- read_lines * raster.tb$nrows
    # get the bands
    bands <-  raster.tb$bands[[1]]
    nbands <-  length(bands)

    # size of the timeline
    ntimes <- length(raster.tb$timeline[[1]])

    # estimated memory bloat
    bloat <- .sits_get_memory_bloat()

    # estimated total memory used (in GB)
    memory_req <- round((blocksize * nbands * ntimes * 4 * bloat)/1000000000, digits = 2)
    message(paste0("expected memory use can be as large as ", memory_req," Gb"))
    message("make sure your computer has this memory available")

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

    layers.lst <- sits_get_raster(raster_class.tb)

    #initiate writing
    layers.lst <- layers.lst %>%
        purrr::map(function(layer){
            layer <- raster::writeStart(layer, layer@file@name, overwrite = TRUE)
            return(layer)
        })

    # recover the input data by blocks for efficiency
    bs <- .sits_raster_block_size(raster_class.tb[1,], blocksize)

    # prepare the data required for classification
    time_index.lst <- .sits_get_time_index(class_info.tb)

    # get attribute names
    attr_names <- .sits_get_attr_names(class_info.tb)



    # read the input raster in blocks

    for (i in 1:bs$n) {
        # extract time series from the block of RasterBrick rows
        data.tb <- .sits_data_from_block(raster.tb, row = bs$row[i], nrows = bs$nrows[i], adj_fun = adj_fun)

        layers.lst <- .sits_classify_block_write_data(data.tb, layers.lst, time_index.lst,
                                                      bands, attr_names, int_labels, bs$row[i],
                                                      ml_model, multicores)
    }
    # finish writing
    layers.lst %>%
        purrr::map(function(layer){
            layer <- raster::writeStop(layer)
            return(layer)
        })
    return(raster_class.tb)
}
#' @title Get a raster object from a raster coverage
#' @name sits_get_raster
#' @description This function retrieves one or more raster objects stored in a raster coverage.
#'              It should be used to ensure that the raster objects are returned correctly.
#'
#' @param raster.tb  Raster coverage
#' @param i          i-th element of the list to retrieve
#'
#' @examples
#' # Define a raster Brick and retrieve the associated object
#' # define the file that has the raster brick
#' files  <- c(system.file ("extdata/raster/mod13q1/sinop-crop-ndvi.tif", package = "sits"))
#' # define the timeline
#' data(timeline_mod13q1)
#' timeline <- lubridate::as_date(timeline_mod13q1$V1)
#' # create a raster metadata file based on the information about the files
#' raster_cov <- sits_coverage(files = files, name = "Sinop-crop",
#'                             timeline = timeline, bands = c("ndvi"))
#' # retrieve the raster object associated to the coverage
#' raster_object <- sits_get_raster(raster_cov, 1)
#' @export
#
sits_get_raster <- function(raster.tb, i = NULL) {

    if (purrr::is_null(i))
        return(raster.tb$r_objs[[1]])

    ensurer::ensure_that(i, (.) <= length(raster.tb$r_objs[[1]]),
                         err_desc = "sits_get_raster: index of raster object cannot be retrieved")

    return(raster.tb$r_objs[[1]][[i]])
}

