
#' @title Extract a time series from a ST raster data set
#' @name sits_fromRaster
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Reads metadata about a raster data set to retrieve a set of
#' time series.
#'
#' @param raster.tb       A tibble with metadata describing a spatio-temporal data set
#' @param file            A CSV file with lat/long locations to be retrieve
#' @param longitude       double - the longitude of the chosen location
#' @param latitude        double - the latitude of the chosen location
#' @param start_date      date - the start of the period
#' @param end_date        date - the end of the period
#' @param label           string - the label to attach to the time series
#' @return data.tb        a SITS tibble with the time series
#'
#' @examples
#'
#' #' # Read a point in a Raster Brick
#' # define the file that has the raster brick
#' files  <- c(system.file ("extdata/raster/mod13q1/sinop-crop-ndvi.tif", package = "sits"))
#' # select the bands
#' bands <- c("ndvi")
#' # define the timeline
#' data(timeline_mod13q1)
#' timeline <- lubridate::as_date (timeline_mod13q1$V1)
#' # create a raster metadata file based on the information about the files
#' raster.tb <- sits_coverageRaster(product = "MOD13Q1", coverage = "Sinop-Crop",
#'              timeline = timeline, bands = c("ndvi"), files = files)
#' # read the point from the raster
#' point.tb <- sits_fromRaster(raster.tb, longitude = -55.55502, latitude = -11.52774)
#'
#' @export
sits_fromRaster <- function(raster.tb,
                            file = NULL,
                            longitude = NULL,
                            latitude = NULL,
                            start_date = NULL,
                            end_date  = NULL,
                            label = "NoClass"){

    # ensure metadata tibble exists
    ensurer::ensure_that(raster.tb, NROW(.) >= 1,
                         err_desc = "sits_classify_raster: need a valid metadata for coverage")

    # get data based on CSV file
    if (!purrr::is_null(file) && tolower(tools::file_ext(file)) == "csv") {
        data.tb <- .sits_ts_fromRasterCSV(raster.tb, file)
    }

    if (!purrr::is_null(longitude) && !purrr::is_null(latitude)) {
        xy <- .sits_latlong_to_proj(longitude, latitude, raster.tb[1, ]$crs)
        data.tb <- .sits_ts_fromRasterXY(raster.tb, xy, longitude, latitude, label)
    }
    return(data.tb)
}
#' @title Classify a set of spatio-temporal raster bricks using machine learning models
#' @name sits_classify_raster
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a set of spatio-temporal raster bricks, whose metadata is
#'              described by tibble (created by \code{\link[sits]{sits_fromRaster}}),
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

    #initiate writing
    for (i in 1:NROW(raster_class.tb))
        raster_class.tb[i,]$r_obj[[1]] <- raster::writeStart(raster_class.tb[i,]$r_obj[[1]],
                                                             raster_class.tb[i,]$r_obj[[1]]@file@name,
                                                             overwrite = TRUE)


    # recover the input data by blocks for efficiency
    bs <- .sits_raster_block_size(raster_class.tb[1,], blocksize)

    # read the input raster in blocks

    for (i in 1:bs$n) {

        # extract time series from the block of RasterBrick rows
        data.mx <- .sits_data_from_block(raster.tb, row = bs$row[i], nrows = bs$nrows[i], adj_fun = adj_fun)

        # classify the time series that are part of the block
        pred.lst <- .sits_classify_block(data.mx, class_info.tb, ml_model, multicores = multicores)

        # write the block back
        raster_class.tb <- .sits_block_from_data(pred.lst, raster_class.tb, int_labels, bs$row[i])
    }
    # finish writing
    for (i in 1:NROW(raster_class.tb))
        raster_class.tb[i,]$r_obj[[1]] <- raster::writeStop(raster_class.tb[i,]$r_obj[[1]])

    return(raster_class.tb)
}
#' @title Create a metadata tibble to store the description of a spatio-temporal raster dataset
#' @name sits_coverageRaster
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description  This function creates a tibble containing the metadata for
#'               a set of spatio-temporal raster files, organized as a set of "Raster Bricks".
#'               These files should be of the same size and
#'               projection. Each raster brick file should contain one band
#'               per time step. Different bands are archived in different raster files.
#'
#' @param  product       The image product where the files are extracted (e.g. MOD13Q1)
#' @param  coverage      The name of the coverage file
#' @param  timeline      Vector of dates with the timeline of the bands
#' @param  bands         The bands contained in the Raster Brick set (in the same order as the files)
#' @param  files         Vector with the file paths of the raster files
#' @return raster.tb     A tibble with metadata information about a raster data set
#'
#'
#' @examples
#' # read a raster file and put it into a vector
#' files  <- c(system.file ("extdata/raster/mod13q1/sinop-crop-ndvi.tif", package = "sits"))
#'
#' # define the timeline
#' data(timeline_mod13q1)
#' timeline <- lubridate::as_date (timeline_mod13q1$V1)
#'
#' # create a raster metadata file based on the information about the files
#' raster.tb <- sits_coverageRaster(product = "MOD13Q1", coverage = "Sinop-crop",
#'              timeline = timeline, bands = c("ndvi"), files = files)
#'
#' @export
sits_coverageRaster <- function(product = "MOD13Q1", coverage = NULL, timeline = NULL, bands, files) {

    ensurer::ensure_that(bands, length(.) == length(files),
                         err_desc = "sits_coverageRaster: number of bands does not match number of files")
    ensurer::ensure_that(coverage, !purrr::is_null(.),
                         err_desc = "sits_coverageRaster: name of the image must be provided")
    ensurer::ensure_that(bands, !purrr::is_null(.),
                         err_desc = "sits_coverageRaster - bands must be provided")
    ensurer::ensure_that(files, !purrr::is_null(.),
                         err_desc = "sits_coverageRaster - files must be provided")

    # get the timeline
    if (purrr::is_null(timeline))
        timeline <- .sits_get_timeline(service = "RASTER", product = product, coverage = coverage)

    # create a list to store the raster objects

    brick.lst <- purrr::pmap(list(files, bands),
                             function(file, band) {
                                 # create a raster object associated to the file
                                 raster.obj <- raster::brick(file)
                                 # find out how many layers the object has
                                 n_layers   <-  raster.obj@file@nbands
                                 # check that there are as many layers as the length of the timeline
                                 ensurer::ensure_that(n_layers, (.) == length(timeline),
                                                      err_desc = "duration of timeline is not matched by number of layers in raster")
                                 # add the object to the raster object list
                                 return(raster.obj)
                             })
    coverage.tb <- .sits_create_raster_coverage(brick.lst   = brick.lst,
                                                service  = "RASTER",
                                                product  = product,
                                                coverage = coverage,
                                                timeline = timeline,
                                                bands    = bands,
                                                files    = files)

    return(coverage.tb)
}

