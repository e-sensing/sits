#' @title Classify a sits tibble using machine learning models
#' @name sits_classify
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function classifies a set of time series, given
#' a set of training samples, an inference model, and an interval.
#' To perform the classification, users should provide a set of
#' labelled samples. Each samples should be associated to one spatial location
#' (latitude/longitude), one time interval and a label.
#'
#' After defining the training samples, the users need to provide a machine learning model.
#' Currenly, sits supports the following models:
#' 'svm' (see \code{\link[sits]{sits_svm}}), 'random forest' (see \code{\link[sits]{sits_rfor}}),
#' 'boosting' (see \code{\link[sits]{sits_gbm}}), 'lda' (see \code{\link[sits]{sits_lda}}),
#' 'qda' (see \code{\link[sits]{sits_qda}}), multinomial logit' (see \code{\link[sits]{sits_mlr}}),
#' 'lasso' (see \code{\link[sits]{sits_mlr}}), and 'ridge' (see \code{\link[sits]{sits_mlr}}).
#'
#' The default is to use an SVM with a radial kernel, but users are encouraged to test
#' alternatives.
#'
#'
#' @param  data.tb           SITS tibble time series (cleaned)
#' @param  train_samples.tb  The samples used for training the classification model
#' @param  ml_method         A machine learning method (see \code{\link[sits]{sits_train}})
#' @param  dist_method       The method to obtain the values to be used from training and classification
#' @param  interval          The interval used for classification
#' @param  multicores        Number of threads to process the time series.
#' @return data.tb           SITS tibble with the predicted labels for each input segment
#' @examples
#'
#' # read a training data set
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' samples_ndvi.tb <- readRDS(system.file("extdata/time_series/samples_mt_ndvi.rds", package = "sits"))
#' # Retrieve a point
#' point_ndvi.tb <- readRDS(system.file("extdata/time_series/point_ndvi.rds", package = "sits"))
#' # classify the point
#' class_ndvi.tb <-  sits_classify (point_ndvi.tb, samples_ndvi.tb)
#' #
#' # plot the classification
#' sits_plot (class_ndvi.tb)
#'
#' \donttest{
#' # define the files that make up a RasterBrick
#' files  <- c(system.file ("extdata/mod13q1/sinop_ndvi_sample.tif", package = "sits"))
#' # read the timeline associated to a RasterBrick
#' timeline <- read.csv(system.file("extdata/mod13q1/timeline.csv", package = "sits"), header = FALSE)
#' timeline <- lubridate::as_date (timeline$V1)
#' # create a raster metadata file based on the information about the files
#' raster.tb <- sits_STRaster (files, timeline, bands = c("ndvi"), scale_factors = c(0.0001))
#' # read a point from the raster
#' point.tb <- sits_getdata(raster.tb, longitude = -55.50563, latitude = -11.71557)
#' # classify the point
#' class.tb <-  sits_classify (point.tb, samples_ndvi.tb)
#' # plot the classification
#' sits_plot (class.tb)
#'
#' # Alternative - read the point from the WTSS server
#' point2.tb <- sits_getdata (longitude = -46.4070, latitude = -10.8630)
#'
#' # select the ndvi
#' point2.tb <- sits_select (point2.tb, bands = c("ndvi"))
#' # classify the point
#' class2.tb <-  sits_classify (point2.tb, samples_ndvi.tb)
#' # plot the classification
#' sits_plot (class2.tb)
#' }
#'
#' @export
sits_classify <- function (data.tb = NULL,  train_samples.tb = NULL,
                           ml_method = sits_svm(kernel = "radial", cost = 10) ,
                           dist_method = sits_distances_from_data(),
                           interval = "12 month", multicores = 1){

    .sits_test_tibble(data.tb)
    .sits_test_tibble(train_samples.tb)

    n_samples <- nrow(train_samples.tb[1,]$time_series[[1]])


    # obtain the machine learning model
    distances.tb <- dist_method (train_samples.tb)

    ml_model = sits_train (distances.tb, ml_method)

    # define the classification info parameters
    class_info.tb <- .sits_class_info(data.tb, train_samples.tb, interval)

    # find the subsets of the input data
    ref_dates.lst <- class_info.tb$ref_dates[[1]]

    # obtain the distances from the data
    distances.tb <- dist_method (data.tb)

    # create a vector to store the predicted results
    predict.vec <- .sits_classify_distances (distances.tb, class_info.tb, ml_model, multicores)

    # Store the result in the input data
    data.tb <- .sits_tibble_prediction(data.tb, class_info.tb, predict.vec, interval)

    return(data.tb)
}
#' @title Classify a distances tibble using machine learning models
#' @name .sits_classify_distances
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns a sits table with the results of the ML classifier.
#'
#' @param  distances.tb    a tibble with distances
#' @param  class_info.tb   a tibble with the information on classification
#' @param  ml_model        a model trained by \code{\link[sits]{sits_train}}
#' @param  multicores      Number of threads to process the time series.
#' @return pred.vec        vector with the predicted labels
.sits_classify_distances <- function (distances.tb, class_info.tb, ml_model = NULL, multicores = 1){

    ensurer::ensure_that(ml_model,  !purrr::is_null(.), err_desc = "sits-classify: please provide a machine learning model already trained")

    # find the subsets of the input data
    dates_index.lst <- class_info.tb$dates_index[[1]]

    # find the number of the samples
    nsamples <- class_info.tb$num_samples

    #retrieve the timeline of the data
    timeline <- class_info.tb$timeline[[1]]

    # retrieve the bands
    bands <- class_info.tb$bands[[1]]

    #retrieve the time index
    time_index.lst  <- .sits_time_index(dates_index.lst, timeline, bands)

    # add a shift of two positions to the time index
    time_index.lst  <- time_index.lst %>%
        purrr::map(function (ti){
            ti <- ti + 2
        })

    # define the column names
    attr_names <- vector()
    bands %>%
        purrr::map (function (b){
            attr_names <<- c(attr_names, paste (c(rep(b, nsamples)), as.character(c(1:nsamples)), sep = ""))
        })

    attr_names <- c("original_row", "reference", attr_names)

    size_dist <- nrow (distances.tb) * length(time_index.lst)

    # create a data table to store the distances
    dist.tb <- data.table::data.table(nrow = 0, ncol = 2 + length(bands)*nsamples)

    # classify a block of data
    classify_block <- function (distances.tb) {
        # create a list to get the predictions

                # create a list to store the rows
        row.lst <- list()
        # create a block of distances to be classified
        for (r in 1:nrow(distances.tb)){
            # create a data table to store the values of the distances
            for (t in 1:length(time_index.lst)){
                # create a data table to store the distances for each row
                row.tb <- data.table::data.table("original_row" = 1, "reference" = "NoClass")
                idx <- time_index.lst[[t]]
                for (b in 1:length(bands)){
                    # retrieve the values used for classification
                    row.tb <- cbind (row.tb, distances.tb[r,idx[(2*b - 1)]:idx[2*b]])
                }
                row.lst[[length(row.lst) + 1 ]] <- row.tb
            }
        }
        dist.tb <- data.table::rbindlist(row.lst)
        colnames(dist.tb) <- attr_names
        # classify the subset data
        pred_block.vec <- sits_predict(dist.tb, ml_model)
        return(pred_block.vec)
    }

    join_blocks <- function (blocks.lst) {

        pred.vec <- vector()
        blocks.lst %>%
            purrr::map (function (block){
                pred.vec <<- c(pred.vec, block )
            })
        return (pred.vec)
    }

    if (multicores > 1) {
        blocks.lst <- split.data.frame(distances.tb, cut(1:nrow(distances.tb),multicores, labels = FALSE))
        # apply parallel processing to the split dat
        results.lst <- parallel::mclapply(blocks.lst, classify_block, mc.cores = multicores)

        pred.vec <- join_blocks(results.lst)
    }
    else
        pred.vec <- classify_block(distances.tb)

    return (pred.vec)
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
#' @param  raster.tb       a tibble with information about a set of space-time raster bricks
#' @param  file            a set of file names to store the output (one file per classified year)
#' @param  samples.tb      The samples used for training the classification model
#' @param  ml_method       a model trained by \code{\link[sits]{sits_train}}
#' @param  dist_method     The method to obtain the values to be used from training and classification
#' @param  interval        The interval between two sucessive classification
#' @param  blocksize       Default size of the block (rows * cols) (see function .sits_raster_block_size)
#' @param  multicores      Number of threads to process the time series.
#' @param  ...             other parameters to be passed to the distance function
#' @return raster_class.tb a SITS tibble with the metadata for the set of RasterLayers
#'
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' samples.tb <- readRDS(system.file("extdata/time_series/samples_mt_ndvi.rds", package = "sits"))
#'
#' # read a raster file and put it into a vector
#' files  <- c(system.file ("extdata/mod13q1/sinop_ndvi_sample.tif", package = "sits"))
#'
#' # define the timeline
#' timeline <- read.csv(system.file("extdata/mod13q1/timeline.csv", package = "sits"), header = FALSE)
#' timeline <- lubridate::as_date (timeline$V1)
#'
#' # create a raster metadata file based on the information about the files
#' raster.tb <- sits_STRaster (files, timeline, bands = c("ndvi"), scale_factors = c(0.0001))
#'
#' # classify the raster file
#' raster_class.tb <- sits_classify_raster (file = "./raster-class", raster.tb, samples.tb,
#'    ml_method = sits_svm(), blocksize = 300000, multicores = 2)
#'
#' # Process a larger-scale raster image brick
#' # these are the symbolic links for the files at dropbox
#' ndvi <- paste0("https://www.dropbox.com/s/epqfo5vdu1cox6i/Sinop_ndvi.tif?raw=1")
#' evi <- paste0("https://www.dropbox.com/s/xb9embetftxyr6w/Sinop_evi.tif?raw=1")
#'
#' # read the files to a local directory
#' download.file(ndvi, dest="./Sinop_ndvi.tif")
#' download.file(evi, dest ="./Sinop_evi.tif")
#'
#' # select the files for processing
#' files <- c("./Sinop_ndvi.tif", "./Sinop_evi.tif")
#'
#' # define the timeline
#' # read the timeline associated to a RasterBrick
#' timeline <- read.csv(system.file("extdata/mod13q1/timeline.csv", package = "sits"), header = FALSE)
#' timeline <- lubridate::as_date (timeline$V1)
#'
#' # define the bands
#' bands <- c("ndvi", "evi")
#' # define the scale factors
#' scale_factors <-  c(0.0001, 0.0001)
#'
#' # create a raster metadata file based on the information about the files
#' raster.tb <- sits_STRaster (files, timeline, bands, scale_factors)
#'
#' # retrieve the samples from EMBRAPA (used as training sets for classification)
#' samples.tb <- readRDS(system.file("extdata/time_series/embrapa_mt.rds", package = "sits"))
#'
#' #select the bands for classification
#' samples.tb <- sits_select(samples.tb, bands = c("ndvi", "evi"))
#'
#' # classify the raster image
#' sits_classify_raster (file = "./sinop-class", raster.tb, samples.tb,
#'      ml_method = sits_svm (cost = 1000, kernel = "radial", tolerance = 0.001, epsilon = 0.1),
#'      blocksize = 300000, multicores = 2)
#'
#' }
#'
#' @export
sits_classify_raster <- function (file = NULL, raster.tb,  samples.tb, ml_method = sits_svm(),
                                  dist_method = sits_distances_from_data(),
                                  interval = "12 month",
                                  blocksize = 250000, multicores = 2){

    # ensure metadata tibble exists
    .sits_test_tibble (raster.tb)
    # ensure patterns tibble exits
    .sits_test_tibble (samples.tb)

    # ensure that file name and prediction model are provided
    ensurer::ensure_that(file,      !purrr::is_null(.), err_desc = "sits-classify-raster: please provide name of output file")

    # set up the ML model
    distances.tb <- dist_method (samples.tb)
    ml_model <- sits_train (distances.tb, ml_method)

    # create the raster objects and their respective filenames
    raster_class.tb <- .sits_create_classified_raster(raster.tb, samples.tb, file, interval)

    # define the classification info parameters
    class_info.tb <- .sits_class_info(raster.tb, samples.tb, interval)

    # get the labels of the data
    labels <- sits_labels(samples.tb)$label

    # create a named vector with integers match the class labels
    int_labels <- c(1:length(labels))
    names (int_labels) <- labels

    #initiate writing
    raster_class.tb$r_obj <- raster_class.tb$r_obj %>%
        purrr::map(function (layer) {
            raster::writeStart(layer, layer@file@name, overwrite = TRUE)
        })

    # recover the input data by blocks for efficiency
    bs <- .sits_raster_block_size (raster_class.tb[1,], blocksize)

    # read the input raster in blocks

    for (i in 1:bs$n){

        # extract time series from the block of RasterBrick rows
        data.mx <- .sits_data_from_block (raster.tb, row = bs$row[i], nrows = bs$nrows[i])

        # classify the time series that are part of the block
        pred.lst <- .sits_classify_block(data.mx, class_info.tb, ml_model, multicores = multicores)

        # write the block back
        raster_class.tb <- .sits_block_from_data (pred.lst, raster_class.tb, int_labels, bs$row[i])
    }
    # finish writing
    raster_class.tb$r_obj <- raster_class.tb$r_obj %>%
        purrr::map(function (layer) {
            raster::writeStop(layer)
        })
    return (raster_class.tb)
}

#' @title Classify a distances matrix extracted from raster using machine learning models
#' @name .sits_classify_block
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns a sits table with the results of the ML classifier.
#'
#' @param  data.mx         a matrix with data values
#' @param  class_info.tb   a tibble with the information on classification
#' @param  ml_model        a model trained by \code{\link[sits]{sits_train}}
#' @param  multicores      Number of threads to process the time series.
#' @return pred.lst        list with the predicted labels for each output time interval
.sits_classify_block <- function (data.mx, class_info.tb, ml_model = NULL, multicores = 1){

    ensurer::ensure_that(ml_model,  !purrr::is_null(.), err_desc = "sits-classify: please provide a machine learning model already trained")

    # find the subsets of the input data
    dates_index.lst <- class_info.tb$dates_index[[1]]

    # find the number of the samples
    nsamples <- dates_index.lst[[1]][2] - dates_index.lst[[1]][1] + 1

    #retrieve the timeline of the data
    timeline <- class_info.tb$timeline[[1]]

    # retrieve the bands
    bands <- class_info.tb$bands[[1]]

    #retrieve the time index
    time_index.lst  <- .sits_time_index(dates_index.lst, timeline, bands)

    # define the column names
    attr_names <- vector()
    bands %>%
        purrr::map (function (b){
            attr_names <<- c(attr_names, paste (c(rep(b, nsamples)), as.character(c(1:nsamples)), sep = ""))
        })

    attr_names <- c("original_row", "reference", attr_names)


    # classify a block of data
    classify_block <- function (block.mx) {
        # create a list to get the predictions
        pred_block.lst <- list()
        for (t in 1:length(time_index.lst)){
            # create an empty matrix to store the subset of the data
            values.mx <- matrix(nrow = nrow (block.mx), ncol = 0)
            idx <- time_index.lst[[t]]
            for (b in 1:length(bands)){
                # retrieve the values used for classification
                values.mx <- cbind(values.mx, block.mx[,idx[(2*b - 1)]:idx[2*b]])
            }
            dist.tb <- data.frame("original_row" = rep(1,nrow(block.mx)) , "reference" = rep("NoClass", nrow(block.mx)))
            dist.tb[,3:(nsamples*length(bands) + 2)] <- values.mx

            colnames(dist.tb) <- attr_names
            # classify the subset data
            pred_block.lst[[t]] <- sits_predict(dist.tb, ml_model)
        }
        return(pred_block.lst)
    }

    join_blocks <- function (blocks.lst) {
        pred.lst <- list()
        for (t in 1:length(time_index.lst)){
            pred.lst[[t]] <- vector()
        }
        for (i in 1:length(blocks.lst)){
            pred.lst <- c(pred.lst[[t]], blocks.lst[[i]][[t]])
        }
        return (pred.lst)
    }

    if (multicores > 1) {
        blocks.lst <- split.data.frame(data.mx, cut(1:nrow(data.mx),multicores, labels = FALSE))
        # apply parallel processing to the split dat
        results <- parallel::mclapply(blocks.lst, classify_block, mc.cores = multicores)

        pred.lst <- join_blocks(results)
    }
    else
        pred.lst <- classify_block(data.mx)

    return (pred.lst)
}



#' @title Define the information required for classifying time series
#' @name .sits_class_info
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Time series classification requires that users do a series of steps:
#' (a) Provide labelled samples that will be used as training data.
#' (b) Provide information on how the classification will be performed, including data timeline,
#' temporal interval, and start and end dates per interval.
#' (c) Clean the training data to ensure it meets the specifications of the classification info.
#' (d) Use the clean data to train a machine learning classifier.
#' (e) Classify non-labelled data sets
#'
#' In this set of steps, this function provides support for step (b). It requires the user
#' to provide a timeline, the classification interval, and the start and end dates of
#' the reference period. The results is a tibble with information that allows the user
#' to perform steps (c) to (e)
#'
#' @param  data.tb         Description on the data being classified
#' @param  samples.tb      The samples used for training the classification model
#' @param  interval        The interval between two sucessive classification
#' @return class_info.tb   A SITS tibble with the classification information
#'
.sits_class_info <- function (data.tb, samples.tb, interval){

    # find the timeline
    timeline <- .sits_timeline (data.tb)

    # find the labels
    labels <- sits_labels(samples.tb)$label
    # find the bands
    bands <- sits_bands (samples.tb)

    # what is the reference start date?
    ref_start_date <- lubridate::as_date(samples.tb[1,]$start_date)
    # what is the reference end date?
    ref_end_date <- lubridate::as_date(samples.tb[1,]$end_date)

    # obtain the reference dates that match the patterns in the full timeline
    ref_dates.lst <- .sits_match_timeline(timeline, ref_start_date, ref_end_date, interval)

    # obtain the indexes of the timeline that match the reference dates
    dates_index.lst <- .sits_match_indexes(timeline, ref_dates.lst)

    # find the number of the samples
    nsamples <- dates_index.lst[[1]][2] - dates_index.lst[[1]][1] + 1

    class_info.tb <- tibble::tibble (
        bands          = list (bands),
        labels         = list (labels),
        interval       = interval,
        timeline       = list(timeline),
        num_samples    = nsamples,
        ref_dates      = list(ref_dates.lst),
        dates_index    = list(dates_index.lst)
    )
    return (class_info.tb)
}

