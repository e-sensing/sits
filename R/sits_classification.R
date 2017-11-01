#' @title Classify a sits tibble using machine learning models
#' @name sits_classify
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns a sits table with the results of the ML classifier.
#'
#' @param  data.tb         a SITS tibble time series
#' @param  patterns.tb     a set of known temporal signatures for the chosen classes
#' @param  ml_model        a model trained by \code{\link[sits]{sits_train}}
#' @param  dist_method     method to compute distances (e.g., sits_TWDTW_distances)
#' @param  interval        the period between two classifications
#' @param  ...             other parameters to be passed to the distance function
#' @return data.tb         a SITS tibble with the predicted labels for each input segment
#' @export
sits_classify <- function (data.tb = NULL, patterns.tb =  NULL,
                           ml_model = NULL, dist_method = sits_distances_from_data(.break = FALSE),
                           interval = "12 month"){

    .sits_test_tibble(data.tb)
    .sits_test_tibble(patterns.tb)
    ensurer::ensure_that(ml_model,  !purrr::is_null(.), err_desc = "sits-classify: please provide a machine learning model already trained")
    # create a tibble to store the result
    result.tb <- sits_tibble_classification()

    # find the subsets of the input data
    ref_dates.lst <- patterns.tb[1,]$ref_dates[[1]]

    # go over every row of the table
    data.tb %>%
        purrrlyr::by_row (function (row) {
            # create a table to store the result
            predict.tb <- sits_tibble_prediction()

            ref_dates.lst %>%
                purrr::map (function (dates){

                    # find the n-th subset of the input data
                    row_subset.tb <- .sits_extract(row, dates[1], dates[2])

                    # find the distances in the subset data
                    distances.tb  <- sits_distances (row_subset.tb, patterns.tb, dist_method = dist_method)

                    # classify the subset data
                    predicted <- sits_predict(distances.tb, ml_model)

                    # save the results
                    predict.tb   <<- tibble::add_row(predict.tb,
                                                from      = lubridate::as_date(dates[1]),
                                                to        = lubridate::as_date(dates[2]),
                                                distance  = 0.0,
                                                predicted = predicted
                    )

                })

            # create a list to store the zoo time series coming from the WTSS service
            pred.lst <- list()
            # transform the zoo list into a tibble to store in memory
            pred.lst[[1]] <- predict.tb

            # include the matches in the SITS table

            result.tb <<- tibble::add_row (result.tb,
                                           longitude   = row$longitude,
                                           latitude    = row$latitude,
                                           start_date  = as.Date(row$start_date),
                                           end_date    = as.Date(row$end_date),
                                           label       = row$label,
                                           coverage    = row$coverage,
                                           time_series = row$time_series,
                                           predicted   = pred.lst)

        })

    return(result.tb)
}
#' @title Classify a set of spatio-temporal raster bricks using machine learning models
#' @name sits_classify_raster
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a set of spatio-temporal raster bricks, whose metadata is
#'              described by tibble (created by \code{\link[sits]{sits_fromRaster}}),
#'              a set of patterns (created by \code{\link[sits]{sits_patterns}}),
#'              a prediction model (created by \code{\link[sits]{sits_train}}), and
#'              a method to extract shape attributes from time_series (used by  \code{\link[sits]{sits_distances}} ),
#'              and produces a classified set of RasterLayers. This function is similar to
#'               \code{\link[sits]{sits_classify}} which is applied to time series stored in a SITS tibble.
#'
#'
#' @param  raster.tb       a tibble with information about a set of space-time raster bricks
#' @param  file            a general set of file names (one file per classified year)
#' @param  patterns.tb     a set of known temporal signatures for the chosen classes
#' @param  ml_model        a model trained by \code{\link[sits]{sits_train}}
#' @param  dist_method     method to compute distances (e.g., sits_TWDTW_distances)
#' @param  interval        the period between two classifications
#' @param  multicores      Number of threads to process the time series.
#' @param  ...             other parameters to be passed to the distance function
#' @return raster_class.tb a SITS tibble with the metadata for the set of RasterLayers
#' @export
sits_classify_raster <- function (raster.tb, file = NULL, patterns.tb, ml_model = NULL,
                                  dist_method = sits_distances_from_data(),
                                  interval = "12 month", multicores = 2){

    # ensure metadata tibble exists
    .sits_test_tibble (raster.tb)
    # ensure patterns tibble exits
    .sits_test_tibble (patterns.tb)

    # ensure that file name and prediction model are provided
    ensurer::ensure_that(file,     !purrr::is_null(.), err_desc = "sits-classify-raster: please provide name of output file")
    ensurer::ensure_that(ml_model, !purrr::is_null(.), err_desc = "sits-classify-raster: please provide a machine learning model already trained")

    # create the raster objects and their respective filenames
    raster_class.tb <- .sits_create_classified_raster(raster.tb, patterns.tb, file, interval)

    # get the labels of the data
    labels <- sits_labels(patterns.tb)$label
    # create a named vector with integers match the class labels
    int_labels <- c(1:length(labels))
    names (int_labels) <- labels

    # find the subsets of the input data
    dates_index.lst <- patterns.tb[1,]$dates_index[[1]]

    # find the number of the samples
    nsamples <- dates_index.lst[[1]][2] - dates_index.lst[[1]][1] + 1

    #retrieve the timeline of the data
    timeline <- patterns.tb[1,]$timeline[[1]]

    # retrieve the bands
    bands <- sits_bands (patterns.tb)

    #retrieve the time index
    time_index.lst  <- .sits_time_index(dates_index.lst, timeline, bands)

    # define the column names
    attr_names <- vector()
    bands %>%
        purrr::map (function (b){
            attr_names <<- c(attr_names, paste (c(rep(b, nsamples)), as.character(c(1:nsamples)), sep = ""))
        })

    attr_names <- c("original_row", "reference", attr_names)

    #initiate writing
    raster_class.tb$r_obj <- raster_class.tb$r_obj %>%
        purrr::map(function (layer) {
            raster::writeStart(layer, layer@file@name, overwrite = TRUE)
        })

    # recover the input data by blocks for efficiency
    bs <- .sits_raster_block_size (raster_class.tb[1,])

    # read the input raster in blocks
    for (i in 1:bs$n){

        # extract time series from the block of RasterBrick rows
        data.mx <- .sits_data_from_block (raster.tb, row = bs$row[i], nrows = bs$nrows[i])

        # classify the time series that are part of the block
        pred.lst <- sits_classify_distances(data.mx, timeline, time_index.lst, nsamples, attr_names, bands, ml_model, multicores = multicores)

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



#' @title Classify a sits distances tibble using machine learning models
#' @name sits_classify_distances
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns a sits table with the results of the ML classifier.
#'
#' @param  data.mx         a matrix with data values
#' @param  timeline        the timeline of the data set
#' @param  time_index.lst  The subsets of the timeline
#' @param  nsamples        number of samples
#' @param  attr_names       Names of columns of distance tibble
#' @param  bands           Bands used for classification
#' @param  ml_model        a model trained by \code{\link[sits]{sits_train}}
#' @param  multicores      Number of threads to process the time series.
#' @return pred.lst        list with the predicted labels for each output time interval
#' @export
sits_classify_distances <- function (data.mx, timeline, time_index.lst, nsamples,
                                    attr_names, bands, ml_model = NULL, multicores = 1){

    ensurer::ensure_that(ml_model,  !purrr::is_null(.), err_desc = "sits-classify: please provide a machine learning model already trained")

    classify_block <- function (data.mx) {
        pred_block.lst <- list()
        for (t in 1:length(time_index.lst)){
            values.mx <- matrix(nrow = nrow (data.mx), ncol = 0)
            idx <- time_index.lst[[t]]
            for (b in 1:length(bands)){
                # find the start and end columns of the distance table
                col1 <- 3 + (b - 1)*nsamples
                col2 <- col1 + nsamples - 1
                # retrieve the values used for classification
                values.mx <- cbind(values.mx, data.mx[,idx[(2*b - 1)]:idx[2*b]])
            }
            dist.tb <- data.frame("original_row" = rep(1,nrow(data.mx)) , "reference" = rep("NoClass", nrow(data.mx)))
            dist.tb[,3:(nsamples*length(bands) + 2)] <- values.mx

            colnames(dist.tb) <- attr_names
            # classify the subset data
            pred_block.lst[[t]] <- sits_predict(dist.tb, ml_model)
        }
        return(pred_block.lst)
    }
    pred.lst <- classify_block (data.mx)
    # results <- parallel::mclapply(split(data.mx, rep(1:multicores)), classify_block, mc.cores = multicores)
    # # #
    # # # # apply parallel processing to the split data
    # pred.lst <- unsplit(results, rep(1:multicores) )

}


