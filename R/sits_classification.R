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
#' @param  adj_fun           Adjustment function to be applied to the data
#' @param  interval          The interval used for classification
#' @param  multicores        Number of threads to process the time series.
#' @return data.tb           SITS tibble with the predicted labels for each input segment
#' @examples
#' \donttest{
#' # read a training data set
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(samples_MT_ndvi)
#' # Retrieve a point
#' data(point_ndvi)
#' # classify the point
#' class_ndvi.tb <-  sits_classify (point_ndvi, samples_MT_ndvi)
#' # plot the classification
#' sits_plot (class_ndvi.tb)
#'
#' # Read a point from the WTSS server
#' point2.tb <- sits_getdata (longitude = -46.4070, latitude = -10.8630)
#' # select the ndvi
#' point2.tb <- sits_select (point2.tb, bands = c("ndvi"))
#' # classify the point
#' class2.tb <-  sits_classify (point2.tb, samples_MT_ndvi)
#' # plot the classification
#' sits_plot (class2.tb)
#' }
#'
#' @export
sits_classify <- function(data.tb = NULL,
                          train_samples.tb = NULL,
                          ml_method = sits_svm(kernel = "radial", cost = 10, coef0 = 0, tolerance = 0.001, epsilon = 0.1, cross = 4) ,
                          adj_fun   = sits_adjust(),
                          interval  = "12 month",
                          multicores = 1) {

    .sits_test_tibble(data.tb)
    .sits_test_tibble(train_samples.tb)

    # obtain the machine learning model based on the training samples
    ml_model = sits_train(train_samples.tb, ml_method = ml_method, adj_fun = adj_fun)

    # define the parameters for breaking up a long time series
    class_info.tb <- .sits_class_info(data.tb, train_samples.tb, interval)

    # find the subsets of the input data
    ref_dates.lst <- class_info.tb$ref_dates[[1]]

    # obtain the distances from the data
    distances.tb <- sits_distances(data.tb, adj_fun)

    # create a vector to store the predicted results
    predict.vec <- .sits_classify_distances(distances.tb, class_info.tb, ml_model, multicores)

    # Store the result in the input data
    data.tb <- .sits_tibble_prediction(data.tb, class_info.tb, predict.vec, interval)

    return(data.tb)
}
#' @title Classify a sits tibble using pre-built machine learning models
#' @name sits_classify_model
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function classifies a set of time series, given
#' a set of training samples, an inference model, and an interval.
#' To perform the classification, users should provide a set of
#' labelled samples. Each samples should be associated to one spatial location
#' (latitude/longitude), one time interval and a label.
#'
#' @param  data.tb           SITS tibble time series (cleaned)
#' @param  train_samples.tb  The samples used for training the classification model
#' @param  model             A pre-built ML model (see \code{\link[sits]{sits_train}})
#' @param  adj_fun           Adjustment function to be applied to the data
#' @param  interval          The interval used for classification
#' @param  multicores        Number of threads to process the time series.
#' @return data.tb           SITS tibble with the predicted labels for each input segment
#' @examples
#' \donttest{
#' # read a training data set
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(samples_MT_ndvi)
#' # select the bands "ndvi", "evi", "nir", and "mir"
#' samples.tb <- sits_select(samples_MT_9classes, bands = c("ndvi","evi","nir","mir"))
#' # find a training model based on the distances and on default values
#' model <- sits_train(samples.tb)
#' # Retrieve a time series
#' data("ts_2000_2016")
#' # select the bands "ndvi", "evi", "nir", and "mir"
#' point.tb <- sits_select(ts_2000_2016, bands = c("ndvi","evi","nir","mir"))
#' # classify the point
#' class.tb <-  sits_classify_model(point.tb, samples.tb, model)
#' # plot the classification
#' sits_plot(class.tb)
#' }
#'
#' @export
sits_classify_model <- function(data.tb = NULL,
                                train_samples.tb = NULL,
                                model      = NULL,
                                adj_fun    = sits_adjust(),
                                interval   = "12 month",
                                multicores = 1){

    # check that the input data exists
    .sits_test_tibble(data.tb)
    .sits_test_tibble(train_samples.tb)

    # ensures that a machine learning model is provided
    ensurer::ensure_that(model, !is.null(.), err_desc = "sits_classify_model: please provide a valid inference model")

    # define the parameters for breaking up a long time series
    class_info.tb <- .sits_class_info(data.tb, train_samples.tb, interval)

    # find the temporal subsets of the input data
    ref_dates.lst <- class_info.tb$ref_dates[[1]]

    # obtain the distances from the data
    distances.tb <- sits_distances(data.tb, adj_fun)

    # create a vector to store the predicted results
    predict.vec <- .sits_classify_distances(distances.tb, class_info.tb, model, multicores)

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
.sits_classify_distances <- function(distances.tb, class_info.tb, ml_model = NULL, multicores = 1) {

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
        purrr::map(function(ti){
            ti <- ti + 2
        })

    # define the column names
    attr_names.lst <- bands %>%
        purrr::map(function(b){
            attr_names_b <- c(paste(c(rep(b, nsamples)), as.character(c(1:nsamples)), sep = ""))
            return(attr_names_b)
        })
    attr_names <- unlist(attr_names.lst)
    attr_names <- c("original_row", "reference", attr_names)

    # create a data table to store the distances
    dist.tb <- data.table::data.table(nrow = 0, ncol = length(attr_names))

    # classify a block of data
    classify_block <- function(distances.tb) {
        # create a list to get the predictions
        row.lst <- list()
        # create a block of distances to be classified
        for (r in 1:nrow(distances.tb)) {
            # create a data table to store the values of the distances
            for (t in 1:length(time_index.lst)) {
                # create a data table to store the distances for each row
                row.tb <- data.table::data.table("original_row" = 1, "reference" = "NoClass")
                idx <- time_index.lst[[t]]
                for (b in 1:length(bands)) {
                    # retrieve the values used for classification
                    row.tb <- cbind(row.tb, distances.tb[r,idx[(2*b - 1)]:idx[2*b]])
                }
                row.lst[[length(row.lst) + 1 ]] <- row.tb
            }
        }
        dist.tb <- data.table::rbindlist(row.lst)
        colnames(dist.tb) <- attr_names
        # classify the subset data
        pred_block.vec <- .sits_predict(dist.tb, ml_model)
        return(pred_block.vec)
    }

    join_blocks <- function(blocks.lst) {

        pred.vec <- vector()
        blocks.lst %>%
            purrr::map(function(block){
                pred.vec <<- c(pred.vec, block )
            })
        return(pred.vec)
    }

    if (multicores > 1) {
        blocks.lst <- split.data.frame(distances.tb, cut(1:nrow(distances.tb),multicores, labels = FALSE))
        # apply parallel processing to the split dat
        results.lst <- parallel::mclapply(blocks.lst, classify_block, mc.cores = multicores)

        pred.vec <- join_blocks(results.lst)
    }
    else
        pred.vec <- classify_block(distances.tb)

    return(pred.vec)
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
.sits_class_info <- function(data.tb, samples.tb, interval){

    # find the timeline
    timeline <- .sits_timeline(data.tb)

    # find the labels
    labels <- sits_labels(samples.tb)$label
    # find the bands
    bands <- sits_bands(samples.tb)

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

    class_info.tb <- tibble::tibble(
        bands          = list(bands),
        labels         = list(labels),
        interval       = interval,
        timeline       = list(timeline),
        num_samples    = nsamples,
        ref_dates      = list(ref_dates.lst),
        dates_index    = list(dates_index.lst)
    )
    return(class_info.tb)
}

