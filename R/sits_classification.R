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
#' @param  start_date      the starting date of the classification
#' @param  end_date        the end date of the classification
#' @param  interval        the period between two classifications
#' @param  data_interval   the period of the input data to be extracted for each classification
#' @param ...             other parameters to be passed to the distance function
#' @return data.tb      a SITS tibble with the predicted label
#' @export
sits_classify <- function (data.tb = NULL, patterns.tb = NULL, ml_model = NULL, dist_method = sits_TWDTW_distances(),
                           start_date = NULL, end_date = NULL,
                           interval = "12 month", data_interval = "12 month"){

    .sits_test_tibble(data.tb)
    .sits_test_tibble(patterns.tb)
    ensurer::ensure_that(ml_model,   !purrr::is_null(.), err_desc = "sits-classify: please provide a machine learning model already trained")
    ensurer::ensure_that(start_date, !purrr::is_null(.), err_desc = "sits-classify: please provide the starting date for the classification")
    ensurer::ensure_that(end_date,   !purrr::is_null(.), err_desc = "sits-classify: please provide the end date for the classification")

    # create a tibble to store the result
    result.tb <- sits_tibble_classification()

    # find the subsets of the input data
    subset_dates.lst <- .sits_subset_dates(start_date, end_date, interval, data_interval)

    # go over every row of the table
    data.tb %>%
        purrrlyr::by_row (function (row) {
            # create a table to store the result
            predict.tb <- sits_tibble_prediction()

            subset_dates.lst %>%
                purrr::map (function (date_pair){

                    # find the n-th subset of the input data
                    row_subset.tb <- sits_extract(row, date_pair[1], date_pair[2])

                    # find the distances in the subset data
                    distances.tb  <- sits_distances (row_subset.tb, patterns.tb, dist_method = dist_method, break_ts = FALSE)

                    # classify the subset data
                    sub_predict.tb <- sits_predict(row_subset.tb, distances.tb, ml_model)

                    # save the results
                    predict.tb   <<- tibble::add_row(predict.tb,
                                                from      = lubridate::as_date(sub_predict.tb$start_date),
                                                to        = lubridate::as_date(sub_predict.tb$end_date),
                                                distance  = 0.0,
                                                predicted = sub_predict.tb$predicted
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
