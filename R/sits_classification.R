#' @title Classify a sits tibble using machine learning models
#' @name sits_classify
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns a sits table with the results of the ML classifier.
#'
#' @param  data.tb         a SITS tibble time series
#' @param  patterns.tb     a set of known temporal signatures for the chosen classes
#' @param  dist_method     method to compute distances (e.g., sits_TWDTW_distances)
#' @param  ml_model        a model trained by \code{\link[sits]{sits_train}}
#' @param  start_date      date - the start of the classification period
#' @param  end_date        date - the end of the classification period
#' @param  interval        the period between two classifications
#' @param ...             other parameters to be passed to the model function
#' @return data.tb      a SITS tibble with the predicted label
#' @export
sits_classify <- function (data.tb, patterns.tb, ml_model, dist_method = sits_TWDTW_distances(),
                           start_date = NULL, end_date = NULL,
                           interval = "12 month", overlap = 0.5){

    class.tb <- data.tb %>%
        purrrlyr::by_row (function (row) {

            if (purrr::is_null (start_date)) {
                start_date  <- row$start_date
                end_date    <- row$end_date
                interval <- lubridate::as_date(end_date) - lubridate::as_date(start_date)
            }

            # define the temporal intervals of each classification
            breaks <- seq(from = as.Date(start_date), to = as.Date(end_date), by = interval)

}
