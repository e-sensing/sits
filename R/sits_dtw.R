#' @title Dynamic Time Warping for Detect changes.
#' @name sits_dtw
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Create a Dynamic Time Warping (DTW) method for the
#' \code{\link[sits]{sits_detect_change_method}}.
#'
#' @param  samples       Time series with the training samples.
#' @param ...            Other relevant parameters.
#' @param  threshold     Threshold used to define if an event was detected.
#' @param  window        ISO8601-compliant time period used to define the
#'                       DTW moving window, with number and unit,
#'                       where "D", "M" and "Y" stands for days, month and
#'                       year; e.g., "P16D" for 16 days. This parameter is not
#'                       used in operations with data cubes.
#' @return               Change detection method prepared to be passed to
#'                       \code{\link[sits]{sits_detect_change_method}}
sits_dtw <-
    function(samples    = NULL,
             ...,
             threshold  = NULL,
             window     = NULL) {
        .check_set_caller("sits_dtw")
        train_fun <-
            function(samples) {
                # Check parameters
                .check_period(window)
                .check_null_parameter(threshold)
                # Sample labels
                labels <- .samples_labels(samples)
                # Get samples patterns (temporal median)
                train_samples <- .predictors(samples)
                patterns <- .pattern_temporal_median(samples)
                # Define detection function
                detect_change_fun <- function(values, type) {
                    # Define the type of the operation
                    dtw_fun <- .dtw_windowed_ts
                    if (type == "cube") {
                        dtw_fun <-  .dtw_complete_ts
                    }
                    # Detect changes
                    dtw_fun(
                        values = values,
                        patterns = patterns,
                        window = window,
                        threshold = threshold
                    )
                }
                # Set model class
                detect_change_fun <- .set_class(detect_change_fun,
                                                "dtw_model",
                                                "sits_model",
                                                class(detect_change_fun))
                return(detect_change_fun)
            }
        # If samples is informed, train a model and return a predict function
        # Otherwise give back a train function to train model further
        result <- .factory_function(samples, train_fun)
        return(result)
    }
