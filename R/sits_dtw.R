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
#'                       Default is `Inf`.
#' @param  window        ISO8601-compliant time period used to define the
#'                       DTW moving window, with number and unit,
#'                       where "D", "M" and "Y" stands for days, month and
#'                       year; e.g., "P16D" for 16 days.
#' @return               Change detection method prepared to be passed to
#'                       \code{\link[sits]{sits_detect_change_method}}
#' @export
#'
sits_dtw <-
    function(samples    = NULL,
             ...,
             threshold  = Inf,
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
                train_samples_patterns <- .pattern_temporal_median(samples)
                # Define detection function
                detect_change_fun <- function(values) {
                    # Extract dates
                    dates <- values[[1]][["Index"]]
                    dates_min  <- min(dates)
                    dates_max  <- max(dates)
                    # Assume time-series are regularized, then use the period
                    # as the step of the moving window. As a result, we have
                    # one step per iteration.
                    dates_step <- lubridate::as.period(
                        lubridate::int_diff(dates)
                    )
                    dates_step <- dates_step[[1]]
                    # Create comparison windows
                    comparison_windows <- .period_windows(
                        period = window,
                        step = dates_step,
                        start_date = dates_min,
                        end_date = dates_max
                    )
                    # Do the change detection for each time-series
                    purrr::map(values, function(value_row) {
                        # Search for the patterns
                        patterns_distances <- .pattern_distance_dtw(
                            data       = value_row,
                            patterns   = train_samples_patterns,
                            windows    = comparison_windows
                        )
                        # Remove distances out the user-defined threshold
                        patterns_distances[patterns_distances > threshold] <- NA
                        # Define where each label was detected. For this, first
                        # get from each label the minimal distance
                        detections_idx <-
                            apply(patterns_distances, 2, which.min)
                        detections_name <- names(detections_idx)
                        # For each label, extract the metadata where they had
                        # minimal distance
                        purrr::map_df(1:length(detections_idx), function(idx) {
                            # Extract detection name and inced
                            detection_name <- detections_name[idx]
                            detection_idx <- detections_idx[idx]
                            # Extract detection distance (min one defined above)
                            detection_distance <-
                                patterns_distances[detection_idx,]
                            detection_distance <-
                                detection_distance[detection_name]
                            detection_distance <-
                                as.numeric(detection_distance)
                            # Extract detection dates
                            detection_dates <-
                                comparison_windows[[detection_idx]]
                            # Prepare result and return it!
                            data.frame(
                                change = detection_name,
                                distance = detection_distance,
                                from = detection_dates[["start"]],
                                to = detection_dates[["end"]]
                            )
                        })
                    })
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
