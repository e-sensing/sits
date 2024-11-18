#' @title Detection disturbance in combined time series or data cubes
#' @name sits_bayts
#' @author Felipe Carvalho, \email{lipecaso@@gmail.com}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description
#' This function implements the algorithm described by Johanes Reiche
#' referenced below.
#'
#' @references Reiche J, De Bruin S, Hoekman D, Verbesselt J, Herold M,
#' "A Bayesian approach to combine Landsat and ALOS PALSAR time
#' series for near real-time deforestation detection.",
#' Remote Sensing, 7, 2015 DOI: 10.3390/rs70504973.
#'
#'
#' @param  samples      Time series with the training samples
#'                      (tibble of class "sits").
#' @param  stats        A tibble with mean and standard deviation values
#'                      of each band. (see details below)
#' @param  start_date   Start date for the detection
#'                      (Date in YYYY-MM-DD format).
#' @param  end_date     End date for the dectection
#'                      (Date im YYYY-MM-DD format).
#' @param  deseasonlize A numeric value with the quantile percentage to
#'                      deseasonlize time series using spatial
#'                      normalization.
#' @param threshold     A numeric value with threshold of the probability
#'                      of Non-Forest above which the first observation
#'                      is flagged. Default = 0.5.
#' @param bwf           A numeric vector with the block weighting function
#'                      to truncate the Non-Forest probability.
#'                      Default = (0.1, 0.9).
#' @param  chi          A numeric with threshold of the probability
#'                      change at which the change is confirmed.
#'                      Default = 0.5.
#' @return              A vector data cube with the detection day
#'                      for each pixel (tibble of class "radd_cube").
#'
#' @noRd
sits_bayts <- function(samples = NULL,
                       stats = NULL,
                       start_date = NULL,
                       end_date = NULL,
                       deseasonlize = NULL,
                       threshold = 0.5,
                       bwf = c(0.1, 0.9),
                       chi = 0.9) {
    # Training function
    train_fun <- function(samples) {
        # Create a stats tibble
        stats <- .bayts_create_stats(samples, stats)

        detect_change_fun <- function(values, ...) {
            dots <- list(...)
            # Extract tile
            tile <- dots[["tile"]]
            # Extract prepared data
            prep_data <- dots[["prep_data"]]
            # Get the number of dates in the timeline
            tile_tl <- .tile_timeline(tile)
            n_times <- length(tile_tl)

            # Get the start and end time of the detection period
            start_detection <- 0
            end_detection <- n_times + 1
            if (.has(start_date) && .has(end_date)) {
                filt_idxs <- which(tile_tl >= start_date & tile_tl <= end_date)
                start_detection <- min(filt_idxs) - 1
                end_detection <- max(filt_idxs)
            }

            # Calculate the probability of a Non-Forest pixel
            values <- C_bayts_calc_nf(
                ts              = values,
                mean            = stats[["mean"]],
                sd              = stats[["sd"]],
                n_times         = n_times,
                quantile_values = prep_data,
                bwf             = bwf
            )
            # Apply detect changes in time series
            C_bayts_detect_changes(
                p_res           = values,
                start_detection = start_detection,
                end_detection   = end_detection,
                threshold = threshold,
                chi = chi
            )
        }
        # Set model class
        predict_fun <- .set_class(
            detect_change_fun, "bayts_model", "sits_model",
            class(detect_change_fun)
        )
        return(predict_fun)
    }
    # If samples is informed, train a model and return a predict function
    # Otherwise give back a train function to train model further
    result <- .factory_function(samples, train_fun)
    return(result)
}
