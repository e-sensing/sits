#' @title Detection disturbance in combined time series or data cubes
#' @name sits_radd
#' @author Felipe Carvalho, \email{lipecaso@@gmail.com}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
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
#' @param  data              Data cube (tibble of class "raster_cube")
#' @param  mean_stats        A tibble with mean value of each band.
#' @param sd_stats           A tibble with the standard deviation
#'                           value of each band.
#' @param  ...               Other parameters for specific functions.
#' @param  impute_fn         Imputation function to remove NA.
#' @param  roi               Region of interest (either an sf object, shapefile,
#'                           or a numeric vector with named XY values
#'                           ("xmin", "xmax", "ymin", "ymax") or
#'                           named lat/long values
#'                           ("lon_min", "lat_min", "lon_max", "lat_max").
#' @param  start_date        Start date for the detection
#'                           (Date in YYYY-MM-DD format).
#' @param  end_date          End date for the dectection
#'                           (Date im YYYY-MM-DD format).
#' @param  memsize           Memory available for classification in GB
#'                           (integer, min = 1, max = 16384).
#' @param  multicores        Number of cores to be used for classification
#'                           (integer, min = 1, max = 2048).
#' @param  deseasonlize      A numeric value with the quantile percentage to
#'                           deseasonlize time series using spatial
#'                           normalization.
#' @param threshold          A numeric value with threshold of the probability
#'                           of Non-Forest above which the first observation
#'                           is flagged. Default = 0.5.
#' @param bwf                A numeric vector with the block weighting function
#'                           to truncate the Non-Forest probability.
#'                           Default = (0.1, 0.9).
#' @param  chi               A numeric with threshold of the probability
#'                           change at which the change is confirmed.
#'                           Default = 0.5.
#' @param  output_dir        Valid directory for output file.
#'                           (character vector of length 1).
#' @param  version           Version of the output
#'                           (character vector of length 1).
#' @param  verbose           Logical: print information about processing time?
#' @param  progress          Logical: Show progress bar?
#'
#' @return                   Time series with detection dates for
#'                           each point (tibble of class "sits")
#'                           or a data cube with the detection day of the year
#'                           for each pixel
#'                           (tibble of class "radd_cube").
#'
#' @note
#'    The \code{roi} parameter defines a region of interest. It can be
#'    an sf_object, a shapefile, or a bounding box vector with
#'    named XY values (\code{xmin}, \code{xmax}, \code{ymin}, \code{ymax}) or
#'    named lat/long values (\code{lon_min}, \code{lon_max},
#'    \code{lat_min}, \code{lat_max})
#'
#'    Parameter \code{memsize} controls the amount of memory available
#'    for classification, while \code{multicores}  defines the number of cores
#'    used for processing. We recommend using as much memory as possible.
#'    Please refer to the sits documentation available in
#'    <https://e-sensing.github.io/sitsbook/> for detailed examples.
#'
#' @export
sits_radd <- function(data,
                      mean_stats,
                      sd_stats, ...,
                      chi = 0.9,
                      start_date = NULL,
                      end_date = NULL) {
    UseMethod("sits_radd", data)
}


# sits_radd.sits <- function(data,
#                            mean_stats,
#                            sd_stats, ...,
#                            chi = 0.9,
#                            start_date = NULL,
#                            end_date = NULL) {
#     # Training function
#     train_fun <- function(data) {
#         # Check 'pdf' parameter
#         .check_chr_parameter(pdf)
#         # Check 'chi' parameter
#         .check_num_min_max(chi, min = 0.1, max = 1)
#         # Check 'start_date' parameter
#         .check_date_parameter(start_date)
#         # Check 'end_date' parameter
#         .check_date_parameter(end_date)
#
#         # Get pdf function
#         pdf_fn <- .pdf_fun(pdf)
#         # Create stats layer
#         if (!.has(stats_layer)) {
#             stats_layer <- .radd_create_stats(data)
#         }
#         # Calculate probability for NF
#         data <- .radd_calc_pnf(
#             data = data,
#             pdf_fn = pdf_fn,
#             stats_layer = stats_layer
#         )
#         predict_fun <- function() {
#             # Now we need to detected the changes
#             data <- .radd_detect_events(
#                 data = data,
#                 threshold = 0.5,
#                 start_date = start_date,
#                 end_date = end_date
#             )
#         }
#         # Set model class
#         predict_fun <- .set_class(
#             predict_fun, "radd_model", "sits_model", class(predict_fun)
#         )
#         return(predict_fun)
#     }
#     # If samples is informed, train a model and return a predict function
#     # Otherwise give back a train function to train model further
#     result <- .factory_function(data, train_fun)
#     return(result)
# }

#' @rdname sits_radd
#' @export
sits_radd <- function(samples = NULL,
                      ...,
                      stats = NULL,
                      start_date = NULL,
                      end_date = NULL,
                      deseasonlize = 0.95,
                      threshold = 0.5,
                      bwf = c(0.1, 0.9),
                      chi = 0.9) {
    # Training function
    train_fun <- function(samples) {
        if (!.has(stats)) {
            stats <- .radd_create_stats(samples)
        }
        mean_stats <- unname(as.matrix(stats[stats$stats == "mean", c(-1, -2)]))
        sd_stats <- unname(as.matrix(stats[stats$stats == "sd", c(-1, -2)]))

        # Get pdf function
        pdf_fn <- .pdf_fun("gaussian")

        detect_change_fun <- function(values, tile, quantile_values) {

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
            values <- C_radd_calc_nf(
                ts = values,
                mean = mean_stats,
                sd = sd_stats,
                n_times = n_times,
                quantile_values = quantile_values,
                bwf = bwf
            )
            # Apply detect changes in time series
            C_radd_detect_changes(
                p_res = values,
                start_detection = start_detection,
                end_detection = end_detection
            )
        }
        # Set model class
        predict_fun <- .set_class(
            detect_change_fun, "radd_model", "sits_model",
            class(detect_change_fun)
        )
        return(predict_fun)
    }
    # If samples is informed, train a model and return a predict function
    # Otherwise give back a train function to train model further
    result <- .factory_function(samples, train_fun)
    return(result)
}


#' @export
.change_detect_tile_prep.radd_model <- function(cd_method, tile, ..., impute_fn) {
    deseasonlize <- environment(cd_method)[["deseasonlize"]]

    if (!.has(deseasonlize)) {
        return(matrix(NA))
    }

    tile_bands <- .tile_bands(tile, FALSE)
    quantile_values <- purrr::map(tile_bands, function(tile_band) {
        tile_paths <- .tile_paths(tile, bands = tile_band)
        r_obj <- .raster_open_rast(tile_paths)
        quantile_values <- .raster_quantile(
            r_obj, quantile = deseasonlize, na.rm = TRUE
        )
        quantile_values <- impute_fn(t(quantile_values))
        # Fill with zeros remaining NA pixels
        quantile_values <- C_fill_na(quantile_values, 0)
        # Apply scale
        band_conf <- .tile_band_conf(tile = tile, band = tile_band)
        scale <- .scale(band_conf)
        if (.has(scale) && scale != 1) {
            quantile_values <- quantile_values * scale
        }
        offset <- .offset(band_conf)
        if (.has(offset) && offset != 0) {
            quantile_values <- quantile_values + offset
        }
        unname(quantile_values)
    })
    do.call(cbind, quantile_values)
}
