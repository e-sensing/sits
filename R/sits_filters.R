#' @title Filter time series and data cubes
#' @name sits_filter
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Given a set of time series, filter them with one of
#' the available filtering algorithms:
#'
#' \itemize{
#'  \item{Whittaker smoother - see \code{\link{sits_whittaker}}}
#'  \item{Savitsky-Golay filter - see \code{\link{sits_sgolay}}}
#'  \item{Envelope filter - see \code{\link{sits_envelope}}}
#'  \item{Interpolation filter - see \code{\link{sits_interp}}}
#' }
#'
#' @param  data          Set of time series
#' @param  filter        Filter to be applied to the data.
#' @return               A set of filtered time series
#'
#' @export
sits_filter <- function(data, filter = sits_whittaker()) {

    # set caller to show in errors
    .check_set_caller("sits_filter")

    # is the input data a valid sits tibble?
    .sits_tibble_test(data)

    # is the train method a function?
    .check_that(
        x = inherits(filter, "function"),
        msg = "filter is not a valid function"
    )

    # compute the training method by the given data
    result <- filter(data)

    # return a valid machine learning method
    return(result)
}

#' @title Filter time series using Savitsky-Golay method
#'
#' @name sits_sgolay
#' @description  An optimal polynomial for warping a time series.
#' The degree of smoothing depends on the filter order (usually 3.0).
#' The order of the polynomial uses the parameter `order` (default = 3),
#' the size of the temporal window uses the parameter `length` (default = 5),
#' and the temporal expansion uses the parameter `scaling`.
#'
#' @references A. Savitzky, M. Golay, "Smoothing and Differentiation of Data by
#' Simplified Least Squares Procedures".
#' Analytical Chemistry, 36 (8): 1627–39, 1964.
#'
#' @param data          A tibble with time series data and metadata.
#' @param order         Filter order (integer).
#' @param length        Filter length (must be odd)
#' @param scaling       Time scaling (integer).
#' @param bands_suffix  Suffix to be appended to the smoothed filters.
#' @return              A tibble with smoothed sits time series.
#'
#' @examples
#' #' # Retrieve a time series with values of NDVI
#' point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
#' # Filter the point using the Savitsky Golay smoother
#' point_sg <- sits_filter(point_ndvi, sits_sgolay(order = 3, length = 5))
#' # Plot the two points to see the smoothing effect
#' plot(sits_merge(point_ndvi, point_sg))
#'
#' @export
sits_sgolay <- function(data = NULL,
                        order = 3,
                        length = 5,
                        scaling = 1,
                        bands_suffix = "sg") {

    filter_fun <- function(data) {
        if (inherits(data, "tbl")) {
            result <- sits_apply(data,
                                 fun = function(band) {
                                     .sits_signal_sgolayfilt(band,
                                                             p = order,
                                                             n = length,
                                                             ts = scaling
                                     )
                                 },
                                 fun_index = function(band) band,
                                 bands_suffix = bands_suffix
            )
        }
        if (inherits(data, "matrix")) {
            result <- apply(data, 2, function(row) {
                .sits_signal_sgolayfilt(row,
                                        p = order,
                                        n = length,
                                        ts = scaling
                )
            })
        }
        return(result)
    }

    result <- .sits_factory_function(data, filter_fun)
    return(result)
}

#' @title Filter time series using Whittaker smoother
#'
#' @name sits_whittaker
#' @description  The algorithm searches for an optimal warping polynomial.
#' The degree of smoothing depends on smoothing factor lambda
#' (usually from 0.5 to 10.0). Use lambda = 0.5 for very slight smoothing
#' and lambda = 5.0 for strong smoothing.
#'
#' @references Francesco Vuolo, Wai-Tim Ng, Clement Atzberger,
#' "Smoothing and gap-filling of high resolution multi-spectral timeseries:
#' Example of Landsat data",
#' Int Journal of Applied Earth Observation and Geoinformation,
#' vol. 57, pg. 202-213, 2107.
#'
#' @param data         A tibble with time series data and metadata.
#' @param lambda       Smoothing factor to be applied (default 0.5).
#' @param bands_suffix Suffix to be appended (default "wf").
#' @return             A tibble with smoothed sits time series.
#'
#' @examples
#' # Retrieve a time series with values of NDVI
#' point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
#' # Filter the point using the whittaker smoother
#' point_whit <- sits_filter(point_ndvi, sits_whittaker(lambda = 3.0))
#' # Plot the two points to see the smoothing effect
#' plot(sits_merge(point_ndvi, point_whit))
#' @export
sits_whittaker <- function(data = NULL, lambda = 0.5, bands_suffix = "wf") {

    filter_fun <- function(data) {
        result <- NULL
        if (inherits(data, "tbl")) {
            result <- sits_apply(data,
                                 fun = function(band) {
                                     smooth_whit(band, lambda = lambda, length = length(band))
                                 },
                                 fun_index = function(band) band,
                                 bands_suffix = bands_suffix
            )
        }
        if (inherits(data, "matrix")) {
            result <- apply(
                data, 2,
                function(row) {
                    smooth_whit(row, lambda = lambda, length = length(row))
                }
            )
        }
        return(result)
    }
    result <- .sits_factory_function(data, filter_fun)
    return(result)
}


#' @title Interpolate values in time series
#' @name sits_interp
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Computes the interpolated bands using a user-defined
#' function (by default the R base function approx)
#' @param data       A tibble with time series data and metadata.
#' @param fun        Interpolation function (by default, stats::approx())
#' @param n          Number of time series elements to be created
#'                   between start date and end date.
#' @param ...        Additional parameters to be used by the fun function.
#' @return A tibble with interpolated samples.
#' @examples
#' # Retrieve a time series with values of NDVI
#' point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
#' # find out how many time instances are there
#' n_times <- length(sits_timeline(point_ndvi))
#' # interpolate three times more points
#' point_int.tb <- sits_interp(point_ndvi, fun = stats::spline, n = 3 * n_times)
#' # plot the result
#' plot(point_int.tb)
#' @export
sits_interp <- function(data,
                        fun = stats::approx,
                        n = 2 * length(sits_timeline(data)), ...) {
    # compute function on data
    result <- sits_apply(data,
                         fun = function(band) {
                             return(fun(band, n = n, ...)$y)
                         },
                         fun_index = function(band) {
                             as.Date(fun(band, n = n, ...)$y,
                                     origin = "1970-01-01"
                             )
                         }
    )
    return(result)

}
