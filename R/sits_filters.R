#' @title Filter time series and data cubes
#'
#' @name sits_filters
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description
#' Filtering functions should be used with `sits_filter()`.
#' The following filtering functions is supported by `sits`:
#'
#' @param data          A time series vector or matrix.
#'
#' @return              A set of filtered time series
#'
#' @seealso \link[sits]{sits_apply}
NULL

#' @rdname sits_filters
#'
#' @description
#' `sits_sgolay()`: An optimal polynomial for warping a time series.
#' The degree of smoothing depends on the filter order (usually 3.0).
#' The order of the polynomial uses the parameter `order` (default = 3),
#' the size of the temporal window uses the parameter `length` (default = 5),
#' and the temporal expansion uses the parameter `scaling`.
#'
#' @references A. Savitzky, M. Golay, "Smoothing and Differentiation of Data by
#' Simplified Least Squares Procedures".
#' Analytical Chemistry, 36 (8): 1627–39, 1964.
#'
#' @param order         Filter order (integer).
#' @param length        Filter length (must be odd)
#' @param scaling       Time scaling (integer).
#'
#' @examples
#' # Retrieve a time series with values of NDVI
#' point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
#' # Filter the point using the Savitsky Golay smoother
#' point_sg <- sits_apply(point_ndvi,
#'                        NDVI.sg = sits_sgolay(NDVI, order = 3, length = 5))
#' # Plot the two points to see the smoothing effect
#' plot(point_sg)
#'
#' @export
sits_sgolay <- function(data = NULL, order = 3, length = 5, scaling = 1) {

    filter_fun <- function(data) {
        if (inherits(data, "matrix")) {
            return(apply(data, 2, .sits_signal_sgolayfilt, p = order,
                         n = length, ts = scaling))
        } else {
            return(.sits_signal_sgolayfilt(data, p = order,
                                           n = length, ts = scaling))
        }
    }

    result <- .sits_factory_function(data, filter_fun)

    return(result)
}

#' @rdname sits_filters
#'
#' @description
#' `sits_whittaker()`: The algorithm searches for an optimal warping polynomial.
#' The degree of smoothing depends on smoothing factor lambda
#' (usually from 0.5 to 10.0). Use lambda = 0.5 for very slight smoothing
#' and lambda = 5.0 for strong smoothing.
#'
#' @param lambda       Smoothing factor to be applied (default 0.5).
#'
#' @references Francesco Vuolo, Wai-Tim Ng, Clement Atzberger,
#' "Smoothing and gap-filling of high resolution multi-spectral timeseries:
#' Example of Landsat data",
#' Int Journal of Applied Earth Observation and Geoinformation,
#' vol. 57, pg. 202-213, 2107.
#'
#' @examples
#' # Retrieve a time series with values of NDVI
#' point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
#' # Filter the point using the Whittaker smoother
#' point_wt <- sits_apply(point_ndvi,
#'                        NDVI.wt = sits_whittaker(NDVI, lambda = 3))
#' # Plot the two points to see the smoothing effect
#' plot(point_wt)
#'
#' @export
sits_whittaker <- function(data = NULL, lambda = 0.5) {

    filter_fun <- function(data) {
        if (inherits(data, "matrix")) {
            return(apply(data, 2, smooth_whit, lambda = lambda,
                         length = ncol(data)))
        } else {
            return(smooth_whit(data, lambda = lambda, length = length(data)))
        }
    }

    result <- .sits_factory_function(data, filter_fun)

    return(result)
}

#' @rdname sits_filters
#'
#' @description
#' `sits_filter()`: applies a filter to all bands.
#'
#' @param filter   a filter function such as `sits_whittaker()` or
#' `sits_sgolay()`.
#'
#' @export
sits_filter <- function(data, filter = sits_whittaker()) {

    result <- .apply_across(data, fn = filter)

    return(result)
}

