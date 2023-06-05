#' @title Filter time series with smoothing filter
#' @name sits_filter
#' @param data          Time series or matrix.
#' @param filter        Filter function to be applied.
#'
#' @description
#'  Applies a filter to all bands, using a filter function
#'  such as `sits_whittaker()` or `sits_sgolay()`.
#'
#' @export
sits_filter <- function(data, filter = sits_whittaker()) {
    result <- filter(data)

    return(result)
}
#' @title Filter time series with whittaker filter
#' @name sits_whittaker
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description
#' The algorithm searches for an optimal warping polynomial.
#' The degree of smoothing depends on smoothing factor lambda
#' (usually from 0.5 to 10.0). Use lambda = 0.5 for very slight smoothing
#' and lambda = 5.0 for strong smoothing.
#'
#' @param data          Time series or matrix.
#' @param lambda        Smoothing factor to be applied (default 0.5).
#' @return              Filtered time series
#'
#' @references Francesco Vuolo, Wai-Tim Ng, Clement Atzberger,
#' "Smoothing and gap-filling of high resolution multi-spectral time series:
#' Example of Landsat data",
#' Int Journal of Applied Earth Observation and Geoinformation,
#' vol. 57, pg. 202-213, 2107.
#'
#' @seealso \link[sits]{sits_apply}
#'
#' @examples
#' if (sits_run_examples()) {
#' # Retrieve a time series with values of NDVI
#' point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
#'
#' # Filter the point using the Whittaker smoother
#' point_whit <- sits_filter(point_ndvi, sits_whittaker(lambda = 3.0))
#' # Merge time series
#' point_ndvi <- sits_merge(point_ndvi, point_whit, suffix = c("", ".WHIT"))
#'
#' # Plot the two points to see the smoothing effect
#' plot(point_ndvi)
#' }
#' @export
sits_whittaker <- function(data = NULL, lambda = 0.5) {
    filter_fun <- function(data) {
        if (inherits(data, "matrix")) {
            return(smooth_whit_mtx(data, lambda = lambda, length = ncol(data)))
        } else {
            return(smooth_whit(data, lambda = lambda, length = length(data)))
        }
    }

    filter_call <- function(data) {
        if (inherits(data, "sits")) {
            .apply_across(data = data, fn = filter_fun)
        } else {
            filter_fun(data)
        }
    }

    result <- .factory_function(data, filter_call)

    return(result)
}

#' @title Filter time series with Savitzky-Golay filter
#' @name sits_sgolay
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description
#' An optimal polynomial for warping a time series.
#' The degree of smoothing depends on the filter order (usually 3.0).
#' The order of the polynomial uses the parameter `order` (default = 3),
#' the size of the temporal window uses the parameter `length` (default = 5).
#'
#' @param data          Time series or matrix.
#' @param order         Filter order (integer).
#' @param length        Filter length (must be odd).
#' @return              Filtered time series
#'
#' @references A. Savitzky, M. Golay, "Smoothing and Differentiation
#' of Data by Simplified Least Squares Procedures".
#' Analytical Chemistry, 36 (8): 1627–39, 1964.
#'
#' @examples
#' if (sits_run_examples()) {
#' # Retrieve a time series with values of NDVI
#' point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
#'
#' # Filter the point using the Savitzky-Golay smoother
#' point_sg <- sits_filter(point_ndvi,
#'     filter = sits_sgolay(order = 3, length = 5)
#' )
#' # Merge time series
#' point_ndvi <- sits_merge(point_ndvi, point_sg, suffix = c("", ".SG"))
#'
#' # Plot the two points to see the smoothing effect
#' plot(point_ndvi)
#' }
#' @export
sits_sgolay <- function(data = NULL, order = 3, length = 5) {
    # compute filter coefficients once
    f_res <- .signal_sgolay_coef(p = order, n = length, ts = 1)
    # function to be applied
    filter_fun <- function(data) {
        # calculate coefficients for sgolay
        if (inherits(data, "matrix")) {
            return(smooth_sg_mtx(
                data,
                f_res = f_res,
                p = order,
                n = length)
            )
        } else {
            return(smooth_sg(
                data,
                f_res = f_res,
                p = order,
                n = length)
            )
        }
    }

    filter_call <- function(data) {
        if (inherits(data, "sits")) {
            .apply_across(data = data, fn = filter_fun)
        } else {
            filter_fun(data)
        }
    }


    result <- .factory_function(data, filter_call)

    return(result)
}
