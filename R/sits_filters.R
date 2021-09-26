#' @title General function for filtering
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
#' @title Envelope filter
#' @name sits_envelope
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  This function computes the envelope of a time series using the
#' streaming algorithm proposed by Lemire (2009).
#' This functions calls `dtwclust::compute_envelope` function.
#' @param data         A tibble with time series data and metadata.
#' @param operations   A character sequence for the sequence operations.
#'                     ("U" for upper filter, "L" for lower filter).
#' @param bands_suffix Suffix of the resulting data.
#' @return             A tibble with filtered time series values.
#' @examples
#' # Select the NDVI band of a point in Mato Grosso
#' point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
#' # Apply the envelope filter
#' point_env <- sits_envelope(point_ndvi)
#' # Merge the filtered with the raw data
#' point2 <- sits_merge(point_ndvi, point_env)
#' # Plot the result
#' plot(point2)
#' @export
sits_envelope <- function(data = NULL,
                          operations = "UULL",
                          bands_suffix = "env") {

    # set caller to show in errors
    .check_set_caller("sits_envelope")

    # verifies if dtwclust package is installed
    if (!requireNamespace("dtwclust", quietly = TRUE)) {
        stop("dtwclust needed for this function to work.
             Please install it.", call. = FALSE)
    }

    filter_fun <- function(data) {
        # definitions of operations
        def_op <- list(
            "U" = "upper", "L" = "lower",
            "u" = "upper", "l" = "lower"
        )

        # split envelope operations
        operations <- strsplit(operations, "")[[1]]

        # verify if operations are either "U" or "L"
        .check_chr_within(
          x = operations,
          within = names(def_op),
          msg = "invalid operation sequence"
        )

        # compute envelopes
        result <- sits_apply(data,
            fun = function(band) {
                for (op in operations) {
                    upper_lower <- dtwclust::compute_envelope(band,
                        window.size = 1,
                        error.check = FALSE
                    )
                    band <- upper_lower[[def_op[[op]]]]
                }
                return(band)
            },
            fun_index = function(band) band,
            bands_suffix = bands_suffix
        )
        return(result)
    }
    result <- .sits_factory_function(data, filter_fun)
    return(result)
}

#' @title Interpolation function of the time series of a sits_tibble
#' @name sits_interp
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Computes the linearly interpolated bands
#'               using the R base function approx.
#' @param data       A tibble with time series data and metadata.
#' @param fun           Interpolation function.
#' @param n             Number of time series elements to be created
#'                      between start date and end date.
#'                      When a class function is passed to `n`,
#'                      it is evaluated with each band time series as
#'                      an argument, e.g. n(band) (default: `length` function).
#' @param ...           Additional parameters to be used by the fun function.
#' @return A tibble with same samples and the new bands.
#' @examples
#' # Retrieve a time series with values of NDVI
#' point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
#' # find out how many time instances are there
#' n_times <- nrow(sits_time_series(point_ndvi))
#' # interpolate three times more points
#' point_int.tb <- sits_interp(point_ndvi, fun = stats::spline, n = 3 * n_times)
#' # plot the result
#' plot(point_int.tb)
#' @export
sits_interp <- function(data = NULL, fun = stats::approx,
                        n = base::length, ...) {
    filter_fun <- function(data) {
        # compute linear approximation
        result <- sits_apply(data,
            fun = function(band) {
                if (inherits(n, "function")) {
                      return(fun(band, n = n(band), ...)$y)
                  }
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
    result <- .sits_factory_function(data, filter_fun)
    return(result)
}
#' @title Interpolation function of the time series in a sits tibble
#' @name sits_linear_interp
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Computes the linearly interpolated bands for a given resolution
#'               using the R base function approx.
#' @param data       A tibble with time series data and metadata.
#' @param n          Number of time series elements to be created
#'                   between start date and end date.
#' @return           A sits tibble with same samples and the new bands.
#' @examples
#' # Retrieve a time series with values of NDVI
#' point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
#' # find out how many time instances are there
#' n_times <- nrow(sits_time_series(point_ndvi))
#' # interpolate three times more points
#' point_int.tb <- sits_linear_interp(point_ndvi, n = 3 * n_times)
#' # plot the result
#' plot(point_int.tb)
#' @export
sits_linear_interp <- function(data = NULL, n = 23) {

    filter_fun <- function(data) {
        # compute linear approximation
        result <- sits_apply(data,
            fun = function(band) stats::approx(band, n = n, ties = mean)$y,
            fun_index = function(band) {
                  as.Date(stats::approx(band, n = n, ties = mean)$y,
                      origin = "1970-01-01"
                  )
              }
        )
        return(result)
    }
    result <- .sits_factory_function(data, filter_fun)
    return(result)
}

#' @title Remove missing values
#' @name sits_missing_values
#' @author Gilberto Camara, \email{gilberto.camara@inpe.br}
#' @description       This function removes the missing values from
#'                    an image time series by substituting them by NA.
#' @param data        A tibble with time series data and metadata.
#' @param miss_value  Number indicating missing values in a time series.
#' @return            Time series data and metadata (missing values removed).
#' @export
sits_missing_values <- function(data, miss_value) {

    # test if data has data
    .sits_tibble_test(data)

    # remove missing values by NAs
    result <- sits_apply(data, fun = function(band) {
          return(ifelse(band == miss_value, NA, band))
      })
    return(result)
}

#' @title Smooth the time series using Savitsky-Golay filter
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

    # verifies if signal package is installed
    if (!requireNamespace("signal", quietly = TRUE)) {
        stop("signal required for this function to work.
             Please install it.", call. = FALSE)
    }

    filter_fun <- function(data) {
        if (inherits(data, "tbl")) {
            result <- sits_apply(data,
                                 fun = function(band) {
                                     signal::sgolayfilt(band,
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
                signal::sgolayfilt(row,
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

#' @title Filter the time series using Whittaker smoother
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

    if (!requireNamespace("ptw", quietly = TRUE)) {
        stop("Please install package ptw.", call. = FALSE)
    }
    filter_fun <- function(data) {
        result <- NULL
        if (inherits(data, "tbl")) {
            result <- sits_apply(data,
                fun = function(band) {
                    ptw::whit2(band, lambda = lambda)
                },
                fun_index = function(band) band,
                bands_suffix = bands_suffix
            )
        }
        if (inherits(data, "matrix")) {
            result <- apply(
                data, 2,
                function(row) {
                    ptw::whit2(row, lambda = lambda)
                }
            )
        }
        return(result)
    }
    result <- .sits_factory_function(data, filter_fun)
    return(result)
}
