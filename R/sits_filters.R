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
#' @param data          Time series or matrix.
#' @return              Filtered time series
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
#'
#' # Filter the point using the Savitsky Golay smoother
#' point_sg <- sits_filter(point_ndvi,
#'   filter = sits_sgolay(order = 3, length = 5)
#' )
#' # Merge time series
#' point_ndvi <- sits_merge(point_ndvi, point_sg, suffix = c("", ".SG"))
#'
#' # Plot the two points to see the smoothing effect
#' plot(point_ndvi)
#' @export
sits_sgolay <- function(data = NULL, order = 3, length = 5, scaling = 1) {
    filter_fun <- function(data) {
        if (inherits(data, "matrix")) {
            return(t(apply(data, 1, .sits_signal_sgolayfilt,
                           p = order,
                           n = length, ts = scaling
            )))
        } else {
            return(.sits_signal_sgolayfilt(data,
                                           p = order,
                                           n = length, ts = scaling
            ))
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
#' point_wt <- sits_filter(point_ndvi, filter = sits_whittaker(lambda = 3))
#' # Merge time series
#' point_ndvi <- sits_merge(point_ndvi, point_wt, suffix = c("", ".WT"))
#' # Plot the two points to see the smoothing effect
#' plot(point_ndvi)
#' @export
sits_whittaker <- function(data = NULL, lambda = 0.5) {
    filter_fun <- function(data) {
        if (inherits(data, "matrix")) {
            return(t(apply(data, 1, smooth_whit,
                           lambda = lambda,
                           length = ncol(data)
            )))
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

# Sits interface to the "signal" package
#
# The "signal" package is A set of signal processing functions originally written for
# 'Matlab' and 'Octave'.  Includes filter generation utilities,
# filtering functions, resampling routines, and visualization of
# filter models. It also includes interpolation functions.
#
# Further information on "signal"
# Authors:             Uwe Ligges [aut, cre] (new maintainer),
#                      Tom Short [aut], Paul Kienzle [aut], Sarah Schnackenberg [ctb],
#                      David Billinghurst [ctb], Hans-Werner Borchers [ctb],
#                      Andre Carezia [ctb], Pascal Dupuis [ctb], John W. Eaton [ctb],
#                      E. Farhi [ctb], Kai Habel [ctb], Kurt Hornik [ctb],
#                      Sebastian Krey [ctb], Bill Lash [ctb], Friedrich Leisch [ctb],
#                      Olaf Mersmann [ctb], Paulo Neis [ctb], Jaakko Ruohio [ctb],
#                      Julius O. Smith III [ctb], Doug Stewart [ctb],
#                      Andreas Weingessel [ctb]
# Maintainer:         Uwe Ligges <ligges@statistik.tu-dortmund.de>

# The code on this file has been lifted from the "signal" package

# The Savitsky-Golay filter of the "signal" package has been
# lifted to be part of "sits" and thus reduce the package load
# Since signal is licensed as GPL >= 2,
# sits is also licensed as GPL >= 2

#' @title Savitsky-Golay smoothing filter
#' @name .sits_signal_sgolayfilt
#'
#' @keywords internal
#'
#' @description  Smooth the data in x with a Savitsky-Golay smoothing filter of
#'   polynomial order p and length n, n odd, n > p.  By default, p=3
#'    and n=p+2 or n=p+3 if p is even. This filters is particularly good
#'    at preserving lineshape while
#'    removing high frequency squiggles

#' @param x            Time series vector.
#' @param p            Filter order (integer).
#' @param n            Filter length (must be odd)
#' @param m            Derivative to calculate (default = 0)
#' @param ts           Time scaling (integer).
#' @return             A time series with filtered values.
#'
.sits_signal_sgolayfilt <- function(x, p = 3, n = p + 3 - p %% 2, m = 0, ts = 1) {

    ## The first k rows of F are used to filter the first k points
    ## of the data set based on the first n points of the data set.
    ## The last k rows of F are used to filter the last k points
    ## of the data set based on the last n points of the dataset.
    ## The remaining data is filtered using the central row of F.
    ## As the filter coefficients are used in the reverse order of what
    ## seems the logical notation, reverse F[k+1,] so that antisymmetric
    ## sequences are used with the right sign.
    len <- length(x)

    F <- .sits_signal_sgolay(p, n, m, ts)
    k <- floor(n / 2)
    z <- .sits_signal_filter(F[k + 1, n:1], 1, x)
    y <- c(F[1:k, ] %*% x[1:n], z[n:len], F[(k + 2):n, ] %*% x[(len - n + 1):len])
    return(y)
}

#' @title Savitsky-Golay smoothing filter coefficients
#' @name .sits_signal_sgolay
#'
#' @description  Computes the filter coefficients for all Savitzky-Golay smoothing
#' filters of order p for length n (odd). m can be used in order to
#' get directly the mth derivative. In this case, ts is a scaling factor.
#'
#' The early rows of F smooth based on future values and later rows
#' smooth based on past values, with the middle row using half future
#' and half past.  In particular, you can use row i to estimate x(k)
#' based on the i-1 preceding values and the n-i following values of x
#' values as y(k) = F(i,:) * x(k-i+1:k+n-i).
#'
#' @keywords internal
#'
#' @param p            Filter order (integer).
#' @param n            Filter length (must be odd)
#' @param m            Derivative to calculate (default = 0)
#' @param ts           Time scaling (integer).
#' @return             filter coefficients
.sits_signal_sgolay <- function(p, n, m = 0, ts = 1) {
    if (n %% 2 != 1) {
        stop("sgolay needs an odd filter length n")
    }
    if (p >= n) {
        stop("sgolay needs filter length n larger than polynomial order p")
    }

    ## Construct a set of filters from complete causal to completely
    ## noncausal, one filter per row.  For the bulk of your data you
    ## will use the central filter, but towards the ends you will need
    ## a filter that doesn't go beyond the end points.
    Fm <- matrix(0., n, n)
    k <- floor(n / 2)
    for (row in 1:(k + 1)) {
        ## Construct a matrix of weights Cij = xi ^ j.  The points xi are
        ## equally spaced on the unit grid, with past points using negative
        ## values and future points using positive values.
        Ce <- (((1:n) - row) %*% matrix(1, 1, p + 1))^(matrix(1, n) %*% (0:p))
        ## A = pseudo-inverse (C), so C*A = I; this is constructed from the SVD
        A <- .sits_MASS_ginv(Ce, tol = .Machine$double.eps)
        ## Take the row of the matrix corresponding to the derivative
        ## you want to compute.
        Fm[row, ] <- A[1 + m, ]
    }
    ## The filters shifted to the right are symmetric with those to the left.
    Fm[(k + 2):n, ] <- (-1)^m * Fm[k:1, n:1]
    # if (m > 0)
    #     Fm <- Fm * prod(1:m) / (ts^m)
    class(Fm) <- "sgolayFilter"
    return(Fm)
}

# Octave/Matlab-compatible filter function
# y = filter (b, a, x)
.sits_signal_filter <- function(filt, a, x, init, init.x, init.y, ...) {
    if (missing(init.x)) {
        init.x <- c(rep(0, length(filt) - 1))
    }
    # if (length(init.x) != length(filt) - 1)
    #     stop("length of init.x should match filter length-1 = ", length(filt) - 1)
    # if (missing(init) && !missing(init.y))
    #     init <- rev(init.y)
    # if (all(is.na(x)))
    #     return(x)
    if (length(filt)) {
        x1 <- stats::filter(c(init.x, x), filt / a[1], sides = 1)
        # if (all(is.na(x1)))
        #     return(x)
        x <- stats::na.omit(x1, filt / a[1], sides = 1)
    }
    # if (length(a) >= 2)
    #     x <- stats::filter(x, -a[-1] / a[1], method = "recursive", init = init)
    return(x)
}
#' @title Generalized Inverse of a Matrix
#'
#' @description Calculates the Moore-Penrose generalized inverse of a matrix X.
#'
#' @keywords internal
#'
#' @param X Matrix for which the Moore-Penrose inverse is required.
#' @param tol A relative tolerance to detect zero singular values.
#' @return A MP generalized inverse matrix for X.
#'
#' @references
#' Venables, W. N. and Ripley, B. D. (1999)
#' Modern Applied Statistics with S-PLUS.
#'
.sits_MASS_ginv <- function(X, tol = sqrt(.Machine$double.eps)) {
    #
    # based on suggestions of R. M. Heiberger, T. M. Hesterberg and WNV
    #
    # if (length(dim(X)) > 2L || !(is.numeric(X) || is.complex(X)))
    #     stop("'X' must be a numeric or complex matrix")
    # if (!is.matrix(X)) X <- as.matrix(X)
    Xsvd <- svd(X)
    # if (is.complex(X)) Xsvd$u <- Conj(Xsvd$u)
    # Positive <- Xsvd$d > max(tol * Xsvd$d[1L], 0)
    # if (all(Positive)) Xsvd$v %*% (1/Xsvd$d * t(Xsvd$u))
    # else if (!any(Positive)) array(0, dim(X)[2L:1L])
    # else Xsvd$v[, Positive, drop = FALSE] %*% ((1/Xsvd$d[Positive]) * t(Xsvd$u[, Positive, drop = FALSE]))
    Xsvd$v %*% (1 / Xsvd$d * t(Xsvd$u))
}
