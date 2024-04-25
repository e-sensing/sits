# Sits interface to the "signal" package
#
# The "signal" package is A set of signal processing functions originally
# written for 'Matlab' and 'Octave'.  Includes filter generation utilities,
# filtering functions, resampling routines, and visualization of
# filter models. It also includes interpolation functions.
#
# Further information on "signal"
# Authors:             Uwe Ligges [aut, cre] (new maintainer),
#                      Tom Short [aut], Paul Kienzle [aut],
#                      Sarah Schnackenberg [ctb], David Billinghurst [ctb],
#                      Hans-Werner Borchers [ctb], Andre Carezia [ctb],
#                      Pascal Dupuis [ctb], John W. Eaton [ctb],
#                      E. Farhi [ctb], Kai Habel [ctb], Kurt Hornik [ctb],
#                      Sebastian Krey [ctb], Bill Lash [ctb],
#                      Friedrich Leisch [ctb], Olaf Mersmann [ctb],
#                      Paulo Neis [ctb], Jaakko Ruohio [ctb],
#                      Julius O. Smith III [ctb], Doug Stewart [ctb],
#                      Andreas Weingessel [ctb]
# Maintainer:          Uwe Ligges <ligges@statistik.tu-dortmund.de>

# The code on this file has been lifted from the "signal" package

# The Savitzky-Golay filter of the "signal" package has been
# lifted to be part of "sits" and thus reduce the package load
# Since signal is licensed as GPL >= 2,
# sits is also licensed as GPL >= 2

#' @title Savitzky-Golay smoothing filter coefficients
#' @name .signal_sgolay_coef
#'
#' @description  Computes the filter coefficients for all Savitzky-Golay
#' smoothing filters of order p for length n (odd). m can be used in order to
#' get directly the mth derivative. In this case, ts is a scaling factor.
#'
#' The early rows of F smooth based on future values and later rows
#' smooth based on past values, with the middle row using half future
#' and half past.  In particular, you can use row i to estimate x(k)
#' based on the i-1 preceding values and the n-i following values of x
#' values as y(k) = F(i,:) * x(k-i+1:k+n-i).
#'
#' @keywords internal
#' @noRd
#' @param p            Filter order (integer).
#' @param n            Filter length (must be odd)
#' @param m            Derivative to calculate (default = 0)
#' @param ts           Time scaling (integer).
#' @return             filter coefficients
.signal_sgolay_coef <- function(p, n, m = 0, ts = 1) {
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
    filter <- matrix(0., n, n)
    k <- floor(n / 2)
    for (row in 1:(k + 1)) {
        ## Construct a matrix of weights Cij = xi ^ j.  The points xi are
        ## equally spaced on the unit grid, with past points using negative
        ## values and future points using positive values.
        weights <- (((1:n) - row) %*%
                        matrix(1, 1, p + 1))^(matrix(1, n) %*% (0:p))
        ## A = pseudo-inverse (C), so C*A = I; this is constructed from the SVD
        pseudo_inv <- .signal_mass_ginv(weights, tol = .Machine[["double.eps"]])
        ## Take the row of the matrix corresponding to the derivative
        ## you want to compute.
        filter[row, ] <- pseudo_inv[1 + m, ]
    }
    ## The filters shifted to the right are symmetric with those to the left.
    filter[(k + 2):n, ] <- (-1)^m * filter[k:1, n:1]
    class(filter) <- "sgolay_filter"
    return(filter)
}


#' @title Generalized Inverse of a Matrix
#'
#' @description Calculates the Moore-Penrose generalized inverse of a matrix X.
#'
#' @keywords internal
#' @noRd
#'
#' @param mtx Matrix for which the Moore-Penrose inverse is required.
#' @param tol A relative tolerance to detect zero singular values.
#' @return A MP generalized inverse matrix for X.
#'
#' @references
#' Venables, W. N. and Ripley, B. D. (1999)
#' Modern Applied Statistics with S-PLUS.
#'
.signal_mass_ginv <- function(mtx, tol = sqrt(.Machine[["double.eps"]])) {
    mtx_svd <- svd(mtx)
    mtx_svd[["v"]] %*% (1 / mtx_svd[["d"]] * t(mtx_svd[["u"]]))
}
