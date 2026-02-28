#' @title Get best split drop for MVP
#' @author Alexandre Assuncao \email{alexcarssuncao@@gmail.com}
#'
#' @description Scores all time series' time steps to determine the most
#'              likely time for an event like deforestation.
#'
#' @param X           A sits time series tibble
#' @param dir         The "direction" of the decrease in the time series value. With NDVI, the ts should be split when
#'                    the sharpest decrease occurs. With SWIR, the opposite holds true. Default: NULL; will split on sharpest decrease.
#' @param min_seg     The smaller size eithr ts segment can have after the split Default 2L.
#' @param combine     Character. The user's preferred method to combine the 'jump' and 'persistense' disruption metrics
#' @param w_jump      Numeric. The weight the jump will have on the drop score
#' @param w_persist   Numeric. The weight the persist will have on the drop score
#'
#' @return A list Containing the max split score and the time step in which it occurred.
.mvp_best_split_drop <- function(X,
                                 dir = NULL,
                                 min_seg = 2L,
                                 combine = c("product", "sum"),
                                 w_jump = 1,
                                 w_persist = 1) {
    # ts into matrix
    X <- as.matrix(X)
    # n_times and n_bands as T and B to avoid clutter
    T <- nrow(X); B <- ncol(X)

    # check if ts is big enough to split
    if (T < 2L * min_seg) return(list(score = 0, k = NA_integer_))

    # scale columns
    #  - Using MAD scaling since quantile normalization will be applied when training model
    med <- apply(X, 2, stats::median, na.rm = TRUE)
    mad <- apply(X, 2, stats::mad, na.rm = TRUE)
    Xs <- sweep(X, 2, med, "-")
    Xs <- sweep(Xs, 2, mad + 1e-6, "/")

    # split ts in all bands
    if (is.null(dir)) dir <- rep(1, B)
    k_grid <- seq.int(min_seg, T - min_seg)

    # calculate drop scores
    scores <- vapply(k_grid, function(k) {
        mu_before <- colMeans(Xs[1:k, , drop = FALSE], na.rm = TRUE)
        mu_after  <- colMeans(Xs[(k + 1L):T, , drop = FALSE], na.rm = TRUE)
        # persistence: before mean - after mean; the sign is in dir
        persist <- max(sum(dir * (mu_before - mu_after)), 0)
        # jump: point k - point k+1; the sign is in dir
        jump <- max(sum(dir * (Xs[k, ] - Xs[k + 1L, ])), 0)
        # combine jump and persistence to catch events
        if (combine == "product") {
            jump * persist
        } else {
            w_jump * jump + w_persist * persist
        }
    }, numeric(1))

    # return split idx and time_step
    k_best <- k_grid[which.max(scores)]
    list(score = max(scores, na.rm = TRUE), k = k_best)
}
