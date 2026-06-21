

.mvp_spatial_filter <- function(sd_samples, q = 0.10, na_rm = TRUE) {

    # predictors: expected (n_samples x n_times x n_bands)
    spatial_ft <- sd_samples |>
        .predictors() |>
        .pred_features()

    n_samples <- nrow(sd_samples)

    # per-sample scalar score:
    # take median across time for each band, then median across bands
    sd_score <- vapply(seq_len(n_samples), function(i) {
        mat <- spatial_ft[i, , , drop = TRUE]         # (n_times x n_bands)
        band_meds <- apply(mat, 2, stats::median, na.rm = na_rm)
        stats::median(band_meds, na.rm = na_rm)
    }, numeric(1))

    # thresholds
    q_low  <- stats::quantile(sd_score, probs = q,     na.rm = na_rm, names = FALSE, type = 7)
    q_high <- stats::quantile(sd_score, probs = 1 - q, na.rm = na_rm, names = FALSE, type = 7)

    # keep only tails (low + high), drop ambiguous middle
    keep_low  <- which(sd_score <= q_low)
    keep_high <- which(sd_score >= q_high)
    keep_idx  <- sort(unique(c(keep_low, keep_high)))

    list(
        sd_score = sd_score,
        q = q,
        q_low = q_low,
        q_high = q_high,
        keep_low = keep_low,
        keep_high = keep_high,
        keep_idx = keep_idx
    )
}

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
#' @param combine     Character. The user's preferred method to combine the 'jump' and 'persistense' disruption metrics. Default is "product".
#' @param w_jump      Numeric. The weight the jump will have on the drop score
#' @param w_persist   Numeric. The weight the persist will have on the drop score
#'
#' @return A list Containing the max split score and the time step in which it occurred.
.mvp_best_split_drop <- function(X,
                                 dir = NULL,
                                 min_seg = 2L,
                                 combine = "product",
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
