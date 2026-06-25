# ---- VICReg self-supervised learning helpers ----

#' @title Split samples into train/val for VICReg pre-training
#' @name .vicreg_data_split
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' Normalises \code{samples} and performs a train/validation split of sample
#' indices. No pre-computed pairs are built because VICReg creates
#' views on-the-fly via time-warping augmentation.
#'
#' @param samples          \code{sits} tibble of time-series samples.
#' @param validation_split Numeric in (0, 1). Fraction held out for
#'   validation.
#'
#' @return A named list with elements \code{train} and \code{val}. Each
#'   element is itself a list with components:
#'   \describe{
#'     \item{\code{feats}}{Numeric matrix of shape
#'       \code{[n_split, n_times * n_bands]} — normalised features.}
#'   }
#'
.vicreg_data_split <- function(samples, validation_split) {
    # Compute normalisation statistics and build normalised feature matrix
    ml_stats <- .samples_stats(samples)
    preds    <- .pred_normalize(.predictors(samples), stats = ml_stats)
    feats    <- as.matrix(.pred_features(preds))   # [n, n_times*n_bands]

    n_samples <- nrow(feats)

    # Train / validation split
    idx   <- sample.int(n_samples)
    n_val <- floor(length(idx) * validation_split)
    if (n_val > 0L) {
        val_idx   <- idx[seq_len(n_val)]
        train_idx <- idx[-seq_len(n_val)]
    } else {
        val_idx   <- integer(0L)
        train_idx <- idx
    }

    list(
        train = list(feats = feats[train_idx, , drop = FALSE]),
        val   = list(feats = feats[val_idx,   , drop = FALSE])
    )
}

#' @title Resampling augmentation for time-series contrastive learning
#' @name .vicreg_apply_resampling
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' Generates two augmented views from a single multivariate time series
#' using the resampling strategy of Saget et al. (2025).  The procedure
#' creates views that preserve the overall temporal structure while
#' introducing controlled variation, making them suitable as positive
#' pairs for contrastive learning.
#'
#' The three-step algorithm:
#' \enumerate{
#'   \item \strong{Upsample} the input to \code{2 * T} timesteps via
#'     linear interpolation.
#'   \item \strong{Subsample} two disjoint subsequences of \code{T / 2}
#'     timesteps each, ensuring at least
#'     \code{floor((T / 2) / 4)} samples fall in each quarter of the
#'     upsampled range (temporal coverage constraint).
#'   \item \strong{Resample} each subsequence back to the original
#'     length \code{T} by rescaling timestamps to \code{[1, T]} and
#'     linearly interpolating.
#' }
#'
#' @param ts_mat Numeric matrix of shape \code{[n_times, n_bands]}.
#'
#' @return A named list with two elements:
#'   \describe{
#'     \item{\code{view1}}{Numeric matrix \code{[n_times, n_bands]}.}
#'     \item{\code{view2}}{Numeric matrix \code{[n_times, n_bands]}.}
#'   }
#'
#' @references
#' Saget, A., Lafabregue, B., Cornuéjols, A., & Gançarski, P. (2025).
#' \emph{Resampling Augmentation for Time Series Contrastive Learning:
#' Application to Remote Sensing}.
#' arXiv:2506.18587.
#'
.vicreg_apply_resampling <- function(ts_mat) {
    n_times <- nrow(ts_mat)
    n_bands <- ncol(ts_mat)
    t_up    <- 2L * n_times

    # Step 1: Upsample to 2T timesteps via linear interpolation
    orig_idx <- seq_len(n_times)
    up_idx   <- seq(1, n_times, length.out = t_up)
    up_mat   <- matrix(0, nrow = t_up, ncol = n_bands)
    for (b in seq_len(n_bands)) {
        up_mat[, b] <- stats::approx(
            orig_idx, ts_mat[, b], xout = up_idx, rule = 2
        )$y
    }

    # Step 2: Draw two disjoint subsequences with quarter coverage
    t_sub        <- as.integer(n_times %/% 2)
    per_quarter  <- as.integer(t_sub %/% 4)
    quarter_size <- as.integer(t_up %/% 4)

    # Partition upsampled indices into 4 quarters
    quarters <- lapply(0:3, function(j) {
        start <- j * quarter_size + 1L
        end   <- min((j + 1L) * quarter_size, t_up)
        seq.int(start, end)
    })

    # For each quarter, draw per_quarter indices for each view
    idx1 <- integer(0)
    idx2 <- integer(0)
    for (q in quarters) {
        sel1 <- sort(sample(q, per_quarter))
        remain <- setdiff(q, sel1)
        sel2 <- sort(sample(remain, per_quarter))
        idx1 <- c(idx1, sel1)
        idx2 <- c(idx2, sel2)
    }

    # Fill remaining slots from leftover indices
    used     <- union(idx1, idx2)
    leftover <- setdiff(seq_len(t_up), used)
    need1    <- t_sub - length(idx1)
    need2    <- t_sub - length(idx2)
    if (need1 + need2 > 0L && length(leftover) > 0L) {
        extra <- sample(leftover, min(need1 + need2, length(leftover)))
        if (need1 > 0L) {
            n_take <- min(need1, length(extra))
            idx1   <- sort(c(idx1, extra[seq_len(n_take)]))
            extra  <- extra[-seq_len(n_take)]
        }
        if (need2 > 0L && length(extra) > 0L) {
            n_take <- min(need2, length(extra))
            idx2   <- sort(c(idx2, extra[seq_len(n_take)]))
        }
    }

    # Extract subsequences
    sub1 <- up_mat[idx1, , drop = FALSE]
    sub2 <- up_mat[idx2, , drop = FALSE]

    # Step 3: Resample each subsequence to the original T positions
    resample_to_t <- function(sub_mat, sub_idx) {
        rng <- max(sub_idx) - min(sub_idx)
        if (rng == 0) rng <- 1
        rescaled <- (sub_idx - min(sub_idx)) / rng * (n_times - 1) + 1
        out    <- matrix(0, nrow = n_times, ncol = n_bands)
        target <- seq_len(n_times)
        for (b in seq_len(n_bands)) {
            out[, b] <- stats::approx(
                rescaled, sub_mat[, b], xout = target, rule = 2
            )$y
        }
        out
    }

    list(
        view1 = resample_to_t(sub1, idx1),
        view2 = resample_to_t(sub2, idx2)
    )
}

#' @title Torch Dataset for VICReg with resampling augmentation
#' @name .vicreg_resampling_dataset
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' A \code{torch::dataset} that yields one item per sample.  For each
#' sample, two augmented views are created on-the-fly using the
#' resampling strategy of Saget et al. (2025) via
#' \code{.vicreg_apply_resampling()}.
#'
#' Each item is a list with:
#' \describe{
#'   \item{\code{x}}{A \code{float} tensor of shape
#'     \code{[2, n_times, n_bands]}.}
#'   \item{\code{y}}{A scalar zero tensor (dummy target; the VICReg
#'     loss ignores it).}
#' }
#'
#' @param split   List with element \code{feats}, a numeric matrix
#'   of shape \code{[n_samples, n_times * n_bands]}.
#' @param n_times Integer. Number of time steps in the time series.
#'
#' @return A \code{torch::dataset} object compatible with
#'   \code{torch::dataloader}.
#'
.vicreg_resampling_dataset <- torch::dataset(
    name = ".VICRegResamplingDataset",

    initialize = function(split, n_times) {
        self$feats   <- split[["feats"]]
        self$n_times <- as.integer(n_times)
        self$n_bands <- as.integer(ncol(split[["feats"]]) / n_times)
    },

    .getitem = function(i) {
        nt <- self$n_times
        nb <- self$n_bands

        # Reshape flat feature vector to [n_times, n_bands]
        sample_mat <- matrix(self$feats[i, ], nrow = nt, ncol = nb)

        # Apply resampling augmentation (returns both views)
        views <- .vicreg_apply_resampling(sample_mat)

        list(
            x = torch::torch_stack(list(
                torch::torch_tensor(views$view1, dtype = torch::torch_float()),
                torch::torch_tensor(views$view2, dtype = torch::torch_float())
            )),
            y = torch::torch_zeros(1L, dtype = torch::torch_float())
        )
    },

    .length = function() {
        nrow(self$feats)
    }
)

#' @title Extract off-diagonal elements of a square tensor
#' @name .vicreg_off_diagonal
#' @keywords internal
#' @noRd
#'
#' @description
#' Returns a 1-D tensor containing every element of the square matrix
#' \code{x} that is \emph{not} on the main diagonal.  Used by the VICReg
#' covariance regularisation term.
#'
#' The implementation mirrors the reference \code{off_diagonal()} function
#' from the Facebook Research VICReg codebase: flatten, drop the last
#' element, reshape to \code{(n-1, n+1)}, take columns \code{2:(n+1)},
#' and flatten again.
#'
#' @param x A square \code{torch_tensor} of shape \code{[n, n]}.
#'
#' @return A 1-D \code{torch_tensor} of length \code{n*(n-1)}.
#'
.vicreg_off_diagonal <- function(x) {
    n <- x$size(1)
    x$flatten()[1:(n * n - 1)]$view(c(n - 1L, n + 1L))[, 2:(n + 1)]$flatten()
}
