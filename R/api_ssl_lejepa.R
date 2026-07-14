# ---- LeJEPA self-supervised learning helpers ----

#' @title Split samples into train/val for LeJEPA pre-training
#' @name .lejepa_data_split
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' Normalises \code{samples} and performs a train/validation split of
#' sample indices.
#'
#' @param samples          \code{sits} tibble of time-series samples.
#' @param ml_stats         List of normalisation statistics returned by
#'   \code{.samples_stats()}.
#' @param validation_split Numeric in (0, 1). Fraction held out for
#'   validation.
#'
#' @return A named list with elements \code{train} and \code{val}. Each
#'   element is itself a list with component \code{feats} (a numeric
#'   matrix of shape \code{[n_split, n_times * n_bands]}).
#'
.lejepa_data_split <- function(samples, ml_stats, validation_split) {
    preds    <- .predictors(samples)
    # [n, n_times*n_bands]
    feats    <- .pred_features_normalize(preds, stats = ml_stats)

    n_samples <- nrow(feats)
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

#' @title Torch Dataset for LeJEPA with resampling augmentation
#' @name .lejepa_resampling_dataset
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' A \code{torch::dataset} that yields one item per sample.  For each
#' sample, two augmented views are created on-the-fly using the
#' resampling strategy via \code{.ssl_apply_resampling()}.
#'
#' @param split   List with element \code{feats}, a numeric matrix
#'   of shape \code{[n_samples, n_times * n_bands]}.
#' @param n_times Integer. Number of time steps in the time series.
#'
#' @return A \code{torch::dataset} object compatible with
#'   \code{torch::dataloader}.
#'
.lejepa_resampling_dataset <- torch::dataset(
    name = ".LeJEPAResamplingDataset",

    initialize = function(split, n_times) {
        self$feats   <- split[["feats"]]
        self$n_times <- as.integer(n_times)
        self$n_bands <- as.integer(ncol(split[["feats"]]) / n_times)
    },

    .getitem = function(i) {
        nt <- self$n_times
        nb <- self$n_bands
        sample_mat <- matrix(self$feats[i, ], nrow = nt, ncol = nb)
        views <- .ssl_apply_resampling(sample_mat)

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

#' @title Sketched Isotropic Gaussian Regularization (SIGReg)
#' @name .lejepa_sigreg
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' A \code{torch::nn_module} implementing the SIGReg objective from
#' LeJEPA (Balestriero & LeCun, 2025).  SIGReg constrains embeddings
#' to follow an isotropic Gaussian distribution by comparing the
#' empirical characteristic function (ECF) of random 1-D projections
#' to the standard Gaussian characteristic function, using the
#' Epps–Pulley statistic with numerical quadrature.
#'
#' The module uses registered buffers for the quadrature knots
#' (\code{t}), the Gaussian window (\code{phi}), and the integration
#' weights (\code{weights}), so they automatically move to the correct
#' device.
#'
#' @param num_knots  Integer. Number of quadrature knots for the ECF
#'   test (default 17).
#' @param num_slices Integer. Number of random projection directions
#'   (default 256).
#'
#' @details
#' The forward pass expects a tensor of shape \code{[V, B, D]} where
#' \code{V} is the number of views, \code{B} is the batch size, and
#' \code{D} is the projection dimensionality.  The ECF is computed
#' per-view across the batch dimension and compared to the standard
#' Gaussian CF \code{exp(-t^2/2)}.  The resulting scalar loss
#' measures departure from Gaussianity.
#'
#' @references
#' Balestriero, R. & LeCun, Y. (2025).
#' \emph{LeJEPA: Provable and Scalable Self-Supervised Learning
#' Without the Heuristics}. arXiv:2511.08544.
#'
.lejepa_sigreg <- torch::nn_module(
    classname = "SIGReg",

    initialize = function(num_knots = 17L, num_slices = 256L) {
        t <- torch::torch_linspace(0, 3, num_knots)
        dt <- 3 / (num_knots - 1L)
        weights <- torch::torch_full(c(num_knots), 2 * dt)
        weights[1] <- dt
        weights[num_knots] <- dt
        phi <- torch::torch_exp(-t$square() / 2.0)
        self$register_buffer("t", t)
        self$register_buffer("phi", phi)
        self$register_buffer("weights", weights * phi)
        self$num_slices <- as.integer(num_slices)
    },

    forward = function(proj) {
        # proj: [V, B, D]
        D <- proj$size(-1)

        # Move buffers to match input device (avoids hardcoding device)
        if (!self$t$device == proj$device) {
            self$to(device = proj$device)
        }

        # Random unit projection directions (with epsilon for stability)
        A <- torch::torch_randn(c(D, self$num_slices), device = proj$device)
        A <- A$div_(A$norm(p = 2L, dim = 1L, keepdim = TRUE) + 1e-8)

        # Project and evaluate ECF at quadrature knots
        # (proj @ A): [V, B, S] -> unsqueeze: [V, B, S, 1] * t: [K]
        x_t <- proj$matmul(A)$unsqueeze(-1L) * self$t    # [V, B, S, K]

        # Empirical CF vs Gaussian CF
        err <- (x_t$cos()$mean(-3L) - self$phi)$square() +
                x_t$sin()$mean(-3L)$square()              # [V, S, K]
        statistic <- err$matmul(self$weights) * proj$size(-2L)  # [V, S]
        statistic$mean()
    }
)
