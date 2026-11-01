#' @title Shared helpers for self-supervised (SSL) pre-training
#' @name api_ssl
#' @keywords internal
#' @noRd
#' @description
#' Internal helpers shared by the self-supervised pre-training methods
#' (`sits_ssl_lejepa`, `sits_ssl_vicreg`, `sits_barlow_twins`,
#' `sits_contrastive_learning`, `sits_ssl_mae`). They consolidate the
#' boilerplate that surrounds each method's unique dataset, model and loss:
#' the projector head, the luz stub used to expose the encoder to
#' `sits_encode()`, and the training callbacks.
NULL

#' @title Build a projector head for joint-embedding SSL methods
#' @name .ssl_projector_head
#' @keywords internal
#' @noRd
#' @description Builds the 3-layer MLP projector head (Linear + BatchNorm +
#' ReLU, twice, then a final bias-free Linear) shared by LeJEPA, VICReg and
#' Barlow Twins. The final layer has no activation.
#' @param embedding_dim Encoder embedding dimension (projector input).
#' @param proj_dim      Projector hidden/output dimension.
#' @return A `torch::nn_sequential` module.
.ssl_projector_head <- function(embedding_dim, proj_dim) {
    torch::nn_sequential(
        torch::nn_linear(embedding_dim, proj_dim, bias = FALSE),
        torch::nn_batch_norm1d(proj_dim),
        torch::nn_relu(),
        torch::nn_linear(proj_dim, proj_dim, bias = FALSE),
        torch::nn_batch_norm1d(proj_dim),
        torch::nn_relu(),
        torch::nn_linear(proj_dim, proj_dim, bias = FALSE)
    )
}

#' @title Build the luz callback list for SSL training
#' @name .ssl_callbacks
#' @keywords internal
#' @noRd
#' @description Builds the callback list for SSL training: a learning-rate
#' scheduler is always included, and early stopping is added only when
#' validation data is available.
#' @param patience         Early-stopping patience.
#' @param min_delta        Early-stopping minimum delta.
#' @param lr_decay_epochs  Step size for the LR scheduler.
#' @param lr_decay_rate    Gamma for the LR scheduler.
#' @param has_validation   Whether validation data is available.
#' @return A list of `luz` callbacks.
.ssl_callbacks <- function(patience, min_delta, lr_decay_epochs,
                           lr_decay_rate, has_validation) {
    callbacks <- list(
        luz::luz_callback_lr_scheduler(
            torch::lr_step,
            step_size = lr_decay_epochs,
            gamma     = lr_decay_rate
        )
    )
    if (has_validation) {
        callbacks <- c(
            list(luz::luz_callback_early_stopping(
                monitor   = "valid_loss",
                mode      = "min",
                patience  = patience,
                min_delta = min_delta
            )),
            callbacks
        )
    }
    callbacks
}

#' @title Create dummy data to register a luz module structure
#' @name .ssl_stub_data
#' @keywords internal
#' @noRd
#' @description Builds a tiny (<= 10 rows) normalized snapshot of the samples,
#' used only to register a module structure via `luz::fit(..., epochs = 0L)`
#' when wrapping a trained encoder for `sits_encode()`.
#' @param samples  Training samples.
#' @param ml_stats Normalization statistics.
#' @param n_times  Number of time steps.
#' @param n_bands  Number of bands.
#' @return A list with `train_x` (3D array) and `train_y` (numeric labels).
.ssl_stub_data <- function(samples, ml_stats, n_times, n_bands) {
    labels <- .samples_labels(samples)
    code_labels <- seq_along(labels)
    names(code_labels) <- labels
    stub_samples <- samples[seq_len(min(10L, nrow(samples))), ]
    train_samples    <- .predictors(stub_samples)
    feats    <- .pred_features_normalize(
        pred  = train_samples,
        stats = ml_stats
    )
    .pred_features(train_samples) <- feats
    list(
        train_x = array(
            data = as.matrix(.pred_features(train_samples)),
            dim  = c(nrow(train_samples), n_times, n_bands)
        ),
        train_y = unname(code_labels[.pred_references(train_samples)])
    )
}

#' @title Wrap a trained SSL encoder in a luz stub for `sits_encode()`
#' @name .ssl_wrap_encoder
#' @keywords internal
#' @noRd
#' @description Extracts the trained encoder from an SSL model, registers it in
#' a minimal luz module (via a zero-epoch fit) so it is compatible with
#' `sits_encode()`, injects the trained weights and preserves the training
#' records. The projector/head is discarded.
#' @param model      Trained SSL luz model (with `$model$encoder`).
#' @param optimizer  Torch optimizer function.
#' @param n_bands    Number of bands.
#' @param n_labels   Number of labels.
#' @param timeline   Sample timeline.
#' @param stub_data  Stub data from `.ssl_stub_data()`.
#' @return A luz model wrapping the encoder, with training records preserved.
.ssl_wrap_encoder <- function(model, optimizer, n_bands, n_labels, timeline,
                              stub_data) {
    # Avoid a global binding note for the torch-provided 'self'
    self <- NULL
    # Move the trained encoder to CPU
    cpu_mod <- model$model$encoder$to(device = "cpu")
    # Minimal module that just forwards through the encoder
    stub_module <- torch::nn_module(
        "StubModule",
        initialize = function(n_bands, n_labels, timeline, ...) {
            self$model <- cpu_mod
        },
        forward = function(x) {
            self$model(x)
        }
    )
    # Zero-epoch fit just registers the module structure
    torch_model <- luz::setup(
        module    = stub_module,
        loss      = torch::nn_cross_entropy_loss(),
        optimizer = optimizer
    ) |>
        luz::set_hparams(
            n_bands  = n_bands,
            n_labels = n_labels,
            timeline = timeline
        ) |>
        luz::fit(
            data    = list(stub_data[["train_x"]], stub_data[["train_y"]]),
            epochs  = 0L,
            verbose = FALSE
        )
    # Inject trained weights (add "model." prefix to match StubModule)
    cpu_sd <- cpu_mod$state_dict()
    names(cpu_sd) <- paste0("model.", names(cpu_sd))
    torch_model[["model"]]$load_state_dict(cpu_sd)
    # Preserve training records for plot.torch_model
    torch_model[["records"]] <- model[["records"]]
    torch_model
}

#' @title Resampling augmentation for joint-embedding SSL
#' @name .ssl_apply_resampling
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description
#' Generates two augmented views from a single multivariate time series using
#' the resampling strategy of Saget et al. (2025), shared by the LeJEPA and
#' VICReg pre-training methods. The series is upsampled to `2 * T` steps, two
#' disjoint quarter-balanced subsequences of `T / 2` steps are drawn, and each
#' is resampled back to the original length `T`.
#' @param ts_mat Numeric matrix of shape `[n_times, n_bands]`.
#' @return A named list with `view1` and `view2`, each a numeric matrix of
#'   shape `[n_times, n_bands]`.
#' @references
#' Saget, A., Lafabregue, B., Cornuejols, A., & Gancarski, P. (2025).
#' Resampling Augmentation for Time Series Contrastive Learning: Application to
#' Remote Sensing. arXiv:2506.18587.
.ssl_apply_resampling <- function(ts_mat) {
    n_times <- nrow(ts_mat)
    n_bands <- ncol(ts_mat)
    t_up    <- 2L * n_times

    # Step 1: Upsample to 2T timesteps via linear interpolation
    orig_idx <- seq_len(n_times)
    up_idx   <- seq(1, n_times, length.out = t_up)
    up_mat   <- matrix(0, nrow = t_up, ncol = n_bands)
    purrr::walk(seq_len(n_bands), function(b) {
        up_mat[, b] <<- stats::approx(
            orig_idx, ts_mat[, b], xout = up_idx, rule = 2
        )$y
    })

    # Step 2: Draw two disjoint subsequences with quarter coverage
    t_sub        <- as.integer(n_times %/% 2)
    per_quarter  <- as.integer(t_sub %/% 4)
    quarter_size <- as.integer(t_up %/% 4)

    quarters <- purrr::map(0:3, function(j) {
        start <- j * quarter_size + 1L
        end   <- min((j + 1L) * quarter_size, t_up)
        seq.int(start, end)
    })

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

    sub1 <- up_mat[idx1, , drop = FALSE]
    sub2 <- up_mat[idx2, , drop = FALSE]

    # Step 3: Resample each subsequence to the original T positions
    resample_to_t <- function(sub_mat, sub_idx) {
        rng <- max(sub_idx) - min(sub_idx)
        if (rng == 0) rng <- 1
        rescaled <- (sub_idx - min(sub_idx)) / rng * (n_times - 1) + 1
        out    <- matrix(0, nrow = n_times, ncol = n_bands)
        target <- seq_len(n_times)
        purrr::walk(seq_len(n_bands), function(b) {
            out[, b] <<- stats::approx(
                rescaled, sub_mat[, b], xout = target, rule = 2
            )$y
        })
        out
    }

    list(
        view1 = resample_to_t(sub1, idx1),
        view2 = resample_to_t(sub2, idx2)
    )
}
