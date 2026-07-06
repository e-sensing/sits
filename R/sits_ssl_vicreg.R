#' @title Self-supervised VICReg pre-training with time-warping augmentation
#' @name sits_ssl_vicreg
#'
#' @description
#' Self-supervised pre-training using the VICReg
#' (Variance-Invariance-Covariance Regularization) loss and a torch
#' encoder.  Two views of each sample are created on-the-fly using the
#' resampling augmentation of Saget et al. (2025): the time series is
#' upsampled, two disjoint subsequences are drawn with a temporal
#' coverage constraint, and each is resampled back to the original
#' length.  No labels are required.
#'
#' Both views are passed through a shared encoder and projector.  The
#' VICReg loss combines three objectives:
#' \enumerate{
#'   \item \strong{Invariance}: MSE between the projected representations
#'     of the two views — pulls paired embeddings together.
#'   \item \strong{Variance}: a hinge loss that keeps the standard
#'     deviation of each embedding feature above a threshold of 1 across
#'     the batch — prevents informational collapse.
#'   \item \strong{Covariance}: penalises off-diagonal entries of the
#'     embedding covariance matrix — decorrelates features.
#' }
#'
#' The function can be used in two ways:
#' \itemize{
#'   \item If \code{samples} is provided, it trains immediately and returns
#'     an encoder-ready model object (see Value).
#'   \item If \code{samples = NULL}, it returns a training function with
#'     signature \code{function(samples)} that can be passed to
#'     \code{\link[sits]{sits_pre_train}} or called later.
#' }
#'
#' @param samples        A \code{sits} samples object.  If \code{NULL}
#'   (default), returns a training function.  If provided, triggers
#'   immediate training.  Base data samples (e.g., \code{sits_base}) are
#'   not supported.
#' @param embedding_dim  Integer. Dimensionality of the encoder embedding
#'   (exported features).  Default: 64L.
#' @param proj_dim       Integer. Dimensionality of the projector head
#'   used only during pre-training.  Default: 128L.
#' @param sim_coeff      Numeric. Weight of the invariance (MSE) term in
#'   the VICReg loss.  Default: 25.0.
#' @param std_coeff      Numeric. Weight of the variance (hinge) term in
#'   the VICReg loss.  Default: 25.0.
#' @param cov_coeff      Numeric. Weight of the covariance
#'   (off-diagonal) term in the VICReg loss.  Default: 1.0.
#' @param encoder_model  Function. Encoder backbone factory (e.g.,
#'   \code{\link[sits]{sits_lighttae}()}).  Must accept \code{samples} and
#'   \code{embedding_dim}.  Default: \code{sits_lighttae()}.
#' @param epochs         Integer. Maximum number of training epochs.
#' @param batch_size     Integer. Batch size for training.  Default: 128L.
#' @param validation_split Numeric in (0, 1). Fraction of samples held
#'   out for validation loss monitoring.
#' @param optimizer      Function. A \code{torch} optimizer constructor
#'   (default: \code{torch::optim_adamw}).
#' @param opt_hparams    Named list of optimizer hyperparameters.
#'   Common entries: \code{lr}, \code{eps}, \code{weight_decay}.
#' @param lr_decay_epochs Integer. Step size (in epochs) for LR decay.
#' @param lr_decay_rate  Numeric. Multiplicative LR decay factor.
#' @param patience       Integer. Early-stopping patience (epochs without
#'   improvement).
#' @param min_delta      Numeric. Minimum improvement required to reset
#'   the patience counter.
#' @param verbose        Logical. Print training progress?
#' @param seed           Integer. Random seed for reproducibility.
#'
#' @return
#' If \code{samples = NULL}, a training function with signature
#' \code{function(samples)} that trains a VICReg model and returns a
#' pretrained encoder (a \code{sits_encoder} closure).
#'
#' If \code{samples} is provided, the result of applying the training
#' function to \code{samples} directly.
#'
#' @details
#' The augmentation strategy creates two views of each sample using the
#' resampling method of Saget et al. (2025).  The original time series
#' (length \code{T}) is upsampled to \code{2T} timesteps by linear
#' interpolation.  Two disjoint subsequences of \code{T/2} timesteps are
#' drawn from the upsampled series, with a constraint that at least
#' \code{floor((T/2)/4)} timesteps fall in each temporal quarter.  Each
#' subsequence is then resampled back to \code{T} positions by rescaling
#' its timestamps and interpolating, producing two views that preserve
#' overall temporal structure while differing in fine-grained detail.
#' Because the subsampling is random, views differ at every epoch.
#'
#' @references
#' Bardes, A., Ponce, J., & LeCun, Y. (2022).
#' \emph{VICReg: Variance-Invariance-Covariance Regularization for
#' Self-Supervised Learning}.  International Conference on Learning
#' Representations (ICLR).
#'
#' Saget, A., Lafabregue, B., Cornuéjols, A., & Gançarski, P. (2025).
#' \emph{Resampling Augmentation for Time Series Contrastive Learning:
#' Application to Remote Sensing}. arXiv:2506.18587.
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @examples
#' if (sits_run_examples()) {
#'     model <- sits_pre_train(
#'         samples_modis_ndvi,
#'         sits_ssl_vicreg(
#'             embedding_dim = 32L,
#'             epochs        = 20L
#'         )
#'     )
#' }
#'
#' @export
sits_ssl_vicreg <- function(samples          = NULL,
                            embedding_dim    = 64L,
                            proj_dim         = 128L,
                            sim_coeff        = 25.0,
                            std_coeff        = 25.0,
                            cov_coeff        = 1.0,
                            encoder_model    = sits_lighttae(),
                            epochs           = 150L,
                            batch_size       = 128L,
                            validation_split = 0.2,
                            optimizer        = torch::optim_adamw,
                            opt_hparams = list(
                                lr           = 5.0e-04,
                                eps          = 1.0e-08,
                                weight_decay = 1.0e-06
                            ),
                            lr_decay_epochs  = 1L,
                            lr_decay_rate    = 0.95,
                            patience         = 20L,
                            min_delta        = 0.01,
                            verbose          = FALSE,
                            seed             = 10L) {
    # set caller for error msg
    .check_set_caller("sits_ssl_vicreg")
    # Verifies if 'torch' and 'luz' packages are installed
    .check_require_packages(c("torch", "luz"))
    # documentation mode? verbose is FALSE
    verbose <- .message_verbose(verbose)
    # Band prefix for embeddings
    bands_prefix <- .conf("embedding_band_prefix")
    .check_chr(bands_prefix, len_min = 1, len_max = 1, allow_empty = FALSE)
    # Function that trains a torch model based on samples
    train_fun <- function(samples) {
        # does not support working with DEM or other base data
        if (inherits(samples, "sits_base")) {
            stop(.conf("messages", "sits_train_base_data"), call. = FALSE)
        }
        # Avoid adding a global variable for 'self' and 'super'
        self  <- NULL
        super <- NULL
        # Pre-conditions
        .check_pre_sits_ssl_vicreg(
            samples       = samples,
            epochs        = epochs,
            batch_size    = batch_size,
            encoder_model = encoder_model,
            sim_coeff     = sim_coeff,
            std_coeff     = std_coeff,
            cov_coeff     = cov_coeff,
            bands_prefix  = bands_prefix,
            verbose       = verbose
        )
        # Other pre-conditions
        .check_int_parameter(seed, allow_null = TRUE)
        # Check opt_hparams — get formals list without the 'param' parameter
        optim_params_function <- formals(optimizer)[-1L]
        .check_opt_hparams(opt_hparams, optim_params_function)
        optim_params_function <- utils::modifyList(
            x   = optim_params_function,
            val = opt_hparams
        )
        # Samples metadata
        labels   <- .samples_labels(samples)
        bands    <- .samples_bands(samples)
        timeline <- .samples_timeline(samples)
        n_labels <- length(labels)
        n_bands  <- length(bands)
        n_times  <- .samples_ntimes(samples)

        # Copy closure variables to local
        embedding_dim <- embedding_dim
        proj_dim      <- proj_dim
        sim_coeff    <- sim_coeff
        std_coeff    <- std_coeff
        cov_coeff    <- cov_coeff
        bands_prefix <- bands_prefix

        # ------------------------------------------------------------------
        # Build augmentation-based datasets for VICReg
        #
        # Each dataset item is a tensor of shape [2, n_times, n_bands]:
        #   view 1 = resampling_view_1(original)
        #   view 2 = resampling_view_2(original)
        # Augmentation is applied on-the-fly (lazy) so views differ
        # each epoch (Saget et al. 2025).
        # ------------------------------------------------------------------
        ml_stats <- .samples_stats(samples)
        splits <- .vicreg_data_split(
            samples          = samples,
            validation_split = validation_split
        )
        n_val <- nrow(splits[["val"]][["feats"]])
        # Torch datasets with on-the-fly resampling augmentation
        train_ds <- .vicreg_resampling_dataset(
            splits[["train"]],
            n_times = n_times
        )
        val_ds <- .vicreg_resampling_dataset(
            splits[["val"]],
            n_times = n_times
        )

        # ------------------------------------------------------------------
        # CREATE DUMMY DATA FOR LUZ STUB
        #   A tiny (≤ 10 rows) normalised snapshot used only to register
        #   the module structure via luz::fit(..., epochs = 0L).
        # ------------------------------------------------------------------
        code_labels      <- seq_along(labels)
        names(code_labels) <- labels
        stub_samples     <- samples[seq_len(min(10L, nrow(samples))), ]
        train_samples    <- .pred_normalize(
            pred  = .predictors(stub_samples),
            stats = ml_stats
        )
        n_samples_train  <- nrow(train_samples)
        train_x <- array(
            data = as.matrix(.pred_features(train_samples)),
            dim  = c(n_samples_train, n_times, n_bands)
        )
        train_y <- unname(code_labels[.pred_references(train_samples)])
        # ------------------------------------------------------------------

        torch_seed <- .torch_seed(seed)
        torch::torch_manual_seed(torch_seed)

        # Set the encoder model closure
        encoder <- encoder_model(
            samples       = samples,
            embedding_dim = embedding_dim
        )

        # ------------------------------------------------------------------
        # Define VICReg model: shared encoder + projector head
        # ------------------------------------------------------------------
        vicreg_model <- torch::nn_module(
            classname = "vicreg_model",

            initialize = function(encoder, embedding_dim, proj_dim = 128L,
                                  n_bands = NULL, n_labels = NULL,
                                  timeline = NULL) {
                super$initialize()
                self$encoder <- encoder

                # Projector: 3-layer MLP with batch norm + ReLU;
                # NO activation on the final layer (matches reference).
                self$projector <- torch::nn_sequential(
                    torch::nn_linear(embedding_dim, proj_dim, bias = FALSE),
                    torch::nn_batch_norm1d(proj_dim),
                    torch::nn_relu(),
                    torch::nn_linear(proj_dim, proj_dim, bias = FALSE),
                    torch::nn_batch_norm1d(proj_dim),
                    torch::nn_relu(),
                    torch::nn_linear(proj_dim, proj_dim, bias = FALSE)
                )

                # Metadata for sits_encode compatibility
                self$n_bands  <- n_bands
                self$n_labels <- n_labels
                self$timeline <- timeline
            },

            forward = function(x) {
                # x: [batch, 2, time, band]
                z_a <- x[, 1, , ]$contiguous() |>
                    self$encoder() |>
                    self$projector()

                z_b <- x[, 2, , ]$contiguous() |>
                    self$encoder() |>
                    self$projector()

                # Stack: output [batch, 2, proj_dim]
                torch::torch_stack(list(z_a, z_b), dim = 2L)
            }
        )

        # ------------------------------------------------------------------
        # VICReg loss (Bardes, Ponce & LeCun 2022)
        #
        # Three terms:
        #   1. Invariance (sim_coeff):  MSE between paired views
        #   2. Variance   (std_coeff):  hinge on per-feature std > 1
        #   3. Covariance (cov_coeff):  off-diagonal penalty
        # ------------------------------------------------------------------
        vicreg_loss <- function(input, target) {
            z_a <- input[, 1L, ]$contiguous()   # [B, proj_dim]
            z_b <- input[, 2L, ]$contiguous()

            # 1. Invariance: MSE between paired representations
            repr_loss <- torch::nnf_mse_loss(z_a, z_b)

            # 2. Variance: hinge loss on per-feature std
            std_a <- torch::torch_sqrt(z_a$var(dim = 1L) + 1e-4)
            std_b <- torch::torch_sqrt(z_b$var(dim = 1L) + 1e-4)
            std_loss <- (torch::torch_mean(torch::nnf_relu(1 - std_a)) +
                             torch::torch_mean(torch::nnf_relu(1 - std_b))) / 2

            # 3. Covariance: penalise off-diagonal of covariance matrix
            B <- z_a$size(1)
            D <- z_a$size(2)
            z_a_c <- z_a - z_a$mean(dim = 1L)
            z_b_c <- z_b - z_b$mean(dim = 1L)
            cov_a <- (z_a_c$t()$matmul(z_a_c)) / (B - 1)
            cov_b <- (z_b_c$t()$matmul(z_b_c)) / (B - 1)
            cov_loss <- (
                .vicreg_off_diagonal(cov_a)$pow(2)$sum() / D +
                .vicreg_off_diagonal(cov_b)$pow(2)$sum() / D
            )

            sim_coeff * repr_loss +
                std_coeff * std_loss +
                cov_coeff * cov_loss
        }

        # Verify if GPU is available
        cpu_train <- .torch_cpu_train()

        # Build callbacks (early stopping only when validation data exists)
        callbacks <- list(
            luz::luz_callback_lr_scheduler(
                torch::lr_step,
                step_size = lr_decay_epochs,
                gamma     = lr_decay_rate
            )
        )
        if (n_val > 0L) {
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

        # Train the model using luz
        model <-
            luz::setup(
                module    = vicreg_model,
                loss      = vicreg_loss,
                metrics   = list(),
                optimizer = optimizer
            ) |>
            luz::set_hparams(
                encoder       = encoder,
                embedding_dim = embedding_dim,
                proj_dim      = proj_dim,
                n_bands       = n_bands,
                timeline      = timeline,
                n_labels      = n_labels
            ) |>
            luz::set_opt_hparams(
                !!!optim_params_function
            ) |>
            luz::fit(
                data       = train_ds,
                epochs     = epochs,
                valid_data = if (n_val > 0L) val_ds else NULL,
                callbacks  = callbacks,
                accelerator        = luz::accelerator(cpu = cpu_train),
                dataloader_options = list(
                    batch_size = batch_size,
                    shuffle    = TRUE
                ),
                verbose = verbose
            )

        # ------------------------------------------------------------------
        # Wrap the encoder in a luz stub for sits_encode() compatibility.
        # The projector is discarded — standard VICReg practice.
        # ------------------------------------------------------------------
        cpu_mod <- model$model$encoder$to(device = "cpu")

        stub_module <- torch::nn_module(
            "StubModule",
            initialize = function(n_bands, n_labels, timeline, ...) {
                self$model <- cpu_mod
            },
            forward = function(x) {
                self$model(x)
            }
        )

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
            # Zero epochs — just registers the module structure
            luz::fit(
                data    = list(train_x, train_y),
                epochs  = 0L,
                verbose = FALSE
            )

        # Inject trained weights (add "model." prefix to match StubModule)
        cpu_sd <- cpu_mod$state_dict()
        names(cpu_sd) <- paste0("model.", names(cpu_sd))
        torch_model[["model"]]$load_state_dict(cpu_sd)

        # Preserve training records for plot.torch_model
        torch_model[["records"]] <- model[["records"]]

        # Serialize model for later deserialization inside predict_fun
        serialized_model <- .torch_serialize_model(torch_model[["model"]])

        # Function that encodes input values using the trained encoder
        predict_fun <- function(values) {
            .check_require_packages("torch")
            suppressWarnings(torch::torch_set_num_threads(1L))
            # Unserialize model
            torch_model[["model"]] <- .torch_unserialize_model(
                model = torch_model[["model"]],
                raw   = serialized_model
            )
            # Reshape the 2D matrix into a 3D array [n_samples, n_times, n_bands]
            n_samples <- nrow(values)
            n_times   <- .samples_ntimes(samples)
            n_bands   <- length(bands)
            # Normalize using training statistics
            values <- .pred_normalize(pred = values, stats = ml_stats)
            values <- array(
                data = as.matrix(values),
                dim  = c(n_samples, n_times, n_bands)
            )
            # GPU or CPU inference
            if (.torch_gpu_classification()) {
                batch_size <- sits_env[["batch_size"]]
                values <- .torch_as_dataset(values)
                values <- torch::dataloader(values, batch_size = batch_size)
                values <- .try(
                    stats::predict(object = torch_model, values),
                    .msg_error = .conf("messages", ".check_gpu_memory_size")
                )
            } else {
                values <- stats::predict(object = torch_model, values)
            }
            values <- torch::as_array(values)
            colnames(values) <- paste0(bands_prefix, seq_len(ncol(values)))
            values
        }
        # Tag with sits model classes
        predict_fun <- .set_class(
            predict_fun, "sits_encoder", "torch_model", "sits_model",
            class(predict_fun)
        )
    }
    # If samples is provided, train immediately; otherwise return train_fun
    .factory_function(samples, train_fun)
}
