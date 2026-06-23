#' @title Self-supervised Barlow Twins pre-training with augmentation
#' @name sits_ssl_barlow_twins
#'
#' @description
#' Truly self-supervised pre-training using the Barlow Twins loss and a torch
#' encoder. Two views of each sample are created on-the-fly by multiplying
#' the time series by independent random scalars drawn from a Gaussian
#' distribution. No labels are required.
#'
#' Both views are passed through a shared encoder and projector. The
#' Barlow Twins loss makes the cross-correlation matrix of the two views'
#' embeddings close to the identity: the diagonal approaches 1 (invariance)
#' and the off-diagonal approaches 0 (redundancy reduction). No negatives
#' or asymmetric architectures are required.
#'
#' The function can be used in two ways:
#' \itemize{
#'   \item If \code{samples} is provided, it trains immediately and returns
#'   an encoder-ready model object (see Value).
#'   \item If \code{samples = NULL}, it returns a training function with
#'   signature \code{function(samples)} that can be passed to
#'   \code{\link[sits]{sits_pre_train}} or called later.
#' }
#'
#' @param samples        A \code{sits} samples object. If \code{NULL}
#'   (default), returns a training function. If provided, triggers immediate
#'   training. Base data samples (e.g., \code{sits_base}) are not supported.
#' @param embedding_dim  Integer. Dimensionality of the encoder embedding
#'   (exported features). Default: 64L.
#' @param proj_dim       Integer. Dimensionality of the projector head used
#'   only during pre-training. Default: 256L.
#' @param bt_lambda      Numeric. Weight of the redundancy-reduction
#'   (off-diagonal) term in the Barlow Twins loss. Default: 5e-3.
#' @param augment_mean   Numeric. Mean of the Gaussian distribution used
#'   to draw the random scaling factor for each view. Default: 1.0.
#' @param augment_variance Numeric. Variance of the Gaussian distribution
#'   used to draw the random scaling factor. Default: 0.1.
#' @param encoder_model  Function. Encoder backbone factory (e.g.,
#'   \code{\link[sits]{sits_lighttae}()}). Must accept \code{samples} and
#'   \code{embedding_dim}. Default: \code{sits_lighttae()}.
#' @param epochs         Integer. Maximum number of training epochs.
#' @param batch_size     Integer. Batch size for training. Larger values
#'   improve the Barlow Twins cross-correlation estimate. Default: 128L.
#' @param validation_split Numeric in (0, 1). Fraction of samples held out
#'   for validation loss monitoring.
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
#' \code{function(samples)} that trains a Barlow Twins model and returns
#' a pretrained encoder (a \code{sits_encoder} closure).
#'
#' If \code{samples} is provided, the result of applying the training function
#' to \code{samples} directly.
#'
#' @details
#' The augmentation strategy creates two views of each sample by multiplying
#' the original time series by a random scalar
#' \code{alpha ~ N(augment_mean, sqrt(augment_variance))}. Two independent
#' draws of \code{alpha} produce two different scaled versions of the same
#' sample. Because the draws differ at every call, the two views are
#' different at each epoch, improving generalisation.
#'
#' @references
#' Zbontar, J., Jing, L., Misra, I., LeCun, Y., & Deny, S. (2021).
#' \emph{Barlow Twins: Self-Supervised Learning via Redundancy Reduction}.
#' Proceedings of the 38th International Conference on Machine Learning
#' (ICML).
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @examples
#' if (sits_run_examples()) {
#'     model <- sits_pre_train(
#'         samples_modis_ndvi,
#'         sits_ssl_barlow_twins(
#'             embedding_dim  = 32L,
#'             epochs         = 20L
#'         )
#'     )
#' }
#'
#' @export
sits_ssl_barlow_twins <- function(samples          = NULL,
                                  embedding_dim    = 64L,
                                  proj_dim         = 256L,
                                  bt_lambda        = 5e-3,
                                  augment_mean     = 1.0,
                                  augment_variance = 0.1,
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
    .check_set_caller("sits_ssl_barlow_twins")
    # Verifies if 'torch' and 'luz' packages are installed
    .check_require_packages(c("torch", "luz"))
    # documentation mode? verbose is FALSE
    verbose <- .message_verbose(verbose)
    # Band prefix for embeddings
    bands_prefix <- .conf("embedding_band_prefix")
    .check_chr(bands_prefix, len_min = 1, lan_max = 1, allow_empty = FALSE)
    # Function that trains a torch model based on samples
    train_fun <- function(samples) {
        # does not support working with DEM or other base data
        if (inherits(samples, "sits_base")) {
            stop(.conf("messages", "sits_train_base_data"), call. = FALSE)
        }
        # Avoid adding a global variable for 'self'
        self <- NULL
        # Pre-conditions
        .check_pre_sits_ssl_barlow_twins(
            samples          = samples,
            epochs           = epochs,
            batch_size       = batch_size,
            encoder_model    = encoder_model,
            augment_mean     = augment_mean,
            augment_variance = augment_variance,
            bands_prefix     = bands_prefix,
            verbose          = verbose
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

        # Copy closure variables to local (makes them available in sub-closures)
        embedding_dim    <- embedding_dim
        proj_dim         <- proj_dim
        bt_lambda        <- bt_lambda
        augment_mean     <- augment_mean
        augment_variance <- augment_variance
        bands_prefix     <- bands_prefix

        # ------------------------------------------------------------------
        # Build augmentation-based datasets for SSL Barlow Twins
        #
        # Each dataset item is a tensor of shape [2, n_times, n_bands]:
        #   view 1 = original × alpha_1 (alpha_1 ~ N(mean, sqrt(var)))
        #   view 2 = original × alpha_2 (alpha_2 ~ N(mean, sqrt(var)))
        # Augmentation is applied on-the-fly (lazy) so views differ
        # each epoch.
        # ------------------------------------------------------------------
        ml_stats <- .samples_stats(samples)
        splits <- .ssl_bt_data_split(
            samples          = samples,
            validation_split = validation_split
        )
        # Torch datasets with on-the-fly scaling augmentation
        train_ds <- .ssl_bt_augment_dataset(
            splits[["train"]],
            n_times          = n_times,
            augment_mean     = augment_mean,
            augment_variance = augment_variance
        )
        val_ds <- .ssl_bt_augment_dataset(
            splits[["val"]],
            n_times          = n_times,
            augment_mean     = augment_mean,
            augment_variance = augment_variance
        )

        # ------------------------------------------------------------------
        # CREATE DUMMY DATA FOR LUZ STUB
        #   A tiny (≤ 10 rows) normalised snapshot of the data is used only
        #   to register the module structure via luz::fit(..., epochs = 0L).
        #   The actual weights come from the BT training above.
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
        # Define Barlow Twins model: shared encoder + projector head
        # ------------------------------------------------------------------
        self  <- NULL
        super <- NULL

        ssl_bt_model <- torch::nn_module(
            classname = "ssl_barlow_twins_model",

            initialize = function(encoder, embedding_dim, proj_dim = 256L,
                                  n_bands = NULL, n_labels = NULL,
                                  timeline = NULL) {
                super$initialize()
                self$encoder <- encoder

                # Projection head: BN + ReLU between linear layers;
                # NO activation on the final layer.
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
                self$n_times  <- length(timeline)
            },

            forward = function(x) {
                # x: [batch, 2, time, band]
                # View A: dim-2 index 1; View B: dim-2 index 2.
                z_a <- x[, 1, , ]$contiguous() |>
                    self$encoder() |>
                    self$projector()

                z_b <- x[, 2, , ]$contiguous() |>
                    self$encoder() |>
                    self$projector()

                # Stack the two projected views: output [batch, 2, proj_dim]
                torch::torch_stack(list(z_a, z_b), dim = 2L)
            }
        )

        # ------------------------------------------------------------------
        # Barlow Twins loss
        # ------------------------------------------------------------------
        ssl_bt_loss <- function(input, target) {
            # input:  [batch, 2, proj_dim] — two stacked views from forward()
            # target: ignored (self-supervised)
            z_a <- input[, 1L, ]$contiguous()
            z_b <- input[, 2L, ]$contiguous()

            bs  <- z_a$size(1L)
            eps <- 1e-5

            # Standardise each feature across the batch (mean 0, std 1)
            z_a_n <- (z_a - z_a$mean(dim = 1L)) / (z_a$std(dim = 1L) + eps)
            z_b_n <- (z_b - z_b$mean(dim = 1L)) / (z_b$std(dim = 1L) + eps)

            # Cross-correlation matrix: [proj_dim, proj_dim]
            c_mat <- z_a_n$t()$matmul(z_b_n) / bs

            # Invariance term: pull diagonal toward 1
            on_diag <- (torch::torch_diagonal(c_mat) - 1)$pow(2)$sum()

            # Redundancy-reduction term: push off-diagonal toward 0
            off_diag <- c_mat$pow(2)$sum() -
                torch::torch_diagonal(c_mat)$pow(2)$sum()

            on_diag + bt_lambda * off_diag
        }

        # Verify if GPU is available
        cpu_train <- .torch_cpu_train()

        # Train the model using luz
        model <-
            luz::setup(
                module    = ssl_bt_model,
                loss      = ssl_bt_loss,
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
                valid_data = val_ds,
                callbacks  = list(
                    luz::luz_callback_early_stopping(
                        monitor   = "valid_loss",
                        mode      = "min",
                        patience  = patience,
                        min_delta = min_delta
                    ),
                    luz::luz_callback_lr_scheduler(
                        torch::lr_step,
                        step_size = lr_decay_epochs,
                        gamma     = lr_decay_rate
                    )
                ),
                accelerator        = luz::accelerator(cpu = cpu_train),
                dataloader_options = list(
                    batch_size = batch_size,
                    shuffle    = TRUE
                ),
                verbose = verbose
            )

        # ------------------------------------------------------------------
        # Wrap the encoder in a luz stub for sits_encode() compatibility.
        # The projector is discarded — standard Barlow Twins practice.
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

        # Serialize model for later deserialization inside predict_fun
        serialized_model <- .torch_serialize_model(torch_model[["model"]])

        # Function that encodes input values using the trained encoder
        predict_fun <- function(values) {
            # Verifies if torch package is installed
            .check_require_packages("torch")
            # Set torch threads to 1
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
            predict_fun, "torch_model", "sits_encoder", class(predict_fun)
        )
    }
    # If samples is provided, train immediately; otherwise return train_fun
    .factory_function(samples, train_fun)
}
