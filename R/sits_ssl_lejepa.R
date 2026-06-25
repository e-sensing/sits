#' @title Self-supervised LeJEPA pre-training with resampling augmentation
#' @name sits_ssl_lejepa
#'
#' @description
#' Self-supervised pre-training using the LeJEPA (Lean Joint-Embedding
#' Predictive Architecture) loss and a torch encoder.  Two views of each
#' sample are created on-the-fly using the resampling augmentation of
#' Saget et al. (2025).  No labels are required.
#'
#' The LeJEPA loss combines two terms:
#' \enumerate{
#'   \item \strong{Invariance}: MSE between each view's projected
#'     embedding and the mean across views — pulls views together.
#'   \item \strong{SIGReg} (Sketched Isotropic Gaussian Regularization):
#'     constrains embeddings to an isotropic Gaussian distribution by
#'     comparing 1-D projections to the Gaussian characteristic function
#'     — prevents representational collapse without teacher-student
#'     networks or stop-gradients.
#' }
#'
#' These two terms are balanced by a single hyperparameter \code{lambda}.
#'
#' The function can be used in two ways:
#' \itemize{
#'   \item If \code{samples} is provided, it trains immediately and
#'     returns an encoder-ready model object (see Value).
#'   \item If \code{samples = NULL}, it returns a training function with
#'     signature \code{function(samples)} that can be passed to
#'     \code{\link[sits]{sits_pre_train}} or called later.
#' }
#'
#' @param samples        A \code{sits} samples object.  If \code{NULL}
#'   (default), returns a training function.  If provided, triggers
#'   immediate training.
#' @param embedding_dim  Integer. Dimensionality of the encoder embedding.
#'   Default: 64L.
#' @param proj_dim       Integer. Dimensionality of the projector head
#'   used only during pre-training.  Default: 128L.
#' @param lambda         Numeric in (0, 1). Trade-off between invariance
#'   (\code{1 - lambda}) and SIGReg (\code{lambda}).  Default: 0.02.
#' @param num_knots      Integer. Number of quadrature knots for the
#'   SIGReg characteristic function test.  Default: 17L.
#' @param num_slices     Integer. Number of random projection directions
#'   for SIGReg.  Default: 256L.
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
#' @param lr_decay_epochs Integer. Step size (in epochs) for LR decay.
#' @param lr_decay_rate  Numeric. Multiplicative LR decay factor.
#' @param patience       Integer. Early-stopping patience.
#' @param min_delta      Numeric. Minimum improvement for early stopping.
#' @param verbose        Logical. Print training progress?
#' @param seed           Integer. Random seed for reproducibility.
#'
#' @return
#' If \code{samples = NULL}, a training function.  If \code{samples} is
#' provided, a pretrained encoder closure (class \code{sits_encoder}).
#'
#' @details
#' The augmentation strategy uses the resampling method of Saget et al.
#' (2025): the time series is upsampled, two disjoint subsequences are
#' drawn with a temporal coverage constraint, and each is resampled back
#' to the original length.
#'
#' Unlike VICReg (which uses three separate regularisation terms),
#' LeJEPA replaces all collapse-prevention heuristics with the single
#' SIGReg objective, yielding a simpler and more theoretically grounded
#' method with a single trade-off hyperparameter.
#'
#' @references
#' Balestriero, R. & LeCun, Y. (2025).
#' \emph{LeJEPA: Provable and Scalable Self-Supervised Learning Without
#' the Heuristics}. arXiv:2511.08544.
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
#'         sits_ssl_lejepa(
#'             embedding_dim = 32L,
#'             epochs        = 20L
#'         )
#'     )
#' }
#'
#' @export
sits_ssl_lejepa <- function(samples          = NULL,
                             embedding_dim    = 64L,
                             proj_dim         = 128L,
                             lambda           = 0.02,
                             num_knots        = 17L,
                             num_slices       = 256L,
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
    .check_set_caller("sits_ssl_lejepa")
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
        .check_pre_sits_ssl_lejepa(
            samples       = samples,
            epochs        = epochs,
            batch_size    = batch_size,
            encoder_model = encoder_model,
            lambda        = lambda,
            num_knots     = num_knots,
            num_slices    = num_slices,
            bands_prefix  = bands_prefix,
            verbose       = verbose
        )
        # Other pre-conditions
        .check_int_parameter(seed, allow_null = TRUE)
        # Check opt_hparams
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
        lambda        <- lambda
        num_knots     <- num_knots
        num_slices    <- num_slices
        bands_prefix  <- bands_prefix

        # ------------------------------------------------------------------
        # Build augmentation-based datasets for LeJEPA
        # ------------------------------------------------------------------
        ml_stats <- .samples_stats(samples)
        splits <- .lejepa_data_split(
            samples          = samples,
            validation_split = validation_split
        )
        n_val <- nrow(splits[["val"]][["feats"]])
        train_ds <- .lejepa_resampling_dataset(
            splits[["train"]],
            n_times = n_times
        )
        val_ds <- .lejepa_resampling_dataset(
            splits[["val"]],
            n_times = n_times
        )

        # ------------------------------------------------------------------
        # CREATE DUMMY DATA FOR LUZ STUB
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
        # Define LeJEPA model: shared encoder + projector head
        # ------------------------------------------------------------------
        lejepa_model <- torch::nn_module(
            classname = "lejepa_model",

            initialize = function(encoder, embedding_dim, proj_dim = 128L,
                                  n_bands = NULL, n_labels = NULL,
                                  timeline = NULL) {
                super$initialize()
                self$encoder <- encoder

                # Projector: 3-layer MLP with batch norm + ReLU;
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
        # LeJEPA loss (Balestriero & LeCun 2025)
        #
        # Two terms:
        #   1. Invariance (1 - lambda): MSE of views from their mean
        #   2. SIGReg    (lambda):      Gaussian regularisation
        # ------------------------------------------------------------------
        sigreg_module <- .lejepa_sigreg(
            num_knots  = num_knots,
            num_slices = num_slices
        )

        lejepa_loss <- function(input, target) {
            # input: [B, 2, proj_dim]
            # Reshape to [V=2, B, proj_dim]
            proj <- input$permute(c(2L, 1L, 3L))$contiguous()

            # 1. Invariance: MSE between mean and each view
            inv_loss <- (proj$mean(1L) - proj)$square()$mean()

            # 2. SIGReg: Gaussian regularisation
            sigreg_loss <- sigreg_module(proj)

            # Combine with single trade-off parameter
            loss <- (1 - lambda) * inv_loss + lambda * sigreg_loss

            # Guard against NaN (return invariance-only if SIGReg is NaN)
            if (torch::torch_isnan(loss)$item()) {
                loss <- inv_loss
            }
            loss
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
                module    = lejepa_model,
                loss      = lejepa_loss,
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
        # The projector is discarded.
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
            luz::fit(
                data    = list(train_x, train_y),
                epochs  = 0L,
                verbose = FALSE
            )

        # Inject trained weights
        cpu_sd <- cpu_mod$state_dict()
        names(cpu_sd) <- paste0("model.", names(cpu_sd))
        torch_model[["model"]]$load_state_dict(cpu_sd)

        # Preserve training records for plot.torch_model
        torch_model[["records"]] <- model[["records"]]

        # Serialize model
        serialized_model <- .torch_serialize_model(torch_model[["model"]])

        # Function that encodes input values using the trained encoder
        predict_fun <- function(values) {
            .check_require_packages("torch")
            suppressWarnings(torch::torch_set_num_threads(1L))
            torch_model[["model"]] <- .torch_unserialize_model(
                model = torch_model[["model"]],
                raw   = serialized_model
            )
            n_samples <- nrow(values)
            n_times   <- .samples_ntimes(samples)
            n_bands   <- length(bands)
            values <- .pred_normalize(pred = values, stats = ml_stats)
            values <- array(
                data = as.matrix(values),
                dim  = c(n_samples, n_times, n_bands)
            )
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
