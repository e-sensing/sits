#' @title Label-guided contrastive learning for time series
#' @name sits_contrastive_learning
#'
#' @description
#'
#' Define a label-guided contrastive method for pre-training `sits`
#' encoders. The method learns an embedding space in which
#' time-series samples sharing the same label are represented closer
#' to each other, while samples
#' associated with different labels are separated.
#'
#' In `sits`, this method is applied to labelled Earth observation time
#' series for generating embeddings. It does not train a classifier
#' and is not formulated as an image-classification pipeline,
#' image encoder, or image-specific augmentation workflow.
#'
#' The label-guided contrastive method uses class labels to define positive and
#' negative relationships among samples. For each sample in a batch, samples
#' from the same class are treated as positives, whereas samples from other
#' classes are treated as negatives. The loss encourages the resulting
#' embeddings to preserve the semantic structure encoded by the labels.
#'
#' For each batch, two views per sample are
#' created (by pairing with a same-class sample), passed through a shared
#' encoder and projection head, and L2-normalised. Both views are then
#' concatenated into a single set of 2B representations, and the full
#' pairwise similarity matrix is computed. For each anchor, the loss
#' computes log-softmax over temperature-scaled cosine similarities to all
#' other representations (excluding the anchor itself), and averages the
#' log-probability at same-class (positive) positions. This symmetric
#' formulation uses all 2B representations as anchors, providing richer
#' gradients than a single-direction cross-view loss.
#'
#' After pre-training, the projection head is discarded and only the encoder
#' is kept for downstream use via \code{\link[sits]{sits_encode}}.
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
#' @param samples          A \code{sits} samples object. If \code{NULL}
#'   (default), returns a training function. If provided, triggers immediate
#'   training. Base data samples (e.g., \code{sits_base}) are not supported.
#' @param embedding_dim    Integer. Dimensionality of the encoder embedding
#'   (exported features). Default: 64L.
#' @param proj_dim         Integer. Dimensionality of the projection head output
#'   used only during pre-training (discarded afterwards). Default: 128L.
#' @param scaling      Numeric. Scaling for the contrastive
#'   loss. Lower values sharpen the similarity distribution.
#'   Default: 0.07.
#' @param num_pairs        Integer or \code{NULL}. Total number of pairs
#'   to form. When \code{NULL} (default), one pair is formed per sample.
#' @param encoder_model    Function. Encoder backbone factory (e.g.,
#'   \code{\link[sits]{sits_lighttae}()}). Must accept \code{samples} and
#'   \code{embedding_dim}. Default: \code{sits_lighttae()}.
#' @param epochs           Integer. Maximum number of training epochs.
#' @param batch_size       Integer. Batch size for training. Larger batches
#'   provide more positives/negatives per sample. Default: 128L.
#' @param validation_split Numeric in (0, 1). Fraction of samples held out
#'   for validation loss monitoring.
#' @param optimizer        Function. A \code{torch} optimizer constructor
#'   (default: \code{torch::optim_adamw}).
#' @param opt_hparams      Named list of optimizer hyperparameters.
#'   Common entries: \code{lr}, \code{eps}, \code{weight_decay}.
#' @param lr_decay_epochs  Integer. Step size (in epochs) for LR decay.
#' @param lr_decay_rate    Numeric. Multiplicative LR decay factor.
#' @param patience         Integer. Early-stopping patience (epochs without
#'   improvement).
#' @param min_delta        Numeric. Minimum improvement required to reset
#'   the patience counter.
#' @param verbose          Logical. Print training progress?
#' @param seed             Integer. Random seed for reproducibility.
#'
#' @return
#' If \code{samples = NULL}, a training function with signature
#' \code{function(samples)} that trains a supervised contrastive model and
#' returns a pretrained encoder (a \code{sits_encoder} closure).
#'
#' If \code{samples} is provided, the result of applying the training function
#' to \code{samples} directly.
#'
#' @references
#' Khosla, P., Teterwak, P., Wang, C., Sarna, A., Tian, Y., Isola, P.,
#' Maschinot, A., Liu, C., & Krishnan, D. (2020).
#' \emph{Supervised Contrastive Learning}.
#' Advances in Neural Information Processing Systems, 33.
#'
#' Zhang, P. & Wu, M. (2024).
#' \emph{Multi-Label Supervised Contrastive Learning}.
#' Proceedings of the AAAI Conference on Artificial Intelligence, 38(15),
#' 16786--16793.
#'
#'
#' @author Alexandre Assuncao, \email{alexcarssuncao@@gmail.com}
#'
#' @examples
#' if (sits_run_examples()) {
#'     model <- sits_pre_train(
#'         samples_modis_ndvi,
#'         sits_contrastive_learning(
#'             embedding_dim  = 32L,
#'             epochs         = 20L
#'         )
#'     )
#' }
#'
#' @export
sits_contrastive_learning <- function(samples            = NULL,
                                      embedding_dim      = 64L,
                                      proj_dim           = 128L,
                                      scaling            = 0.07,
                                      num_pairs          = NULL,
                                      encoder_model      = sits_lighttae(),
                                      epochs             = 150L,
                                      batch_size         = 128L,
                                      validation_split   = 0.2,
                                      optimizer          = torch::optim_adamw,
                                      opt_hparams = list(
                                          lr           = 5.0e-04,
                                          eps          = 1.0e-08,
                                          weight_decay = 1.0e-06
                                      ),
                                      lr_decay_epochs    = 1L,
                                      lr_decay_rate      = 0.95,
                                      patience           = 20L,
                                      min_delta          = 0.01,
                                      verbose            = FALSE,
                                      seed               = 10L) {
    # set caller for error msg
    .check_set_caller("sits_contrastive_learning")
    # Verifies if 'torch' and 'luz' packages are installed
    .check_require_packages(c("torch", "luz"))
    # Band prefix for embeddings
    bands_prefix = .conf("embedding_band_prefix")
    .check_chr(bands_prefix, len_min = 1, lan_max = 1, allow_empty = FALSE)
    # documentation mode? verbose is FALSE
    verbose <- .message_verbose(verbose)
    # Function that trains a torch model based on samples
    train_fun <- function(samples) {
        # does not support working with DEM or other base data
        if (inherits(samples, "sits_base")) {
            stop(.conf("messages", "sits_train_base_data"), call. = FALSE)
        }
        # Avoid adding a global variable for 'self'
        self <- NULL
        # Pre-conditions
        .check_pre_sits_contrastive_learning(
            samples         = samples,
            epochs          = epochs,
            batch_size      = batch_size,
            encoder_model   = encoder_model,
            bands_prefix    = bands_prefix,
            verbose         = verbose
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
        scaling       <- scaling
        bands_prefix  <- bands_prefix

        # ------------------------------------------------------------------
        # Build view-pairs for contrastive training
        #
        # Each dataset item is a tensor of shape [2, n_times, n_bands]
        # plus an integer label for the anchor.
        # ------------------------------------------------------------------
        ml_stats <- .samples_stats(samples)
        pairs <- .contrastive_learning_data_split(
            samples          = samples,
            validation_split = validation_split,
            num_pairs        = num_pairs
        )

        # Torch datasets
        train_ds <- .contrastive_learning_dataset(pairs[["train"]],
                                                n_times = n_times)
        val_ds   <- .contrastive_learning_dataset(pairs[["val"]],
                                                n_times = n_times)

        # ------------------------------------------------------------------
        # CREATE DUMMY DATA FOR LUZ STUB
        #   A tiny (<=10 rows) normalised snapshot of the data is used only
        #   to register the module structure via luz::fit(..., epochs = 0L).
        #   The actual weights come from the contrastive training above.
        # ------------------------------------------------------------------
        code_labels <- seq_along(labels)
        names(code_labels) <- labels
        stub_samples <- samples[seq_len(min(10L, nrow(samples))), ]
        train_samples <- .pred_normalize(
            pred  = .predictors(stub_samples),
            stats = ml_stats
        )
        n_samples_train <- nrow(train_samples)
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
        # Define contrastive model: encoder + projection head
        #   encoder → MLP head (Linear → ReLU → Linear) → L2-normalize
        # ------------------------------------------------------------------
        self  <- NULL
        super <- NULL

        contrastive_model <- torch::nn_module(
            classname = "contrastive_model",

            initialize = function(encoder, embedding_dim, proj_dim = 128L,
                                  n_bands = NULL, n_labels = NULL,
                                  timeline = NULL) {
                super$initialize()
                self$encoder <- encoder

                # MLP projection head
                self$head <- torch::nn_sequential(
                    torch::nn_linear(embedding_dim, embedding_dim),
                    torch::nn_relu(),
                    torch::nn_linear(embedding_dim, proj_dim)
                )

                self$n_bands  <- n_bands
                self$n_labels <- n_labels
                self$timeline <- timeline
                self$n_times  <- length(timeline)
            },

            forward = function(x) {
                # x: [batch, 2, time, band]
                # Encode both views through shared encoder + head, L2-normalize
                z_a <- x[, 1, , ]$contiguous() |>
                    self$encoder() |>
                    self$head() |>
                    torch::nnf_normalize(p = 2, dim = 2)

                z_b <- x[, 2, , ]$contiguous() |>
                    self$encoder() |>
                    self$head() |>
                    torch::nnf_normalize(p = 2, dim = 2)

                # Output: [batch, 2, proj_dim]
                torch::torch_stack(list(z_a, z_b), dim = 2)
            }
        )

        # ------------------------------------------------------------------
        # Adaptation of supervised contrastive loss (Khosla et al. 2020)
        # for 1D satellite image time series
        #
        # Both views are concatenated into a single set of 2B
        # representations. For each anchor, the loss computes
        # log-softmax over temperature-scaled cosine similarities
        # to all other representations (self excluded), then averages
        # the log-probability at same-class (positive) positions.
        # ------------------------------------------------------------------
        contrastive_loss <- function(input, target) {
            # input:  [B, 2, proj_dim] — two L2-normalised views
            # target: [B] — integer class labels
            z_a <- input[, 1, ]
            z_b <- input[, 2, ]
            B   <- z_a$size(1)

            # Concatenate both views: [2B, proj_dim]
            z_all <- torch::torch_cat(list(z_a, z_b), dim = 1)

            # Full similarity matrix: [2B, 2B]
            sim <- torch::torch_matmul(z_all, z_all$t()) / scaling

            # Duplicate labels for both views: [2B]
            labels_all <- torch::torch_cat(list(target, target))

            # Positive mask: same label, excluding self
            labels_col <- labels_all$contiguous()$view(c(-1, 1))
            pos_mask <- torch::torch_eq(
                labels_col, labels_col$t()
            )$to(dtype = torch::torch_float())
            # Remove self-similarity from positive mask
            self_mask <- torch::torch_eye(
                2L * B, dtype = torch::torch_float(), device = sim$device
            )
            pos_mask <- pos_mask * (1 - self_mask)

            # Exclude self from denominator by setting diagonal to -inf
            logits_mask <- 1 - self_mask
            sim <- sim * logits_mask + (-1e9) * self_mask

            # Log-softmax (numerically stable)
            log_prob <- torch::nnf_log_softmax(sim, dim = 2)

            # Average log-prob at positive positions
            num_pos <- pos_mask$sum(dim = 2)
            num_pos <- torch::torch_clamp(num_pos, min = 1)
            loss <- -(log_prob * pos_mask)$sum(dim = 2) / num_pos

            loss$mean()
        }

        # Verify if GPU is available
        cpu_train <- .torch_cpu_train()

        # Train the model using luz
        model <-
            luz::setup(
                module    = contrastive_model,
                loss      = contrastive_loss,
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
        # The projection head is discarded — standard practice.
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
            predict_fun, "sits_encoder", "torch_model", "sits_model", class(predict_fun)
        )
    }
    # If samples is provided, train immediately; otherwise return train_fun
    .factory_function(samples, train_fun)
}
