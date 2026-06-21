#' @title Pre-train a Masked Autoencoder on SITS time-series data
#'
#' @name sits_mae
#'
#' @description
#' \code{sits_mae()} creates a masked autoencoder (MAE) pretraining factory
#' compatible with \code{\link{sits_pre_train}}. It performs self-supervised
#' learning by masking a subset of timesteps in each sample time series,
#' training an encoder-decoder model to reconstruct the original signal,
#' and returning the pretrained encoder as a \code{torch} module.
#'
#' The function can be used in two ways:
#' \itemize{
#' \item If \code{samples} is provided, it trains immediately and returns
#' an encoder-ready model object (see Value).
#' \item If \code{samples = NULL}, it returns a training function with
#' signature \code{function(samples)} that can be passed to
#' \code{\link{sits_pre_train}} or called later.
#' }
#'
#' @param samples A \code{sits} samples object. If \code{NULL} (default),
#'   returns a training function. If provided, triggers immediate training.
#'   Base data samples (e.g., \code{sits_base}) are not supported.
#' @param embedding_dim Integer. Dimensionality of the latent embedding
#'   produced by the encoder (Default: 32L).
#' @param encoder_model Function or encoder factory. Defines the encoder
#'   backbone to be instantiated for MAE pretraining (e.g., a
#'   \code{sits_lighttae()} factory). Must accept \code{samples} and
#'   \code{embedding_dim} and return a \code{torch::nn_module}.
#' @param decoder_width Integer. Width of the decoder MLP hidden layer.
#' @param masking_method Character. Mask selection strategy. Options are
#'   passed to internal masking helpers and typically include
#'   \code{"random"}, \code{"contiguous"}, or \code{"mixed"}.
#' @param mask_ratio Numeric in (0, 1). Fraction of timesteps to mask.
#' @param mask_value Numeric. Fill value used for masked timesteps.
#' @param masked_bands Character vector specifying which bands to mask.
#'   If \code{NULL}, all bands are eligible for masking.
#' @param epochs Integer. Maximum number of training epochs.
#' @param batch_size Integer. Batch size used for training and validation.
#' @param validation_split Numeric in (0, 1). Fraction of samples held out
#'   for validation loss monitoring.
#' @param optimizer Function. A \code{torch} optimizer constructor, such as
#'   \code{torch::optim_adamw}.
#' @param opt_hparams List of optimizer hyperparameters passed to
#'   \code{optimizer}. Common entries include \code{lr}, \code{eps}, and
#'   \code{weight_decay}. Only parameters supported by the chosen optimizer
#'   are accepted.
#' @param lr_decay_epochs Integer. Step size (in epochs) for learning-rate
#'   decay when using the step scheduler.
#' @param lr_decay_rate Numeric. Multiplicative decay factor applied by the
#'   learning-rate scheduler.
#' @param patience Integer. Number of epochs without improvement in
#'   validation loss before early stopping.
#' @param min_delta Numeric. Minimum decrease in validation loss required
#'   to reset the early-stopping patience counter.
#' @param verbose Logical. If \code{TRUE}, prints training progress and
#'   per-epoch losses.
#' @param seed Integer. Random seed used to initialize Torch randomness.
#'
#' @details
#' During training, the procedure:
#' \enumerate{
#' \item Masks each sample time series according to \code{masking_method},
#' \code{mask_ratio}, \code{mask_value}, and \code{masked_bands}.
#' \item Normalizes inputs and targets using quantile-based statistics
#' derived from the original samples.
#' \item Splits data into training and validation partitions according
#' to \code{validation_split}.
#' \item Trains an encoder-decoder model with a masked MSE objective,
#' where loss is computed only over masked positions.
#' \item Applies early stopping and step learning-rate scheduling.
#' \item Discards the decoder after training, returning the pretrained
#' encoder for use in downstream tasks.
#' }
#'
#' The decoder used during pretraining is a multilayer perceptron (MLP)
#' specifically designed for MAE reconstruction. This decoder corresponds
#' to the internal MLP decoder described earlier in the documentation
#' (see the MAE decoder MLP), and maps latent embeddings back to full
#' time-series representations before being discarded after pretraining.
#'
#' When GPU execution is enabled in the environment, training may run on
#' GPU via \pkg{luz} accelerators. Otherwise, it runs on CPU.
#'
#' @return
#' If \code{samples = NULL}, returns a training function with signature
#' \code{function(samples)} that trains an MAE and returns a pretrained
#' encoder (a \code{torch} module).
#'
#' If \code{samples} is provided, returns the result of applying the
#' training function to \code{samples} (i.e., a pretrained encoder-ready
#' model object used by the \code{sits} pretraining pipeline).
#'
#' @references
#' He, K., Chen, X., Xie, S., Li, Y., Dollár, P., & Girshick, R. (2022).
#' \emph{Masked Autoencoders Are Scalable Vision Learners}. Proceedings of
#' the IEEE/CVF Conference on Computer Vision and Pattern Recognition
#' (CVPR).
#'
#' @author Alexandre Assuncao \email{alexcarssuncao@@gmail.com}
#' @export
sits_mae <- function(samples = NULL,
                     embedding_dim = 32L,
                     encoder_model = sits_lighttae(),
                     decoder_width = 128L,
                     masking_method = "contiguous",
                     mask_ratio = 0.6,
                     mask_value = 0,
                     masked_bands = NULL,
                     epochs = 150L,
                     batch_size = 128L,
                     validation_split = 0.2,
                     optimizer = torch::optim_adamw,
                     opt_hparams = list(
                         lr           = 5.0e-04,
                         eps          = 1.0e-08,
                         weight_decay = 1.0e-06
                     ),
                     lr_decay_epochs = 1,
                     lr_decay_rate = 0.95,
                     patience = 20,
                     min_delta = 0.01,
                     verbose = FALSE,
                     seed = 10L) {
    # set caller for error msg
    .check_set_caller("sits_mae")
    # Verifies if 'torch' and 'luz' packages is installed
    .check_require_packages(c("torch", "luz"))
    # documentation mode? verbose is FALSE
    verbose <- .message_verbose(verbose)
    # Band prefix for embeddings
    bands_prefix = .conf("embedding_band_prefix")
    .check_chr(bands_prefix, len_min = 1, lan_max = 1, allow_empty = FALSE)
    # Function that trains a torch model based on samples
    train_fun <- function(samples) {
        # does not support working with DEM or other base data
        if (inherits(samples, "sits_base")) {
            stop(.conf("messages", "sits_train_base_data"), call. = FALSE)
        }
        # Avoid add a global variable for 'self' and 'super'
        self <- NULL
        super <- NULL
        # Pre-conditions
        .check_pre_sits_mae(
            samples = samples,
            epochs = epochs,
            batch_size = batch_size,
            encoder = encoder_model,
            decoder_width = decoder_width,
            masking_method = masking_method,
            mask_ratio = mask_ratio,
            masked_bands = masked_bands,
            bands_prefix = bands_prefix,
            verbose = verbose
        )

        # Other pre-conditions
        .check_int_parameter(seed, allow_null = TRUE)
        # Check opt_hparams
        # Get parameters list and remove the 'param' parameter
        optim_params_function <- formals(optimizer)[-1L]
        .check_opt_hparams(opt_hparams, optim_params_function)
        optim_params_function <- utils::modifyList(
            x = optim_params_function,
            val = opt_hparams
        )

        # Samples labels
        labels <- .samples_labels(samples)
        # Samples bands
        bands <- .samples_bands(samples)
        # Samples timeline
        timeline <- .samples_timeline(samples)
        # Number of labels, bands, and number of samples (used below)
        n_labels <- length(labels)
        n_bands <- length(bands)
        n_times <- length(timeline)
        # Copy embedding_dim from parent environment to local
        embedding_dim <- embedding_dim
        # Copy bands_prefix from parent environment to local
        bands_prefix <- bands_prefix


        # If not has masked_bands set to all bands
        if (!.has(masked_bands)) {
            masked_bands <- bands
        }
        # Process samples for mae training
        ml_stats <- .samples_stats(samples)

        # Split train and validation
        idx <- sample.int(nrow(samples))
        n_val <- floor(length(idx) * validation_split)
        if (n_val > 0L) {
            val_idx <- idx[seq_len(n_val)]
            train_idx <- idx[-seq_len(n_val)]
        } else {
            val_idx <- integer(0)
            train_idx <- idx
        }

        # Torch dataset and dataloaders (datasets lazy)
        train_ds <- .mae_dataset_lazy(
            samples = samples,
            indices = train_idx,
            stats = ml_stats,
            bands = bands,
            timeline = timeline,
            mask_ratio = mask_ratio,
            masking_method = masking_method,
            mask_value = mask_value,
            masked_bands = masked_bands
        )

        val_ds <- .mae_dataset_lazy(
            samples = samples,
            indices = val_idx,
            stats = ml_stats,
            bands = bands,
            timeline = timeline,
            mask_ratio = mask_ratio,
            masking_method = masking_method,
            mask_value = mask_value,
            masked_bands = masked_bands
        )

        # Create a torch seed (we define a new variable to allow users
        # to access this seed number from the model environment)
        torch_seed <- .torch_seed(seed)
        # Set torch seed
        torch::torch_manual_seed(torch_seed)

        # Set the encoder model closure
        encoder <- encoder_model(
            samples = samples,
            embedding_dim = embedding_dim
        )

        # Set the decoder model closure
        decoder <- .sits_mae_decoder_mlp(
            embedding_dim = embedding_dim,
            decoder_width = decoder_width,
            n_times = n_times,
            n_bands = n_bands
        )

        # Define full masked autoencoder model
        mae_model <- torch::nn_module(
            classname = "MAE_model",
            initialize = function(encoder,
                                  decoder,
                                  n_bands = NULL,
                                  n_labels = NULL,
                                  timeline = NULL) {
                super$initialize()
                self$encoder <- encoder
                self$decoder <- decoder

                # keep metadata around for safety
                self$n_bands <- n_bands
                self$n_labels <- n_labels
                self$timeline <- timeline
            },
            forward = function(x) {
                x <- self$encoder(x)
                x <- self$decoder(x)
                torch::nnf_sigmoid(x)
            },
            predict = function(x) {
                x <- self$encoder(x)
                torch::nnf_sigmoid(x)
            }
        )

        # Loss function
        mae_loss <- function(pred, target) {
            if (!is.null(target$y)) {
                y_true <- target$y
                mask <- target$mask
            } else {
                y_true <- target[[1]]
                mask <- target[[2]]
            }
            # calc loss numerator
            num <- torch::nnf_mse_loss(
                pred * mask,
                y_true * mask,
                reduction = "sum"
            )
            # avoid divide-by-zero edge cases
            denom <- mask$sum()$clamp_min(1)
            # return loss
            num / denom
        }

        # verify if GPU is available
        cpu_train <- .torch_cpu_train()
        # Train the model using luz
        torch_model <-
            luz::setup(
                module = mae_model,
                loss = mae_loss,
                # metrics = list(luz::luz_metric_accuracy()),
                optimizer = optimizer
            ) |>
            luz::set_hparams(
                encoder  = encoder,
                decoder  = decoder,
                n_bands  = n_bands,
                n_labels = n_labels,
                timeline = timeline
            ) |>
            luz::set_opt_hparams(
                !!!optim_params_function
            ) |>
            luz::fit(
                data = train_ds,
                epochs = epochs,
                valid_data = val_ds,
                callbacks = list(
                    luz::luz_callback_early_stopping(
                        monitor = "valid_loss",
                        mode = "min",
                        patience = patience,
                        min_delta = min_delta
                    ),
                    luz::luz_callback_lr_scheduler(
                        torch::lr_step,
                        step_size = lr_decay_epochs,
                        gamma = lr_decay_rate
                    )
                ),
                accelerator = luz::accelerator(cpu = cpu_train),
                dataloader_options = list(
                    batch_size = batch_size,
                    shuffle = TRUE
                ),
                verbose = verbose
            )

        # Serialize model
        serialized_model <- force(.torch_serialize_model(torch_model$model))

        # Function that predicts labels of input values
        predict_fun <- function(values) {
            # Verifies if torch package is installed
            .check_require_packages("torch")
            # Set torch threads to 1
            suppressWarnings(torch::torch_set_num_threads(1L))
            # Unserialize model
            torch_model$model <- .torch_unserialize_model(
                model = torch_model$model,
                raw = serialized_model
            )
            # Transform input into a 3D tensor
            # Reshape the 2D matrix into a 3D array
            n_samples <- nrow(values)
            # Performs data normalization
            values <- .pred_normalize(pred = values, stats = ml_stats)
            # Represent matrix values as array
            values <- array(
                data = as.matrix(values), dim = c(n_samples, n_times, n_bands)
            )
            # GPU or CPU classification?
            if (.torch_gpu_classification()) {
                # Get batch size
                batch_size <- sits_env[["batch_size"]]
                # Transform the input array to a dataset
                values <- .torch_as_dataset(values)
                # Transform to dataloader to use the batch size
                values <- torch::dataloader(values, batch_size = batch_size)
                # Do GPU classification
                values <- .try(
                    stats::predict(object = torch_model, values),
                    .msg_error = .conf("messages", ".check_gpu_memory_size")
                )
            } else {
                # Do CPU classification
                values <- stats::predict(object = torch_model, values)
            }
            # Convert from tensor to array
            values <- torch::as_array(values)
            # Update the columns names to labels
            colnames(values) <- paste0(bands_prefix, seq_len(ncol(values)))
            values
        }
        # Set model class
        predict_fun <- .set_class(
            predict_fun, "torch_model", "sits_encoder", class(predict_fun)
        )
    }
    # If samples is informed, train a model and return a predict function
    # Otherwise give back a train function to train model further
    .factory_function(samples, train_fun)
}
