#' @title Pre-train a Masked Autoencoder on SITS time-series data
#'
#' @name sits_mae
#'
#' @author Alexandre Assuncao \email{alexcarssuncao@@gmail.com}
#'
#' @description
#' `sits_mae()` returns a training-factory function for a Masked Autoencoder (MAE)
#' that you can pass to [sits_pre_train()].  The resulting closure takes a SITS
#' `samples` object, runs a self-supervised reconstruction loop over masked time-steps,
#' and returns the pretrained encoder.
#'
#' @param encoder_model  Character. Which encoder backbone to use. Supported values:
#'                       `"lighttae"` (temporal attention encoder) or
#'                       `"tempcnn"` (1D convolutional).
#' @param decoder_model  Character. Which decoder head to use. Currently only `"mlp"`.
#' @param masking_method Character. How to select masked positions. One of
#'                       `"random"` or `"contiguous"`.
#' @param mask_ratio     Numeric in (0,1). Fraction of time-steps to mask.
#' @param mask_value A numeric value specifying the values of masked samples.
#' @param masked_bands      A character vector specifying which bands will be masked. In the default case, bands = NULL,
#'                          all bands are masked.
#' @param epochs         Integer. Number of training epochs.
#' @param batch_size     Integer. Batch size for both training and validation.
#' @param validation_split Numeric in (0,1). Fraction of samples held out for validation.
#' @param optimizer_fn   Function. A `torch` optimizer constructor (e.g. `torch::optim_adamw`).
#' @param opt_hparams        Hyperparameters for optimizer:
#'                           lr : Learning rate of the optimizer
#'                           eps: Term added to the denominator
#'                                to improve numerical stability.
#'                           weight_decay:       L2 regularization
#' @param lr_decay_epochs    Number of epochs to reduce learning rate.
#' @param lr_decay_rate      Decay factor for reducing learning rate.
#' @param patience           Number of epochs without improvements until
#'                           training stops.
#' @param min_delta	         Minimum improvement in loss function
#'                           to reset the patience counter.
#' @param bands_prefix   Character. Specifying the prefix of the embedding dimensions' names. Default: "EMB"
#' @param verbose        Logical. If `TRUE`, print per-epoch training/validation losses.
#'
#' @return
#' A function with signature `function(samples)` which, when called on a SITS
#' samples object, trains a masked autoencoder and returns the pretrained encoder
#' (as a `torch` module).
#'
#' @references
#' He, K., Chen, X., Xie, S., Li, Y., Dollár, P., & Girshick, R. (2022).
#' *Masked Autoencoders Are Scalable Vision Learners*. Proceedings of the
#' IEEE/CVF Conference on Computer Vision and Pattern Recognition (CVPR).
#'
#' @export

sits_mae <- function(samples            = NULL,
                     encoder_model      = "lighttae",
                     decoder_model      = "mlp",
                     masking_method     = "contiguous",
                     mask_ratio         = 0.6,
                     mask_value         = 0,
                     masked_bands       = NULL,
                     epochs             = 150L,
                     batch_size         = 128L,
                     validation_split   = 0.2,
                     optimizer          = torch::optim_adamw,
                     opt_hparams = list(
                         lr           = 5.0e-04,
                         eps          = 1.0e-08,
                         weight_decay = 1.0e-06
                     ),
                     lr_decay_epochs    = 1,
                     lr_decay_rate      = 0.95,
                     patience           = 20,
                     min_delta          = 0.01,
                     bands_prefix       = "EMB",
                     verbose            = FALSE,
                     seed               = 10L) {
    # set caller for error msg
    .check_set_caller("sits_mae")
    # Verifies if 'torch' and 'luz' packages is installed
    .check_require_packages(c("torch", "luz"))
    # documentation mode? verbose is FALSE
    verbose <- .message_verbose(verbose)
    # Function that trains a torch model based on samples
    train_fun <- function(samples) {
        # does not support working with DEM or other base data
        if (inherits(samples, "sits_base")) {
            stop(.conf("messages", "sits_train_base_data"), call. = FALSE)
        }
        # Avoid add a global variable for 'self'
        self <- NULL
        # Pre-conditions
        .check_pre_sits_mae(samples, epochs, batch_size,
                            encoder_model, decoder_model,
                            masking_method, mask_ratio, masked_bands,
                            bands_prefix, verbose)

        # Other pre-conditions:
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
        n_bands  <- length(bands)
        n_times  <- .samples_ntimes(samples)
        train_samples <- .predictors(samples)
        # -------------
        # Process samples for mae training
        # ------------
        ml_stats <- .samples_stats(samples)
        mae_data <- .mae_data_split_train_val(samples, mask_ratio, masking_method,
                                              mask_value, masked_bands, validation_split)
        # # Organize data for model training
        # train_x <- mae_data$train_x
        # train_y <- list(
        #     y    = mae_data$train_y,
        #     mask = mae_data$train_mask
        # )
        # # Create the test data
        # test_x   <- mae_data$val_x
        # test_y   <- list(
        #     y    = mae_data$val_y,
        #     mask = mae_data$val_mask
        # )

        # Torch dataset and dataloaders
        train_ds <- .mae_dataset(mae_data$train_x, mae_data$train_y, mae_data$train_mask)
        train_dl <- torch::dataloader(train_ds, batch_size = batch_size, shuffle = TRUE)
        val_ds   <- .mae_dataset(mae_data$val_x, mae_data$val_y, mae_data$val_mask)
        val_dl   <- torch::dataloader(val_ds, batch_size = batch_size)

        # Create a torch seed (we define a new variable to allow users
        # to access this seed number from the model environment)
        torch_seed <- .torch_seed(seed)
        # Set torch seed
        torch::torch_manual_seed(torch_seed)
        # -------------
        # Set the encoder model closure
        # ------------
        encoder_fn <- switch(encoder_model,
                             "lighttae"  = .sits_mae_encoder_lighttae,
                             "mlp"       = .sits_mae_encoder_mlp,
                             "tempcnn"   = .sits_mae_encoder_tempcnn
        )
        encoder <- encoder_fn(
            samples  = samples,
            n_bands  = n_bands,
            timeline = timeline
        )

        # -------------
        # Set the decoder model closure
        # ------------
        decoder_fn <- switch(decoder_model,
                             "mlp"    = .sits_mae_decoder_mlp,
                             "linear" = .sits_mae_decoder_linear,
        )
        decoder <- decoder_fn(
            embedding_dim = encoder$embedding_dim,
            n_times = n_times,
            n_bands = n_bands
        )

        # -------------
        # Define full masked autoencoder model
        # ------------
        self  <- NULL
        super <- NULL
        mae_model <- torch::nn_module(
            classname = "MAE_model",

            initialize = function(encoder, decoder, n_bands = NULL, n_labels = NULL, timeline = NULL) {
                super$initialize()
                self$encoder  <- encoder
                self$decoder  <- decoder

                # keep metadata around for safety
                self$n_bands  <- n_bands
                self$n_labels <- n_labels
                self$timeline <- timeline
            },

            forward = function(x) {
                x <- self$encoder(x)
                x <- self$decoder(x)
                torch::nnf_sigmoid(x)
            }
        )

        embedding_dim <- encoder$embedding_dim

        # -------------
        # THE TRAINING LOOP
        # ------------
        mae_loss <- function(pred, target) {
            if (!is.null(target$y)) {
                y_true <- target$y
                mask   <- target$mask
            } else {
                y_true <- target[[1]]
                mask   <- target[[2]]
            }
            # calc loss numerator
            num <- torch::nnf_mse_loss(pred * mask, y_true * mask, reduction = "sum")
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
                #metrics = list(luz::luz_metric_accuracy()),
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
                dataloader_options = list(batch_size = batch_size, shuffle = TRUE),
                verbose = verbose
            )

        torch_model$model$decoder <- torch::nn_identity()

        # Serialize model
        serialized_model <- .torch_serialize_model(torch_model[["model"]])

        # Function that predicts labels of input values
        predict_fun <- function(values) {
            # Verifies if torch package is installed
            .check_require_packages("torch")
            # Set torch threads to 1
            suppressWarnings(torch::torch_set_num_threads(1L))
            # Unserialize model
            torch_model[["model"]] <- .torch_unserialize_model(serialized_model)
            # Transform input into a 3D tensor
            # Reshape the 2D matrix into a 3D array
            n_samples <- nrow(values)
            n_times <- .samples_ntimes(samples)
            n_bands <- length(bands)
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
