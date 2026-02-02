#' @title Contrastive neural net pre-training for sits
#' @name sits_contrastive_network
#'
#' @description
#' Self-supervised pre-training using contrastive triplet loss and torch encoder.
#'
#' @param epochs Number of training epochs.
#' @param batch_size Batch size for training.
#' @param lr Learning rate.
#' @param margin Margin for triplet loss.
#' @param verbose Whether to print training progress.
#' @param log_every Period of epochs in which the model logs partial results
#'
#' @return A function for \code{\link[sits]{sits_pre_train}} (to be used as the \code{deep_method} parameter).
#'
#' @examples
#' if (sits_run_examples()) {
#'     model <- sits_pre_train(samples_modis_ndvi, sits_contrastive_network())
#' }
#'
#' @export
sits_contrastive_network <- function(samples            = NULL,
                                     margin             = 1.0,
                                     triplet_smp_method = "random",
                                     encoder_model      = "tempcnn",
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
    .check_set_caller("sits_contrastive_network")
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

        # TODO add .sits_contrastive_pre_check()

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
        # Process samples for contrastive training
        # ------------
        ml_stats <- .samples_stats(samples)
        triplets <- .contrastive_training_data_split(samples, validation_split)
        # Torch dataset
        train_ds <- .triplet_dataset(triplets, margin = margin) # x = anchor, y = list(positive, negative, margin)
        val_ds   <- .triplet_dataset(triplets, margin = margin) # x = anchor, y = list(positive, negative, margin)
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
            timeline = timeline,
        )
        # -------------
        # Define full masked autoencoder model
        # ------------
        self  <- NULL
        super <- NULL
        contrastive_model <- torch::nn_module(
            classname = "contrastive_model",

            initialize = function(encoder, n_bands = NULL, n_labels = NULL, timeline = NULL) {
                super$initialize()
                self$encoder  <- encoder

                # keep metadata around for safety
                self$n_bands  <- n_bands
                self$n_labels <- n_labels
                self$timeline <- timeline
                self$n_times  <- length(timeline)
            },

            forward = function(x) {
                # Split x into anchor, positive, and negative for model to
                # work with luz.
                anchor <- x[ , 1:self$n_times, , drop = FALSE] |>
                    self$encoder() |>
                    torch::nnf_sigmoid()

                pos <- x[,
                         (self$n_times + 1):(2 * self$n_times),
                         ,
                         drop = FALSE] |>
                    self$encoder() |>
                    torch::nnf_sigmoid()

                neg <- x[,
                         (2 * self$n_times + 1):(3 * self$n_times),
                         ,
                         drop = FALSE] |>
                    self$encoder() |>
                    torch::nnf_sigmoid()

                # Re-concatenate model outputs
                torch::torch_cat(list(anchor, pos, neg), dim = 2)
            }
        )

        embedding_dim <- encoder$embedding_dim


        # -------------
        # THE TRAINING LOOP
        # ------------
        triplet_loss <- function(x, y) {
            # Split x into anchor, positive, and negative for model to
            # work with luz.
            anchor <- x[ , 1:self$n_times, , drop = FALSE] |>
                self$encoder() |>
                torch::nnf_sigmoid()

            pos <- x[,
                     (self$n_times + 1):(2 * self$n_times),
                     ,
                     drop = FALSE] |>
                self$encoder() |>
                torch::nnf_sigmoid()

            neg <- x[,
                     (2 * self$n_times + 1):(3 * self$n_times),
                     ,
                     drop = FALSE] |>
                self$encoder() |>
                torch::nnf_sigmoid()

            # Calculate loss
            pos_dist <- torch::torch_sum((anchor - pos) ^ 2, dim = 2)
            neg_dist <- torch::torch_sum((anchor - neg) ^ 2, dim = 2)
            loss <- torch::torch_clamp(pos_dist - neg_dist + margin, min = 0)
            loss$mean()
        }
        # verify if GPU is available
        cpu_train <- .torch_cpu_train()
        # Train the model using luz
        torch_model <-
            luz::setup(
                module = contrastive_model,
                loss = triplet_loss,
                optimizer = optimizer
            ) |>
            luz::set_hparams(
                n_bands = n_bands,
                n_times = n_times,
                n_labels = n_labels,
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
