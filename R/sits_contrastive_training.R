#' @title Contrastive neural net pre-training for sits
#' @name sits_contrastive_network
#'
#' @description
#' \code{sits_contrastive_network()} creates a self-supervised pre-training
#' factory compatible with \code{\link[sits]{sits_pre_train}}. It trains a
#' Torch encoder using a contrastive triplet objective, where embeddings of
#' an anchor sample are encouraged to be closer to embeddings of a
#' positive sample than to embeddings of a negative sample by at least a
#' margin.
#'
#' The function can be used in two ways:
#' \itemize{
#'   \item If \code{samples} is provided, it trains immediately and returns
#'   an encoder-ready model object.
#'   \item If \code{samples = NULL} (default), it returns a training
#'   function with signature \code{function(samples)} that can be passed
#'   to \code{\link[sits]{sits_pre_train}} or called later.
#' }
#'
#' @param samples A \code{sits} samples object. If \code{NULL}, returns a
#'   training function. Base data samples (e.g., \code{sits_base}) are not
#'   supported.
#' @param embedding_dim Integer. Dimensionality of the latent embedding
#'   produced by the encoder.
#' @param margin Numeric. Margin used in the triplet loss. Larger values
#'   enforce a stronger separation between positive and negative pairs.
#' @param triplet_smp_method Character. Strategy used to generate triplets
#'   of samples. The supported values depend on internal triplet sampling
#'   helpers (see Details).
#' @param encoder_model Function or encoder factory used to instantiate the
#'   backbone encoder (e.g., \code{sits_tempcnn()}). Must accept
#'   \code{samples} and \code{embedding_dim} and return a
#'   \code{torch::nn_module}.
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
#' @param bands_prefix Character. Prefix used to name embedding dimensions
#'   when producing encoder outputs downstream. Default is \code{"E"}.
#' @param verbose Logical. If \code{TRUE}, prints training progress and
#'   per-epoch losses.
#' @param seed Integer. Random seed used to initialize Torch randomness.
#'
#' @details
#' Triplets are created from the provided \code{samples} using an internal
#' helper (e.g., \code{.contrastive_training_data_split}). Each training
#' item is composed of three time series: anchor, positive, and negative.
#'
#' The model applies the same encoder to each element of the triplet and
#' computes a triplet loss based on squared Euclidean distances between
#' embeddings. Training uses \pkg{luz} with early stopping and a step
#' learning-rate scheduler.
#'
#' After training, the returned object keeps the pretrained encoder for
#' downstream use in \code{sits} pipelines.
#'
#' @return
#' If \code{samples = NULL}, returns a training function with signature
#' \code{function(samples)} that trains a contrastive model and returns a
#' pretrained encoder (a \code{torch} module).
#'
#' If \code{samples} is provided, returns the result of applying the
#' training function to \code{samples} (i.e., a pretrained encoder-ready
#' model object used by the \code{sits} pretraining pipeline).
#'
#' @examples
#' if (sits_run_examples()) {
#'     model <- sits_pre_train(samples_modis_ndvi, sits_contrastive_network())
#' }
#'
#' @author Alexandre Assuncao \email{alexcarssuncao@@gmail.com}
sits_contrastive_network <- function(samples = NULL,
                                     embedding_dim = 64L,
                                     margin = 1.0,
                                     triplet_smp_method = "random",
                                     encoder_model = sits_tempcnn(),
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
                                     bands_prefix = "E",
                                     verbose = FALSE,
                                     seed = 10L) {
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
        n_bands <- length(bands)
        n_times <- .samples_ntimes(samples)
        train_samples <- .predictors(samples)
        # Copy embedding_dim from parent environment to local
        embedding_dim <- embedding_dim
        # Copy bands_prefix from parent environment to local
        bands_prefix <- bands_prefix

        # Process samples for contrastive training
        ml_stats <- .samples_stats(samples)
        triplets <- .contrastive_training_data_split(samples, validation_split)
        # Torch dataset
        # x = anchor, y = list(positive, negative, margin)
        train_ds <- .triplet_dataset(triplets, margin = margin)
        # x = anchor, y = list(positive, negative, margin)
        val_ds <- .triplet_dataset(triplets, margin = margin)
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

        # Define full masked autoencoder model
        self <- NULL
        super <- NULL
        contrastive_model <- torch::nn_module(
            classname = "contrastive_model",
            initialize = function(encoder,
                                  n_bands = NULL,
                                  n_labels = NULL,
                                  timeline = NULL) {
                super$initialize()
                self$encoder <- encoder

                # keep metadata around for safety
                self$n_bands <- n_bands
                self$n_labels <- n_labels
                self$timeline <- timeline
                self$n_times <- length(timeline)
            },
            forward = function(x) {
                # Split x into anchor, positive, and negative for model to
                # work with luz.
                anchor <- x[, 1:self$n_times, , drop = FALSE] |>
                    self$encoder() |>
                    torch::nnf_sigmoid()

                pos <- x[,
                    (self$n_times + 1):(2 * self$n_times), ,
                    drop = FALSE
                ] |>
                    self$encoder() |>
                    torch::nnf_sigmoid()

                neg <- x[,
                    (2 * self$n_times + 1):(3 * self$n_times), ,
                    drop = FALSE
                ] |>
                    self$encoder() |>
                    torch::nnf_sigmoid()

                # Re-concatenate model outputs
                torch::torch_cat(list(anchor, pos, neg), dim = 2)
            }
        )

        embedding_dim <- encoder$embedding_dim

        # Loss function
        triplet_loss <- function(x, y) {
            # Split x into anchor, positive, and negative for model to
            # work with luz.
            anchor <- x[, 1:self$n_times, , drop = FALSE] |>
                self$encoder() |>
                torch::nnf_sigmoid()

            pos <- x[,
                (self$n_times + 1):(2 * self$n_times), ,
                drop = FALSE
            ] |>
                self$encoder() |>
                torch::nnf_sigmoid()

            neg <- x[,
                (2 * self$n_times + 1):(3 * self$n_times), ,
                drop = FALSE
            ] |>
                self$encoder() |>
                torch::nnf_sigmoid()

            # Calculate loss
            pos_dist <- torch::torch_sum((anchor - pos)^2, dim = 2)
            neg_dist <- torch::torch_sum((anchor - neg)^2, dim = 2)
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
                dataloader_options = list(
                    batch_size = batch_size,
                    shuffle = TRUE
                ),
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
            torch_model[["model"]] <-
                .torch_unserialize_model(serialized_model)
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
