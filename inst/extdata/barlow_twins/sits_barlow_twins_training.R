#' @title Barlow Twins neural net pre-training for sits
#' @name sits_barlow_twins_network
#'
#' @description
#' Self-supervised pre-training using the Barlow Twins loss and a torch encoder.
#' Two views of the same location (e.g. the same place in two different years)
#' are passed through a shared encoder + projector. The Barlow Twins loss makes
#' the cross-correlation matrix of the two views' embeddings close to the
#' identity: the diagonal -> 1 (invariance) and the off-diagonal -> 0
#' (redundancy reduction). No negatives are required.
#'
#' @param embedding_dim Dimension of the encoder embedding (exported features).
#' @param proj_dim Dimension of the projector head used only during pre-training.
#' @param bt_lambda Weight of the redundancy-reduction (off-diagonal) term.
#' @param epochs Number of training epochs.
#' @param batch_size Batch size for training (larger is better for Barlow Twins).
#' @param verbose Whether to print training progress.
#'
#' @return A function for \code{\link[sits]{sits_pre_train}} (to be used as the \code{deep_method} parameter).
#'
#' @examples
#' if (sits_run_examples()) {
#'     model <- sits_pre_train(samples_modis_ndvi, sits_barlow_twins_network())
#' }
#'
#' @export
sits_barlow_twins_network <- function(samples            = NULL,
                                      embedding_dim      = 64,
                                      proj_dim           = 256L,
                                      bt_lambda          = 5e-3,
                                      pair_smp_method    = "random",
                                      num_pairs          = NULL,
                                      encoder_model      = sits_tempcnn(),
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
    .check_set_caller("sits_barlow_twins_network")
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
        # TODO add num_pairs to check
        .check_pre_sits_contrastive_net(
            samples, epochs, batch_size,
            encoder_model, pair_smp_method,
            bands_prefix, verbose
        )
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

        # Copy embedding_dim from parent environment to local
        embedding_dim <- embedding_dim
        # Copy proj_dim from parent environment to local
        proj_dim <- proj_dim
        # Copy bt_lambda from parent environment to local (used by the loss)
        bt_lambda <- bt_lambda
        # Copy bands_prefix from parent environment to local
        bands_prefix <- bands_prefix

        # -------------
        # Process samples for Barlow Twins training
        #
        # NOTE: the data helpers must now produce *pairs* of views instead of
        # triplets. Each dataset item is expected to be a tensor of shape
        # [2, time, band], where view 1 is the anchor (e.g. current year) and
        # view 2 is the positive (e.g. the same location in a previous year).
        # `.contrastive_training_data_split()` / `.pair_dataset()` are the
        # pixel-pairing logic and must be adapted accordingly.
        # ------------
        ml_stats <- .samples_stats(samples)
        pairs <- .contrastive_training_data_split(samples, validation_split, num_pairs = num_pairs)
        # Torch dataset (two-view; no margin needed for Barlow Twins)
        train_ds <- .pair_dataset(pairs$train, n_times = n_times)
        val_ds   <- .pair_dataset(pairs$val, n_times = n_times)

        # --------------------------------------------------------------
        # CREATE DUMMY DATA FOR LUZ STUB
        #   Only here for compatibility with sits_encode
        code_labels <- seq_along(labels)
        names(code_labels) <- labels
        train_test_data <- .torch_train_test_samples(
            samples = samples[1:10, ],
            samples_validation = NULL,
            ml_stats = ml_stats,
            labels = labels,
            code_labels = code_labels,
            timeline = timeline,
            bands = bands,
            validation_split = 0.5
        )
        train_samples <- train_test_data[["train_samples"]]
        n_samples_train <- nrow(train_samples)
        train_x <- array(
            data = as.matrix(.pred_features(train_samples)),
            dim = c(n_samples_train, n_times, n_bands)
        )
        train_y <- unname(code_labels[.pred_references(train_samples)])
        # -------------------------------------------------------------

        torch_seed <- .torch_seed(seed)
        # Set torch seed
        torch::torch_manual_seed(torch_seed)

        # -------------
        # Set the encoder model closure
        # ------------
        encoder <- encoder_model(
            samples = samples,
            embedding_dim = embedding_dim
        )

        # -------------
        # Define Barlow Twins model: shared encoder + projector head, two views
        # ------------
        self  <- NULL
        super <- NULL

        barlow_twins_model <- torch::nn_module(
            classname = "barlow_twins_model",

            initialize = function(encoder, embedding_dim, proj_dim = 256L,
                                  n_bands = NULL, n_labels = NULL, timeline = NULL) {
                super$initialize()
                self$encoder <- encoder

                # Projection head: expand to a wider space for the BT loss.
                # BN + ReLU between linear layers; NO activation on final layer.
                self$projector <- torch::nn_sequential(
                    torch::nn_linear(embedding_dim, proj_dim, bias = FALSE),
                    torch::nn_batch_norm1d(proj_dim),
                    torch::nn_relu(),
                    torch::nn_linear(proj_dim, proj_dim, bias = FALSE),
                    torch::nn_batch_norm1d(proj_dim),
                    torch::nn_relu(),
                    torch::nn_linear(proj_dim, proj_dim, bias = FALSE)
                )

                # keep metadata around for safety / sits_encode compatibility
                self$n_bands  <- n_bands
                self$n_labels <- n_labels
                self$timeline <- timeline
                self$n_times  <- length(timeline)
            },

            forward = function(x) {
                # x: [batch, 2, time, band] -> two views of the same location.
                # View A: current year   View B: same place, previous year.
                # NOTE: no sigmoid here. Barlow Twins standardizes features
                # inside the loss, so the projector output stays linear.
                z_a <- x[, 1, , ]$contiguous() |>
                    self$encoder() |>
                    self$projector()

                z_b <- x[, 2, , ]$contiguous() |>
                    self$encoder() |>
                    self$projector()

                # Stack the two projected views for the loss to split (dim = 2)
                torch::torch_stack(list(z_a, z_b), dim = 2)
            }
        )

        # -------------
        # THE BARLOW TWINS LOSS
        # ------------
        barlow_twins_loss <- function(input, target) {
            # input: [batch, 2, proj_dim] -- the two stacked views from forward()
            # target is ignored (Barlow Twins is self-supervised); bt_lambda is
            # taken from the enclosing train_fun() environment.
            x <- input
            z_a <- x[, 1, ]$contiguous()
            z_b <- x[, 2, ]$contiguous()

            batch_size <- z_a$size(1L)

            # Standardize each feature dimension across the batch (mean 0, std 1).
            # dim = 1 is the batch axis in torch's 1-based indexing.
            eps   <- 1e-5
            z_a_n <- (z_a - z_a$mean(dim = 1L)) / (z_a$std(dim = 1L) + eps)
            z_b_n <- (z_b - z_b$mean(dim = 1L)) / (z_b$std(dim = 1L) + eps)

            # Cross-correlation matrix: [proj_dim, proj_dim]
            c_mat <- z_a_n$t()$matmul(z_b_n) / batch_size

            # Invariance term: pull diagonal toward 1
            on_diag <- (torch::torch_diagonal(c_mat) - 1)$pow(2)$sum()

            # Redundancy-reduction term: push off-diagonal toward 0
            off_diag <- c_mat$pow(2)$sum() - torch::torch_diagonal(c_mat)$pow(2)$sum()

            on_diag + bt_lambda * off_diag
        }
        # verify if GPU is available
        cpu_train <- .torch_cpu_train()
        # Train the model using luz
        model <-
            luz::setup(
                module = barlow_twins_model,
                loss = barlow_twins_loss,
                optimizer = optimizer
            ) |>
            luz::set_hparams(
                encoder = encoder,
                embedding_dim = embedding_dim,
                proj_dim = proj_dim,
                n_bands = n_bands,
                timeline = timeline,
                n_labels = n_labels
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

        # ---------------------------------------------------------
        # Wrap in a luz stub for sits_encode() compatibility
        #   We export the ENCODER ONLY and discard the projector, which is the
        #   standard Barlow Twins practice.
        # ---------------------------------------------------------
        cpu_mod <- model$model$encoder$to(device = "cpu")
        # 2) Define a trivial nn_module *generator*
        stub_module <- torch::nn_module(
            "StubModule",
            initialize = function(n_bands, n_labels, timeline, ...) {
                # stash trained module
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
            # Set hyperparams
            luz::set_hparams(
                n_bands  = n_bands,
                n_labels = n_labels,
                timeline = timeline
            ) |>
            # zero epochs -- just registers module
            luz::fit(
                data    = list(train_x, train_y),
                epochs  = 0L,
                verbose = FALSE
            )
        # Grab the pure state‐dict from cpu_mod
        cpu_sd <- cpu_mod$state_dict()
        # Add "model." prefix to every name
        names(cpu_sd) <- paste0("model.", names(cpu_sd))
        # Inject the real weights back into the luz model
        torch_model[["model"]]$load_state_dict(cpu_sd)

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
