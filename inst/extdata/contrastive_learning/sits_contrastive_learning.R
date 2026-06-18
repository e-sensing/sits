#' @title Contrastive neural net pre-training for sits
#' @name sits_contrastive_learning
#' @author Alexandre Assuncao \email{alexcarssuncao@@gmail.com}
#' @references
#' Schroff, F., Kalenichenko, D., & Philbin, J. (2015).
#' FaceNet: A Unified Embedding for Face Recognition and Clustering.
#' In Proceedings of the IEEE Conference on Computer Vision and Pattern Recognition (CVPR), 815–823. doi:10.1109/CVPR.2015.7298682
#' @export
sits_contrastive_learning <- function(samples            = NULL,
                                      margin             = 1e-3,
                                      triplet_smp_method = "random",
                                      num_triplets       = NULL,
                                      encoder_model      = "lighttae",
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
        # TODO add num_triplets to check
        .check_pre_sits_contrastive_net(
            samples, epochs, batch_size,
            encoder_model, triplet_smp_method,
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
        # -------------
        # Process samples for contrastive training
        # ------------
        ml_stats <- .samples_stats(samples)
        triplets <- .contrastive_learning_data_split(samples, validation_split, num_triplets = num_triplets)
        # Torch dataset
        train_ds <- .triplet_dataset(triplets$train, margin = margin, n_times = n_times)
        val_ds   <- .triplet_dataset(triplets$val, margin = margin, n_times = n_times)

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
                anchor <- x[, 1, , ] |>
                    self$encoder() |>
                    torch::nnf_normalize(p = 2, dim = 2)
                #torch::nnf_sigmoid()

                pos <- x[, 2, , ] |>
                    self$encoder() |>
                    torch::nnf_normalize(p = 2, dim = 2)
                #torch::nnf_sigmoid()

                neg <- x[, 3, , ] |>
                    self$encoder() |>
                    torch::nnf_normalize(p = 2, dim = 2)
                #torch::nnf_sigmoid()

                # Re-concatenate model outputs
                torch::torch_stack(list(anchor, pos, neg), dim = 2)
            }
        )

        embedding_dim <- encoder$embedding_dim


        # -------------
        # THE TRAINING LOOP
        # ------------
        triplet_loss <- function(x, margin) {
            # x: anchor, positive, and negative concatenated into one tensor
            # Split x into anchor, positive, and negative for model to
            # work with luz.
            anchor <- x[, 1, ]
            pos    <- x[, 2, ]
            neg    <- x[, 3, ]
            # Calculate loss
            pos_dist <- torch::torch_sum((anchor - pos) ^ 2, dim = 2)
            neg_dist <- torch::torch_sum((anchor - neg) ^ 2, dim = 2)
            loss <- torch::torch_clamp(pos_dist - neg_dist + margin$squeeze(), min = 0)
            #cat(sprintf("Pos Dist: %s    Neg Dist: %s\n", as.numeric(pos_dist), as.numeric(pos_dist)))
            #cat(sprintf("Loss: %s\n", as.numeric(loss$mean())))
            loss$mean()
        }
        # verify if GPU is available
        cpu_train <- .torch_cpu_train()
        # Train the model using luz
        model <-
            luz::setup(
                module = contrastive_model,
                loss = triplet_loss,
                optimizer = optimizer
            ) |>
            luz::set_hparams(
                encoder = encoder,
                n_bands = n_bands,
                timeline = timeline,
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

        # ---------------------------------------------------------
        # Wrap in a luz stub for sits_encode() compatibility
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
