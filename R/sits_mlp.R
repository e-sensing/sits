#' @title Train multi-layer perceptron models using torch
#' @name sits_mlp
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Use a multi-layer perceptron algorithm to classify data.
#' This function uses the R "torch" and "luz" packages.
#' Please refer to the documentation of those package for more details.
#'
#' @param samples            Time series with the training samples.
#' @param samples_validation Time series with the validation samples. if the
#'                           \code{samples_validation} parameter is provided,
#'                           the \code{validation_split} parameter is ignored.
#' @param layers             Vector with number of hidden nodes in each layer.
#' @param dropout_rates      Vector with the dropout rates (0,1)
#'                           for each layer.
#' @param optimizer          Optimizer function to be used.
#' @param opt_hparams        Hyperparameters for optimizer:
#'                           lr : Learning rate of the optimizer
#'                           eps: Term added to the denominator
#'                                to improve numerical stability..
#'                           weight_decay:       L2 regularization
#' @param epochs             Number of iterations to train the model.
#' @param batch_size         Number of samples per gradient update.
#' @param validation_split   Number between 0 and 1.
#'                           Fraction of the training data for validation.
#'                           The model will set apart this fraction
#'                           and will evaluate the loss and any model metrics
#'                           on this data at the end of each epoch.
#' @param patience           Number of epochs without improvements until
#'                           training stops.
#' @param min_delta	         Minimum improvement in loss function
#'                           to reset the patience counter.
#' @param seed               Seed for random values.
#' @param verbose            Verbosity mode (TRUE/FALSE). Default is FALSE.
#' @return                   A torch mlp model to be used for classification.
#'
#'
#' @note
#'
#' \code{sits} provides a set of default values for all classification models.
#' These settings have been chosen based on testing by the authors.
#' Nevertheless, users can control all parameters for each model.
#' Novice users can rely on the default values,
#' while experienced ones can fine-tune deep learning models
#' using \code{\link[sits]{sits_tuning}}.
#'
#' The default parameters for the MLP have been chosen based on the work by
#' Wang et al. 2017 that takes multilayer perceptrons as the baseline
#' for time series classifications:
#' (a) Three layers with 512 neurons each, specified by the parameter `layers`;
#' (b) dropout rates of 10%, 20%, and 30% for the layers;
#' (c) the "optimizer_adam" as optimizer (default value);
#' (d) a number of training steps (`epochs`) of 100;
#' (e) a `batch_size` of 64, which indicates how many time series
#' are used for input at a given steps;
#' (f) a validation percentage of 20%, which means 20% of the samples
#' will be randomly set side for validation.
#' (g) The "relu" activation function.
#'
#' @references
#' Zhiguang Wang, Weizhong Yan, and Tim Oates,
#' "Time series classification from scratch with deep neural networks:
#'  A strong baseline",
#'  2017 international joint conference on neural networks (IJCNN).
#'
#' @examples
#' if (sits_run_examples()) {
#'     # create an MLP model
#'     torch_model <- sits_train(
#'         samples_modis_ndvi,
#'         sits_mlp(epochs = 20, verbose = TRUE)
#'     )
#'     # plot the model
#'     plot(torch_model)
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     # classify a data cube
#'     probs_cube <- sits_classify(
#'         data = cube, ml_model = torch_model, output_dir = tempdir()
#'     )
#'     # plot the probability cube
#'     plot(probs_cube)
#'     # smooth the probability cube using Bayesian statistics
#'     bayes_cube <- sits_smooth(probs_cube, output_dir = tempdir())
#'     # plot the smoothed cube
#'     plot(bayes_cube)
#'     # label the probability cube
#'     label_cube <- sits_label_classification(
#'         bayes_cube,
#'         output_dir = tempdir()
#'     )
#'     # plot the labelled cube
#'     plot(label_cube)
#' }
#' @export
#'
sits_mlp <- function(samples = NULL,
                     samples_validation = NULL,
                     layers = c(512L, 512L, 512L),
                     dropout_rates = c(0.20, 0.30, 0.40),
                     optimizer = torch::optim_adamw,
                     opt_hparams = list(
                         lr = 0.001,
                         eps = 1e-08,
                         weight_decay = 1.0e-06
                     ),
                     epochs = 150L,
                     batch_size = 128L,
                     validation_split = 0.2,
                     patience = 20L,
                     min_delta = 0.005,
                     seed = NULL,
                     verbose = FALSE) {
    # set caller for error msg
    .check_set_caller("sits_mlp")
    # Verifies if 'torch' and 'luz' packages is installed
    .check_require_packages(c("torch", "luz"))
    # documentation mode? verbose is FALSE
    verbose <- .message_verbose(verbose)
    # Function that trains a torch model based on samples
    train_fun <- function(samples, embedding_dim = NULL) {
        # does not support working with DEM or other base data
        if (inherits(samples, "sits_base")) {
            stop(.conf("messages", "sits_train_base_data"), call. = FALSE)
        }
        # Add a global variable for 'self'
        self <- NULL
        # Check validation_split parameter if samples_validation is not passed
        if (is.null(samples_validation)) {
            .check_num_parameter(validation_split, exclusive_min = 0.0, max = 0.5)
        }
        # Pre-conditions - checking parameters
        .check_pre_sits_mlp(
            samples = samples, epochs = epochs,
            batch_size = batch_size, layers = layers,
            dropout_rates = dropout_rates, patience = patience,
            min_delta = min_delta,
            embedding_dim = embedding_dim,
            verbose = verbose
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
        # Create numeric labels vector
        code_labels <- seq_along(labels)
        names(code_labels) <- labels
        # # Data normalization
        ml_stats <- .samples_stats(samples)

        # Organize train and the test data
        # Data normalization
        ml_stats <- .samples_stats(samples)
        train_samples <- .predictors(samples)
        train_samples <- .pred_normalize(pred = train_samples, stats = ml_stats)
        # Post condition: is predictor data valid?
        .check_predictors(pred = train_samples, samples = samples)
        # Are there samples for validation?
        if (!is.null(samples_validation)) {
            .check_samples_validation(
                samples_validation = samples_validation, labels = labels,
                timeline = timeline, bands = bands
            )
            # Test samples are extracted from validation data
            test_samples <- .predictors(samples_validation)
            test_samples <- .pred_normalize(
                pred = test_samples, stats = ml_stats
            )
        } else {
            # Split the data into training and validation data sets
            # Create partitions different splits of the input data
            test_samples <- .pred_sample(
                pred = train_samples, frac = validation_split
            )
            # Remove the lines used for validation
            sel <- !train_samples[["sample_id"]] %in%
                test_samples[["sample_id"]]
            train_samples <- train_samples[sel, ]
        }
        # Shuffle the data
        train_samples <- train_samples[sample(
            nrow(train_samples), nrow(train_samples)
        ), ]
        test_samples <- test_samples[sample(
            nrow(test_samples), nrow(test_samples)
        ), ]

        # Organize data for model training
        train_x <- as.matrix(.pred_features(train_samples))
        train_y <- unname(code_labels[.pred_references(train_samples)])
        # Create the test data
        test_x <- as.matrix(.pred_features(test_samples))
        test_y <- unname(code_labels[.pred_references(test_samples)])
        # Create a torch seed (we define a new variable to allow users
        # to access this seed number from the model environment)
        torch_seed <- .torch_seed(seed)
        # Set torch seed
        torch::torch_manual_seed(torch_seed)
        # Define the MLP architecture
        mlp_model <- torch::nn_module(
            initialize = function(num_pred, layers, dropout_rates, y_dim) {
                tensors <- list()
                # input layer
                tensors[[1L]] <- .torch_linear_relu_dropout(
                    input_dim = num_pred,
                    output_dim = layers[[1L]],
                    dropout_rate = dropout_rates[[1L]]
                )
                # if hidden layers is a vector then we add those layers
                if (length(layers) > 1L) {
                    for (i in 2L:length(layers)) {
                        tensors[[length(tensors) + 1L]] <-
                            .torch_linear_batch_norm_relu_dropout(
                                input_dim = layers[[i - 1L]],
                                output_dim = layers[[i]],
                                dropout_rate = dropout_rates[[i]]
                            )
                    }
                }
                # add output layer
                if (!.has(embedding_dim)) {
                    tensors[[length(tensors) + 1L]] <-
                        torch::nn_linear(layers[length(layers)], y_dim)
                } else {
                    tensors[[length(tensors) + 1L]] <-
                        torch::nn_linear(layers[length(layers)], embedding_dim)
                }
                # softmax is done externally
                # create a sequential module that calls the layers
                self$model <- torch::nn_sequential(!!!tensors)
            },
            forward = function(x) {
                self$model(x)
            }
        )

        # return encoder model
        if (.has(embedding_dim)) {
            return(mlp_model(
                num_pred = ncol(train_x),
                layers = layers,
                dropout_rates = dropout_rates,
                y_dim = length(code_labels)
            ))
        }
        # Train with CPU or GPU?
        cpu_train <- .torch_cpu_train()
        # Train the model using luz
        torch_model <-
            luz::setup(
                module = mlp_model,
                loss = torch::nn_cross_entropy_loss(),
                metrics = list(luz::luz_metric_accuracy()),
                optimizer = optimizer
            ) |>
            luz::set_hparams(
                num_pred = ncol(train_x),
                layers = layers,
                dropout_rates = dropout_rates,
                y_dim = length(code_labels)
            ) |>
            luz::set_opt_hparams(
                !!!optim_params_function
            ) |>
            luz::fit(
                data = list(train_x, train_y),
                epochs = epochs,
                valid_data = list(test_x, test_y),
                callbacks = list(luz::luz_callback_early_stopping(
                    patience = patience,
                    min_delta = min_delta
                )),
                dataloader_options = list(batch_size = batch_size),
                accelerator = luz::accelerator(cpu = cpu_train),
                verbose = verbose
            )
        # remove data used for training
        force(rm(train_samples, test_samples,
                 train_y, train_x, test_y, test_x))
        gc()
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
            # GPU or CPU classification?
            use_gpu <- (
                is.list(values) &&
                    !is.null(values[["callback"]]) &&
                    .torch_gpu_classification()
            )
            # Classify!
            if (use_gpu) {
                # Get multicores
                multicores <- sits_env[["multicores"]]
                # Transform dataset into a dataloader
                block_dataloader <- torch::dataloader(
                    dataset = values[["dataset"]],
                    num_workers = multicores,
                    batch_size = 1L
                )
                # Predict!
                values <- stats::predict(
                    object = torch_model,
                    newdata = block_dataloader,
                    callbacks = list(values[["callback"]]),
                    stack = FALSE
                )
                # Prepare results
                values <- unlist(values)
            } else {
                # Transform input into a 3D tensor
                n_samples <- nrow(values)
                n_times <- .samples_ntimes(samples)
                n_bands <- length(bands)
                # Performs data normalization on CPU
                values <- .pred_normalize(pred = values, stats = ml_stats)
                values <- array(
                    data = as.matrix(values),
                    dim = c(n_samples, n_times, n_bands)
                )
                # CPU classification
                values <- stats::predict(
                    object = torch_model,
                    newdata = values,
                    accelerator = luz::accelerator(cpu = TRUE)
                )
                # Convert from tensor to array
                values <- torch::as_array(values)
                # Update the columns names to labels
                colnames(values) <- sample_labels
            }
            # Return!
            values
        }
        # Set model class
        predict_fun <- .set_class(
            predict_fun, "torch_model", "sits_model", class(predict_fun)
        )
        predict_fun
    }
    # If samples is informed, train a model and return a predict function
    # Otherwise give back a train function to train model further
    .factory_function(samples, train_fun)
}
