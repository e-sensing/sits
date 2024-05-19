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
#' @param verbose            Verbosity mode (TRUE/FALSE). Default is FALSE.
#' @return                   A torch mlp model to be used for classification.
#'
#'
#' @note
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
#'     torch_model <- sits_train(samples_modis_ndvi, sits_mlp())
#'     # plot the model
#'     plot(torch_model)
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
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
                     layers = c(512, 512, 512),
                     dropout_rates = c(0.20, 0.30, 0.40),
                     optimizer = torchopt::optim_adamw,
                     opt_hparams = list(
                         lr = 0.001,
                         eps = 1e-08,
                         weight_decay = 1.0e-06
                     ),
                     epochs = 100,
                     batch_size = 64,
                     validation_split = 0.2,
                     patience = 20,
                     min_delta = 0.01,
                     verbose = FALSE) {
    # set caller for error msg
    .check_set_caller("sits_mlp")
    # Function that trains a torch model based on samples
    train_fun <- function(samples) {
        # Avoid add a global variable for 'self'
        self <- NULL
        # Verifies if 'torch' and 'luz' packages is installed
        .check_require_packages(c("torch", "luz"))
        # Pre-conditions:
        .check_samples_train(samples)
        .check_int_parameter(epochs)
        .check_int_parameter(batch_size)
        .check_null_parameter(optimizer)
        # Check layers and dropout_rates
        .check_int_parameter(layers)
        .check_num_parameter(dropout_rates, min = 0, max = 1,
            len_min = length(layers), len_max = length(layers)
        )
        .check_that(length(layers) == length(dropout_rates),
            msg = .conf("messages", "sits_mlp_layers_dropout")
        )
        # Check validation_split parameter if samples_validation is not passed
        if (is.null(samples_validation)) {
            .check_num_parameter(validation_split, exclusive_min = 0, max = 0.5)
        }
        # Check opt_hparams
        # Get parameters list and remove the 'param' parameter
        optim_params_function <- formals(optimizer)[-1]
        if (.has(opt_hparams)) {
            .check_lst_parameter(opt_hparams,
                                 msg = .conf("messages", ".check_opt_hparams")
            )
            .check_chr_within(
                x = names(opt_hparams),
                within = names(optim_params_function),
                msg = .conf("messages", ".check_opt_hparams")
            )
            optim_params_function <- utils::modifyList(
                x = optim_params_function, val = opt_hparams
            )
        }
        # Other pre-conditions:
        .check_int_parameter(patience)
        .check_num_parameter(min_delta, min = 0)
        .check_lgl_parameter(verbose)
        # Samples labels
        labels <- .samples_labels(samples)
        # Samples bands
        bands <- .samples_bands(samples)
        # Samples timeline
        timeline <- sits_timeline(samples)
        # Create numeric labels vector
        code_labels <- seq_along(labels)
        names(code_labels) <- labels
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
        # Set torch seed
        torch::torch_manual_seed(sample.int(10^5, 1))

        # Define the MLP architecture
        mlp_model <- torch::nn_module(
            initialize = function(num_pred, layers, dropout_rates, y_dim) {
                tensors <- list()
                # input layer
                tensors[[1]] <- .torch_linear_relu_dropout(
                    input_dim = num_pred,
                    output_dim = layers[[1]],
                    dropout_rate = dropout_rates[[1]]
                )
                # if hidden layers is a vector then we add those layers
                if (length(layers) > 1) {
                    for (i in 2:length(layers)) {
                        tensors[[length(tensors) + 1]] <-
                            .torch_linear_batch_norm_relu_dropout(
                                input_dim = layers[[i - 1]],
                                output_dim = layers[[i]],
                                dropout_rate = dropout_rates[[i]]
                            )
                    }
                }
                # add output layer
                # output layer
                tensors[[length(tensors) + 1]] <-
                    torch::nn_linear(layers[length(layers)], y_dim)
               # add softmax tensor
                tensors[[length(tensors) + 1]] <- torch::nn_softmax(dim = 2)
                # create a sequential module that calls the layers in the same
                # order.
                self$model <- torch::nn_sequential(!!!tensors)
            },
            forward = function(x) {
                self$model(x)
            }
        )
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
                verbose = verbose
            )
        # Serialize model
        serialized_model <- .torch_serialize_model(torch_model[["model"]])

        # Function that predicts labels of input values
        predict_fun <- function(values) {
            # Verifies if torch package is installed
            .check_require_packages("torch")
            # Set torch threads to 1
            # Note: function does not work on MacOS
            suppressWarnings(torch::torch_set_num_threads(1))
            # Unserialize model
            torch_model[["model"]] <- .torch_unserialize_model(serialized_model)
            # Used to check values (below)
            n_input_pixels <- nrow(values)
            # Performs data normalization
            values <- .pred_normalize(pred = values, stats = ml_stats)
            # Transform input into matrix
            values <- as.matrix(values)
            # if CUDA is available, transform to torch data set
            # Load into GPU
            if (torch::cuda_is_available()) {
                values <- .as_dataset(values)
                # We need to transform in a dataloader to use the batch size
                values <- torch::dataloader(
                    values, batch_size = 2^15
                )
                # Do GPU classification
                values <- .try(
                    stats::predict(object = torch_model, values),
                    .msg_error = .conf("messages", ".check_gpu_memory_size")
                )
            } else {
                # Do CPU classification
                values <- stats::predict(object = torch_model, values)
            }
            # Convert to tensor cpu to support GPU processing
            values <- torch::as_array(
                x = torch::torch_tensor(values, device = "cpu")
            )
            # Are the results consistent with the data input?
            .check_processed_values(
                values = values, n_input_pixels = n_input_pixels
            )
            # Update the columns names to labels
            colnames(values) <- labels
            return(values)
        }
        # Set model class
        predict_fun <- .set_class(
            predict_fun, "torch_model", "sits_model", class(predict_fun)
        )
        return(predict_fun)
    }
    # If samples is informed, train a model and return a predict function
    # Otherwise give back a train function to train model further
    result <- .factory_function(samples, train_fun)
    return(result)
}
