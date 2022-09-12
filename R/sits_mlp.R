#' @title Train multi-layer perceptron models using torch
#' @name sits_mlp
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Souza, \email{lipecaso@@gmail.com}
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
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
#' The parameters for the MLP have been chosen based on the work by
#' Wang et al. 2017
#' that takes multilayer perceptrons as the baseline for time series
#' classifications:
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
#' #' @references
#'
#' Zhiguang Wang, Weizhong Yan, and Tim Oates,
#' "Time series classification from scratch with deep neural networks:
#'  A strong baseline",
#'  2017 international joint conference on neural networks (IJCNN).
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#' @examples
#' if (sits_run_examples()) {
#'     # select a set of samples
#'     samples_ndvi <- sits_select(samples_modis_4bands, bands = c("NDVI"))
#'     # create an MLP model
#'     torch_model <- sits_train(samples_ndvi, sits_mlp())
#'     # plot the model
#'     plot(torch_model)
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
#'         data_dir = data_dir,
#'         delim = "_",
#'         parse_info = c("X1", "tile", "band", "date")
#'     )
#'     # classify a data cube
#'     probs_cube <- sits_classify(data = cube, ml_model = torch_model)
#'     # plot the probability cube
#'     plot(probs_cube)
#'     # smooth the probability cube using Bayesian statistics
#'     bayes_cube <- sits_smooth(probs_cube)
#'     # plot the smoothed cube
#'     plot(bayes_cube)
#'     # label the probability cube
#'     label_cube <- sits_label_classification(bayes_cube)
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

    # set caller to show in errors
    .check_set_caller("sits_mlp")

    # function that returns a torch model based on samples
    result_fun <- function(samples) {

        # verifies if torch and luz packages is installed
        .check_require_packages(c("torch", "luz"))
        # pre-conditions
        .check_samples_train(samples)
        # check layers
        .check_int_parameter(layers, len_max = 2^31 - 1)
        # check dropout_rates
        .check_num_parameter(dropout_rates,
                             min = 0, max = 1,
                             len_min = length(layers), len_max = length(layers)
        )
        # check layers and dropout_rates
        .check_that(
            x = length(layers) == length(dropout_rates),
            msg = "number of layers does not match number of dropout rates"
        )
        # check optimizer
        .check_null(x = optimizer)
        # check epochs
        .check_int_parameter(epochs)
        # check batch_size
        .check_int_parameter(batch_size)
        # check validation_split parameter if samples_validation is not passed
        if (purrr::is_null(samples_validation))
            .check_num_parameter(validation_split, exclusive_min = 0, max = 0.5)
        # check patience
        .check_int_parameter(patience)
        # check min_delta
        .check_num_parameter(min_delta, min = 0)
        # check verbose
        .check_lgl(verbose)

        # data normalization
        stats <- .sits_ml_normalization_param(samples)
        train_samples <- .sits_distances(
            .sits_ml_normalize_data(samples, stats)
        )

        # get parameters list and remove the 'param' parameter
        optim_params_function <- formals(optimizer)[-1]
        if (!is.null(opt_hparams)) {

            .check_lst(x = opt_hparams)

            .check_chr_within(
                x = names(opt_hparams),
                within = names(optim_params_function)
            )
            optim_params_function <- utils::modifyList(
                optim_params_function,
                opt_hparams
            )
        }
        # get the timeline of the data
        timeline <- sits_timeline(samples)
        # get the bands of the data
        bands <- sits_bands(samples)
        # get the labels of the data
        labels <- sits_labels(samples)

        # create a named vector with integers match the class labels
        n_labels <- length(labels)
        int_labels <- c(1:n_labels)
        names(int_labels) <- labels

        if (!is.null(samples_validation)) {
            # check samples validation
            .check_samples_validation(samples_validation,
                                      labels, timeline, bands)
            # test samples are extracted from validation data
            test_samples <- .sits_distances(
                .sits_ml_normalize_data(samples_validation, stats)
            )
        } else {
            # split the data into training and validation data sets
            # create partitions different splits of the input data
            test_samples <- .sits_distances_sample(
                train_samples,
                frac = validation_split
            )

            # remove the lines used for validation
            train_samples <- train_samples[!test_samples, on = "original_row"]
        }
        # shuffle the data
        train_samples <- train_samples[sample(
            nrow(train_samples),
            nrow(train_samples)
        ), ]
        test_samples <- test_samples[sample(
            nrow(test_samples),
            nrow(test_samples)
        ), ]

        # organize data for model training
        train_x <- data.matrix(train_samples[, -2:0])
        train_y <- unname(int_labels[as.vector(train_samples$reference)])

        # create the test data
        test_x <- data.matrix(test_samples[, -2:0])
        test_y <- unname(int_labels[as.vector(test_samples$reference)])

        # set torch seed
        torch::torch_manual_seed(sample.int(10^5, 1))

        mlp_module <- torch::nn_module(
            "mlp_module",
            initialize = function(num_pred, layers, dropout_rates, y_dim) {
                tensors <- list()

                # input layer
                tensors[[1]] <- .torch_linear_relu_dropout(
                    input_dim = num_pred,
                    output_dim = layers[1],
                    dropout_rate = dropout_rates[1]
                )

                # if hidden layers is a vector then we add those layers
                if (length(layers) > 1) {
                    for (i in 2:length(layers)) {
                        tensors[[length(tensors) + 1]] <-
                            .torch_linear_batch_norm_relu_dropout(
                                input_dim = layers[i - 1],
                                output_dim = layers[i],
                                dropout_rate = dropout_rates[i]
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
        # train the model using the "luz" package
        torch::torch_set_num_threads(1)
        torch_model <-
            luz::setup(
                module = mlp_module,
                loss = torch::nn_cross_entropy_loss(),
                metrics = list(luz::luz_metric_accuracy()),
                optimizer = optimizer
            ) %>%
            luz::set_hparams(
                num_pred = ncol(train_x),
                layers = layers,
                dropout_rates = dropout_rates,
                y_dim = length(int_labels)
            ) %>%
            luz::set_opt_hparams(
                !!!optim_params_function
            ) %>%
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

        model_to_raw <- function(model) {
            con <- rawConnection(raw(), open = "wr")
            torch::torch_save(model, con)
            on.exit(
                {
                    close(con)
                },
                add = TRUE
            )
            r <- rawConnectionValue(con)
            r
        }

        model_from_raw <- function(object) {
            con <- rawConnection(object)
            on.exit(
                {
                    close(con)
                },
                add = TRUE
            )
            module <- torch::torch_load(con)
            module
        }
        # serialize model
        serialized_model <- model_to_raw(torch_model$model)

        # build predict closure function
        model_predict <- function(values) {

            # verifies if torch package is installed
            .check_require_packages("torch")

            # set torch threads to 1
            # function does not work on MacOS
            suppressWarnings(torch::torch_set_num_threads(1))

            # restore model
            torch_model$model <- model_from_raw(serialized_model)

            # retrieve the prediction probabilities
            predicted <- data.table::as.data.table(
                torch::as_array(
                    stats::predict(torch_model, values)
                )
            )

            # add the class labels as the column names
            colnames(predicted) <- labels

            return(predicted)
        }
        class(model_predict) <- c(
            "torch_model", "sits_model",
            class(model_predict)
        )
        return(model_predict)
    }

    result <- .sits_factory_function(samples, result_fun)
    return(result)
}
