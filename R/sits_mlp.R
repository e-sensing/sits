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
#' @param samples           Time series with the training samples.
#' @param layers            Vector with number of hidden nodes in each layer.
#' @param dropout_rates     Vector with the dropout rates (0,1)
#'                          for each layer.
#' @param learning_rate     Learning rate of the optimizer
#' @param epochs            Number of iterations to train the model.
#' @param batch_size        Number of samples per gradient update.
#' @param validation_split  Number between 0 and 1.
#'                          Fraction of the training data for validation.
#'                          The model will set apart this fraction
#'                          and will evaluate the loss and any model metrics
#'                          on this data at the end of each epoch.
#' @param verbose           Verbosity mode (0 = silent,
#'                          1 = progress bar, 2 = one line per epoch).
#' @return                  Either a model to be passed in sits_predict
#'                          or a function prepared to be called further.
#'
#' @note
#' The parameters for the MLP have been chosen based on the work by Wang et al. 2017
#' that takes multilayer perceptrons as the baseline for time series classifications:
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
#'#' @references
#'
#' Zhiguang Wang, Weizhong Yan, and Tim Oates,
#' "Time series classification from scratch with deep neural networks:
#'  A strong baseline",
#'  2017 international joint conference on neural networks (IJCNN).
#'
#'
#' @examples
#' \dontrun{
#' # Retrieve the set of samples for the Mato Grosso region
#' # Build a machine learning model based on deep learning
#' dl_model <- sits_train(samples_modis_4bands, sits_mlp())
#' # get a point with a 16 year time series
#' point_4classes <- sits_select(point_mt_6bands,
#'                               bands = c("NDVI", "EVI", "NIR", "MIR"))
#' # classify the point
#' point_class <- sits_classify(point_4classes, dl_model)
#' # plot the classified point
#' plot(point_class)
#' }
#' @export
#'
sits_mlp <- function(samples = NULL,
                     layers = c(512, 512, 512),
                     dropout_rates = c(0.20, 0.30, 0.40),
                     learning_rate = 0.001,
                     epochs = 100,
                     batch_size = 64,
                     validation_split = 0.2,
                     verbose = 0) {

    # set caller to show in errors
    .check_set_caller("sits_mlp")

    # function that returns a torch model based on samples
    result_fun <- function(data) {

        # verifies if torch package is installed
        if (!requireNamespace("torch", quietly = TRUE)) {
            stop("Please install package torch", call. = FALSE)
        }

        # verifies if luz package is installed
        if (!requireNamespace("luz", quietly = TRUE)) {
            stop("Please install package luz", call. = FALSE)
        }

        # pre-conditions
        .check_that(
            x = length(layers) == length(dropout_rates),
            msg = "number of layers does not match number of dropout rates"
        )
        # data normalization
        stats <- .sits_ml_normalization_param(data)
        train_data <- .sits_distances(.sits_ml_normalize_data(data, stats))

        # is the training data correct?
        .check_chr_within(
            x = "reference",
            within = names(train_data),
            discriminator = "any_of",
            msg = "input data does not contain distances"
        )

        # get the labels of the data
        labels <- sits_labels(data)

        # create a named vector with integers match the class labels
        n_labels <- length(labels)
        int_labels <- c(1:n_labels)
        names(int_labels) <- labels

        # split the data into training and validation data sets
        # create partitions different splits of the input data
        test_data <- .sits_distances_sample(train_data,
                                            frac = validation_split
        )
        # remove the lines used for validation
        train_data <- train_data[!test_data, on = "original_row"]

        # shuffle the data
        train_data <- train_data[sample(
            nrow(train_data),
            nrow(train_data)
        ), ]
        test_data <- test_data[sample(
            nrow(test_data),
            nrow(test_data)
        ), ]

        # organize data for model training
        train_x <- data.matrix(train_data[, -2:0])
        train_y <- unname(int_labels[as.vector(train_data$reference)])

        # create the test data
        test_x <- data.matrix(test_data[, -2:0])
        test_y <- unname(int_labels[as.vector(test_data$reference)])

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

                # create a sequential module that calls the layers in the same order.
                self$model <- torch::nn_sequential(!!!tensors)
            },
            forward = function(x) {
                self$model(x)
            }
        )
        # train the model using the "luz" package
        torch_model <-
            luz::setup(
                module = mlp_module,
                loss = torch::nn_cross_entropy_loss(),
                metrics = list(luz::luz_metric_accuracy()),
                optimizer = torch::optim_adam
            ) %>%
            luz::set_hparams(
                num_pred = ncol(train_x),
                layers = layers,
                dropout_rates = dropout_rates,
                y_dim = length(int_labels)
            ) %>%
            luz::set_opt_hparams(
                lr = learning_rate
            ) %>%
            luz::fit(
                data = list(train_x, train_y),
                epochs = epochs,
                valid_data = list(test_x, test_y),
                callbacks = list(luz::luz_callback_early_stopping(
                    patience = 10,
                    min_delta = 0.05
                )),
                verbose = verbose
            )

        model_to_raw <- function(model) {
            con <- rawConnection(raw(), open = "wr")
            torch::torch_save(model, con)
            on.exit( {close(con)}, add = TRUE)
            r <- rawConnectionValue(con)
            r
        }

        model_from_raw <- function(object) {
            con <- rawConnection(object)
            on.exit( {close(con)}, add = TRUE)
            module <- torch::torch_load(con)
            module
        }
        # serialize model
        serialized_model <- model_to_raw(torch_model$model)

        # build predict closure function
        model_predict <- function(values) {

            # verifies if torch package is installed
            if (!requireNamespace("torch", quietly = TRUE)) {
                stop("Please install package torch", call. = FALSE)
            }

            # restore model
            torch_model$model <- model_from_raw(serialized_model)

            # transform input (data.table) into a matrix
            # (remove first two columns)
            values <- data.matrix(values[, -2:0])

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
