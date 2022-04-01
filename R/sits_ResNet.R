#' @title Train ResNet classification models
#' @name sits_resnet
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Souza, \email{lipecaso@@gmail.com}
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @author Charlotte Pelletier, \email{charlotte.pelletier@@univ-ubs.fr}
#' @author Daniel Falbel, \email{dfalbel@@gmail.com}
#'
#' @description Use a ResNet architecture for classifying image time series.
#' The ResNet (or deep residual network) was proposed by a team
#' in Microsoft Research for 2D image classification.
#' ResNet tries to address the degradation of accuracy
#' in a deep network. The idea is to replace a deep network
#' with a combination of shallow ones.
#' In the paper by Fawaz et al. (2019), ResNet was considered the best method
#' for time series classification, using the UCR dataset.
#' Please refer to the paper for more details.
#'
#' The R-torch version is based on the code made available by Zhiguang Wang,
#' author of the original paper. The code was developed in python using keras.
#'
#' https://github.com/cauchyturing/UCR_Time_Series_Classification_Deep_Learning_Baseline/blob/master/ResNet.py
#'
#' The R-torch version also considered the code by Ignacio Oguiza,
#' whose implementation is available at
#' https://github.com/timeseriesAI/tsai/blob/main/tsai/models/ResNet.py.
#'
#' There are differences between Wang's Keras code and Oguiza torch code.
#' In this case, we have used Wang's keras code as the main reference.
#'
#' @references Hassan Fawaz, Germain Forestier, Jonathan Weber,
#' Lhassane Idoumghar,  and Pierre-Alain Muller,
#' "Deep learning for time series classification: a review",
#' Data Mining and Knowledge Discovery, 33(4): 917--963, 2019.
#'
#' Zhiguang Wang, Weizhong Yan, and Tim Oates,
#' "Time series classification from scratch with deep neural networks:
#'  A strong baseline",
#'  2017 international joint conference on neural networks (IJCNN).
#'
#' @param samples           Time series with the training samples.
#' @param blocks            Number of 1D convolutional filters for
#'                          each block of three layers.
#' @param kernels           Size of the 1D convolutional kernels
#'                          for each layer of each block.
#' @param optimizer         Function with a pointer to the optimizer function
#'                          (default is optimization_adam()).

#' @param epochs            Number of iterations to train the model.
#' @param batch_size        Number of samples per gradient update.
#' @param validation_split  Fraction of training data
#'                          to be used as validation data.
#' @param optimizer         Optimizer function to be used.
#' @param learning_rate     Initial learning rate of the optimizer.
#' @param lr_decay_epochs   Number of epochs to reduce learning rate.
#' @param lr_decay_rate     Decay factor for reducing learning rate.
#' @param patience          Number of epochs without improvements until
#'                          training stops.
#' @param min_delta	        Minimum improvement in loss function
#'                          to reset the patience counter.
#' @param patience          Number of epochs without improvements until
#'                          training stops.
#' @param min_delta	        Minimum improvement to reset the patience counter.
#' @param verbose           Verbosity mode (0 = silent, 1 = progress bar,
#'                          2 = one line per epoch).
#'
#' @return A fitted model to be passed to \code{\link[sits]{sits_classify}}
#'
#' @examples
#' \dontrun{
#' # Retrieve the set of samples for the Mato Grosso (provided by EMBRAPA)
#'
#' # Build a machine learning model based on deep learning
#' rn_model <- sits_train(samples_modis_4bands, sits_resnet())
#' # Plot the model
#' plot(rn_model)
#'
#' # get a point and classify the point with the ml_model
#' point <- sits_select(point_mt_6bands,
#'     bands = c("NDVI", "EVI", "NIR", "MIR")
#' )
#' class <- sits_classify(point, rn_model)
#' plot(class, bands = c("NDVI", "EVI"))
#' }
#' @export
sits_resnet <- function(samples = NULL,
                        blocks = c(64, 128, 128),
                        kernels = c(7, 5, 3),
                        epochs = 100,
                        batch_size = 64,
                        validation_split = 0.2,
                        optimizer = torch::optim_adam,
                        learning_rate = 0.001,
                        lr_decay_epochs = 1,
                        lr_decay_rate = 0.95,
                        patience = 20,
                        min_delta = 0.01,
                        verbose = FALSE) {

    # set caller to show in errors
    .check_set_caller("sits_ResNet")

    # function that returns torch model based on a sits sample data.table
    result_fun <- function(data) {
        # verifies if torch package is installed
        if (!requireNamespace("torch", quietly = TRUE)) {
            stop("Please install package torch", call. = FALSE)
        }
        # verifies if luz package is installed
        if (!requireNamespace("luz", quietly = TRUE)) {
            stop("Please install package luz", call. = FALSE)
        }
        .check_that(
            x = length(kernels) == 3,
            msg = "should inform size of three kernels"
        )
        # preconditions
        .check_num(
            x = learning_rate,
            min = 0,
            max = 0.1,
            allow_zero = FALSE,
            len_max = 1,
            msg = "invalid learning rate"
        )
        .check_num(
            x = lr_decay_epochs,
            is_integer = TRUE,
            len_max = 1,
            min = 1,
            msg = "invalid learning rate decay epochs"
        )
        .check_num(
            x = lr_decay_rate,
            len_max = 1,
            max = 1,
            min = 0,
            allow_zero = FALSE,
            msg = "invalid learning rate decay"
        )
        # get the labels of the data
        labels <- sits_labels(data)
        # create a named vector with integers match the class labels
        n_labels <- length(labels)
        int_labels <- c(1:n_labels)
        names(int_labels) <- labels

        # number of bands and number of samples
        n_bands <- length(sits_bands(data))
        n_times <- nrow(sits_time_series(data[1, ]))

        # data normalization
        stats <- .sits_ml_normalization_param(data)
        train_data <- .sits_distances(.sits_ml_normalize_data(data, stats))

        # split the data into training and validation data sets
        # create partitions different splits of the input data
        test_data <- .sits_distances_sample(train_data,
                                            frac = validation_split
        )
        # remove the lines used for validation
        train_data <- train_data[!test_data, on = "original_row"]

        n_samples_train <- nrow(train_data)
        n_samples_test <- nrow(test_data)

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
        train_x <- array(
            data = as.matrix(train_data[, 3:ncol(train_data)]),
            dim = c(n_samples_train, n_times, n_bands)
        )
        train_y <- unname(int_labels[as.vector(train_data$reference)])

        # create the test data
        test_x <- array(
            data = as.matrix(test_data[, 3:ncol(test_data)]),
            dim = c(n_samples_test, n_times, n_bands)
        )
        test_y <- unname(int_labels[as.vector(test_data$reference)])

        # set torch seed
        torch::torch_manual_seed(sample.int(10^5, 1))

        # Block associated to ResNet
        res_block <- torch::nn_module(
            classname = "ResBlock",
            initialize = function(in_channels,
                                  out_channels,
                                  kernels){
                # create first convolution block
                self$conv_block1 <- .torch_batch_conv1D_batch_norm_relu(
                    input_dim   = in_channels,
                    output_dim  = out_channels,
                    kernel_size = kernels[1],
                    padding     = "same"
                )
                # create second convolution block
                self$conv_block2 <- .torch_conv1D_batch_norm_relu(
                    input_dim   = out_channels,
                    output_dim  = out_channels,
                    kernel_size = kernels[2],
                    padding     = "same"
                )
                # create third convolution block
                self$conv_block3 <- .torch_conv1D_batch_norm(
                    input_dim   = out_channels,
                    output_dim  = out_channels,
                    kernel_size = kernels[3],
                    padding     = "same"
                )
                # create shortcut
                self$shortcut = .torch_conv1D_batch_norm(
                    input_dim   = in_channels,
                    output_dim  = out_channels,
                    kernel_size = 1,
                    padding     = "same"
                )
                # activation
                self$act = torch::nn_relu()
            },
            forward = function(x){
                res <-  self$shortcut(x)
                x <-  self$conv_block1(x)
                x <-  self$conv_block2(x)
                x <-  self$conv_block3(x)
                x <-  torch::torch_add(x, res)
                x <-  self$act(x)
                return(x)
            }
        )
        # ResNet architecture as proposed by Wang(2017)
        res_net <- torch::nn_module(
            classname = "res_net",
            initialize = function(n_bands,
                                  n_times,
                                  n_labels,
                                  blocks,
                                  kernels){
                self$res_block1 <- res_block(n_bands, blocks[1], kernels)
                self$res_block2 <- res_block(blocks[1], blocks[2], kernels)
                self$res_block3 <- res_block(blocks[2], blocks[3], kernels)
                self$gap <- torch::nn_adaptive_avg_pool1d(output_size = n_bands)

                # flatten 3D tensor to 2D tensor
                self$flatten <- torch::nn_flatten()
                # classification using softmax
                self$softmax <- torch::nn_sequential(
                    torch::nn_linear(blocks[3]*n_bands, n_labels),
                    torch::nn_softmax(dim = -1)
                )
            },
            forward = function(x){
                x <- torch::torch_transpose(x, 2, 3)
                x <- x %>%
                    self$res_block1() %>%
                    self$res_block2() %>%
                    self$res_block3() %>%
                    self$gap() %>%
                    self$flatten() %>%
                    self$softmax()
            }
        )
        # train the model using luz
        torch_model <-
            luz::setup(
                module = res_net,
                loss = torch::nn_cross_entropy_loss(),
                metrics = list(luz::luz_metric_accuracy()),
                optimizer = optimizer
            ) %>%
            luz::set_hparams(
                n_bands  = n_bands,
                n_times  = n_times,
                n_labels = n_labels,
                blocks   = blocks,
                kernels  = kernels
            ) %>%
            luz::set_opt_hparams(
                lr = learning_rate
            ) %>%
            luz::fit(
                data = list(train_x, train_y),
                epochs = epochs,
                valid_data = list(test_x, test_y),
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
                dataloader_options = list(batch_size = batch_size),
                verbose = verbose
            )

        model_to_raw <- function(model) {
            con <- rawConnection(raw(), open = "wr")
            torch::torch_save(model, con)
            on.exit(close(con), add = TRUE)
            r <- rawConnectionValue(con)
            return(r)
        }

        model_from_raw <- function(object) {
            con <- rawConnection(object)
            on.exit(close(con), add = TRUE)
            module <- torch::torch_load(con)
            return(module)
        }
        # serialize model
        serialized_model <- model_to_raw(torch_model$model)

        # construct model predict closure function and returns
        model_predict <- function(values) {

            # verifies if torch package is installed
            if (!requireNamespace("torch", quietly = TRUE)) {
                stop("Please install package torch", call. = FALSE)
            }

            # set torch threads to 1
            torch::torch_set_num_threads(1)

            # restore model
            torch_model$model <- model_from_raw(serialized_model)

            # transform input (data.table) into a 3D tensor
            # remove first two columns
            # reshape the 2D matrix into a 3D array
            n_samples <- nrow(values)
            n_times <- nrow(sits_time_series(data[1, ]))
            n_bands <- length(sits_bands(data))
            values_x <- array(
                data = as.matrix(values[, -2:0]),
                dim = c(n_samples, n_times, n_bands)
            )
            # retrieve the prediction probabilities
            prediction <- data.table::as.data.table(
                torch::as_array(
                    stats::predict(torch_model, values_x)
                )
            )
            # adjust the names of the columns of the probs
            colnames(prediction) <- labels

            return(prediction)
        }

        class(model_predict) <- c("torch_model", "sits_model",
                                  class(model_predict))

        return(model_predict)
    }

    result <- .sits_factory_function(samples, result_fun)
    return(result)
}
