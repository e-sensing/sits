#' @title Train ResNet classification models
#' @name sits_ResNet
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
#'                          Options: optimizer_adadelta(), optimizer_adagrad(),
#'                          optimizer_adam(), optimizer_adamax(),
#'                          optimizer_nadam(), optimizer_rmsprop(),
#'                          optimizer_sgd().
#' @param epochs            Number of iterations to train the model.
#' @param learning_rate     Nunber with learning rate of model.
#' @param batch_size        Number of samples per gradient update.
#' @param validation_split  Number between 0 and 1. Fraction of training data
#'                          to be used as validation data.
#'                          The model will set apart this fraction of the
#'                          training data, will not train on it,
#'                          and will evaluate the loss and any model metrics
#'                          on this data at the end of each epoch.
#'                          The validation data is selected from the last
#'                          samples in the x and y data provided,
#'                          before shuffling.
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
#' rn_model <- sits_train(samples_modis_4bands, sits_ResNet())
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
sits_ResNet <- function(samples = NULL,
                        blocks = c(64, 128, 128),
                        kernels = c(7, 5, 3),
                        optimizer = torch::optim_adam,
                        learning_rate = 0.001,
                        epochs = 100,
                        batch_size = 64,
                        validation_split = 0.2,
                        verbose = FALSE) {

    # set caller to show in errors
    .check_set_caller("sits_ResNet")

    # function that returns torch model based on a sits sample data.table
    result_fun <- function(data) {
        # verifies if torch package is installed
        if (!requireNamespace("torch", quietly = TRUE)) {
            stop("Please install package torch", call. = FALSE)
        }
        .check_that(
            x = length(kernels) == 3,
            msg = "should inform size of three kernels"
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
                optimizer = torch::optim_adam
            ) %>%
            luz::set_hparams(
                n_bands  = n_bands,
                n_times  = n_times,
                n_labels = n_labels,
                blocks   = blocks,
                kernels  = kernels
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
