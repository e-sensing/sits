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
#' https://github.com/cauchyturing
#' (repo: UCR_Time_Series_Classification_Deep_Learning_Baseline)
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
#' @param samples            Time series with the training samples.
#' @param samples_validation Time series with the validation samples. if the
#'                           \code{samples_validation} parameter is provided,
#'                           the \code{validation_split} parameter is ignored.
#' @param blocks             Number of 1D convolutional filters for
#'                           each block of three layers.
#' @param kernels            Size of the 1D convolutional kernels
#' @param epochs             Number of iterations to train the model.
#'                           for each layer of each block.
#' @param batch_size         Number of samples per gradient update.
#' @param validation_split   Fraction of training data
#'                           to be used as validation data.
#' @param optimizer          Optimizer function to be used.
#' @param opt_hparams        Hyperparameters for optimizer:
#'                           lr : Learning rate of the optimizer
#'                           eps: Term added to the denominator
#'                                to improve numerical stability.
#'                           weight_decay:       L2 regularization
#' @param lr_decay_epochs    Number of epochs to reduce learning rate.
#' @param lr_decay_rate      Decay factor for reducing learning rate.
#' @param patience           Number of epochs without improvements until
#'                           training stops.
#' @param min_delta	         Minimum improvement in loss function
#'                           to reset the patience counter.
#' @param verbose            Verbosity mode (TRUE/FALSE). Default is FALSE.
#'
#' @return A fitted model to be used for classification.
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#' @examples
#' if (sits_run_examples()) {
#'     # select a set of samples
#'     samples_ndvi <- sits_select(samples_modis_4bands, bands = c("NDVI"))
#'     # create a ResNet model
#'     torch_model <- sits_train(samples_ndvi, sits_resnet())
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
sits_resnet <- function(samples = NULL,
                        samples_validation = NULL,
                        blocks = c(64, 128, 128),
                        kernels = c(7, 5, 3),
                        epochs = 100,
                        batch_size = 64,
                        validation_split = 0.2,
                        optimizer = torchopt::optim_adamw,
                        opt_hparams = list(
                            lr = 0.001,
                            eps = 1e-08,
                            weight_decay = 1e-06
                        ),
                        lr_decay_epochs = 1,
                        lr_decay_rate = 0.95,
                        patience = 20,
                        min_delta = 0.01,
                        verbose = FALSE) {

    # set caller to show in errors
    .check_set_caller("sits_resnet")


    # function that returns torch model based on a sits sample data.table
    result_fun <- function(samples) {

        # verifies if torch and luz packages are installed
        .check_require_packages(c("torch", "luz"))
        # preconditions
        .check_int_parameter(blocks, min = 1, len_max = 2^31 - 1)
        .check_int_parameter(kernels, min = 1,
                             len_min = length(blocks), len_max = length(blocks))
        .check_int_parameter(epochs)
        .check_int_parameter(batch_size)
        .check_that(!purrr::is_null(optimizer),
                    msg = "invalid 'optimizer' parameter")
        .check_int_parameter(lr_decay_epochs)
        .check_num_parameter(lr_decay_rate, exclusive_min = 0, max = 1)
        # check patience
        .check_int_parameter(patience)
        # check min_delta
        .check_num_parameter(min_delta, min = 0)
        # check verbose
        .check_lgl(verbose)

        # check validation_split parameter if samples_validation is not passed
        if (purrr::is_null(samples_validation))
            .check_num_parameter(validation_split, exclusive_min = 0, max = 0.5)

        # get parameters list and remove the 'param' parameter
        optim_params_function <- formals(optimizer)[-1]
        if (!is.null(opt_hparams)) {
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

        # number of bands and number of samples
        n_bands <- length(sits_bands(samples))
        n_times <- nrow(sits_time_series(samples[1, ]))

        # data normalization
        stats <- .sits_ml_normalization_param(samples)
        train_samples <- .sits_distances(
            .sits_ml_normalize_data(samples, stats)
        )

        # is the training data correct?
        .check_chr_within(
            x = "reference",
            within = names(train_samples),
            discriminator = "any_of",
            msg = "input data does not contain distances"
        )

        if (!is.null(samples_validation)) {

            # check if the labels matches with train data
            .check_that(
                all(sits_labels(samples_validation) %in% labels) &&
                    all(labels %in% sits_labels(samples_validation))
            )
            # check if the timeline matches with train data
            .check_that(
                length(sits_timeline(samples_validation)) == length(timeline)
            )
            # check if the bands matches with train data
            .check_that(
                all(sits_bands(samples_validation) %in% bands) &&
                    all(bands %in% sits_bands(samples_validation))
            )

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
        n_samples_train <- nrow(train_samples)
        n_samples_test <- nrow(test_samples)

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
        train_x <- array(
            data = as.matrix(train_samples[, 3:ncol(train_samples)]),
            dim = c(n_samples_train, n_times, n_bands)
        )
        train_y <- unname(int_labels[as.vector(train_samples$reference)])

        # create the test data
        test_x <- array(
            data = as.matrix(test_samples[, 3:ncol(test_samples)]),
            dim = c(n_samples_test, n_times, n_bands)
        )
        test_y <- unname(int_labels[as.vector(test_samples$reference)])

        # set torch seed
        torch::torch_manual_seed(sample.int(10^5, 1))

        # Block associated to ResNet
        res_block <- torch::nn_module(
            classname = "ResBlock",
            initialize = function(in_channels,
                                  out_channels,
                                  kernels) {
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
                self$shortcut <- .torch_conv1D_batch_norm(
                    input_dim   = in_channels,
                    output_dim  = out_channels,
                    kernel_size = 1,
                    padding     = "same"
                )
                # activation
                self$act <- torch::nn_relu()
            },
            forward = function(x) {
                res <- self$shortcut(x)
                x <- self$conv_block1(x)
                x <- self$conv_block2(x)
                x <- self$conv_block3(x)
                x <- torch::torch_add(x, res)
                x <- self$act(x)
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
                                  kernels) {
                self$res_block1 <- res_block(n_bands, blocks[1], kernels)
                self$res_block2 <- res_block(blocks[1], blocks[2], kernels)
                self$res_block3 <- res_block(blocks[2], blocks[3], kernels)
                self$gap <- torch::nn_adaptive_avg_pool1d(output_size = n_bands)

                # flatten 3D tensor to 2D tensor
                self$flatten <- torch::nn_flatten()
                # classification using softmax
                self$softmax <- torch::nn_sequential(
                    torch::nn_linear(blocks[3] * n_bands, n_labels),
                    torch::nn_softmax(dim = -1)
                )
            },
            forward = function(x) {
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
        torch::torch_set_num_threads(1)
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
                !!!optim_params_function
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
            .check_require_packages("torch")

            # set torch threads to 1
            # function does not work on MacOS
            suppressWarnings(torch::torch_set_num_threads(1))

            # restore model
            torch_model$model <- model_from_raw(serialized_model)

            # transform input (data.table) into a 3D tensor
            # remove first two columns
            # reshape the 2D matrix into a 3D array
            n_samples <- nrow(values)
            n_times <- nrow(sits_time_series(samples[1, ]))
            n_bands <- length(sits_bands(samples))
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

        class(model_predict) <- c(
            "torch_model", "sits_model",
            class(model_predict)
        )

        return(model_predict)
    }

    result <- .sits_factory_function(samples, result_fun)
    return(result)
}
