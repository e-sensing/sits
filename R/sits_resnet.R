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
#'     # create a ResNet model
#'     torch_model <- sits_train(samples_modis_ndvi, sits_resnet())
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
#'         bayes_cube, output_dir = tempdir()
#'     )
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

    # Function that trains a torch model based on samples
    train_fun <- function(samples) {
        # Avoid add a global variable for 'self'
        self <- NULL
        # Verifies if 'torch' and 'luz' packages is installed
        .check_require_packages(c("torch", "luz"))
        # Pre-conditions:
        .check_samples_train(samples)
        .check_int_parameter(param = blocks, min = 1, len_max = 2^31 - 1)
        .check_int_parameter(
            param = kernels, min = 1, len_min = length(blocks),
            len_max = length(blocks)
        )
        .check_int_parameter(epochs)
        .check_int_parameter(batch_size)
        .check_null(optimizer, msg = "invalid 'optimizer' parameter")
        # Check validation_split parameter if samples_validation is not passed
        if (is.null(samples_validation)) {
            .check_num_parameter(
                param = validation_split, exclusive_min = 0, max = 0.5
            )
        }
        # Check opt_hparams
        # Get parameters list and remove the 'param' parameter
        optim_params_function <- formals(optimizer)[-1]
        if (!is.null(opt_hparams)) {
            .check_lst(opt_hparams, msg = "invalid 'opt_hparams' parameter")
            .check_chr_within(
                x = names(opt_hparams),
                within = names(optim_params_function),
                msg = "invalid hyperparameters provided in optimizer"
            )
            optim_params_function <- utils::modifyList(
                x = optim_params_function, val = opt_hparams
            )
        }
        # Other pre-conditions:
        .check_int_parameter(lr_decay_epochs)
        .check_num_parameter(param = lr_decay_rate, exclusive_min = 0, max = 1)
        .check_int_parameter(patience)
        .check_num_parameter(param = min_delta, min = 0)
        .check_lgl(verbose)

        # Samples labels
        labels <- sits_labels(samples)
        # Samples bands
        bands <- sits_bands(samples)
        # Samples timeline
        timeline <- sits_timeline(samples)

        # Create numeric labels vector
        code_labels <- seq_along(labels)
        names(code_labels) <- labels

        # Number of labels, bands, and number of samples (used below)
        n_labels <- length(labels)
        n_bands <- length(bands)
        n_times <- .samples_ntimes(samples)

        # Data normalization
        ml_stats <- .samples_stats(samples)
        train_samples <- .predictors(samples)
        train_samples <- .pred_normalize(pred = train_samples, stats = ml_stats)

        # Post condition: is predictor data valid?
        .check_predictors(pred = train_samples, samples = samples)

        if (!is.null(samples_validation)) {
            .check_samples_validation(
                samples_validation = samples_validation, labels = labels,
                timeline = timeline, bands = bands
            )
            # Test samples are extracted from validation data
            test_samples <- .predictors(samples)
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
            sel <- !train_samples$sample_id %in% test_samples$sample_id
            train_samples <- train_samples[sel, ]
        }
        n_samples_train <- nrow(train_samples)
        n_samples_test <- nrow(test_samples)
        # Shuffle the data
        train_samples <- train_samples[sample(
            nrow(train_samples), nrow(train_samples)
        ), ]
        test_samples <- test_samples[sample(
            nrow(test_samples), nrow(test_samples)
        ), ]
        # Organize data for model training
        train_x <- array(
            data = as.matrix(.pred_features(train_samples)),
            dim = c(n_samples_train, n_times, n_bands)
        )
        train_y <- unname(code_labels[.pred_references(train_samples)])
        # Create the test data
        test_x <- array(
            data = as.matrix(.pred_features(test_samples)),
            dim = c(n_samples_test, n_times, n_bands)
        )
        test_y <- unname(code_labels[.pred_references(test_samples)])
        # Set torch seed
        torch::torch_manual_seed(sample.int(10^5, 1))
        # Block associated to ResNet
        resnet_block <- torch::nn_module(
            classname = "block_resnet",
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
        # Define the ResNet architecture
        resnet_model <- torch::nn_module(
            classname = "model_resnet",
            initialize = function(n_bands, n_times, n_labels, blocks, kernels) {
                self$res_block1 <- resnet_block(n_bands, blocks[1], kernels)
                self$res_block2 <- resnet_block(blocks[1], blocks[2], kernels)
                self$res_block3 <- resnet_block(blocks[2], blocks[3], kernels)
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
        # train the model using luz
        torch_model <-
            luz::setup(
                module = resnet_model,
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
        # Serialize model
        serialized_model <- .torch_serialize_model(torch_model$model)

        # Function that predicts labels of input values
        predict_fun <- function(values) {
            # Verifies if torch package is installed
            .check_require_packages("torch")
            # Set torch threads to 1
            # Note: function does not work on MacOS
            suppressWarnings(torch::torch_set_num_threads(1))
            # Unserialize model
            torch_model$model <- .torch_unserialize_model(serialized_model)
            # Used to check values (below)
            input_pixels <- nrow(values)
            # Transform input into a 3D tensor
            # Reshape the 2D matrix into a 3D array
            n_samples <- nrow(values)
            n_times <- .samples_ntimes(samples)
            n_bands <- length(bands)
            # Performs data normalization
            values <- .pred_normalize(pred = values, stats = ml_stats)
            values <- array(
                data = as.matrix(values), dim = c(n_samples, n_times, n_bands)
            )
            # Do classification
            values <- stats::predict(object = torch_model, values)
            # Convert to tensor cpu to support GPU processing
            values <- torch::as_array(
                x = torch::torch_tensor(values, device = "cpu")
            )
            .check_processed_values(
                values = values, input_pixels = input_pixels
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
    # Otherwise give back a train function to train the model later
    result <- .factory_function(samples, train_fun)
    return(result)
}
