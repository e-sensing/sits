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
                        optimizer = torch::optim_adamw,
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
    # set caller for error msg
    .check_set_caller("sits_resnet")
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
        # Check validation_split parameter if samples_validation is not passed
        if (is.null(samples_validation)) {
            .check_num_parameter(validation_split, exclusive_min = 0.0, max = 0.5)
        }
        .check_pre_sits_resnet(
            samples = samples,
            blocks = blocks,
            kernels = kernels,
            epochs = epochs,
            batch_size = batch_size,
            lr_decay_epochs = lr_decay_epochs,
            lr_decay_rate = lr_decay_rate,
            patience = patience,
            min_delta = min_delta,
            verbose = verbose
        )
        # Check opt_hparams
        # Get parameters list and remove the 'param' parameter
        optim_params_function <- formals(optimizer)[-1L]
        .check_opt_hparams(opt_hparams, optim_params_function)
        optim_params_function <- utils::modifyList(
            x = optim_params_function,
            val = opt_hparams
        )
        # Sample labels
        sample_labels <- .samples_labels(samples)
        # Sample bands
        bands <- .samples_bands(samples)
        # Sample timeline
        timeline <- .samples_timeline(samples)
        # Create numeric labels vector
        code_labels <- seq_along(sample_labels)
        names(code_labels) <- sample_labels
        # Number of labels, bands, and number of samples (used below)
        n_labels <- length(sample_labels)
        n_bands <- length(bands)
        n_times <- .samples_ntimes(samples)
        # Data normalization
        ml_stats <- .samples_stats(samples)

        # Organize train and the test data
        train_test_data <- .torch_train_test_samples(
            samples = samples,
            samples_validation = samples_validation,
            ml_stats = ml_stats,
            labels = sample_labels,
            code_labels = code_labels,
            timeline = timeline,
            bands = bands,
            validation_split = validation_split
        )
        # Obtain the train and the test data
        train_samples <- train_test_data[["train_samples"]]
        test_samples <- train_test_data[["test_samples"]]

        # Organize data for model training
        n_samples_train <- nrow(train_samples)
        n_samples_test <- nrow(test_samples)
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
        torch::torch_manual_seed(sample.int(100000L, 1L))
        # Define the Resnet architecture
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
        # create ResNet
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
                x <- x |>
                    self$res_block1() |>
                    self$res_block2() |>
                    self$res_block3() |>
                    self$gap() |>
                    self$flatten() |>
                    self$softmax()
            }
        )
        # train with CPU or GPU?
        cpu_train <- .torch_cpu_train()
        # Train the model using luz
        torch_model <-
            luz::setup(
                module = resnet_model,
                loss = torch::nn_cross_entropy_loss(),
                metrics = list(luz::luz_metric_accuracy()),
                optimizer = optimizer
            ) |>
            luz::set_opt_hparams(
                !!!optim_params_function
            ) |>
            luz::set_hparams(
                n_bands  = n_bands,
                n_times  = n_times,
                n_labels = n_labels,
                blocks   = blocks,
                kernels  = kernels
            ) |>
            luz::fit(
                data = list(train_x, train_y),
                epochs = epochs,
                valid_data = list(test_x, test_y),
                callbacks = list(
                    luz::luz_callback_early_stopping(
                        monitor = "valid_loss",
                        patience = patience,
                        min_delta = min_delta,
                        mode = "min"
                    ),
                    luz::luz_callback_lr_scheduler(
                        torch::lr_step,
                        step_size = lr_decay_epochs,
                        gamma = lr_decay_rate
                    )
                ),
                accelerator = luz::accelerator(cpu = cpu_train),
                dataloader_options = list(batch_size = batch_size),
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
            colnames(values) <- sample_labels
            values
        }
        # Set model class
        predict_fun <- .set_class(
            predict_fun, "torch_model", "sits_model", class(predict_fun)
        )
    }
    # If samples is informed, train a model and return a predict function
    # Otherwise give back a train function to train model further
    .factory_function(samples, train_fun)
}
