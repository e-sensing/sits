#' @title Train a model using Lightweight Temporal Self-Attention Encoder
#' @name sits_lighttae
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Charlotte Pelletier, \email{charlotte.pelletier@@univ-ubs.fr}
#'
#' @description Implementation of Light Temporal Attention Encoder (L-TAE)
#' for satellite image time series
#'
#' @note
#' \code{sits} provides a set of default values for all classification models.
#' These settings have been chosen based on testing by the authors.
#' Nevertheless, users can control all parameters for each model.
#' Novice users can rely on the default values,
#' while experienced ones can fine-tune deep learning models
#' using \code{\link[sits]{sits_tuning}}.
#'
#' This function is based on the paper by Vivien Garnot referenced below
#' and code available on github at
#' https://github.com/VSainteuf/lightweight-temporal-attention-pytorch
#' If you use this method, please cite the original TAE and the LTAE paper.
#'
#' We also used the code made available by Maja Schneider in her work with
#' Marco Körner referenced below and available at
#' https://github.com/maja601/RC2020-psetae.
#'
#' @references
#' Vivien Garnot, Loic Landrieu, Sebastien Giordano, and Nesrine Chehata,
#' "Satellite Image Time Series Classification with Pixel-Set Encoders
#' and Temporal Self-Attention",
#' 2020 Conference on Computer Vision and Pattern Recognition.
#' pages 12322-12331.
#' DOI: 10.1109/CVPR42600.2020.01234
#'
#' Vivien Garnot, Loic Landrieu,
#' "Lightweight Temporal Self-Attention  for Classifying
#' Satellite Images Time Series",
#' arXiv preprint arXiv:2007.00586, 2020.
#'
#' Schneider, Maja; Körner, Marco,
#' "[Re] Satellite Image Time Series Classification
#' with Pixel-Set Encoders and Temporal Self-Attention."
#' ReScience C 7 (2), 2021.
#' DOI: 10.5281/zenodo.4835356
#'
#' @param samples            Time series with the training samples
#'                           (tibble of class "sits").
#' @param samples_validation Time series with the validation samples
#'                           (tibble of class "sits").
#'                           If \code{samples_validation} parameter is provided,
#'                           \code{validation_split} is ignored.
#' @param epochs             Number of iterations to train the model
#'                           (integer, min = 1, max = 20000).
#' @param batch_size         Number of samples per gradient update
#'                           (integer, min = 16L, max = 2048L)
#' @param validation_split   Fraction of training data
#'                           to be used as validation data.
#' @param optimizer          Optimizer function to be used.
#' @param opt_hparams        Hyperparameters for optimizer:
#'                           \code{lr} : Learning rate of the optimizer
#'                           \code{eps}: Term added to the denominator
#'                                to improve numerical stability.
#'                           \code{weight_decay}:       L2 regularization rate.
#' @param lr_decay_epochs    Number of epochs to reduce learning rate.
#' @param lr_decay_rate      Decay factor for reducing learning rate.
#' @param patience           Number of epochs without improvements until
#'                           training stops.
#' @param min_delta	         Minimum improvement in loss function
#'                           to reset the patience counter.
#' @param verbose            Verbosity mode (TRUE/FALSE). Default is FALSE.
#'
#' @return A fitted model to be used for classification of data cubes.
#'
#'
#' @examples
#' if (sits_run_examples()) {
#'     # create a lightTAE model
#'     torch_model <- sits_train(samples_modis_ndvi, sits_lighttae())
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
sits_lighttae <- function(samples = NULL,
                          samples_validation = NULL,
                          epochs = 150L,
                          batch_size = 128L,
                          validation_split = 0.2,
                          optimizer = torch::optim_adamw,
                          opt_hparams = list(
                              lr = 0.0005,
                              eps = 1e-08,
                              weight_decay = 7e-04
                          ),
                          lr_decay_epochs = 50L,
                          lr_decay_rate = 1.0,
                          patience = 20L,
                          min_delta = 0.01,
                          verbose = FALSE) {
    # set caller for error msg
    .check_set_caller("sits_lighttae")
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
            .check_num_parameter(validation_split,
                exclusive_min = 0.0, max = 0.5
            )
        }
        # Pre-conditions
        .check_pre_sits_lighttae(
            samples = samples, epochs = epochs,
            batch_size = batch_size,
            lr_decay_epochs = lr_decay_epochs,
            lr_decay_rate = lr_decay_rate,
            patience = patience, min_delta = min_delta,
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
        # Samples labels
        labels <- .samples_labels(samples)
        # Samples bands
        bands <- .samples_bands(samples)
        # Samples timeline
        timeline <- .samples_timeline(samples)
        # Create numeric labels vector
        code_labels <- seq_along(labels)
        names(code_labels) <- labels
        # Number of labels, bands, and number of samples (used below)
        n_labels <- length(labels)
        n_bands <- length(bands)
        n_times <- .samples_ntimes(samples)
        # Data normalization
        ml_stats <- .samples_stats(samples)
        # Organize train and the test data
        train_test_data <- .torch_train_test_samples(
            samples = samples,
            samples_validation = samples_validation,
            ml_stats = ml_stats,
            labels = labels,
            code_labels = code_labels,
            timeline = timeline,
            bands = bands,
            validation_split = validation_split
        )
        # Obtain the train and the test data
        train_samples <- train_test_data[["train_samples"]]
        test_samples <- train_test_data[["test_samples"]]
        n_samples_train <- nrow(train_samples)
        n_samples_test <- nrow(test_samples)

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
        torch::torch_manual_seed(sample.int(10000L, 1L))
        # Define the L-TAE architecture
        light_tae_model <- torch::nn_module(
            classname = "model_ltae",
            initialize = function(n_bands,
                                  n_labels,
                                  timeline,
                                  layers_spatial_encoder = c(32L, 64L, 128L),
                                  n_heads = 16L,
                                  n_neurons = c(256L, 128L),
                                  dropout_rate = 0.2,
                                  dim_input_decoder = 128L,
                                  dim_layers_decoder = c(64L, 32L)) {
                # define an spatial encoder
                self$spatial_encoder <-
                    .torch_pixel_spatial_encoder(
                        n_bands = n_bands,
                        layers_spatial_encoder = layers_spatial_encoder
                    )
                # number of input channels == last layer of mlp2
                in_channels <-
                    layers_spatial_encoder[length(layers_spatial_encoder)]
                # define a temporal encoder
                self$temporal_encoder <-
                    .torch_light_temporal_attention_encoder(
                        timeline     = timeline,
                        in_channels  = in_channels,
                        n_heads      = n_heads,
                        n_neurons    = n_neurons,
                        dropout_rate = dropout_rate
                    )
                # add a final layer to the decoder
                # with a dimension equal to the number of layers
                dim_layers_decoder[length(dim_layers_decoder) + 1L] <- n_labels
                # decode the tensor
                self$decoder <- .torch_multi_linear_batch_norm_relu(
                    dim_input_decoder,
                    dim_layers_decoder
                )
            },
            forward = function(input) {
                out <- input |>
                    self$spatial_encoder() |>
                    self$temporal_encoder() |>
                    self$decoder()
                out
                # softmax is done externally
                # by .ml_normalize.torch_model function
            }
        )
        # torch 12.0 with luz not working with Apple MPS
        cpu_train <- .torch_cpu_train()
        # Train the model using luz
        torch_model <-
            luz::setup(
                module = light_tae_model,
                loss = torch::nn_cross_entropy_loss(),
                metrics = list(luz::luz_metric_accuracy()),
                optimizer = optimizer
            ) |>
            luz::set_hparams(
                n_bands  = n_bands,
                n_labels = n_labels,
                timeline = timeline
            ) |>
            luz::set_opt_hparams(
                !!!optim_params_function
            ) |>
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
                accelerator = luz::accelerator(cpu = cpu_train),
                dataloader_options = list(batch_size = batch_size),
                verbose = verbose
            )
        # Serialize model
        serialized_model <- .torch_serialize_model(torch_model[["model"]])

        # Retrieve attention mask
        # Get the encoder
        # encoder <- torch_model$model$temporal_encoder
        # Retrieve the attention mask from the encoder
        # attn_mask <- encoder$attention_heads$attention$attention_mask

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
            values <- array(
                data = as.matrix(values), dim = c(n_samples, n_times, n_bands)
            )
            # CPU or GPU classification?
            if (.torch_gpu_classification()) {
                # Get batch size
                batch_size <- sits_env[["batch_size"]]
                # transform the input array to a dataset
                values <- .torch_as_dataset(values)
                # Transform data set to dataloader to use the batch size
                values <- torch::dataloader(values, batch_size = batch_size)
                # GPU classification
                values <- .try(
                    stats::predict(object = torch_model, values),
                    .msg_error = .conf("messages", ".check_gpu_memory_size")
                )
            } else {
                #  CPU classification
                values <- stats::predict(object = torch_model, values)
            }
            # Convert from tensor to array
            values <- torch::as_array(values)
            # Update the columns names to labels
            colnames(values) <- labels
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
