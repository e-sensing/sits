#' @title Train a model using Lightweight Temporal Self-Attention Encoder
#' @name sits_lighttae
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Charlotte Pelletier, \email{charlotte.pelletier@@univ-ubs.fr}
#'
#' @description Implementation of Light Temporal Attention Encoder (L-TAE)
#' for satellite image time series
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
sits_lighttae <- function(samples = NULL,
                          samples_validation = NULL,
                          epochs = 150L,
                          batch_size = 128L,
                          validation_split = 0.2,
                          optimizer = torch::optim_adamw,
                          opt_hparams = list(
                              lr = 0.005,
                              eps = 1e-08,
                              weight_decay = 1e-06
                          ),
                          lr_decay_epochs = 50L,
                          lr_decay_rate = 1.0,
                          patience = 20L,
                          min_delta = 0.01,
                          verbose = FALSE) {
    # set caller for error msg
    .check_set_caller("sits_lighttae")
    # Function that trains a torch model based on samples
    train_fun <- function(samples) {
        # Avoid add a global variable for 'self'
        self <- NULL
        # Verifies if 'torch' and 'luz' packages is installed
        .check_require_packages(c("torch", "luz"))
        # Pre-conditions:
        .check_samples_train(samples)
        .check_int_parameter(epochs, min = 1L, max = 20000L)
        .check_int_parameter(batch_size, min = 16L, max = 2048L)
        .check_null_parameter(optimizer)
        # Check validation_split parameter if samples_validation is not passed
        if (is.null(samples_validation)) {
            .check_num_parameter(validation_split, exclusive_min = 0, max = 0.5)
        }
        # Check opt_hparams
        # Get parameters list and remove the 'param' parameter
        optim_params_function <- formals(optimizer)[-1]
        if (.has(opt_hparams)) {
            .check_lst_parameter(opt_hparams)
            .check_chr_within(names(opt_hparams),
                within = names(optim_params_function),
                msg = .conf("messages", ".check_opt_hparams")
            )
            optim_params_function <- utils::modifyList(
                x = optim_params_function, val = opt_hparams
            )
        }
        # Other pre-conditions:
        .check_int_parameter(lr_decay_epochs, min = 1)
        .check_num_parameter(lr_decay_rate, exclusive_min = 0, max = 1.0)
        .check_int_parameter(patience, min = 1)
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
        # Are there additional samples for validation?
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
        # Define the L-TAE architecture
        light_tae_model <- torch::nn_module(
            classname = "model_ltae",
            initialize = function(n_bands,
                                  n_labels,
                                  timeline,
                                  layers_spatial_encoder = c(32, 64, 128),
                                  n_heads = 16,
                                  n_neurons = c(256, 128),
                                  dropout_rate = 0.2,
                                  dim_input_decoder = 128,
                                  dim_layers_decoder = c(64, 32)) {
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
                dim_layers_decoder[length(dim_layers_decoder) + 1] <- n_labels
                # decode the tensor
                self$decoder <- .torch_multi_linear_batch_norm_relu(
                    dim_input_decoder,
                    dim_layers_decoder
                )
                # classify using softmax
                self$softmax <- torch::nn_softmax(dim = -1)
            },
            forward = function(input) {
                out <- self$spatial_encoder(input)
                out <- self$temporal_encoder(out)
                out <- self$decoder(out)
                out <- self$softmax(out)
                return(out)
            }
        )
        # torch 12.0 not working with Apple MPS
        if (torch::backends_mps_is_available())
            cpu_train <-  TRUE
        else
            cpu_train <-  FALSE
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
            suppressWarnings(torch::torch_set_num_threads(1))
            # Unserialize model
            torch_model[["model"]] <- .torch_unserialize_model(serialized_model)
            # Used to check values (below)
            n_input_pixels <- nrow(values)
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
            # Convert to tensor CPU
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
