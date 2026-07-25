#' @title Train a model using Lightweight Temporal Self-Attention Encoder
#' @name sits_lighttae
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Charlotte Pelletier, \email{charlotte.pelletier@@univ-ubs.fr}
#'
#' @description Implementation of Light Temporal Attention Encoder (L-TAE)
#' for satellite image time series. This is a lightweight version of the
#' temporal attention encoder proposed by Garnot et al. For the TAE,
#' please see \code{\link[sits]{sits_tae}}.
#'
#' TAE is a simplified version of the well-known self-attention architecture
#' which is used in large language models.
#' Its modified self-attention scheme that uses the input
#' embeddings as values. TAE defines a single master query for each sequence,
#' computed from the temporal average of the queries. This master query is compared
#' to the sequence of keys to produce a single attention mask
#' used to weight the temporal mean of values into a single feature vector.
#'
#' The lightweight version of TAE further simplifies the TAE model.
#' It defines master query of each head as a model parameter instead
#' of the results of a linear layer, as is done it TAE.
#' The authors argue that such simplification reduces the number of parameters,
#' while the lack of flexibility is compensated by the larger number of available heads.
#'
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
#' \url{https://github.com/VSainteuf/lightweight-temporal-attention-pytorch}
#' If you use this method, please cite the original TAE and the LTAE paper.
#'
#' We also used the code made available by Maja Schneider in her work with
#' Marco Körner referenced below and available at
#' \url{https://github.com/maja601/RC2020-psetae}.
#'
#' @references
#' Vivien Garnot, Loic Landrieu, Sebastien Giordano, and Nesrine Chehata,
#' "Satellite Image Time Series Classification with Pixel-Set Encoders
#' and Temporal Self-Attention",
#' 2020 Conference on Computer Vision and Pattern Recognition.
#' pages 12322-12331.
#' \doi{10.1109/CVPR42600.2020.01234}
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
#' \doi{10.5281/zenodo.4835356}
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
#' @param seed               Seed for random values.
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
                          seed = NULL,
                          verbose = FALSE) {
    # set caller for error msg
    .check_set_caller("sits_lighttae")
    # Verifies if 'torch' and 'luz' packages is installed
    .check_require_packages(c("torch", "luz"))
    # documentation mode? verbose is FALSE
    verbose <- .message_verbose(verbose)
    # Function that trains a torch model based on samples
    train_fun <- function(samples, embedding_dim = NULL) {
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
            samples = samples,
            epochs = epochs,
            batch_size = batch_size,
            lr_decay_epochs = lr_decay_epochs,
            lr_decay_rate = lr_decay_rate,
            patience = patience, min_delta = min_delta,
            embedding_dim = embedding_dim,
            verbose = verbose
        )
        # Other pre-conditions:
        .check_int_parameter(seed, allow_null = TRUE)

        # Get optimizer hyperparameters
        optim_params_function <- .torch_optim_params(optimizer, opt_hparams)
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

        # obtain training samples
        train_samples    <- .predictors(samples)
        # [n, n_times*n_bands]
        feats    <- .pred_features_normalize(train_samples, stats = ml_stats)
        .pred_features(train_samples) <- feats
        # Post condition: is predictor data valid?
        .check_predictors(pred = train_samples, samples = samples)
        # Are there samples for validation?
        if (.has(samples_validation)) {
            .check_samples_validation(
                samples_validation = samples_validation,
                labels = labels,
                timeline = timeline,
                bands = bands
            )
            # Test samples are extracted from validation data
            test_samples    <- .predictors(samples_validation)
            # [n, n_times*n_bands]
            feats    <- .pred_features_normalize(test_samples, stats = ml_stats)
            .pred_features(test_samples) <- feats
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
        # number of samples
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
        # Create a torch seed (we define a new variable to allow users
        # to access this seed number from the model environment)
        torch_seed <- .torch_set_seed(seed)

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
                if (!.has(embedding_dim)) {
                    dim_layers_decoder[length(dim_layers_decoder) + 1L] <-
                        n_labels
                    self$decoder <- .torch_multi_linear_batch_norm_relu(
                        dim_input_decoder,
                        dim_layers_decoder
                    )
                    self$embedding_layer <- NULL
                } else {
                    # Intermediate layers with BatchNorm + ReLU
                    self$decoder <- .torch_multi_linear_batch_norm_relu(
                        dim_input_decoder,
                        dim_layers_decoder
                    )
                    # Final plain linear layer for embeddings
                    # (no BatchNorm or ReLU — preserves full
                    # representational capacity)
                    last_hidden <-
                        dim_layers_decoder[length(dim_layers_decoder)]
                    self$embedding_layer <- torch::nn_linear(
                        last_hidden, embedding_dim
                    )
                }
            },
            forward = function(input) {
                out <- input |>
                    self$spatial_encoder() |>
                    self$temporal_encoder() |>
                    self$decoder()
                if (!is.null(self$embedding_layer)) {
                    out <- self$embedding_layer(out)
                }
                out
                # softmax is done externally
                # by .ml_normalize.torch_model function
            }
        )

        # return encoder model
        if (.has(embedding_dim)) {
            return(light_tae_model(
                n_bands  = n_bands,
                n_labels = n_labels,
                timeline = timeline
            ))
        }
        # train with CPU or GPU?
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
        # remove data used for training
        force(rm(
            train_samples, test_samples,
            train_y, train_x, test_y, test_x
        ))
        gc()
        # Serialize model
        serialized_model <- .torch_serialize_model(torch_model$model)
        # Function that predicts labels of input values
        predict_fun <- function(values) {
            # Verifies if torch package is installed
            .check_require_packages("torch")

            # Unserialize model
            torch_model$model <- .torch_unserialize_model(
                model = torch_model$model,
                raw = serialized_model
            )
            # GPU or CPU classification?
            use_gpu <- (
                is.list(values) &&
                    !is.null(values[["callback"]]) &&
                    .torch_gpu_classification()
            )
            # Classify!
            if (use_gpu) {
                # Predict!
                values <- .torch_predict_chunks(
                    torch_model = torch_model,
                    dataset = values[["dataset"]],
                    callback = values[["callback"]]
                )
                # Prepare results
                values <- unlist(values)
            } else {
                # Transform input into a 3D tensor
                n_samples <- nrow(values)
                n_times <- .samples_ntimes(samples)
                n_bands <- length(bands)
                # Performs data normalization
                values <- .pred_features_normalize(values, stats = ml_stats)
                # Represent matrix values as array
                C_as_array_inplace(values, c(n_samples, n_times, n_bands))
                # CPU classification
                values <- stats::predict(
                    object = torch_model,
                    newdata = values,
                    accelerator = luz::accelerator(cpu = TRUE)
                )
                # Convert from tensor to array
                values <- torch::as_array(values)
                # Update the columns names to labels
                colnames(values) <- labels
            }
            # Return!
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
