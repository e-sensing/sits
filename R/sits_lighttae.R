#' @title Train a model using Lightweight Temporal Self-Attention Encoder
#' @name sits_lighttae
#'
#' @author Charlotte Pelletier, \email{charlotte.pelletier@@univ-ubs.fr}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
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
#' @param samples            Time series with the training samples.
#' @param samples_validation Time series with the validation samples. if the
#'                           \code{samples_validation} parameter is provided,
#'                           the \code{validation_split}
#'                           parameter is ignored.
#' @param epochs             Number of iterations to train the model.
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
#' @return A fitted model to be passed to \code{\link[sits]{sits_classify}}
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#'
#' @export
sits_lighttae <- function(samples = NULL,
                          samples_validation = NULL,
                          epochs = 150,
                          batch_size = 128,
                          validation_split = 0.2,
                          optimizer = torchopt::optim_adamw,
                          opt_hparams = list(lr = 0.005,
                                             eps = 1e-08,
                                             weight_decay = 1e-06),
                          lr_decay_epochs = 50,
                          lr_decay_rate = 1,
                          patience = 20,
                          min_delta = 0.01,
                          verbose = FALSE) {


    # set caller to show in errors
    .check_set_caller("sits_lighttae")

    # function that returns torch model based on a sits sample data.table
    result_fun <- function(samples) {
        # verifies if torch package is installed
        if (!requireNamespace("torch", quietly = TRUE)) {
            stop("Please install package torch", call. = FALSE)
        }
        # verifies if torch package is installed
        if (!requireNamespace("luz", quietly = TRUE)) {
            stop("Please install package luz", call. = FALSE)
        }
        # preconditions
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

        # get parameters list and remove the 'param' parameter
        optim_params_function <- formals(optimizer)[-1]
        if (!is.null(names(opt_hparams))) {
            .check_chr_within(
                x = names(opt_hparams),
                within = names(optim_params_function)
            )
            optim_params_function <- utils::modifyList(optim_params_function,
                                                       opt_hparams)
        }

        # get the labels
        labels <- sits_labels(samples)
        # create a named vector with integers match the class labels
        n_labels <- length(labels)
        int_labels <- c(1:n_labels)
        names(int_labels) <- labels
        bands <- sits_bands(samples)

        # number of bands and number of samples
        n_bands <- length(sits_bands(samples))
        n_times <- nrow(sits_time_series(samples[1, ]))
        # timeline of samples
        timeline <- sits_timeline(samples)

        # data normalization
        stats <- .sits_ml_normalization_param(samples)
        train_samples <- .sits_distances(.sits_ml_normalize_data(samples, stats))

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

        # define the PSE-TAE model
        light_tae_model <- torch::nn_module(
            classname = "model_light_temporal_attention_encoder",
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
                in_channels = layers_spatial_encoder[length(layers_spatial_encoder)]
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
            forward = function(input){
                out <- self$spatial_encoder(input)
                out <- self$temporal_encoder(out)
                out <- self$decoder(out)
                out <- self$softmax(out)
                return(out)
            }
        )
        torch::torch_set_num_threads(1)
        # train the model using luz
        torch_model <-
            luz::setup(
                module = light_tae_model,
                loss = torch::nn_cross_entropy_loss(),
                metrics = list(luz::luz_metric_accuracy()),
                optimizer = optimizer
            ) %>%
            luz::set_hparams(
                n_bands  = n_bands,
                n_labels = n_labels,
                timeline = timeline
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
            if (!requireNamespace("torch", quietly = TRUE)) {
                stop("Please install package torch", call. = FALSE)
            }

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

        class(model_predict) <- c("torch_model", "sits_model",
                                  class(model_predict))

        return(model_predict)
    }

    result <- .sits_factory_function(samples, result_fun)
    return(result)
}
