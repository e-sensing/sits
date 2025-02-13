#' @title Preconditions for multi-layer perceptron
#' @name .pre_sits_mlp
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param samples            Time series with the training samples.
#' @param epochs             Number of iterations to train the model.
#' @param batch_size         Number of samples per gradient update.
#' @param layers             Vector with number of hidden nodes in each layer.
#' @param dropout_rates      Vector with the dropout rates (0,1)
#'                           for each layer.
#' @param patience           Number of epochs without improvements until
#'                           training stops.
#' @param min_delta	         Minimum improvement in loss function
#'                           to reset the patience counter.
#' @param verbose            Verbosity mode (TRUE/FALSE). Default is FALSE.
#' @keywords internal
#' @noRd
#' @return                   Called for side effects.
#'
.pre_sits_mlp <- function(samples, epochs, batch_size,
                          layers, dropout_rates,
                          patience, min_delta, verbose) {
    # Pre-conditions:
    .check_samples_train(samples)
    .check_int_parameter(epochs)
    .check_int_parameter(batch_size)
    .check_int_parameter(layers)
    .check_num_parameter(dropout_rates, min = 0, max = 1,
                         len_min = length(layers), len_max = length(layers)
    )
    .check_that(length(layers) == length(dropout_rates),
                msg = .conf("messages", "sits_mlp_layers_dropout")
    )
    .check_int_parameter(patience)
    .check_num_parameter(min_delta, min = 0)
    .check_lgl_parameter(verbose)

    return(invisible(NULL))
}
#' @title Preconditions for temporal convolutional neural network models
#' @name .pre_sits_tempcnn
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param samples            Time series with the training samples.
#' @param cnn_layers         Number of 1D convolutional filters per layer
#' @param cnn_kernels        Size of the 1D convolutional kernels.
#' @param cnn_dropout_rates  Dropout rates for 1D convolutional filters.
#' @param dense_layer_nodes  Number of nodes in the dense layer.
#' @param dense_layer_dropout_rate  Dropout rate (0,1) for the dense layer.
#' @param epochs             Number of iterations to train the model.
#' @param batch_size         Number of samples per gradient update.
#' @param lr_decay_epochs    Number of epochs to reduce learning rate.
#' @param lr_decay_rate      Decay factor for reducing learning rate.
#' @param patience           Number of epochs without improvements until
#'                           training stops.
#' @param min_delta	         Minimum improvement in loss function
#'                           to reset the patience counter.
#' @param verbose            Verbosity mode (TRUE/FALSE). Default is FALSE.
#'
#' @keywords internal
#' @noRd
#'
#' @return                   Called for side effects.
#'
.pre_sits_tempcnn <- function(samples, cnn_layers, cnn_kernels,
                              cnn_dropout_rates, dense_layer_nodes,
                              dense_layer_dropout_rate, epochs, batch_size,
                              lr_decay_epochs, lr_decay_rate,
                              patience, min_delta, verbose) {
    # Pre-conditions:
    .check_samples_train(samples)
    .check_int_parameter(cnn_layers, len_max = 2^31 - 1)
    .check_int_parameter(cnn_kernels,
                         len_min = length(cnn_layers),
                         len_max = length(cnn_layers))
    .check_num_parameter(cnn_dropout_rates, min = 0, max = 1,
                         len_min = length(cnn_layers),
                         len_max = length(cnn_layers))
    .check_int_parameter(dense_layer_nodes, len_max = 1)
    .check_num_parameter(dense_layer_dropout_rate,
                         min = 0, max = 1, len_max = 1)
    .check_int_parameter(epochs)
    .check_int_parameter(batch_size)
    .check_int_parameter(lr_decay_epochs)
    .check_num_parameter(lr_decay_rate, exclusive_min = 0, max = 1)
    .check_int_parameter(patience)
    .check_num_parameter(min_delta, min = 0)
    .check_lgl_parameter(verbose)

    return(invisible(NULL))
}
#' @title Preconditions for Lightweight Temporal Self-Attention Encoder
#'        and Temporal Self-Attention Encoder.
#' @name .pre_sits_lighttae
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param samples            Time series with the training samples
#'                           (tibble of class "sits").
#' @param epochs             Number of iterations to train the model
#'                           (integer, min = 1, max = 20000).
#' @param batch_size         Number of samples per gradient update
#'                           (integer, min = 16L, max = 2048L)
#' @param lr_decay_epochs    Number of epochs to reduce learning rate.
#' @param lr_decay_rate      Decay factor for reducing learning rate.
#' @param patience           Number of epochs without improvements until
#'                           training stops.
#' @param min_delta	         Minimum improvement in loss function
#'                           to reset the patience counter.
#' @param verbose            Verbosity mode (TRUE/FALSE). Default is FALSE.
#'
#' @keywords internal
#' @noRd
#' @return Called for side effects.
#'
.pre_sits_lighttae <- function(samples, epochs, batch_size,
                               lr_decay_epochs, lr_decay_rate,
                               patience, min_delta, verbose) {
    # Pre-conditions:
    .check_samples_train(samples)
    .check_int_parameter(epochs, min = 1L, max = 20000L)
    .check_int_parameter(batch_size, min = 16L, max = 2048L)
    .check_int_parameter(lr_decay_epochs, min = 1)
    .check_num_parameter(lr_decay_rate, exclusive_min = 0, max = 1.0)
    .check_int_parameter(patience, min = 1)
    .check_num_parameter(min_delta, min = 0)
    .check_lgl_parameter(verbose)

    return(invisible(NULL))
}
