
#' @title Train a model using Lightweight Temporal Self-Attention
#' @name sits_LTAE
#'
#' @author Charlotte Pelletier, \email{charlotte.pelletier@@univ-ubs.fr}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Implementation of the Light Temporal Attention Encoder (L-TAE)
#' for satellite image time series classification.
#'
#' This function is based on the paper by Vivien Garnot referenced below
#' and code available on github (https://github.com/VSainteuf/pytorch-psetae).
#' If you use this method, please cite the original LTAE paper.
#'
#' @references Vivien Sainte Fare Garnot and Loic Landrieu,
#' "Lightweight Temporal Self-Attention
#' for Classifying Satellite Image Time Series", https://arxiv.org/abs/2007.00586
#'
#' @param samples           Time series with the training samples.
#' @param in_channels       Number of channels of the input embeddings
#' @param n_head            Number of attention heads
#' @param d_k               Dimension of the key and query vectors
#' @param n_neurons         Defines the dimensions of the successive feature spaces of the MLP that processes
#'                          the concatenated outputs of the attention heads
#' @param dropout           dropout
#' @param T                 Period to use for the positional encodin
#' @param len_max_seq       Maximum sequence length, used to pre-compute the positional encoding table
#' @param positions         List of temporal positions to use instead of position in the sequence
#' @param return_att        If true, the module returns the attention masks along with the embeddings (default False)

#' @param mlp_layers        Number of nodes in the multi-layer-perceptron.
#' @param mlp_activation    Names of 2D activation functions for the MLP.
#'                          Valid values: {'relu', 'elu', 'selu', 'sigmoid'}.
#' @param mlp_dropout_rates Dropout rates (0,1) for each layer in the MLP.
#' @param optimizer         Function with a pointer to the optimizer function
#'                          (default is optimization_adam()).
#'                          Options: optimizer_adadelta(), optimizer_adagrad(),
#'                          optimizer_adam(), optimizer_adamax(),
#'                          optimizer_nadam(), optimizer_rmsprop(),
#'                          optimizer_sgd().
#' @param epochs            Number of iterations to train the model.
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
#' # Build a machine learning model
#' ltae_model <- sits_train(samples_modis_4bands, sits_LTAE(epochs = 75))
#' # Plot the model
#' plot(ltae_model)
#'
#' # get a point and classify the point with the ml_model
#' point <- sits_select(point_mt_6bands, bands = c("NDVI", "EVI", "NIR", "MIR"))
#' class <- sits_classify(point, ltae_model)
#'
#' plot(class, bands = c("NDVI", "EVI"))
#' }
#' @export
sits_LTAE <- function(samples = NULL,
                      cnn_layers = c(64, 64, 64),
                      cnn_kernels = c(5, 5, 5),
                      cnn_activation = "relu",
                      cnn_L2_rate = 1e-06,
                      cnn_dropout_rates = c(0.50, 0.50, 0.50),
                      mlp_layers = c(256),
                      mlp_activation = "relu",
                      mlp_dropout_rates = c(0.50),
                      optimizer = keras::optimizer_adam(lr = 0.001),
                      epochs = 150,
                      batch_size = 128,
                      validation_split = 0.2,
                      verbose = 0) {


    # # function that returns keras model based on a sits sample data.table
    # result_fun <- function(data) {
    #
    #     # verifies if keras package is installed
    #     if (!requireNamespace("keras", quietly = TRUE)) {
    #         stop(paste("keras required for this function to work.",
    #                    "Please install it."), call. = FALSE)
    #     }
    #
    #     # pre-conditions
    #     valid_activations <- c("relu", "elu", "selu", "sigmoid")
    #
    #     .check_that(
    #         x = length(cnn_layers) == length(cnn_kernels),
    #         msg = "sits_tempCNN: 1D layers must match 1D kernel sizes"
    #     )
    #
    #     .check_that(
    #         x = length(cnn_layers) == length(cnn_dropout_rates),
    #         msg = "sits_tempCNN: 1D layers must match 1D dropout rates"
    #     )
    #
    #     .check_that(
    #         x = length(mlp_layers) == length(mlp_dropout_rates),
    #         msg = "sits_tempCNN: 2D units must match 2D dropout rates"
    #     )
    #
    #     .check_chr_within(
    #         x = cnn_activation,
    #         within = valid_activations,
    #         discriminator = "any_of",
    #         msg = "sits_tempCNN: invalid CNN activation method"
    #     )
    #
    #     .check_chr_within(
    #         x = mlp_activation,
    #         within = valid_activations,
    #         discriminator = "any_of",
    #         msg = "sits_tempCNN: invalid node activation method"
    #     )
    #
    #     # get the labels of the data
    #     labels <- sits_labels(data)
    #     # create a named vector with integers match the class labels
    #     n_labels <- length(labels)
    #     int_labels <- c(1:n_labels)
    #     names(int_labels) <- labels
    #
    #     # number of bands and number of samples
    #     n_bands <- length(sits_bands(data))
    #     n_times <- nrow(sits_time_series(data[1, ]))
    #
    #     # data normalization
    #     stats <- .sits_ml_normalization_param(data)
    #     train_data <- .sits_distances(.sits_ml_normalize_data(data, stats))
    #
    #     # split the data into training and validation data sets
    #     # create partitions different splits of the input data
    #     test_data <- .sits_distances_sample(train_data,
    #                                         frac = validation_split
    #     )
    #     # remove the lines used for validation
    #     train_data <- train_data[!test_data, on = "original_row"]
    #
    #     n_samples_train <- nrow(train_data)
    #     n_samples_test <- nrow(test_data)
    #
    #     # shuffle the data
    #     train_data <- train_data[sample(
    #         nrow(train_data),
    #         nrow(train_data)
    #     ), ]
    #     test_data <- test_data[sample(
    #         nrow(test_data),
    #         nrow(test_data)
    #     ), ]
    #
    #     # organize data for model training
    #     train_x <- array(
    #         data = as.matrix(train_data[, 3:ncol(train_data)]),
    #         dim = c(n_samples_train, n_times, n_bands)
    #     )
    #     train_y <- unname(int_labels[as.vector(train_data$reference)]) - 1
    #
    #     # create the test data for keras
    #     test_x <- array(
    #         data = as.matrix(test_data[, 3:ncol(test_data)]),
    #         dim = c(n_samples_test, n_times, n_bands)
    #     )
    #     test_y <- unname(int_labels[as.vector(test_data$reference)]) - 1
    #
    #     ## -- Positional encoding
    #     # def positional_encoding(positions, d_model, T=10000):
    #     #
    #     #     if isinstance(positions, int):
    #     #     positions = np.arange(positions)
    #     # else:
    #     #     positions = np.array(positions)
    #     #
    #     # def _get_angles(pos, i, d_model):
    #     #     angle_rates = 1 / np.power(T, (2 * (i//2)) / np.float32(d_model))
    #     # return pos * angle_rates
    #     #
    #     # depths = np.arange(d_model)
    #     #
    #     # angle_rads = _get_angles(positions[:, np.newaxis],
    #     #                          depths[np.newaxis, :],
    #     #                          d_model)
    #     #
    #     # # apply sin to even indices in the array; 2i
    #     # angle_rads[:, 0::2] = np.sin(angle_rads[:, 0::2])
    #     #
    #     # # apply cos to odd indices in the array; 2i+1
    #     # angle_rads[:, 1::2] = np.cos(angle_rads[:, 1::2])
    #     #
    #     # pos_encoding = angle_rads[np.newaxis, ...]
    #     # -----
    #     # return tf.cast(pos_encoding, dtype=tf.float32)
    #
    #     # build the model step by step
    #     #
    #     self.positions = len_max_seq + 1
    #     self.d_model = 128
    #     self.T <- 1000
    #
    #     # create the input_tensor for 1D convolution
    #     input_tensor <- keras::layer_input(shape = c(n_times, n_bands))
    #     output_tensor <- input_tensor
    #
    #     output_tensor <- keras::layer_layer_normalization(output_tensor)
    #
    #
    #     # build a set 1D convolution layers
    #     # Add a Convolution1D
    #     output_tensor <- keras::layer_conv_1d(
    #         output_tensor,
    #         filters = self.d_model,
    #         kernel_size = 1,
    #         kernel_initializer = "initializer_lecun_uniform",
    #         padding = "valid"
    #     )
    #     # batch normalization
    #     output_tensor <- keras::layer_layer_normalization(output_tensor)

        # pos_encoding = self.position_enc[:, :seq_len, :]
        # if self.positions is None:
        #     pos_encoding = self.position_enc[:, 1:seq_len+1, :]
        #
        # enc_output = x + pos_encoding

        # Activation
    #     output_tensor <- keras::layer_multi_head_attention(output_tensor,
    #                      num_heads = 4,
    #                      key_dim =  32,
    #                      droput = 0.2)
    #     # Apply layer dropout
    #
    #     # reshape a tensor into a 2D shapew
    #     output_tensor <- keras::layer_flatten(output_tensor)
    #
    #     # build the 2D nodes
    #     for (i in seq_len(length(mlp_layers))) {
    #         output_tensor <- keras::layer_dense(
    #             output_tensor,
    #             units = mlp_layers[[i]]
    #         )
    #
    #         # batch normalization
    #         output_tensor <- keras::layer_batch_normalization(output_tensor)
    #         # Activation
    #         output_tensor <- keras::layer_activation(output_tensor,
    #                                                  activation = mlp_activation)
    #         # dropout
    #         output_tensor  <- keras::layer_dropout(output_tensor,
    #                                                rate = mlp_dropout_rates[[i]])
    #     }
    #
    #     # create the final tensor
    #     model_loss <- "categorical_crossentropy"
    #     if (n_labels == 2) {
    #         output_tensor <- keras::layer_dense(
    #             output_tensor,
    #             units = 1,
    #             activation = "sigmoid"
    #         )
    #         model_loss <- "binary_crossentropy"
    #     }
    #     else {
    #         output_tensor <- keras::layer_dense(
    #             output_tensor,
    #             units = n_labels,
    #             activation = "softmax"
    #         )
    #         # keras requires categorical data to be put in a matrix
    #         train_y <- keras::to_categorical(train_y, n_labels)
    #         test_y <- keras::to_categorical(test_y, n_labels)
    #     }
    #     # create the model
    #     model_keras <- keras::keras_model(input_tensor, output_tensor)
    #     # compile the model
    #     model_keras %>% keras::compile(
    #         loss = model_loss,
    #         optimizer = optimizer,
    #         metrics = "accuracy"
    #     )
    #
    #     # fit the model
    #     history <- model_keras %>% keras::fit(
    #         train_x, train_y,
    #         epochs = epochs, batch_size = batch_size,
    #         validation_data = list(test_x, test_y),
    #         verbose = verbose, view_metrics = "auto"
    #     )
    #
    #     # import model to R
    #     R_model_keras <- keras::serialize_model(model_keras)
    #
    #     # construct model predict closure function and returns
    #     model_predict <- function(values) {
    #
    #         # verifies if keras package is installed
    #         if (!requireNamespace("keras", quietly = TRUE)) {
    #             stop(paste("keras required for this function to work.",
    #                        "Please install it."), call. = FALSE)
    #         }
    #
    #         # restore model keras
    #         model_keras <- keras::unserialize_model(R_model_keras)
    #
    #         # transform input (data.table) into a 3D tensor
    #         # (remove first two columns)
    #         n_samples <- nrow(values)
    #         n_timesteps <- nrow(sits_time_series(data[1, ]))
    #         n_bands <- length(sits_bands(data))
    #         values_x <- array(
    #             data = as.matrix(values[, 3:ncol(values)]),
    #             dim = c(n_samples, n_timesteps, n_bands)
    #         )
    #         # retrieve the prediction probabilities
    #         prediction <- data.table::as.data.table(stats::predict(
    #             model_keras,
    #             values_x
    #         ))
    #         # If binary classification,
    #         # adjust the prediction values to match binary classification
    #         if (n_labels == 2) {
    #             prediction <- .sits_keras_binary_class(prediction)
    #         }
    #
    #         # adjust the names of the columns of the probs
    #         colnames(prediction) <- labels
    #
    #         return(prediction)
    #     }
    #
    #     class(model_predict) <- c("keras_model", "sits_model",
    #                               class(model_predict))
    #     return(model_predict)
    # }
    #
    # result <- .sits_factory_function(samples, result_fun)
    # return(result)
}
