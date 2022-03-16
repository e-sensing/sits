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
#' and code available on github at
#' https://github.com/VSainteuf/lightweight-temporal-attention-pytorch/blob/master/models/ltae.py.
#' If you use this method, please cite the original LTAE paper.
#'
#' @references Vivien Sainte Fare Garnot and Loic Landrieu,
#' "Lightweight Temporal Self-Attention
#' for Classifying Satellite Image Time Series", https://arxiv.org/abs/2007.00586
#'
#' @param samples           Time series with the training samples.
#' @param blocks            Number of 1D convolutional filters for
#'                          each block of three layers.
#' @param kernels           Size of the 1D convolutional kernels
#'                          for each layer of each block.
#' @param activation        Activation function for 1D convolution.
#'                          Valid values: {'relu', 'elu', 'selu', 'sigmoid'}.
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
#' # Build a machine learning model based on deep learning
#' rn_model <- sits_train(samples_modis_4bands, sits_LTAE(epochs = 75))
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
sits_LTAE <- function(samples = NULL,
                        blocks = c(64, 128, 128),
                        kernels = c(7, 5, 3),
                        activation = "relu",
                        optimizer = torch::optim_adam,
                        learning_rate = 0.001,
                        epochs = 300,
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

        .check_chr_within(
            x = activation,
            within = .config_get("dl_activation_methods"),
            discriminator = "one_of",
            msg = "invalid CNN activation method"
        )

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

        # Function to create torch datasets
        sits_dataset <- torch::dataset(
            name = "sits_dataset",
            initialize = function(dist_x, labels_y) {
                # create a torch tensor for x data
                self$x <- torch::torch_tensor(dist_x)
                # create a torch tensor for y data
                self$y <- torch::torch_tensor(labels_y)
            },
            .getitem = function(i){
                list(x = self$x[i, ], y = self$y[i])
            },
            .length = function(){
                self$y$size()[[1]]
            }
        )
        # create train and test datasets
        train_ds <- sits_dataset(train_x, train_y)
        test_ds  <- sits_dataset(test_x, test_y)

        # create the dataloaders for torch
        train_dl <- torch::dataloader(train_ds, batch_size = batch_size)
        test_dl  <- torch::dataloader(test_ds, batch_size = batch_size)


        torch::torch_manual_seed(sample.int(10^5, 1))

        #

        scaled_dot_product_attention <- torch::nn_module(
            classname = "scaled_dot_product_attention",

            initialize = function(temperature,
                                  attn_dropout = 0.1){

                self$temperature = temperature
                self$dropout = torch::nn_dropout(attn_dropout)
                self$softmax = torch::nn_softmax(dim = 2)

            },
            forward = function(q, k, v){
                attn = torch::torch.matmul(torch::torch_unsqueeze(q, dim = 1),
                                           torch::torch_transpose(k, 1, 2))

                attn = attn / self$temperature
                attn = self$softmax(attn)
                attn = self$dropout(attn)
                output = torch::torch_matmul(attn, v)
                return(list(output = output, attn = attn))
            }
        )
        multi_head_attention <- torch::nn_module(
            classname = "multi_head_attention",

            initialize = function(n_head, d_k, d_in){

                self$n_head = n_head
                self$d_k = d_k
                self$d_in = d_in

                self$Q = torch::nn_parameter(
                    torch::torch_zeros((n_head, d_k), requires_grad = TRUE)

                torch::nn_init_normal_(self$Q,
                                       mean = 0, std = sqrt(2.0 / (d_k)))


                self$.fc1_k = torch::nn_linear(d_in, n_head * d_k)
                torch::nn.init.normal_(self$fc1_k$weight,
                                       mean = 0,
                                       std = sqrt(2.0 / (d_k)))

                self.attention = scaled_dot_product_attention
                                         (temperature = exp(d_k, 0.5))

            };



        def forward(self, q, k, v):
            d_k, d_in, n_head = self.d_k, self.d_in, self.n_head
        sz_b, seq_len, _ = q.size()

        q = torch.stack([self.Q for _ in range(sz_b)], dim=1).view(-1, d_k)  # (n*b) x d_k

        k = self.fc1_k(v).view(sz_b, seq_len, n_head, d_k)
        k = k.permute(2, 0, 1, 3).contiguous().view(-1, seq_len, d_k)  # (n*b) x lk x dk

        v = torch.stack(v.split(v.shape[-1] // n_head, dim=-1)).view(n_head * sz_b, seq_len, -1)
        output, attn = self.attention(q, k, v)
        attn = attn.view(n_head, sz_b, 1, seq_len)
        attn = attn.squeeze(dim=2)

        output = output.view(n_head, sz_b, 1, d_in // n_head)
        output = output.squeeze(dim=2)

        return(output)
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
                data = train_dl,
                epochs = epochs,
                valid_data = test_dl,
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
