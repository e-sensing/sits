#' @title  Generic interface for plotting a Keras model
#' @name   plot.keras_model
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a deep learning model
#'
#' @param  x             Object of class "keras_model"
#' @param  y             Ignored
#' @param  ...           Further specifications for \link{plot}.
#'
#' @examples
#' \donttest{
#' # Get a set of samples
#' samples_ndvi_evi <- sits_select(samples_mt_4bands, bands = c("NDVI", "EVI"))
#'
#' # train a deep learning model
#' dl_model <-  sits_train(samples_ndvi_evi, ml_method = sits_deeplearning(
#'                          layers           = c(512, 512, 512),
#'                          activation       = 'relu',
#'                          dropout_rates    = c(0.50, 0.40, 0.35),
#'                          epochs = 100,
#'                          batch_size = 128,
#'                          validation_split = 0.2))
#' plot(dl_model)
#' }
#' @export
plot.keras_model <- function(x, y, ...) {
    stopifnot(missing(y))
    graphics::plot(environment(x)$history)
}

#' @title Save a Keras model for later processing in sits
#' @name sits_keras_save
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a Keras model and saves it in two files.
#' One file is the keras model itself (saved as hdf5)
#' and the other is the R enviroment required for sits
#' to work with the model.
#'
#' @param  model           An R Keras model.
#' @param  hdffile         An hdf5 file where the keras model is to be saved.
#' @param  rdsfile         A rds file where the R environment is to be saved.
#'
#' @export
sits_keras_save <-  function(model,
                             hdffile = "./model_keras.h5",
                             rdsfile = "./model_keras.rds"){
    # retrieve the keras model from the sits model object
    model.keras <- environment(model)$model.keras
    # save the keras model in a HDF5 file
    keras::save_model_hdf5(model.keras, hdffile)
    # save the sits model in an RDS file
    saveRDS(model, rdsfile)
}

#' @title Load a Keras model for processing in sits
#' @name sits_keras_load
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a save Keras model
#' been saved in two files and loads it in memory for further processing.
#' One file is the keras model itself (saved as hdf5)
#' and the other is the R enviroment required for sits
#' to work with the model.
#'
#' @param  hdffile         An hdf5 file where the keras model is to be saved.
#' @param  rdsfile         A rds file where the R environment is to be saved.
#' @return An R Keras model trained by \code{\link[sits]{sits_deeplearning}}.
#'
#' @export
sits_keras_load <-  function(hdffile, rdsfile){
    # loads the keras model from an hdf5 file
    model.keras <- keras::load_model_hdf5(hdffile)
    # loads the sits model object from an RDS file
    dl_model <- readRDS(rdsfile)
    # load the Keras model in the sits model environment
    environment(dl_model)$model.keras <- environment(model.keras)$x
    # returns the dl_model
    return(dl_model)
}

#' @title Diagnostic information about a Keras deep learning model
#' @name sits_keras_diagnostics
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description After the Keras deeplearning model is compiled and fit, this
#'              function provides access to the history plot
#'              and the evaluation results.
#'
#' @param dl_model  A valid keras model.
#'
#' @return This function returns NULL. It only prints the model diagnostics.
#'
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso (provided by EMBRAPA)
#' data(cerrado_2classes)
#'  # obtain a DL model
#' dl_model <- sits_train(cerrado_2classes,
#'      sits_deeplearning(layers = c(512, 512), dropout_rates = c(0.45, 0.25),
#'                        epochs = 100))
#' # run the keras diagnostics
#' sits_keras_diagnostics(dl_model)
#' }
#' @export
sits_keras_diagnostics <- function(dl_model) {
    if (purrr::is_null(environment(dl_model)$model.keras)) {
        message("Please configure a keras model before running this function")
        return(FALSE)
    }

    test_eval <- keras::evaluate(environment(dl_model)$model.keras,
                                 environment(dl_model)$test.x,
                                 environment(dl_model)$test.y, verbose = 0)
    message("Estimated loss and accuracy based on test data")
    message(paste0("Estimated accuracy: ", round(test_eval["accuracy"], digits = 3),
                   " estimated loss: ", round(test_eval["loss"], digits = 3)))
    return(test_eval)
}

#' @title Adjust keras prediction for the binary classification case
#' @name .sits_keras_binary_class
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description For binary classification, the prediction function produces only
#' one column (the probability of label 1). For compatibility with the
#' code in the sits package, this function includes a second column
#' in the prediction values to match the results of multi-class classification.
#'
#' @param prediction_DT     Predicted values from the keras model
#'                          for the binary classification case (one column)
#' @return                  Data.table with an additional column for multi-class
#'                          compatibility
#'
.sits_keras_binary_class <- function(prediction_DT) {
    # binary classification prediction has one column (the second label)
    # create a second column for compatibility with the rest of the code
    prediction_DT <- prediction_DT[, V0 := 1.0 - V1]
    # swap columns
    prediction_DT <- prediction_DT[,c("V0", "V1")]
    return(prediction_DT)
}

#' @title Prepare data for keras model training
#' @name .sits_keras_prepare_data
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Given a training set organised as a SITS tibble, prepare
#' the data for keras training, providing test and training data.
#'
#' @param data              Time series with the training samples.
#' @param validation_split  Number between 0 and 1.
#'                          Fraction of the training data for validation.
#'
#' @param int_labels        Integer vector named with the labels.
#' @param n_bands           Number of bands.
#' @param n_times           Number of time steps.
#' @return                  List with four elements (training data (X and Y)
#'                          and test data (X and Y))
#'
.sits_keras_prepare_data <- function(data,
                                     validation_split,
                                     int_labels,
                                     n_bands,
                                     n_times){
    # pre-condition
    assertthat::assert_that(validation_split > 0.0 && validation_split < 0.5,
                     msg = ".sits_dl_prepare_data - invalid validation split")

    # data normalization
    stats <- .sits_normalization_param(data)
    train_data_DT <- .sits_distances(.sits_normalize_data(data, stats))

    # is the train data correct?
    assertthat::assert_that("reference" %in% names(train_data_DT),
               msg = "sits_deeplearning: input data does not contain distances")

    # split the data into training and validation data sets
    # create partitions different splits of the input data
    test_data_DT <- .sits_sample_distances(train_data_DT,
                                           frac = validation_split)

    # remove the lines used for validation
    train_data_DT <- train_data_DT[!test_data_DT, on = "original_row"]

    # shuffle the data
    train_data_DT <- train_data_DT[sample(nrow(train_data_DT),
                                          nrow(train_data_DT)),]
    test_data_DT  <- test_data_DT[sample(nrow(test_data_DT),
                                         nrow(test_data_DT)),]

    n_samples_train <- nrow(train_data_DT)
    n_samples_test  <- nrow(test_data_DT)

    # organize data for model training
    train.x <- array(data = as.matrix(train_data_DT[,3:ncol(train_data_DT)]),
                     dim = c(n_samples_train, n_times, n_bands))

    train.y <- unname(int_labels[as.vector(train_data_DT$reference)]) - 1

    # create the test data for keras
    test.x <- array(data = as.matrix(test_data_DT[,3:ncol(test_data_DT)]),
                    dim = c(n_samples_test, n_times, n_bands))

    test.y <- unname(int_labels[as.vector(test_data_DT$reference)]) - 1

    return(list(train.x = train.x,
                train.y = train.y,
                test.x = test.x,
                test.y = test.y))
}


