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
#' \dontrun{
#' # Retrieve the set of samples for the Mato Grosso (provided by EMBRAPA)
#' data(cerrado_2classes)
#' # obtain a DL model
#' dl_model <- sits_train(
#'     cerrado_2classes,
#'     sits_deeplearning(
#'         layers = c(512, 512), dropout_rates = c(0.45, 0.25),
#'         epochs = 100
#'     )
#' )
#' # run the keras diagnostics
#' sits_keras_diagnostics(dl_model)
#' }
#' @export
sits_keras_diagnostics <- function(dl_model) {
    if (purrr::is_null(environment(dl_model)$model_keras)) {
        message("Please configure a keras model before running this function")
        return(FALSE)
    }

    test_eval <- keras::evaluate(environment(dl_model)$model_keras,
        environment(dl_model)$test_x,
        environment(dl_model)$test_y,
        verbose = 0
    )
    message("Estimated loss and accuracy based on test data")
    message(paste0(
        "Estimated accuracy: ", round(test_eval["accuracy"], digits = 3),
        " estimated loss: ", round(test_eval["loss"], digits = 3)
    ))
    return(test_eval)
}

#' @title Adjust keras prediction for the binary classification case
#' @name .sits_keras_binary_class
#' @keywords internal
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description For binary classification, the prediction function produces only
#' one column (the probability of label 1). For compatibility with the
#' code in the sits package, this function includes a second column
#' in the prediction values to match the results of multi-class classification.
#'
#' @param prediction        Predicted values from the keras model
#'                          for the binary classification case
#'                          (data.table with one column)
#' @return                  Data.table with an additional column for multi-class
#'                          compatibility
#'
.sits_keras_binary_class <- function(prediction) {
    # binary classification prediction has one column (the second label)
    # create a second column for compatibility with the rest of the code
    prediction <- prediction[, V0 := 1.0 - V1]
    # swap columns
    prediction <- prediction[, c("V0", "V1")]
    return(prediction)
}
