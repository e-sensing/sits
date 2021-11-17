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
