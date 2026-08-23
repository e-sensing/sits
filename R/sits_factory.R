#' @title Create a closure for calling functions with and without data
#' @name sits_factory_function
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function implements the factory method pattern.
#' Its creates a generic interface to closures in R so that the functions
#' in the sits package can be called in two different ways:
#' 1. Called directly, passing input data and parameters.
#' 2. Called as second-order values (parameters of another function).
#'    In the second case, the call will pass no data values
#'    and only pass the parameters for execution
#'
#' The factory pattern is used in many situations in the sits package,
#' to allow different alternatives
#' for filtering, pattern creation, training, and cross-validation
#'
#' Please see the chapter "Technical Annex" in the sits book for details.
#'
#' @param data      Input data.
#' @param fun       Function that performs calculation on the input data.
#' @return          A closure that encapsulates the function applied to data.
#'
#' @examples
#' # example code
#' if (sits_run_examples()) {
#'     # Include a new machine learning function (naive bayes)
#'
#'     sits_naive_bayes <- function(samples = NULL){
#'         train_fun <- function(samples) {
#'             # Data normalization
#'             ml_stats <- sits_stats(samples)
#'             train_samples <- sits_predictors(samples)
#'             train_samples <- sits_pred_normalize(
#'                 pred = train_samples,
#'                 stats = ml_stats
#'             )
#'             # call method and return the trained model
#'             nb_model <- e1071::naiveBayes(
#'                 x = sits_pred_features(train_samples),
#'                 y = as.factor(sits_pred_references(train_samples))
#'             )
#'
#'             # construct model predict closure function and returns
#'             predict_fun <- function(values) {
#'                 # retrieve the prediction (values and probs)
#'                 prediction <- tibble::as_tibble(
#'                     stats::predict(nb_model,
#'                         newdata = values,
#'                         type = "raw"
#'                     )
#'                 )
#'                 return(prediction)
#'             }
#'             class(predict_fun) <- c("sits_model", "naiveBayes", class(predict_fun))
#'             return(predict_fun)
#'         }
#'         result <- sits_factory_function(samples, train_fun)
#'         return(result)
#'     }
#'     # create an naiveBayes model using a set of samples
#'     nb_model <- sits_train(samples_modis_ndvi, sits_naive_bayes)
#'     # classify a point
#'     point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
#'     point_class <- sits_classify(point_ndvi, nb_model, multicores = 1)
#'     plot(point_class)
#' }
#' @export
sits_factory_function <- function(data, fun) {
    .check_set_caller("sits_factory_function")
    # if no data is given, we prepare a
    # function to be called as a parameter of other functions
    if (.has_not(data)) {
        fun
    } else {
        # ...otherwise compute the result on the input data
        fun(data)
    }
}
