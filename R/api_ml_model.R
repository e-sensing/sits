#' @title Return machine learning model inside a closure
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param  ml_model  Closure that contains ML model and its environment
#' @return           ML model as specified by the original ML function
.ml_model <- function(ml_model) {
    if ("model" %in% ls(environment(ml_model))) {
        environment(ml_model)[["model"]]
    } else if ("torch_model" %in% ls(environment(ml_model))) {
        environment(ml_model)[["torch_model"]]
    } else {
        stop(.conf("messages", ".ml_model"))
    }
}
#' @title Return statistics of ML model inside a closure (old version)
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param  ml_model  Closure that contains ML model and its environment
#' @return           Data statistics contained in the model closure
.ml_stats_0 <- function(ml_model) {
    # Old stats variable
    environment(ml_model)[["stats"]]
}
#' @title Return statistics of ML model inside a closure (new version)
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param  ml_model  Closure that contains ML model and its environment
#' @return           Data statistics contained in the model closure
.ml_stats <- function(ml_model) {
    # New stats variable
    environment(ml_model)[["ml_stats"]]
}
#' @title Return samples of ML model inside a closure
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @param  ml_model  Closure that contains ML model and its environment
#' @return           Samples used for ML construction
.ml_samples <- function(ml_model) {
    environment(ml_model)[["samples"]]
}
#' @title Return class of ML model inside a closure
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @param  ml_model  Closure that contains ML model and its environment
#' @return           ML model class
.ml_class <- function(ml_model) {
    class(ml_model)[[1L]]
}
#' @title Return names of features used to train ML model
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @param  ml_model  Closure that contains ML model and its environment
#' @return           Features used to build the model
.ml_features_name <- function(ml_model) {
    # Get feature names from variable used in training
    names(environment(ml_model)[["train_samples"]])[-2L:0L]
}
#' @title Return names of bands used to train ML model
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @param  ml_model  Closure that contains ML model and its environment
#' @return           Bands used to build the model
.ml_bands <- function(ml_model) {
    .samples_bands(.ml_samples(ml_model))
}
#' @title Return labels of samples of used to train ML model
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @param  ml_model  Closure that contains ML model and its environment
#' @return           Sample labels used to build the model
.ml_labels <- function(ml_model) {
    .samples_labels(.ml_samples(ml_model))
}
#' @title Return codes of sample labels of used to train ML model
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @param  ml_model  Closure that contains ML model and its environment
#' @return           Codes of sample labels used to build the model
.ml_labels_code <- function(ml_model) {
    labels <- .ml_labels(ml_model)
    names(labels) <- seq_along(labels)
    labels
}
#' @title Clean GPU memory allocation
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @param  ml_model  Closure that contains ML model and its environment
#' @return           Called for side effects
.ml_gpu_clean <- function(ml_model) {
    # Clean torch allocations
    if (.torch_cuda_enabled() && .ml_is_torch_model(ml_model)) {
        torch::cuda_empty_cache()
    }
    return(invisible(NULL))
}

#' @title normalize the probability results
#' @keywords internal
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @param  values    Values to be normalized
#' @param  ml_model  Closure that contains ML model and its environment
#' @return           Normalized values
#'
.ml_normalize <- function(values, ml_model) {
    UseMethod(".ml_normalize", ml_model)
}
#' @export
#'
.ml_normalize.torch_model <- function(values, ml_model) {
    # Correct the default behaviour of softmax in torch models
    # Run softmax here instead of inside a torch model
    column_names <- colnames(values)
    values[is.na(values)] <- 0
    values <- softmax(values)
    colnames(values) <- column_names
    values
}
#' @export
#'
.ml_normalize.default <- function(values, ml_model) {
    values[is.na(values)] <- 0
    values
}
#' @title Update multicores for models that do internal multiprocessing
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @param  ml_model   Closure that contains ML model and its environment
#' @param  multicores Current multicores setting
#' @return            Updated multicores
#'
.ml_update_multicores <- function(ml_model, multicores){
    # xgboost model has internal multiprocessing
    if ("xgb_model" %in% .ml_class(ml_model))
        multicores <- 1L
    # torch in GPU has internal multiprocessing
    else if (.torch_gpu_classification() && .ml_is_torch_model(ml_model))
        multicores <- 1L

    return(multicores)
}
#' @title Is the ML model a torch model?
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @param  ml_model   Closure that contains ML model and its environment
#' @return            TRUE/FALSE
#'
.ml_is_torch_model <- function(ml_model) {
    inherits(ml_model, "torch_model")
}
