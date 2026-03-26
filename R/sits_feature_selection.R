#' @title Select the most representative bands for spatiotemporal
#' classification
#' @name sits_feature_selection
#'
#' @author Felipe Carvalho, \email{lipecaso@@gmail.com}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Use recursive feature elimination for selecting the
#' most representative bands for each iteration. For each iteration,
#' one band is removed if the accuracy loss is lower than the threshold.
#' The feature selection stops in two ways: (1) when the accuracy loss
#' reaches the threshold value; (2) or when it reaches the maximum
#' iteration number.
#'
#' @param samples    Time series with the training samples
#'                   (tibble of class "sits").
#' @param ml_method  Machine learning method (default = RandomForest)
#' @param filter_fn  Smoothing filter to be applied - optional
#' @param impute_fn  Imputation function to remove NA.
#' @param folds      Number of partitions to create.
#' @param metric     Importance metric. Use \code{"mda"} for mean decrease
#'                   accuracy and \code{"mdg"} for mean decrease gini.
#' @param n_iter     Number of iterations for feature selection.
#' @param acc_loss   Number of maximum accuracy loss accepted (0 to 1).
#' @param multicores Number of cores to be used for cross-validation.
#'                   (integer, min = 1, max = folds).
#'
#' @return           Samples with selected bands
#'
#' @examples
#' if (sits_run_examples()) {
#'  samples_selected <- sits_feature_selection(
#'      samples    = samples_l8_rondonia_2bands,
#'      ml_method  = sits_rfor(),
#'      n_iter     = 1,
#'      acc_loss   = 0.01,
#'      multicores = 2L
#'  )
#' }
#'
sits_feature_selection <- function(samples,
                                   ml_method = sits_rfor(),
                                   impute_fn = impute_linear(),
                                   filter_fn = NULL,
                                   folds = 5L,
                                   metric = "mda",
                                   n_iter = 5,
                                   acc_loss = 0.02,
                                   multicores = 2L) {
    # Pre-conditions
    .check_samples(samples)
    .check_niter(n_iter, samples)
    .check_metric(metric)
    .check_num_parameter(acc_loss, min = 0, max = 1)
    .check_model(substitute(ml_method), "sits_rfor")
    # Apply feature selection
    samples <- .feature_selection(
        samples    = samples,
        ml_method  = ml_method,
        impute_fn  = impute_fn,
        filter_fn  = filter_fn,
        folds      = folds,
        metric     = metric,
        n_iter     = n_iter,
        acc_loss   = acc_loss,
        multicores = multicores
    )
    # Return selected samples
    return(samples)
}
