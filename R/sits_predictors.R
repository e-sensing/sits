#' @title  Obtain predictors for time series samples
#' @name sits_predictors
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Predictors are X-Y values required for machine learning
#' algorithms, organized as a data table where each row corresponds
#' to a training sample. The first two columns of the predictors table
#' are categorical (\code{label_id} and \code{label}). The other columns are
#' the values of each band and time, organized first by band and then by time.
#'
#' @param  samples     Time series in sits format (tibble of class "sits")
#'
#' @return The predictors for the sample: a data.frame with one row per sample.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # Include a new machine learning function (multiple linear regression)
#'     # function that returns mlr model based on a sits sample tibble
#'
#'     sits_mlr <- function(samples = NULL, formula = sits_formula_linear(),
#'                          n_weights = 20000, maxit = 2000) {
#'
#'     # create a training function
#'         train_fun <- function(samples) {
#'             # Data normalization
#'             ml_stats <- sits_stats(samples)
#'             train_samples <- sits_predictors(samples)
#'             train_samples <- sits_pred_normalize(
#'                 pred = train_samples,
#'                 stats = ml_stats
#'             )
#'             formula <- formula(train_samples[, -1])
#'             # call method and return the trained model
#'             result_mlr <- nnet::multinom(
#'                 formula = formula,
#'                 data = train_samples,
#'                 maxit = maxit,
#'                 MaxNWts = n_weights,
#'                 trace = FALSE,
#'                 na.action = stats::na.fail
#'             )
#'
#'             # construct model predict closure function and returns
#'             predict_fun <- function(values) {
#'                 # retrieve the prediction (values and probs)
#'                 prediction <- tibble::as_tibble(
#'                     stats::predict(result_mlr,
#'                         newdata = values,
#'                         type = "probs"
#'                     )
#'                 )
#'                 return(prediction)
#'             }
#'             class(predict_fun) <- c("sits_model", class(predict_fun))
#'             return(predict_fun)
#'         }
#'         result <- sits_factory_function(samples, train_fun)
#'         return(result)
#'     }
#'     # create an mlr model using a set of samples
#'     mlr_model <- sits_train(samples_modis_ndvi, sits_mlr)
#'     # classify a point
#'     point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
#'     point_class <- sits_classify(point_ndvi, mlr_model, multicores = 1)
#'     plot(point_class)
#' }
#'
#' @export
sits_predictors <- function(samples) {
    .check_set_caller("sits_predictors")
    .check_na_null_parameter(samples)
    samples <- .check_samples_ts(samples)
    pred <- .predictors(samples)
    return(pred)
}

#' @title  Obtain numerical values of predictors for time series samples
#' @name sits_pred_features
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Predictors are X-Y values required for machine learning
#' algorithms, organized as a data table where each row corresponds
#' to a training sample. The first two columns of the predictors table
#' are categorical ("label_id" and "label"). The other columns are
#' the values of each band and time, organized first by band and then by time.
#' This function returns the numeric values associated to each sample.
#'
#' @param  pred    X-Y predictors: a data.frame with one row per sample.
#'
#' @return The Y predictors for the sample: data.frame with one row per sample.
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#' @examples
#' if (sits_run_examples()) {
#'     pred <- sits_predictors(samples_modis_ndvi)
#'     features <- sits_pred_features(pred)
#' }
#' @export
sits_pred_features <- function(pred) {
    features <- .pred_features(pred)
    return(features)
}
#' @title  Obtain categorical id and predictor labels for time series samples
#' @name sits_pred_references
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Predictors are X-Y values required for machine learning
#' algorithms, organized as a data table where each row corresponds
#' to a training sample. The first two columns of the predictors table
#' are categorical ("label_id" and "label"). The other columns are
#' the values of each band and time, organized first by band and then by time.
#' This function returns the numeric values associated to each sample.
#'
#' @param  pred    X-Y predictors: a data.frame with one row per sample.
#'
#' @return A character vector with labels associated to training samples.
#'
#' @examples
#' if (sits_run_examples()) {
#'     pred <- sits_predictors(samples_modis_ndvi)
#'     ref <- sits_pred_references(pred)
#' }
#' @export
sits_pred_references <- function(pred) {
    ref <- .pred_references(pred)
    return(ref)
}
#' @title  Normalize predictor values
#' @name sits_pred_normalize
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Most machine learning algorithms require data to be
#' normalized. This applies to the "SVM" method and to all deep learning ones.
#' To normalize the predictors, it is required that the statistics per band
#' for each sample have been obtained by the "sits_stats" function.
#'
#' @param  pred    X-Y predictors: a data.frame with one row per sample.
#' @param  stats   Values of time series for Q02 and Q98 of the data
#'                 (list of numeric values with two elements)
#'
#' @return A data.frame with normalized predictor values
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#' @examples
#' if (sits_run_examples()) {
#'     stats <- sits_stats(samples_modis_ndvi)
#'     pred <- sits_predictors(samples_modis_ndvi)
#'     pred_norm <- sits_pred_normalize(pred, stats)
#' }
#' @export
sits_pred_normalize <- function(pred, stats) {
    .check_set_caller("sits_pred_normalize")
    .check_na_null_parameter(pred)
    .check_na_null_parameter(stats)
    pred <- .pred_normalize(pred, stats)
    return(pred)
}
#' @title  Obtain a fraction of the predictors data frame
#' @name sits_pred_sample
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Many machine learning algorithms (especially deep learning)
#' use part of the original samples as test data to adjust its hyperparameters
#' and to find an optimal point of convergence using gradient descent.
#' This function extracts a fraction of the predictors to serve as test values
#' for the deep learning algorithm.
#'
#' @param  pred    X-Y predictors: a data.frame with one row per sample.
#' @param  frac    Fraction of the X-Y predictors to be extracted
#'
#' @return A data.frame with the chosen fraction of the X-Y predictors.
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#' @examples
#' if (sits_run_examples()) {
#'     pred <- sits_predictors(samples_modis_ndvi)
#'     pred_frac <- sits_pred_sample(pred, frac = 0.5)
#' }
#' @export
sits_pred_sample <- function(pred, frac) {
    sample <- .pred_sample(pred, frac)
    return(sample)
}
#' @title  Obtain statistics for all sample bands
#' @name sits_stats
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Most machine learning algorithms require data to be
#' normalized. This applies to the "SVM" method and to all deep learning ones.
#' To normalize the predictors, it is necessary to extract the statistics
#' of each band of the samples. This function computes the 2% and 98% quantiles
#' of the distribution of each band of the samples. This values are used as
#' minimum and maximum values in the normalization operation performed by
#' the sits_pred_normalize() function.
#'
#' @param  samples    Time series samples uses as training data.
#'
#' @return A list with the 2% and 98% quantiles for each band of the
#' training data.
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#' @examples
#' if (sits_run_examples()) {
#'     stats <- sits_stats(samples_modis_ndvi)
#' }
#' @export
sits_stats <- function(samples) {
    stats <- .samples_stats(samples)
    return(stats)
}
