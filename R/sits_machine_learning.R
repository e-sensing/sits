#' @title Train sits classification models
#' @name sits_train
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Alexandre Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#'
#' @description Given a tibble with a set of distance measures,
#'    returns trained models. Currenly, sits supports the following models:
#' 'svm' (see \code{\link[sits]{sits_svm}}),
#' random forests (see \code{\link[sits]{sits_rfor}}),
#' linear discriminant analysis (see \code{\link[sits]{sits_lda}}),
#' quadratic discriminant analysis (see \code{\link[sits]{sits_qda}}),
#' multinomial logit (see \code{\link[sits]{sits_mlr}}) and its variants
#' 'lasso' (see \code{\link[sits]{sits_mlr}}) and
#' 'ridge' (see \code{\link[sits]{sits_mlr}}),
#' extreme gradient boosting (see \code{\link[sits]{sits_xgboost}}),
#' and different deep learning functions, including multi-layer perceptrons
#' (see \code{\link[sits]{sits_deeplearning}}, 1D convolutional neural networks
#' \code{\link[sits]{sits_FCN}}, mixed 1D and MLP networks
#' \code{\link[sits]{sits_TempCNN}}, a 1D version of ResNet
#' \code{\link[sits]{sits_ResNet}}), and a combined LSTM-FCN model
#' \code{\link[sits]{sits_LSTM_FCN}}.
#'
#' @param  data             Time series with the training samples.
#' @param  ml_method        Machine learning method.
#' @return                  Model fitted to input data
#'                          to be passed to \code{\link[sits]{sits_classify}}
#'
#' @examples
#' # Retrieve the set of samples for Mato Grosso (provided by EMBRAPA)
#' # fit a training model (RFOR model)
#' samples <- sits_select(samples_mt_6bands, bands = c("NDVI"))
#' ml_model <- sits_train(samples, sits_rfor(num_trees = 100))
#' # get a point and classify the point with the ml_model
#' class <- sits_classify(point_ndvi, ml_model)
#' @export
sits_train <- function(data, ml_method = sits_svm()) {
    # backward compatibility
    data <- .sits_tibble_rename(data)

    # is the input data a valid sits tibble?
    assertthat::assert_that("label" %in% names(data),
        msg = "sits_train: input data does not contain a valid sits tibble"
    )

    # is the train method a function?
    assertthat::assert_that(class(ml_method) == "function",
        msg = "sits_train: ml_method is not a valid function"
    )

    # compute the training method by the given data
    result <- ml_method(data)

    # return a valid machine learning method
    return(result)
}

#' @title Train a sits classification model using linear discriminant analysis
#' @name sits_lda
#'
#' @author Alexandre Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function receives a tibble with a set of attributes X
#' for each observation Y. These attributes are the values of the time series
#' for each band. The method performs a linear discriminant analysis (lda)
#' to obtain a predictive model.
#' This function is a front-end to the "lda" method in the "MASS" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param data             Time series with the training samples.
#' @param formula          A symbolic description of the model to be fit.
#'                         (default: sits_formula_logref).
#' @param ...              Other parameters to be passed to MASS::lda function.
#' @return                 Model fitted to input data
#'                         to be passed to \code{\link[sits]{sits_classify}}
#'
#' @examples
#' \dontrun{
#'
#' # Retrieve the set of samples for Mato Grosso region (provided by EMBRAPA)
#' samples_2bands <- sits_select(samples_mt_4bands, bands = c("NDVI", "EVI"))
#'
#' # Build a machine learning model based on deep learning
#' ml_model <- sits_train(samples_2bands, sits_lda())
#'
#' # get a point and classify the point with the ml_model
#' point.tb <- sits_select(point_mt_6bands, bands = c("NDVI", "EVI"))
#' class.tb <- sits_classify(point.tb, ml_model)
#' plot(class.tb, bands = c("NDVI", "EVI"))
#' }
#' @export
sits_lda <- function(data = NULL, formula = sits_formula_logref(), ...) {
    # backward compatibility
    data <- .sits_tibble_rename(data)
    # verifies if MASS package is installed
    if (!requireNamespace("MASS", quietly = TRUE)) {
        stop("MASS required for this function to work.
             Please install it.", call. = FALSE)
    }
    # function that returns MASS::lda model based on a sits sample tibble
    result_fun <- function(data) {

        # data normalization
        stats <- .sits_normalization_param(data)
        train_data <- .sits_distances(.sits_normalize_data(data, stats))

        # is the input data the result of a TWDTW matching function?
        assertthat::assert_that("reference" %in% names(train_data),
            msg = "sits_lda: input data does not contain distance"
        )

        # if parameter formula is a function
        # Call it passing as argument the input data sample.
        # The function must return a valid formula.
        if (class(formula) == "function") {
              formula <- formula(train_data)
          }

        # call MASS::lda method and return the trained lda model
        result_lda <- MASS::lda(
            formula = formula,
            data = train_data, ...,
            na.action = stats::na.fail
        )

        # construct model predict closure function and returns
        model_predict <- function(values) {
            # retrieve the prediction (values and probs)
            preds <- stats::predict(result_lda, newdata = values)
            # return probabilities
            prediction <- data.table::as.data.table(preds$posterior)

            return(prediction)
        }
        class(model_predict) <- c("lda_model", class(model_predict))
        return(model_predict)
    }

    result <- .sits_factory_function(data, result_fun)
    return(result)
}

#' @title Train a classification model using quadratic discriminant analysis
#' @name sits_qda
#'
#' @author Alexandre Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function receives a tibble with a set of attributes X
#' for each observation Y. These attributes are the values of the time series
#' for each band. The function performs a quadratic discriminant analysis (qda)
#' to obtain a predictive model.
#' This function is a front-end to the "qda" method in the "MASS" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param data      Time series with the training samples.
#' @param formula   Symbolic description of the model to be fit.
#'                  (default: sits_formula_logref).
#' @param ...       Other parameters to be passed to MASS::qda function.
#' @return          Model fitted to input data
#'                  (to be passed to \code{\link[sits]{sits_classify}})
#'
#' @examples
#' # Retrieve the set of samples for Mato Grosso region (provided by EMBRAPA)
#' # Select the NDVI band
#' samples_mt_ndvi <- sits_select(samples_mt_4bands, bands = "NDVI")
#' # Train a QDA model
#' qda_model <- sits_train(samples_mt_ndvi, sits_qda())
#' # Classify a point
#' class <- sits_classify(point_ndvi, qda_model)
#' @export
sits_qda <- function(data = NULL, formula = sits_formula_logref(), ...) {
    # backward compatibility
    data <- .sits_tibble_rename(data)
    # verifies if MASS package is installed
    if (!requireNamespace("MASS", quietly = TRUE)) {
        stop("MASS required for this function to work.
             Please install it.", call. = FALSE)
    }
    # function that returns MASS::qda model based on a sits sample tibble
    result_fun <- function(data) {

        # data normalization
        stats <- .sits_normalization_param(data)
        train_data <- .sits_distances(.sits_normalize_data(data, stats))

        # If parameter formula is a function
        # Call it passing as argument the input data sample.
        # The function must return a valid formula.
        if (class(formula) == "function") {
              formula <- formula(train_data)
          }

        # call MASS::qda method and return the trained lda model
        result_qda <- MASS::qda(
            formula = formula,
            data = train_data, ...,
            na.action = stats::na.fail
        )

        # construct model predict closure function and returns
        model_predict <- function(values) {
            # retrieve the prediction (values and probs)
            preds <- stats::predict(result_qda, newdata = values)
            # return probabilities
            prediction <- data.table::as.data.table(preds$posterior)

            return(prediction)
        }
        class(model_predict) <- c("qda_model", class(model_predict))
        return(model_predict)
    }
    result <- .sits_factory_function(data, result_fun)
    return(result)
}

#' @title Train a sits classification model using multinomial log-linear
#' @name sits_mlr
#'
#' @author Alexandre Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Use multinomial log-linear (mlr) fitting model to classify data.
#' This function receives a tibble with a set of attributes X
#' for each observation Y. These attributes are the values of the time series
#' for each band.
#' This function is a front-end to the "multinom" method in the "nnet" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param data             Time series with the training samples.
#' @param formula          Symbolic description of the model to be fit.
#'                         (default: sits_formula_logref).
#' @param n_weights        Maximum number of weights
#'                         (should be proportional to size of input data).
#' @param maxit            Maximum number of iterations (default 300).
#' @param ...              Other parameters to be passed to nnet::multinom.
#' @return                 Model fitted to input data
#'                        (to be passed to \code{\link[sits]{sits_classify}})
#' @examples
#' \dontrun{
#' # Retrieve the set of samples for  Mato Grosso region (provided by EMBRAPA)
#' samples_2bands <- sits_select(samples_mt_4bands, bands = c("NDVI", "EVI"))
#'
#' # Build a machine learning model based on deep learning
#' ml_model <- sits_train(samples_2bands, sits_mlr())
#'
#' # get a point and classify the point with the ml_model
#' point.tb <- sits_select(point_mt_6bands, bands = c("NDVI", "EVI"))
#' class.tb <- sits_classify(point.tb, ml_model)
#' plot(class.tb, bands = c("NDVI", "EVI"))
#' }
#' @export
sits_mlr <- function(data = NULL, formula = sits_formula_linear(),
                     n_weights = 20000, maxit = 2000, ...) {

    # verifies if nnet package is installed
    if (!requireNamespace("nnet", quietly = TRUE)) {
        stop("nnet required for this function to work.
             Please install it.", call. = FALSE)
    }
    # backward compatibility
    data <- .sits_tibble_rename(data)
    # function that returns nnet::multinom model based on a sits sample tibble
    result_fun <- function(data) {
        # data normalization
        stats <- .sits_normalization_param(data)
        train_data <- .sits_distances(.sits_normalize_data(data, stats))

        # if parameter formula is a function
        # call it passing as argument the input data sample.
        # The function must return a valid formula.
        if (class(formula) == "function") {
              formula <- formula(train_data)
          }

        # call nnet::multinom method and return the trained multinom model
        result_mlr <- nnet::multinom(
            formula = formula,
            data = train_data,
            maxit = maxit,
            MaxNWts = n_weights,
            trace = FALSE, ...,
            na.action = stats::na.fail
        )

        # construct model predict closure function and returns
        model_predict <- function(values) {
            # return probabilities
            prediction <- data.table::as.data.table(
                stats::predict(result_mlr, newdata = values, type = "probs")
            )

            return(prediction)
        }
        class(model_predict) <- c("mlr_model", class(model_predict))
        return(model_predict)
    }

    result <- .sits_factory_function(data, result_fun)
    return(result)
}
#' @title Train a sits classifiction model using fast random forest algorithm
#' @name sits_ranger
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Alexandre Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Use Fast Random Forest algorithm to classify data.
#' This function is a front-end to the "ranger" method in the "ranger" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param data         Time series with the training samples.
#' @param num_trees    Number of trees to grow. This should not be set
#'                      to too small a number,
#'                      to ensure that every input row gets predicted
#'                      at least a few times. (default: 2000).
#' @param importance   Variable importance mode, one of 'none',
#'                      'impurity', 'impurity_corrected', 'permutation'.
#'                     The 'impurity' measure is the Gini index.
#' @param ...          Other \code{\link[ranger]{ranger}}  parameters
#' @return             Model fitted to input data
#'                     (to be passed to \code{\link[sits]{sits_classify}})
#' @examples
#' # Retrieve the set of samples for Mato Grosso  (provided by EMBRAPA)
#' samples_ndvi <- sits_select(samples_mt_6bands, bands = c("NDVI"))
#'
#' # Build a machine learning model based on deep learning
#' ml_model <- sits_train(samples_ndvi, sits_ranger(num_trees = 100))
#'
#' # get a point and classify the point with the ml_model
#' class <- sits_classify(point_ndvi, ml_model)
#' @export
sits_ranger <- function(data = NULL,
                        num_trees = 2000,
                        importance = "impurity", ...) {
    # backward compatibility
    data <- .sits_tibble_rename(data)

    # verifies if ranger package is installed
    if (!requireNamespace("ranger", quietly = TRUE)) {
        stop("ranger required for this function to work.
             Please install it.", call. = FALSE)
    }
    # function that returns a randomForest model based on a sits sample tibble
    result_fun <- function(data) {
        valid_importance <- c("none", "impurity", "permutation")

        # is the input data consistent?
        assertthat::assert_that(importance %in% valid_importance,
            msg = "sits_ranger: invalid variable importance value"
        )

        # get the labels of the data
        labels <- sits_labels(data)$label
        assertthat::assert_that(length(labels) > 0,
            msg = "sits_ranger: invalid data - bad labels"
        )
        n_labels <- length(labels)

        # create a named vector with integers match the class labels
        int_labels <- c(1:n_labels)
        names(int_labels) <- labels

        # calculate the distances
        train_data <- .sits_distances(data)

        # obtain a valid formula for training
        formula <- sits_formula_linear()(train_data)

        # call `ranger::ranger` method and return the trained model
        result_ranger <- ranger::ranger(
            formula = formula,
            data = train_data[, 2:ncol(train_data)],
            probability = TRUE, importance = importance,
            num.trees = num_trees, min.node.size = 1, ...
        )

        # construct model predict closure function and return it
        model_predict <- function(values) {
            # retrieve the prediction results
            preds <- stats::predict(result_ranger,
                data = values,
                type = "response"
            )
            # return the prediction values and their probabilities
            prediction <- data.table::as.data.table(preds$predictions)

            return(prediction)
        }
        class(model_predict) <- c("ranger_model", class(model_predict))
        return(model_predict)
    }

    result <- .sits_factory_function(data, result_fun)
    return(result)
}
#' @title Train a SITS classifiction model using random forest algorithm
#' @name sits_rfor
#'
#' @author Alexandre Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Use Random Forest algorithm to classify data.
#' This function is a front-end to the "randomForest" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param data             time series with the training samples
#' @param num_trees        number of trees to grow.
#'                         This should not be set to too small a number,
#'                         to ensure that every input row gets predicted
#'                         at least a few times (default: 2000).
#' @param nodesize         minimum size of terminal nodes
#'                         (default 1 for classification)
#' @param ...              other parameters to be passed
#'                         to `randomForest::randomForest` function
#' @return                 model fitted to input data
#'                         (to be passed to \code{\link[sits]{sits_classify}})
#' @examples
#' # Retrieve the set of samples for the Mato Grosso region
#' samples_MT_ndvi <- sits_select(samples_mt_4bands, bands = "NDVI")
#' # Build a random forest model
#' rfor_model <- sits_train(samples_MT_ndvi, sits_rfor(num_trees = 300))
#' # get a point with a 16 year time series
#' data(point_ndvi)
#' # classify the point
#' class.tb <- sits_classify(point_ndvi, rfor_model)
#' @export
sits_rfor <- function(data = NULL, num_trees = 2000, nodesize = 1, ...) {

    # verifies if ranger package is installed
    if (!requireNamespace("randomForest", quietly = TRUE)) {
        stop("randomForest required for this function to work.
             Please install it.", call. = FALSE)
    }
    # function that returns `randomForest::randomForest` model
    result_fun <- function(data) {
        train_data <- .sits_distances(data)

        # call `randomForest::randomForest` method and return the trained model
        reference <- train_data[, reference]
        result_rfor <- randomForest::randomForest(
            x = train_data[, 3:ncol(train_data)],
            y = as.factor(reference),
            data = NULL,
            ntree = num_trees,
            nodesize = 1,
            norm.votes = FALSE, ...,
            na.action = stats::na.fail
        )

        # construct model predict enclosure function and returns
        model_predict <- function(values) {
            return(stats::predict(result_rfor,
                newdata = values,
                type = "prob"
            ))
        }
        return(model_predict)
    }

    result <- .sits_factory_function(data, result_fun)
    return(result)
}
#' @title Train a sits classification model using a support vector machine
#' @name sits_svm
#'
#' @author Alexandre Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function receives a tibble with a set of attributes X
#' for each observation Y. These attributes are the values of the time series
#' for each band.
#' The SVM algorithm is used for multiclass-classification.
#' For this purpose, it uses the "one-against-one" approach,
#' in which k(k-1)/2 binary classifiers are trained;
#' the appropriate class is found by a voting scheme.
#' This function is a front-end to the "svm" method in the "e1071" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param data             Time series with the training samples.
#' @param formula          Symbolic description of the model to be fit.
#'                         (default: sits_formula_logref).
#' @param scale            Logical vector indicating the variables to be scaled.
#' @param cachesize        Cache memory in MB (default = 1000).
#' @param kernel           Kernel used in training and predicting.
#'                         options: "linear", "polynomial", "radial", "sigmoid"
#'                         (default: "radial").
#' @param degree           Exponential of polynomial type kernel (default: 3).
#' @param coef0            Parameter needed for kernels of type polynomial
#'                         and sigmoid (default: 0).
#' @param cost             Cost of constraints violation.
#' @param tolerance        Tolerance of termination criterion (default: 0.001).
#' @param epsilon          Epsilon in the insensitive-loss function
#'                         (default: 0.1).
#' @param cross            Number of cross validation folds applied
#'                         to assess the quality of the model.
#' @param ...              Other parameters to be passed to e1071::svm function.
#' @return                 Model fitted to input data
#'                         (to be passed to \code{\link[sits]{sits_classify}})
#' @examples
#' \dontrun{
#' # Retrieve the set of samples for  Mato Grosso  (provided by EMBRAPA)
#' samples_2bands <- sits_select(samples_mt_4bands, bands = c("NDVI", "EVI"))
#'
#' # Build a machine learning model based on deep learning
#' ml_model <- sits_train(samples_2bands, sits_svm())
#'
#' # get a point and classify the point with the ml_model
#' point.tb <- sits_select(point_mt_6bands, bands = c("NDVI", "EVI"))
#' class.tb <- sits_classify(point.tb, ml_model)
#' plot(class.tb, bands = c("NDVI", "EVI"))
#' }
#' @export
sits_svm <- function(data = NULL, formula = sits_formula_logref(),
                     scale = FALSE, cachesize = 1000,
                     kernel = "radial", degree = 3, coef0 = 0,
                     cost = 10, tolerance = 0.001,
                     epsilon = 0.1, cross = 0, ...) {
    # verifies if e1071 package is installed
    if (!requireNamespace("e1071", quietly = TRUE)) {
        stop("e1071 required for this function to work.
             Please install it.", call. = FALSE)
    }
    # backward compatibility
    data <- .sits_tibble_rename(data)
    # function that returns e1071::svm model based on a sits sample tibble
    result_fun <- function(data) {

        # data normalization
        stats <- .sits_normalization_param(data)
        train_data <- .sits_distances(.sits_normalize_data(data, stats))

        # The function must return a valid formula.
        if (class(formula) == "function") {
              formula <- formula(train_data)
          }

        # call e1071::svm method and return the trained svm model
        result_svm <- e1071::svm(
            formula = formula, data = train_data,
            scale = scale, kernel = kernel,
            degree = degree, cost = cost, coef0 = coef0,
            cachesize = cachesize, tolerance = tolerance,
            epsilon = epsilon, cross = cross,
            probability = TRUE, ...,
            na.action = stats::na.fail
        )

        # construct model predict closure function and returns
        model_predict <- function(values) {
            # get the prediction
            preds <- stats::predict(result_svm,
                newdata = values,
                probability = TRUE
            )
            # retrieve the predicted probabilities
            prediction <- data.table::as.data.table(attr(
                preds,
                "probabilities"
            ))
            # reorder the matrix according to the column names
            data.table::setcolorder(
                prediction,
                sort(colnames(prediction))
            )

            return(prediction)
        }
        class(model_predict) <- c("svm_model", class(model_predict))
        return(model_predict)
    }
    result <- .sits_factory_function(data, result_fun)
    return(result)
}


#' @title Train a model with an extreme gradient boosting machine
#' @name sits_xgboost
#'
#' @author Alexandre Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function uses the extreme gradient boosting algorithm.
#' Boosting iteratively adds basis functions in a greedy fashion
#' so that each new basis function further reduces the selected loss function.
#' This function is a front-end to the methods in the "xgboost" package.
#' Please refer to the documentation in that package for more details.
#'
#' @references          Tianqi Chen, Carlos Guestrin,
#'                      "XGBoost : Reliable Large-scale Tree Boosting System",
#'                      SIG KDD 2016.
#'
#' @param data             Time series with the training samples.
#' @param learning_rate    Learning rate: scale the contribution
#'                         of each tree by a factor of 0 < lr < 1
#'                         when it is added to the current approximation.
#'                         Used to prevent overfitting. Default: 0.15
#' @param min_split_loss   Minimum loss reduction to make a further
#'                         partition of a leaf.  Default: 1.
#' @param max_depth        Maximum depth of a tree.
#'                         Increasing this value makes the model more complex
#'                         and more likely to overfit. Default: 5.
#' @param min_child_weight If the leaf node has a minimum sum of instance
#'                         weights lower than min_child_weight,
#'                         tree splitting stops. The larger min_child_weight is,
#'                         the more conservative the algorithm is. Default: 1.
#' @param max_delta_step   Maximum delta step we allow each leaf output to be.
#'                         If the value is set to 0, there is no constraint.
#'                         If it is set to a positive value, it can help making
#'                         the update step more conservative. Default: 1.
#' @param subsample        Percentage of samples supplied to a tree.
#'                         Default: 0.8.
#' @param nfold            Number of the subsamples for the cross-validation.
#' @param nrounds          Number of rounds to iterate the cross-validation
#'                         (default: 100)
#' @param early_stopping_rounds Training with a validation set will stop
#'                         if the performance doesn't improve for k rounds.
#' @param verbose          Print information on statistics during the process
#' @return                 Model fitted to input data
#'                         (to be passed to \code{\link[sits]{sits_classify}})
#'
#' @examples
#' \dontrun{
#' # Retrieve the set of samples for Mato Grosso (provided by EMBRAPA)
#'
#' # Build a machine learning model based on xgboost
#' xgb_model <- sits_train(samples_mt_4bands, sits_xgboost(nrounds = 10))
#'
#' # get a point and classify the point with the ml_model
#' point.tb <- sits_select(point_mt_6bands,
#'     bands = c("NDVI", "EVI", "NIR", "MIR")
#' )
#' class.tb <- sits_classify(point.tb, xgb_model)
#' plot(class.tb, bands = c("NDVI", "EVI"))
#' }
#' @export
#'
sits_xgboost <- function(data = NULL,
                         learning_rate = 0.15,
                         min_split_loss = 1,
                         max_depth = 5,
                         min_child_weight = 1,
                         max_delta_step = 1,
                         subsample = 0.8,
                         nfold = 5,
                         nrounds = 100,
                         early_stopping_rounds = 20,
                         verbose = TRUE) {
    # verifies if xgboost package is installed
    if (!requireNamespace("xgboost", quietly = TRUE)) {
        stop("xgboost required for this function to work.
             Please install it.", call. = FALSE)
    }
    # function that returns xgb model
    result_fun <- function(data) {

        # get the labels of the data
        labels <- sits_labels(data)$label
        assertthat::assert_that(length(labels) > 0,
            msg = "sits_rfor: invalid data - bad labels"
        )
        n_labels <- length(labels)

        # create a named vector with integers match the class labels
        int_labels <- c(1:n_labels)
        names(int_labels) <- labels

        # get the training data
        train_data <- .sits_distances(data)

        # reference labels for each sample expressed as numerical values
        references <- unname(int_labels[as.vector(train_data$reference)]) - 1

        # define the parameters of the model
        params <- list(
            booster = "gbtree",
            objective = "multi:softprob",
            eval_metric = "mlogloss",
            eta = learning_rate,
            gamma = min_split_loss,
            max_depth = max_depth,
            min_child_weight = min_child_weight,
            max_delta_step = max_delta_step,
            subsample = subsample
        )

        # run the cross-validation
        xgbcv <- xgboost::xgb.cv(
            params = params,
            data = as.matrix(train_data[, 3:length(train_data)]),
            label = references,
            num_class = length(labels),
            nrounds = nrounds,
            nfold = nfold,
            early_stopping_rounds = early_stopping_rounds,
            print_every_n = 10,
            verbose = verbose,
            maximize = FALSE
        )

        # get the best iteration of the model based on the CV
        nrounds_best <- xgbcv$best_iteration

        # define the model
        model_xgb <- xgboost::xgboost(
            data = as.matrix(train_data[, 3:length(train_data)]),
            label = references,
            num_class = length(labels),
            params = params,
            nrounds = nrounds_best,
            verbose = FALSE
        )

        ntreelimit <- model_xgb$best_ntreelimit

        # construct model predict closure function and returns
        model_predict <- function(values) {
            # transform input  into a matrix (remove first two columns)
            # retrieve the prediction probabilities
            prediction <- data.table::as.data.table(
                stats::predict(model_xgb, data.matrix(values[, - (1:2)]),
                    ntreelimit = ntreelimit,
                    reshape = TRUE
                )
            )
            # adjust the names of the columns of the probs
            colnames(prediction) <- labels
            # retrieve the prediction results
            return(prediction)
        }
        class(model_predict) <- c("xgb_model", class(model_predict))
        return(model_predict)
    }

    result <- .sits_factory_function(data, result_fun)
    return(result)
}

#' @title Define a loglinear formula for classification models
#' @name sits_formula_logref
#'
#' @author Alexandre Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description A function to be used as a symbolic description
#' of some fitting models such as svm and random forest.
#' This function tells the models to do a log transformation of the inputs.
#' The `predictors_index` parameter informs
#' the positions of `tb` fields corresponding to formula independent variables.
#' If no value is given, the default is NULL,
#' a value indicating that all fields will be used as predictors.
#'
#' @param predictors_index  Index of the valid columns
#'                          to compose formula (default: -2:0).
#' @return A function that computes a valid formula.
#'
#' @export
sits_formula_logref <- function(predictors_index = -2:0) {
    # store configuration information about model formula
    sits_env$model_formula <- "log"

    # this function returns a formula like
    # 'factor(reference~log(f1)+log(f2)+...+log(fn)' where f1, f2, ..., fn are
    # the predictor fields given by the predictor index.
    result_fun <- function(tb) {
        assertthat::assert_that(NROW(tb) > 0,
            msg = "sits_formula_logref - invalid data"
        )
        n_rows_tb <- NROW(tb)
        # if no predictors_index are given, assume all tb's fields are used
        if (purrr::is_null(predictors_index)) {
              predictors_index <- 1:n_rows_tb
          }

        # get predictors names
        categories <- names(tb)[c(predictors_index)]

        # compute formula result
        result_for <- stats::as.formula(paste0(
            "factor(reference)~",
            paste0(paste0("log(`", categories, "`)"),
                collapse = "+"
            )
        ))
        return(result_for)
    }
    return(result_fun)
}

#' @title Define a linear formula for classification models
#' @name sits_formula_linear
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Alexandre Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Provides a symbolic description of a fitting model.
#' Tells the model to do a linear transformation of the input values.
#' The `predictors_index` parameter informs the positions of fields
#' corresponding to formula independent variables.
#' If no value is given,  that all fields will be used as predictors.
#'
#' @param predictors_index  Index of the valid columns
#'                    whose names are used to compose formula (default: -2:0).
#' @return A function that computes a valid formula.
#'
#' @export
sits_formula_linear <- function(predictors_index = -2:0) {
    # store configuration information about model formula
    sits_env$model_formula <- "linear"

    # this function returns a formula
    # 'factor(reference~log(f1)+log(f2)+...+log(fn)' where f1, f2, ..., fn are
    #  the predictor fields.
    result_fun <- function(tb) {
        assertthat::assert_that(NROW(tb) > 0,
            msg = "sits_formula_logref - invalid data"
        )
        n_rows_tb <- NROW(tb)
        # if no predictors_index are given, assume that all fields are used
        if (purrr::is_null(predictors_index)) {
              predictors_index <- 1:n_rows_tb
          }

        # get predictors names
        categories <- names(tb)[c(predictors_index)]

        # compute formula result
        result_for <- stats::as.formula(paste0(
            "factor(reference)~",
            paste0(paste0(categories,
                collapse = "+"
            ))
        ))
        return(result_for)
    }
    return(result_fun)
}



#' @title Normalize the time series in the given sits_tibble
#' @name .sits_normalize_data
#' @keywords internal
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description This function normalizes the time series using the mean and
#' standard deviation of all the time series.
#'
#' @param data     A sits tibble.
#' @param stats    Statistics for normalization.
#' @param multicores  Number of cores to process.
#' @return A normalized sits tibble.
.sits_normalize_data <- function(data, stats, multicores = 2) {
    # backward compatibility
    data <- .sits_tibble_rename(data)
    .sits_test_tibble(data)

    # get the bands of the input data
    bands <- sits_bands(data)
    # check that input bands are included in the statistics already calculated
    assertthat::assert_that(all(sort(bands) == sort(colnames(stats[, -1]))),
        msg = paste0(
            "sits_normalize: data bands (",
            paste(bands, collapse = ", "),
            ") do not match model bands (",
            paste(colnames(stats[, -1]),
                collapse = ", "
            ), ")"
        )
    )

    # extract the values of the time series to a list of tibbles
    values <- data$time_series
    n_values <- length(values)

    # normalise values of time series
    normalize_chunk <- function(chunk) {
        norm_chunk <- chunk %>%
            purrr::map(function(ts) {
                norm <- bands %>%
                    purrr::map(function(b) {
                        # retrieve values from data table
                        # note the use of "..b" instead of ",b"
                        quant_2 <- as.numeric(stats[2, ..b])
                        quant_98 <- as.numeric(stats[3, ..b])
                        # call C++ for better performance
                        m <- normalize_data(as.matrix(ts[, b]),
                                            quant_2,
                                            quant_98
                        )
                        # give a name to the matrix column because
                        # tibble does not like matrices without names
                        colnames(m) <- b
                        val <- tibble::as_tibble(m, .name_repair = "unique")
                        return(val)
                    })
                ts_tb <- dplyr::bind_cols(norm)
                ts_tb <- dplyr::bind_cols(list(ts[, 1], ts_tb))
                colnames(ts_tb) <- colnames(ts)
                return(ts_tb)
            })
        return(norm_chunk)
    }

    if (multicores > 1) {
        chunks <- split(values, cut(1:n_values, 2, labels = FALSE))
        norm_values <- chunks %>%
          parallel::mclapply(normalize_chunk, mc.cores = multicores) %>%
          unlist(recursive = FALSE)
    }
    else
          norm_values <- normalize_chunk(values)

    data$time_series <- norm_values
    return(data)
}

#' @title Normalize the time series values in the case of a matrix
#' @name .sits_normalize_matrix
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description this function normalizes one band of the values read
#' from a raster
#'
#' @param  data           Matrix of values.
#' @param  stats          Statistics for normalization.
#' @param  band           Band to be normalized.
#' @param  multicores     Number of cores.
#' @return                A normalized matrix.
.sits_normalize_matrix <- function(data, stats, band, multicores) {
    # select the 2% and 98% quantiles
    # note the use of "..b" instead of ",b"
    quant_2 <- as.numeric(stats[2, ..band])
    quant_98 <- as.numeric(stats[3, ..band])

    # auxiliary function to normalize a block of data
    normalize_block <- function(chunk, quant_2, quant_98) {
        # normalize a block of data
        values_block <- normalize_data(chunk, quant_2, quant_98)
        return(values_block)
    }

    # parallel processing for normalization
    if (multicores > 1) {
        blocks <- .sits_raster_data_split(data, multicores)
        rows <- parallel::mclapply(
            blocks,
            normalize_block,
            quant_2,
            quant_98,
            mc.cores = multicores
        )
        data <- do.call(rbind, rows)
    }
    else {
          data <- normalize_data(data, quant_2, quant_98)
      }

    return(data)
}


#' @title Normalize the time series in the given sits_tibble
#' @name .sits_normalization_param
#' @keywords internal
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description this function normalizes the time series using the mean and
#' standard deviation of all the time series.
#'
#' @param data     A sits tibble.
#' @return A tibble with statistics.
.sits_normalization_param <- function(data) {
    .sits_test_tibble(data)

    dt <- data.table::data.table(dplyr::bind_rows(data$time_series))
    dt[, Index := NULL]

    # compute statistics
    dt_med <- dt[, lapply(.SD, stats::median, na.rm = TRUE)]
    dt_quant_2 <- dt[, lapply(.SD, function(x) {
        stats::quantile(x, 0.02,
            na.rm = TRUE
        )
    })]
    dt_quant_98 <- dt[, lapply(.SD, function(x) {
        stats::quantile(x, 0.98,
            na.rm = TRUE
        )
    })]
    stats <- dplyr::bind_cols(
        stats = c("med", "quant_2", "quant_98"),
        dplyr::bind_rows(dt_med, dt_quant_2, dt_quant_98)
    )

    return(stats)
}
