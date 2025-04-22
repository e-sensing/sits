#' @title Train random forest models
#' @name sits_rfor
#'
#' @author Alexandre Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Use Random Forest algorithm to classify samples.
#' This function is a front-end to the \code{randomForest} package.
#' Please refer to the documentation in that package for more details.
#'
#' @param samples    Time series with the training samples
#'                   (tibble of class "sits").
#' @param num_trees  Number of trees to grow. This should not be set to too
#'                   small a number, to ensure that every input
#'                   row gets predicted at least a few times (default: 100)
#'                   (integer, min = 50, max = 150).
#' @param mtry       Number of variables randomly sampled as candidates at
#'                   each split (default: NULL - use default value of
#'                   \code{randomForest::randomForest()} function, i.e.
#'                   \code{floor(sqrt(features))}).
#' @param ...        Other parameters to be passed
#'                   to `randomForest::randomForest` function.
#' @return           Model fitted to input data
#'                   (to be passed to \code{\link[sits]{sits_classify}}).
#'
#' @examples
#' if (sits_run_examples()) {
#'     # Example of training a model for time series classification
#'     # Retrieve the samples for Mato Grosso
#'     # train a random forest model
#'     rf_model <- sits_train(samples_modis_ndvi,
#'         ml_method = sits_rfor
#'     )
#'     # classify the point
#'     point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
#'     # classify the point
#'     point_class <- sits_classify(
#'         data = point_ndvi, ml_model = rf_model
#'     )
#'     plot(point_class)
#' }
#' @export
#'
sits_rfor <- function(samples = NULL, num_trees = 100L, mtry = NULL, ...) {
    .check_set_caller("sits_rfor")
    # Function that trains a random forest model
    train_fun <- function(samples) {
        # Verifies if 'randomForest' package is installed
        .check_require_packages("randomForest")
        # Checks 'num_trees'
        .check_int_parameter(num_trees, min = 20L)
        # Get labels (used later to ensure column order in result matrix)
        labels <- .samples_labels(samples)
        # Get predictors features
        train_samples <- .predictors(samples)
        # Post condition: is predictor data valid?
        .check_predictors(pred = train_samples, samples = samples)
        # determine number of random forest
        n_features <- ncol(train_samples) - 2L
        # Apply the 'mtry' default value of 'randomForest' package
        if (.has(mtry)) {
            # Checks 'mtry'
            .check_int_parameter(mtry, min = 1L, max = n_features)
        } else {
            # set the default values of `mtry`
            mtry <- floor(sqrt(n_features))
        }
        # Train a random forest model
        model <- randomForest::randomForest(
            x = .pred_features(train_samples),
            y = as.factor(.pred_references(train_samples)),
            samples = NULL, ntree = num_trees, mtry = mtry,
            nodesize = 1L, localImp = TRUE, norm.votes = FALSE, ...,
            na.action = stats::na.fail
        )
        # Function that predicts results
        predict_fun <- function(values) {
            # Verifies if randomForest package is installed
            .check_require_packages("randomForest")
            # Used to check values (below)
            input_pixels <- nrow(values)
            # Do classification
            values <- stats::predict(
                object = model, newdata = values, type = "prob"
            )
            # Are the results consistent with the data input?
            .check_processed_values(values, input_pixels)
            # Reorder matrix columns if needed
            if (any(labels != colnames(values))) {
                values <- values[, labels]
            }
            values
        }
        # Set model class
        predict_fun <- .set_class(
            predict_fun, "rfor_model", "sits_model", class(predict_fun)
        )
        predict_fun
    }
    # If samples is informed, train a model and return a predict function
    # Otherwise give back a train function to train model further
    .factory_function(samples, train_fun)
}
#' @title Train support vector machine models
#' @name sits_svm
#' @author Alexandre Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
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
#' @param samples          Time series with the training samples.
#' @param formula          Symbolic description of the model to be fit.
#'                         (default: sits_formula_linear).
#' @param scale            Logical vector indicating the variables to be scaled.
#' @param cachesize        Cache memory in MB (default = 1000).
#' @param kernel           Kernel used in training and predicting.
#'                         options: "linear", "polynomial", "radial", "sigmoid"
#'                         (default: "radial").
#' @param degree           Exponential of polynomial type kernel (default: 3).
#' @param coef0            Parameter needed for kernels of type polynomial
#'                         and sigmoid (default: 0).
#' @param cost             Cost of constraints violation (default: 10).
#' @param tolerance        Tolerance of termination criterion (default: 0.001).
#' @param epsilon          Epsilon in the insensitive-loss function
#'                         (default: 0.1).
#' @param cross            Number of cross validation folds applied
#'                         to assess the quality of the model (default: 10).
#' @param ...              Other parameters to be passed to e1071::svm function.
#' @return                 Model fitted to input data
#'                         (to be passed to \code{\link[sits]{sits_classify}})
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#' @examples
#' if (sits_run_examples()) {
#'     # Example of training a model for time series classification
#'     # Retrieve the samples for Mato Grosso
#'     # train an SVM model
#'     ml_model <- sits_train(samples_modis_ndvi, ml_method = sits_svm)
#'     # classify the point
#'     point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
#'     # classify the point
#'     point_class <- sits_classify(
#'         data = point_ndvi, ml_model = ml_model
#'     )
#'     plot(point_class)
#' }
#' @export
#'
sits_svm <- function(samples = NULL, formula = sits_formula_linear(),
                     scale = FALSE,
                     cachesize = 1000L,
                     kernel = "radial",
                     degree = 3L,
                     coef0 = 0L,
                     cost = 10.0,
                     tolerance = 0.001,
                     epsilon = 0.1,
                     cross = 10L, ...) {
    .check_set_caller("sits_svm")
    # Function that trains a support vector machine model
    train_fun <- function(samples) {
        # does not support working with DEM or other base data
        if (inherits(samples, "sits_base"))
            stop(.conf("messages", "sits_train_base_data"), call. = FALSE)
        # Verifies if e1071 package is installed
        .check_require_packages("e1071")
        # Get labels (used later to ensure column order in result matrix)
        labels <- .samples_labels(samples)
        # Get normalized training samples
        ml_stats <- .samples_stats(samples)
        # Get predictors features
        train_samples <- .predictors(samples)
        # Normalize predictors
        train_samples <- .pred_normalize(pred = train_samples, stats = ml_stats)
        # Post condition: is predictor data valid?
        .check_predictors(pred = train_samples, samples = samples)
        # Update formula parameter
        if (inherits(formula, "function")) {
            formula <- formula(train_samples)
        }
        # Train an svm model
        model <- e1071::svm(
            formula = formula, data = train_samples, scale = scale,
            kernel = kernel, degree = degree, cost = cost, coef0 = coef0,
            cachesize = cachesize, tolerance = tolerance, epsilon = epsilon,
            cross = cross, probability = TRUE, ..., na.action = stats::na.fail
        )
        # Function that predicts labels of input values
        predict_fun <- function(values) {
            # Verifies if e1071 package is installed
            .check_require_packages("e1071")
            # Used to check values (below)
            input_pixels <- nrow(values)
            # Performs data normalization
            values <- .pred_normalize(pred = values, stats = ml_stats)
            # Do classification
            values <- stats::predict(
                object = model, newdata = values, probability = TRUE
            )
            # Get the predicted probabilities
            values <- attr(values, "probabilities")
            # Are the results consistent with the data input?
            .check_processed_values(values, input_pixels)
            # Reorder matrix columns if needed
            if (any(labels != colnames(values))) {
                values <- values[, labels]
            }
            values
        }
        # Set model class
        predict_fun <- .set_class(
            predict_fun, "svm_model", "sits_model", class(predict_fun)
        )
        predict_fun
    }
    # If samples is informed, train a model and return a predict function
    # Otherwise give back a train function to train model further
    .factory_function(samples, train_fun)
}
#' @title Train extreme gradient boosting models
#' @name sits_xgboost
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
#' @param samples          Time series with the training samples.
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
#' @param nthread          Number of threads (default = 6)
#' @param early_stopping_rounds Training with a validation set will stop
#'                         if the performance doesn't improve for k rounds.
#' @param verbose          Print information on statistics during the process
#' @return                 Model fitted to input data
#'                         (to be passed to \code{\link[sits]{sits_classify}})
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#' @examples
#' if (sits_run_examples()) {
#'     # Example of training a model for time series classification
#'     # Retrieve the samples for Mato Grosso
#'     # train a xgboost model
#'     ml_model <- sits_train(samples_modis_ndvi, ml_method = sits_xgboost)
#'     # classify the point
#'     point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
#'     # classify the point
#'     point_class <- sits_classify(
#'         data = point_ndvi, ml_model = ml_model
#'     )
#'     plot(point_class)
#' }
#' @export
#'
sits_xgboost <- function(samples = NULL,
                         learning_rate = 0.15,
                         min_split_loss = 1.0,
                         max_depth = 5L,
                         min_child_weight = 1.0,
                         max_delta_step = 1.0,
                         subsample = 0.85,
                         nfold = 5L,
                         nrounds = 100L,
                         nthread = 6L,
                         early_stopping_rounds = 20L,
                         verbose = FALSE) {
    .check_set_caller("sits_xgboost")
    # documentation mode? verbose is FALSE
    verbose <- .message_verbose(verbose)
    # Function that trains a xgb model
    train_fun <- function(samples) {
        # verifies if xgboost package is installed
        .check_require_packages("xgboost")
        # Get labels (used later to ensure column order in result matrix)
        labels <- .samples_labels(samples)
        # Get predictors features
        train_samples <- .predictors(samples)
        # Post condition: is predictor data valid?
        .check_predictors(pred = train_samples, samples = samples)
        # Transform labels to integer code before train
        code_labels <- seq_along(labels)
        names(code_labels) <- labels
        # Reference labels for each sample expressed as numerical values
        references <-
            unname(code_labels[.pred_references(train_samples)]) - 1L
        # Define the parameters of the model
        params <- list(
            booster = "gbtree", objective = "multi:softprob",
            eval_metric = "mlogloss", eta = learning_rate,
            gamma = min_split_loss, max_depth = max_depth,
            min_child_weight = min_child_weight,
            max_delta_step = max_delta_step, subsample = subsample,
            nthread = nthread
        )
        if (verbose)
            verbose <-  1L
        else
            verbose <-  0L
        # transform predictors in a xgb.DMatrix
        xgb_matrix <- xgboost::xgb.DMatrix(
            data = as.matrix(.pred_features(train_samples)),
            label = references)
        # train the model
        model <- xgboost::xgb.train(xgb_matrix,
            num_class = length(labels), params = params,
            nrounds = nrounds, verbose = verbose
        )
        # Get best ntreelimit
        ntreelimit <- model[["best_ntreelimit"]]
        # Function that predicts labels of input values
        predict_fun <- function(values) {
            # Verifies if xgboost package is installed
            .check_require_packages("xgboost")
            # Used to check values (below)
            input_pixels <- nrow(values)
            # Do classification
            values <- stats::predict(
                object = model, as.matrix(values), ntreelimit = ntreelimit,
                reshape = TRUE
            )
            # Are the results consistent with the data input?
            .check_processed_values(values, input_pixels)
            # Update the columns names to labels
            colnames(values) <- labels
            values
        }
        # Set model class
        predict_fun <- .set_class(
            predict_fun, "xgb_model", "sits_model", class(predict_fun)
        )
        predict_fun
    }
    # If samples is informed, train a model and return a predict function
    # Otherwise give back a train function to train model further
    .factory_function(samples, train_fun)
}

#' @title Define a loglinear formula for classification models
#' @name sits_formula_logref
#'
#' @author Alexandre Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
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
#' @return A function that computes a valid formula using a log function.
#' @examples
#' if (sits_run_examples()) {
#'     # Example of training a model for time series classification
#'     # Retrieve the samples for Mato Grosso
#'     # train an SVM model
#'     ml_model <- sits_train(samples_modis_ndvi,
#'         ml_method = sits_svm(formula = sits_formula_logref())
#'     )
#'     # classify the point
#'     point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
#'     # classify the point
#'     point_class <- sits_classify(
#'         data = point_ndvi, ml_model = ml_model
#'     )
#'     plot(point_class)
#' }
#' @export
#'
sits_formula_logref <- function(predictors_index = -2L:0L) {
    # set caller to show in errors
    .check_set_caller("sits_formula_logref")

    # store configuration information about model formula
    sits_env[["model_formula"]] <- "log"

    # this function returns a formula like
    # 'factor(reference~log(f1)+log(f2)+...+log(fn)' where f1, f2, ..., fn are
    # the predictor fields given by the predictor index.
    result_fun <- function(tb) {
        .check_that(nrow(tb) > 0)
        # if no predictors_index are given, assume all tb's fields are used
        if (!.has(predictors_index))
            predictors_index <- seq_len(nrow(tb))
        # get predictors names
        categories <- names(tb)[c(predictors_index)]
        # compute formula result
        stats::as.formula(paste0(
            "factor(label)~",
            paste0(paste0("log(`", categories, "`)"),
                collapse = "+"
            )
        ))
    }
    result_fun
}

#' @title Define a linear formula for classification models
#' @name sits_formula_linear
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Alexandre Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @description Provides a symbolic description of a fitting model.
#' Tells the model to do a linear transformation of the input values.
#' The `predictors_index` parameter informs the positions of fields
#' corresponding to formula independent variables.
#' If no value is given,  that all fields will be used as predictors.
#'
#' @param predictors_index  Index of the valid columns
#'                    whose names are used to compose formula (default: -2:0).
#' @return A function that computes a valid formula using a linear function.
#' @examples
#' if (sits_run_examples()) {
#'     # Example of training a model for time series classification
#'     # Retrieve the samples for Mato Grosso
#'     # train an SVM model
#'     ml_model <- sits_train(samples_modis_ndvi,
#'         ml_method = sits_svm(formula = sits_formula_logref())
#'     )
#'     # classify the point
#'     point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
#'     # classify the point
#'     point_class <- sits_classify(
#'         data = point_ndvi, ml_model = ml_model
#'     )
#'     plot(point_class)
#' }
#' @export
#'
sits_formula_linear <- function(predictors_index = -2L:0L) {
    # set caller to show in errors
    .check_set_caller("sits_formula_linear")

    # store configuration information about model formula
    sits_env[["model_formula"]] <- "linear"

    # this function returns a formula
    # 'factor(reference~log(f1)+log(f2)+...+log(fn)' where f1, f2, ..., fn are
    #  the predictor fields.
    result_fun <- function(tb) {
        .check_content_data_frame(tb)
        n_rows_tb <- nrow(tb)
        # if no predictors_index are given, assume that all fields are used
        if (!.has(predictors_index))
            predictors_index <- seq_len(n_rows_tb)

        # get predictors names
        categories <- names(tb)[c(predictors_index)]

        # compute formula result
        stats::as.formula(paste(
            "factor(label)~",
            paste(paste(categories,
                collapse = "+"
            ))
        ))
    }
    result_fun
}
