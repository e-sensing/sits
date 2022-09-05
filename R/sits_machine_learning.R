#' @title Train random forest models
#' @name sits_rfor
#'
#' @author Alexandre Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Use Random Forest algorithm to classify samples.
#' This function is a front-end to the "randomForest" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param samples    Time series with the training samples.
#' @param num_trees  Number of trees to grow. This should not be set to too
#'   small a number, to ensure that every input row gets predicted
#'   at least a few times (default: 120).
#' @param mtry       Number of variables randomly sampled as candidates at
#'   each split (default: NULL - use default value of
#'   \code{randomForest::randomForest()} function, i.e.
#'   \code{floor(sqrt(features))}).
#' @param ...        Other parameters to be passed
#'                   to `randomForest::randomForest` function.
#' @return           Model fitted to input data
#'                   (to be passed to \code{\link[sits]{sits_classify}}).
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # Example of training a model for time series classification
#'     # Retrieve the samples for Mato Grosso
#'     # train a random forest model
#'     rf_model <- sits_train(samples_modis_4bands,
#'                            ml_method = sits_rfor(mtry = 20))
#'     # select the bands to classify the point
#'     sample_bands <- sits_bands(samples_modis_4bands)
#'     point_4bands <- sits_select(point_mt_6bands, bands = sample_bands)
#'     # classify the point
#'     point_class <- sits_classify(point_4bands, rf_model)
#'     plot(point_class)
#' }
#' @export
#'
sits_rfor <- function(samples = NULL,
                      num_trees = 120,
                      mtry = NULL, ...) {

    # function that returns `randomForest::randomForest` model
    result_fun <- function(samples) {

        # verifies if randomForest package is installed
        .check_require_packages("randomForest")

        # get predictors features
        train_samples <- .sits_distances(samples)

        # check num_trees
        .check_num(
            x = num_trees,
            min = 1,
            len_min = 1,
            len_max = 1,
            is_integer = TRUE,
            msg = "invalid 'num_trees' parameter"
        )

        # check mtry
        # apply the same mtry default value of randomForest package
        n_features <- ncol(train_samples) - 2
        if (purrr::is_null(mtry))
            mtry <- floor(sqrt(n_features))
        .check_num(
            x = mtry,
            min = 1,
            max = n_features,
            len_min = 1,
            len_max = 1,
            is_integer = TRUE,
            msg = "invalid 'mtry' parameter"
        )

        # call `randomForest::randomForest` method and return the trained model
        reference <- train_samples[, reference]
        result_rfor <- randomForest::randomForest(
            x = train_samples[, 3:ncol(train_samples)],
            y = as.factor(reference),
            samples = NULL,
            ntree = num_trees,
            mtry = mtry,
            nodesize = 1,
            localImp = TRUE,
            norm.votes = FALSE, ...,
            na.action = stats::na.fail
        )

        # construct model predict enclosure function and returns
        model_predict <- function(values) {

            # verifies if ranger package is installed
            .check_require_packages("randomForest")

            return(stats::predict(result_rfor,
                newdata = values,
                type = "prob"
            ))
        }
        class(model_predict) <- c(
            "rfor_model", "sits_model",
            class(model_predict)
        )
        return(model_predict)
    }

    result <- .sits_factory_function(samples, result_fun)
    return(result)
}
#' @title Train support vector machine models
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
#' @param samples             Time series with the training samples.
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
#'     ml_model <- sits_train(samples_modis_4bands, ml_method = sits_svm)
#'     # select the bands to classify the point
#'     sample_bands <- sits_bands(samples_modis_4bands)
#'     point_4bands <- sits_select(point_mt_6bands, bands = sample_bands)
#'     # classify the point
#'     point_class <- sits_classify(point_4bands, ml_model)
#'     plot(point_class)
#' }
#' @export
#'
sits_svm <- function(samples = NULL, formula = sits_formula_linear(),
                     scale = FALSE, cachesize = 1000,
                     kernel = "radial", degree = 3, coef0 = 0,
                     cost = 10, tolerance = 0.001,
                     epsilon = 0.1, cross = 10, ...) {

    # function that returns e1071::svm model based on a sits sample tibble
    result_fun <- function(samples) {

        # verifies if e1071 package is installed
        .check_require_packages("e1071")

        # data normalization
        stats <- .sits_ml_normalization_param(samples)
        train_samples <- .sits_distances(
            .sits_ml_normalize_data(samples, stats)
        )

        # The function must return a valid formula.
        if (inherits(formula, "function")) {
            formula <- formula(train_samples)
        }

        # call e1071::svm method and return the trained svm model
        result_svm <- e1071::svm(
            formula = formula, data = train_samples,
            scale = scale, kernel = kernel,
            degree = degree, cost = cost, coef0 = coef0,
            cachesize = cachesize, tolerance = tolerance,
            epsilon = epsilon, cross = cross,
            probability = TRUE, ...,
            na.action = stats::na.fail
        )

        # construct model predict closure function and returns
        model_predict <- function(values) {

            # verifies if e1071 package is installed
            .check_require_packages("e1071")

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
        class(model_predict) <- c(
            "svm_model", "sits_model",
            class(model_predict)
        )
        return(model_predict)
    }
    result <- .sits_factory_function(samples, result_fun)
    return(result)
}
#' @title Train extreme gradient boosting models
#' @name sits_xgboost
#'
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
#'     ml_model <- sits_train(samples_modis_4bands, ml_method = sits_xgboost)
#'     # select the bands to classify the point
#'     sample_bands <- sits_bands(samples_modis_4bands)
#'     point_4bands <- sits_select(point_mt_6bands, bands = sample_bands)
#'     # classify the point
#'     point_class <- sits_classify(point_4bands, ml_model)
#'     plot(point_class)
#' }
#' @export
#'
sits_xgboost <- function(samples = NULL,
                         learning_rate = 0.15,
                         min_split_loss = 1,
                         max_depth = 5,
                         min_child_weight = 1,
                         max_delta_step = 1,
                         subsample = 0.8,
                         nfold = 5,
                         nrounds = 100,
                         early_stopping_rounds = 20,
                         verbose = FALSE) {

    # set caller to show in errors
    .check_set_caller("sits_xgboost")

    # function that returns xgb model
    result_fun <- function(samples) {

        # verifies if xgboost package is installed
        .check_require_packages("xgboost")

        # get the labels of the data
        labels <- sits_labels(samples)
        .check_length(
            x = labels,
            len_min = 1,
            msg = "invalid number of labels"
        )
        n_labels <- length(labels)

        # create a named vector with integers match the class labels
        int_labels <- c(1:n_labels)
        names(int_labels) <- labels

        # get the training data
        train_samples <- .sits_distances(samples)

        # reference labels for each sample expressed as numerical values
        references <- unname(int_labels[as.vector(train_samples$reference)]) - 1

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

        # define the model
        model_xgb <- xgboost::xgboost(
            data = as.matrix(train_samples[, -2:0]),
            label = references,
            num_class = length(labels),
            params = params,
            nrounds = nrounds,
            verbose = FALSE
        )

        ntreelimit <- model_xgb$best_ntreelimit

        # construct model predict closure function and returns
        model_predict <- function(values) {

            # verifies if xgboost package is installed
            .check_require_packages("xgboost")

            # transform input  into a matrix (remove first two columns)
            # retrieve the prediction probabilities
            prediction <- data.table::as.data.table(
                stats::predict(model_xgb, values,
                    ntreelimit = ntreelimit,
                    reshape = TRUE
                )
            )
            # adjust the names of the columns of the probs
            colnames(prediction) <- labels
            # retrieve the prediction results
            return(prediction)
        }
        class(model_predict) <- c(
            "xgb_model", "sits_model",
            class(model_predict)
        )
        return(model_predict)
    }

    result <- .sits_factory_function(samples, result_fun)
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
#' @return A function that computes a valid formula using a log function.
#' @examples
#' if (sits_run_examples()) {
#'     # Example of training a model for time series classification
#'     # Retrieve the samples for Mato Grosso
#'     # train an SVM model
#'     ml_model <- sits_train(samples_modis_4bands,
#'         ml_method = sits_svm(formula = sits_formula_logref()))
#'     # select the bands to classify the point
#'     sample_bands <- sits_bands(samples_modis_4bands)
#'     point_4bands <- sits_select(point_mt_6bands, bands = sample_bands)
#'     # classify the point
#'     point_class <- sits_classify(point_4bands, ml_model)
#'     plot(point_class)
#' }
#' @export
#'
sits_formula_logref <- function(predictors_index = -2:0) {

    # set caller to show in errors
    .check_set_caller("sits_formula_logref")

    # store configuration information about model formula
    sits_env$model_formula <- "log"

    # this function returns a formula like
    # 'factor(reference~log(f1)+log(f2)+...+log(fn)' where f1, f2, ..., fn are
    # the predictor fields given by the predictor index.
    result_fun <- function(tb) {
        .check_that(
            x = nrow(tb) > 0,
            msg = "invalid data"
        )
        n_rows_tb <- nrow(tb)

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
#' @return A function that computes a valid formula using a linear function.
#' @examples
#' if (sits_run_examples()) {
#'     # Example of training a model for time series classification
#'     # Retrieve the samples for Mato Grosso
#'     # train an SVM model
#'     ml_model <- sits_train(samples_modis_4bands,
#'         ml_method = sits_svm(formula = sits_formula_logref()))
#'     # select the bands to classify the point
#'     sample_bands <- sits_bands(samples_modis_4bands)
#'     point_4bands <- sits_select(point_mt_6bands, bands = sample_bands)
#'     # classify the point
#'     point_class <- sits_classify(point_4bands, ml_model)
#'     plot(point_class)
#' }
#' @export
#'
sits_formula_linear <- function(predictors_index = -2:0) {

    # set caller to show in errors
    .check_set_caller("sits_formula_linear")

    # store configuration information about model formula
    sits_env$model_formula <- "linear"

    # this function returns a formula
    # 'factor(reference~log(f1)+log(f2)+...+log(fn)' where f1, f2, ..., fn are
    #  the predictor fields.
    result_fun <- function(tb) {
        .check_that(
            x = nrow(tb) > 0,
            msg = "invalid data"
        )
        n_rows_tb <- nrow(tb)
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
#' @name .sits_ml_normalize_data
#' @keywords internal
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description This function normalizes the time series using the mean and
#' standard deviation of all the time series.
#'
#' @param data     Time series.
#' @param stats    Statistics for normalization.
#'
#' @return         Normalized time series.
#'
.sits_ml_normalize_data <- function(data, stats) {

    # set caller to show in errors
    .check_set_caller(".sits_ml_normalize_data")

    # test if data is valid
    .sits_tibble_test(data)

    # get the bands of the input data
    bands <- sits_bands(data)

    # check that input bands are included in the statistics already calculated
    .check_chr_within(
        x = sort(bands),
        within = sort(colnames(stats[, -1])),
        msg = paste0(
            "data bands (",
            paste(bands, collapse = ", "),
            ") do not match model bands (",
            paste(colnames(stats[, -1]),
                collapse = ", "
            ), ")"
        )
    )

    # extract the values of the time series to a list of tibbles
    values <- data$time_series

    # normalise values of time series
    normalize_chunk <- function(chunk) {
        norm_chunk <- chunk %>%
            purrr::map(function(ts) {
                norm <- bands %>%
                    purrr::map(function(b) {
                        # retrieve values from data table
                        # note the use of "..b" instead of ",b"
                        quant_2 <- as.numeric(stats[2, b, with = FALSE])
                        quant_98 <- as.numeric(stats[3, b, with = FALSE])
                        # call C++ for better performance
                        m <- normalize_data(
                            as.matrix(ts[, b]),
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

    norm_values <- normalize_chunk(values)

    data$time_series <- norm_values
    return(data)
}

#' @title Normalize the time series values in the case of a matrix
#' @name .sits_ml_normalize_matrix
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description this function normalizes one band of the values read
#' from a raster
#'
#' @param  data           Matrix of values.
#' @param  stats          Statistics for normalization.
#' @param  band           Band to be normalized.
#' @return                A normalized matrix.
.sits_ml_normalize_matrix <- function(data, stats, band) {
    # select the 2% and 98% quantiles
    # note the use of "..b" instead of ",b"
    quant_2 <- as.numeric(stats[2, band, with = FALSE])
    quant_98 <- as.numeric(stats[3, band, with = FALSE])

    data <- normalize_data(data, quant_2, quant_98)

    return(data)
}

#' @title Normalize the time series in the given sits_tibble
#' @name .sits_ml_normalization_param
#' @keywords internal
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description this function normalizes the time series using the mean and
#' standard deviation of all the time series.
#'
#' @param data     A sits tibble.
#' @return A tibble with statistics for normalization of time series.
.sits_ml_normalization_param <- function(data) {
    .sits_tibble_test(data)
    Index <- NULL # to avoid setting global variable

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
#' @title Get the samples from a ml_model
#' @name .sits_ml_model_samples
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description this function get the samples from a ml_model object.
#'
#' @param ml_model     A trained model.
#'
#' @return A tibble with samples used to train a machine learning model.
#'
.sits_ml_model_samples <- function(ml_model) {

    # pre-condition
    .check_that(
        x = inherits(ml_model, "function"),
        local_msg = "value should be a function",
        msg = "invalid 'ml_model' parameter"
    )

    # pre-condition
    .check_chr_contains(
        x = ls(environment(ml_model)),
        contains = "samples",
        msg = "invalid 'ml_model' function environment"
    )

    samples <- environment(ml_model)$samples

    # post-condition
    .sits_tibble_test(samples)

    return(samples)
}
