#' Functions for machine learning associated to the SITS package
#' The attributes for the training functions are the DTW distances
#' computed by the TWTDW function (see documentation on sits_TWDTW_matches_tibble)
#'
#'
#' models supported: 'svm', 'random forests', 'boosting', 'lda', 'qda'
#'                   'multinomial logit', 'lasso', 'ridge', 'elnet', 'best model'

#' @title Train SITS classification models
#' @name sits_train
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Given a tibble with a set of distance measures,
#' returns trained models using support vector machines. This function will
#' use the TWDTW alignment information for all classes as the attributes
#' of the chosen machine learning methods. Please use this function in the following way:
#' 1. call sits_patterns to produce a set a labelled patterns from a reference data set
#' 2. call a method to get distances between a time series and patterns to produce a set of alignements
#' 3. use the distances tibble as an input to the training function
#'
#'
#' @param distances.tb     a time series with a set of distance measures for each training sample
#' @param tr_method        a traning method that returns a model for prediction
#' @return result          a model fitted into input data given by train_method parameter
#' @export
#'
sits_train <- function(distances.tb, tr_method = sits_svm()){

    # is the input data the result of a TWDTW matching function?
    ensurer::ensure_that(distances.tb, "reference" %in% names (.), err_desc = "sits_train: input data does not contain TWDTW matches")

    # is the train method a function?
    ensurer::ensure_that(tr_method, class(.) == "function", err_desc = "sits_train: train_method is not a valid function")

    # compute the training method by the given data
    result <- tr_method(distances.tb)
    return(result)
}

#' @title Train SITS classifier with a Support Vector Machine
#' @name sits_svm
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function receives a tibble with a set of attributes X for each observation Y
#' These attributes are distance metrics between patterns and observations, obtained by a distance
#' function in SITS (either sits_distances or sits_TWTDTW_distances).
#' The SVM algorithm is used for multiclass-classification.
#' For this purpose, it uses the "one-against-one" approach, in which k(k-1)/2 binary
#' classifiers are trained; the appropriate class is found by a voting scheme.
#' This function is a front-end to the "svm" method in the "e1071" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param distances.tb     a time series with a set of distance measures for each training sample
#' @param formula          a symbolic description of the model to be fit. SITS offers a set of such formulas (default: sits_svm)
#' @param kernel           the kernel used in training and predicting (options = linear, polynomial, radial basis, sigmoid)
#' @param degree           exponential of polynomial type kernel
#' @param coef0	           parameter needed for kernels of type polynomial and sigmoid (default: 0)
#' @param cost             cost of constraints violation
#' @param tolerance	       tolerance of termination criterion (default: 0.001)
#' @param epsilon	       epsilon in the insensitive-loss function (default: 0.1)
#' @param cross            the number of cross validation folds applied on the training data to assess the quality of the model,
#' @param ...              other parameters to be passed to e1071::svm function
#' @return result          a fitted model function to be passed in sits_predict
#' @export
#'
sits_svm <- function(distances.tb = NULL, formula = sits_formula_logref(), kernel = "linear",
                     degree = 3, coef0 = 0, cost = 1, tolerance = 0.001, epsilon = 0.1, cross = 4, ...) {

    # function that returns e1071::svm model based on a sits sample tibble
    result_fun <- function(train_data.tb){

        # verify if data input is not empty
        .sits_test_tibble (train_data.tb)

        # is the input data the result of a TWDTW matching function?
        ensurer::ensure_that(train_data.tb, "reference" %in% names (.),
                             err_desc = "sits_svm: input data does not contain distance")

        # there is something to learn?
        ensurer::ensure_that(train_data.tb, length(base::unique(.$reference)) > 1,
                             err_desc = "sits_svm: input data has ")

        # if parameter formula is a function call it passing as argument the input data sample. The function must return a valid formula.
        if (class(formula) == "function")
            formula <- formula(train_data.tb)

        # call e1071::svm method and return the trained svm model
        result_svm <- e1071::svm(formula = formula, data = train_data.tb, kernel = kernel,
                                 degree = degree, cost = cost, coef0 = coef0,
                                 tolerance = tolerance, epsilon = epsilon, cross = cross, ...)

        # construct model predict enclosure function and returns
        model_predict <- function(values.tb){
            return(stats::predict(result_svm, newdata = values.tb))
        }
        return(model_predict)
    }

    result <- .sits_factory_function (distances.tb, result_fun)
}
#' @title Train SITS classifiction models with Linear Discriminant Analysis
#' @name sits_lda
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function receives a tibble with a set of attributes X for each observation Y
#' These attributes are distance metrics between patterns and observations, obtained by a distance
#' function in SITS (either sits_distances or sits_TWTDTW_distances).
#' The method performs a linear discriminant analysis (lda) to obtain a predictive model.
#' This function is a front-end to the "lda" method in the "MASS" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param distances.tb     a time series with a set of distance measures for each training sample
#' @param formula          a symbolic description of the model to be fit. SITS offers a set of such formulas (default: sits_formula_logref)
#' @param ...              other parameters to be passed to MASS::lda function
#' @return result          a model function to be passed in sits_predict
#' @export
#'
sits_lda <- function(distances.tb = NULL, formula = sits_formula_logref(), ...) {

    # function that returns MASS::lda model based on a sits sample tibble
    result_fun <- function(train_data.tb){

        # verify if data input is not empty
        .sits_test_tibble (train_data.tb)

        # is the input data the result of a TWDTW matching function?
        ensurer::ensure_that(train_data.tb, "reference" %in% names (.), err_desc = "sits_lda: input data does not contain distance")

        # if parameter formula is a function call it passing as argument the input data sample. The function must return a valid formula.
        if (class(formula) == "function")
            formula <- formula(train_data.tb)

        # call MASS::lda method and return the trained lda model
        result_lda <- MASS::lda(formula = formula, data = train_data.tb, ...)

        # construct model predict enclosure function and returns
        model_predict <- function(values.tb){
            return(stats::predict(result_lda, newdata = values.tb)$class)
        }
        return(model_predict)
    }

    result <- .sits_factory_function (distances.tb, result_fun)
    return(result)
}

#' @title Train SITS classifiction models with Quadratic Discriminant Analysis
#' @name sits_qda
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function receives a tibble with a set of attributes X for each observation Y
#' These attributes are distance metrics between patterns and observations, obtained by a distance
#' function in SITS (either sits_distances or sits_TWTDTW_distances).
#' The function performs a quadratic discriminant analysis (qda) to obtain a predictive model.
#' This function is a front-end to the "qda" method in the "MASS" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param distances.tb     a time series with a set of distance measures for each training sample
#' @param formula          a symbolic description of the model to be fit. SITS offers a set of such formulas (default: sits_formula_logref)
#' @param ...              other parameters to be passed to MASS::lda function
#' @return result          a model function to be passed in sits_predict
#' @export
#'
sits_qda <- function(distances.tb = NULL, formula = sits_formula_logref(), ...) {

    # function that returns MASS::lda model based on a sits sample tibble
    result_fun <- function(train_data.tb){

        # verify if data input is not empty
        .sits_test_tibble (train_data.tb)

        # is the input data the result of a TWDTW matching function?
        ensurer::ensure_that(train_data.tb, "reference" %in% names (.), err_desc = "sits_qda: input data does not contain distance")

        # if parameter formula is a function call it passing as argument the input data sample. The function must return a valid formula.
        if (class(formula) == "function")
            formula <- formula(train_data.tb)

        # call MASS::qda method and return the trained lda model
        result_qda <- MASS::qda(formula = formula, data = train_data.tb, ...)

        # construct model predict enclosure function and returns
        model_predict <- function(values.tb){
            return(stats::predict(result_qda, newdata = values.tb)$class)
        }
        return(model_predict)
    }

    result <- .sits_factory_function (distances.tb, result_fun)
    return(result)
}

#' @title Train SITS classifiction models
#' @name sits_mlr
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Use multinomial log-linear (mlr) fitting model via neural networks to classify data.
#' These attributes are distance metrics between patterns and observations, obtained by a distance
#' function in SITS (either sits_distances or sits_TWTDTW_distances).
#' This function is a front-end to the "multinom" method in the "nnet" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param distances.tb     a time series with a set of distance measures for each training sample
#' @param formula          a symbolic description of the model to be fit. SITS offers a set of such formulas (default: sits_formula_logref)
#' @param ...              other parameters to be passed to nnet::multinom function
#' @return result          a model function to be passed in sits_predict
#' @export
#'
sits_mlr <- function(distances.tb = NULL, formula = sits_formula_logref(), ...) {

    # function that returns nnet::multinom model based on a sits sample tibble
    result_fun <- function(train_data.tb){

        # verify if data input is not empty
        .sits_test_tibble (train_data.tb)

        # is the input data the result of a TWDTW matching function?
        ensurer::ensure_that(train_data.tb, "reference" %in% names (.), err_desc = "sits_mlr: input data does not contain distance")

        # if parameter formula is a function call it passing as argument the input data sample. The function must return a valid formula.
        if (class(formula) == "function")
            formula <- formula(train_data.tb)

        # call nnet::multinom method and return the trained multinom model
        result_mlr <- nnet::multinom(formula = formula, data = train_data.tb, ...)

        # construct model predict enclosure function and returns
        model_predict <- function(values.tb){
            return(stats::predict(result_mlr, newdata = values.tb))
        }
        return(model_predict)
    }

    result <- .sits_factory_function (distances.tb, result_fun)
    return(result)
}

#' @title Train SITS classifiction models with Generalised Linear Models
#' @name sits_glm
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Use generalized liner model (glm) via penalized maximim likelihood to classify data.
#' These attributes are distance metrics between patterns and observations, obtained by a distance
#' function in SITS (either sits_distances or sits_TWTDTW_distances).
#' This function is a front-end to the "cv.glmnet" method in the "glmnet" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param distances.tb     a time series with a set of distance measures for each training sample
#' @param family           Response type. Can be either "gaussian", "binomial", "poisson", "multinomial", "cox", or "mgaussian". (default: "multinomial")
#' @param alpha            the elasticnet mixing parameter, with 0<=alpha<=1.
#'                         Set alpha = 0.0 to obtain ridge model, and alpha = 1.0 to obtain lasso model.
#'                         (refer to `glmnet::cv.glmnet` function for more details)
#' @param lambda_kfolds    number of folds to find best lambda parameter (default: 10)
#' @param ...              other parameters to be passed to `glmnet::cv.glmnet` function
#' @return result          a model function to be passed in sits_predict
#' @export
#'
sits_glm <- function(distances.tb = NULL, family = "multinomial", alpha = 1.0, lambda_kfolds = 10, ...) {

    # function that returns glmnet::multinom model based on a sits sample tibble
    result_fun <- function(train_data.tb){

        # verify if data input is not empty
        .sits_test_tibble (train_data.tb)

        # is the input data the result of a TWDTW matching function?
        ensurer::ensure_that(train_data.tb, "reference" %in% names (.), err_desc = "sits_glm: input data does not contain distance")

        # call glmnet::multinom method and return the trained multinom model
        result_glm <- glmnet::cv.glmnet(y = factor(data.matrix(train_data.tb$reference)),
                                        x = log(data.matrix(train_data.tb[,3:NCOL(train_data.tb)])),
                                        family = family, alpha = alpha, k = lambda_kfolds, ...)

        # construct model predict enclosure function and returns
        model_predict <- function(values.tb){
            return(stats::predict(result_glm,
                                  s = result_glm$lambda.min,
                                  newx = log(data.matrix(values.tb[,3:NCOL(values.tb)])), type = "class"))
        }
        return(model_predict)
    }

    result <- .sits_factory_function (distances.tb, result_fun)
    return(result)
}

#' @title Train SITS classifiction models with Gradient Boosting Machine
#' @name sits_gbm
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function implements the generalized boosted modeling framework.
#' Boosting is the process of iteratively adding basis functions in a greedy fashion
#' so that each additional basis function further reduces the selected loss function.
#' This function is a front-end to the "gbm" method in the "gbm" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param distances.tb     a time series with a set of distance measures for each training sample
#' @param formula          a symbolic description of the model to be fit. SITS offers a set of such formulas (default: sits_formula_logref)
#' @param distribution     The name of the distribution - use "multinomial" for classification
#' @param n.trees          Number of trees to fit. This should not be set to too small a number,
#'                         to ensure that every input row gets predicted at least a few times. (default: 5000)
#' @param interaction.depth  The maximum depth of variable interactions. 1 implies an additive model, 2 implies a model with up to 2-way interactions.
#' @param shrinkage        a shrinkage parameter applied to each tree in the expansion.
#'                         Also known as the learning rate or step-size reduction.
#' @param cv.folds         number of cross-validations to run
#' @param n.cores          number of cores to run
#' @param ...              other parameters to be passed to `gbm::gbm` function
#' @return result          a model function to be passed in sits_predict or an function prepared that can be called further to compute multinom training model
#' @export
#'
sits_gbm <- function(distances.tb = NULL, formula = sits_formula_logref(), distribution = "multinomial",
                     n.trees = 5000, interaction.depth = 4, shrinkage = 0.001, cv.folds = 5, n.cores = 1, ...) {

    # function that returns glmnet::multinom model based on a sits sample tibble
    result_fun <- function(train_data.tb){

        # verify if data input is not empty
        .sits_test_tibble (train_data.tb)

        # is the input data the result of a TWDTW matching function?
        ensurer::ensure_that(train_data.tb, "reference" %in% names (.), err_desc = "sits_gbm: input data does not contain distance")

        # if parameter formula is a function call it passing as argument the input data sample. The function must return a valid formula.
        if (class(formula) == "function")
            formula <- formula(train_data.tb)

        # call gbm::gbm method and return the trained multinom model
        df <- data.frame (train_data.tb[-1:0])
        result_gbm <- gbm::gbm(formula = formula, data = df,
                               distribution = distribution, n.trees = n.trees, interaction.depth = interaction.depth,
                               shrinkage = shrinkage, cv.folds = cv.folds, n.cores = n.cores,...)

        # check performance using 5-fold cross-validation
        best.iter <- gbm::gbm.perf(result_gbm, method="cv")

        # construct model predict enclosure function and returns
        # construct model predict enclosure function and returns
        model_predict <- function(values.tb){
            result <- stats::predict(result_gbm, newdata = values.tb, best.iter)
            return(colnames(result)[max.col(result[,,1])])
        }
        return(model_predict)
    }

    result <- .sits_factory_function (distances.tb, result_fun)
    return(result)
}


#' @title Train SITS classifiction models
#' @name sits_rfor
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Use Random Forest algorithm to classify data.
#' This function is a front-end to the "randomForest" method in the "randomForest" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param distances.tb     a time series with a set of distance measures for each training sample
#' @param ntree            Number of trees to grow. This should not be set to too small a number,
#'                         to ensure that every input row gets predicted at least a few times. (default: 500)
#' @param ...              other parameters to be passed to `randomForest::randomForest` function
#' @return result          either an model function to be passed in sits_predict or an function prepared that can be called further to compute multinom training model
#' @export
#'
sits_rfor <- function(distances.tb = NULL, ntree = 500, ...) {

    # function that returns `randomForest::randomForest` model based on a sits sample tibble
    result_fun <- function(train_data.tb){

        # verify if data input is not empty
        .sits_test_tibble (train_data.tb)

        # is the input data the result of a TWDTW matching function?
        ensurer::ensure_that(train_data.tb, "reference" %in% names (.), err_desc = "sits_rfor: input data does not contain distance")

        # call `randomForest::randomForest` method and return the trained multinom model
        df <- data.frame (train_data.tb[-1:0])
        result_rfor <- randomForest::randomForest(x = df[-1:0],
                                                  y = as.factor(df$reference),
                                                  data = NULL, ntree = ntree, nodesize = 1,
                                                  norm.votes = FALSE, ...)

        # construct model predict enclosure function and returns
        model_predict <- function(values.tb){
            return(stats::predict(result_rfor, newdata = values.tb, type = "response"))
        }
        return(model_predict)
    }

    result <- .sits_factory_function (distances.tb, result_fun)
    return(result)
}

#' @title Train SITS classifiction models with deep learning
#' @name sits_deep_learning
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Use deep learning algorithm to classify data.
#' This function is a front-end to the "deeplearning" method in the "randomForest" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param distances.tb     a time series with a set of distance measures for each training sample
#' @param ...              other parameters to be passed to `h2o::h2o.deeplearning` function
#' @return result          either an model function to be passed in sits_predict or an function prepared that can be called further to compute multinom training model
#' @export
#'
sits_deep_learning <- function(distances.tb = NULL, ...) {

    # function that returns `randomForest::randomForest` model based on a sits sample tibble
    result_fun <- function(train_data.tb){

        # verify if data input is not empty
        .sits_test_tibble (train_data.tb)

        # is the input data the result of a TWDTW matching function?
        ensurer::ensure_that(train_data.tb, "reference" %in% names (.), err_desc = "sits_rfor: input data does not contain distance")

        # list of attributes
        attrs.lst <- colnames(train_data.tb[-2:0])

        # convert the training data to h2o format
        train.df           <- data.frame(train_data.tb[-1:0])
        train.df$reference <- as.factor(train.df$reference)
        train.h2o          <- h2o::as.h2o(train.df)

        # implement deep learning
        result.h2o  <- h2o::h2o.deeplearning(y = "reference", x = attrs.lst,
                                             training_frame = train.h2o, distribution = "multinomial")

        # construct model predict enclosure function and returns
        model_predict <- function(values.tb){
            vls.df           <- data.frame (values.tb[-1:0])
            vls.df$reference <- as.factor(vls.df$reference)
            vls.h2o          <- h2o::as.h2o(vls.df)
            pred.h2o         <- h2o::h2o.predict(result.h2o, newdata = vls.h2o)

            return ( as.character(as.vector(pred.h2o$predict)) )
        }
        return(model_predict)
    }

    result <- .sits_factory_function (distances.tb, result_fun)
    return(result)
}
#' @title Train SITS classifiction models
#' @name sits_formula_logref
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description A function to be used as a symbolic description of some fitting models such as svm.
#' `predictors_index` parameter informs the positions of `tb` fields corresponding to formula independent variables.
#' If no value is given, the default is NULL, a value indicating that all fields will be used as predictors.
#'
#' @param predictors_index  the index of the valid columns whose names are used to compose formula (default: NULL)
#' @return result_fun       a function that computes a valid formula
#' @export
#'
sits_formula_logref <- function(predictors_index = -2:0){

    # this function returns a formula like 'factor(reference~log(f1)+log(f2)+...+log(fn)' where f1, f2, ..., fn are,
    # respectivelly, the reference and predictors fields of tibble in `tb` parameter.
    result_fun <- function(tb){

        # if no predictors_index are given, assume that all tb's fields are used
        if (is.null(predictors_index))
            predictors_index <- 1:NROW(tb)

        # get predictors names
        categories <- names(tb)[c(predictors_index)]

        # compute formula result
        result_for <- stats::as.formula(paste0("factor(reference)~", paste0(paste0('log(`', categories, '`)'), collapse = "+")))
        return(result_for)
    }
    return(result_fun)
}

#' @title Train SITS classifiction models
#' @name sits_formula_linear
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description A function to be used as a symbolic description of some fitting models such as svm.
#' `predictors_index` parameter informs the positions of `tb` fields corresponding to formula independent variables.
#' If no value is given, the default is NULL, a value indicating that all fields will be used as predictors.
#'
#' @param predictors_index  the index of the valid columns whose names are used to compose formula (default: NULL)
#' @return result_fun       a function that computes a valid formula
#' @export
#'
sits_formula_linear <- function(predictors_index = -2:0){

    # this function returns a formula like 'factor(reference~log(f1)+log(f2)+...+log(fn)' where f1, f2, ..., fn are,
    # respectivelly, the reference and predictors fields of tibble in `tb` parameter.
    result_fun <- function(tb){

        # if no predictors_index are given, assume that all tb's fields are used
        if (is.null(predictors_index))
            predictors_index <- 1:NROW(tb)

        # get predictors names
        categories <- names(tb)[c(predictors_index)]

        # compute formula result
        result_for <- stats::as.formula(paste0("factor(reference)~", paste0(paste0(categories, collapse = "+"))))
        return(result_for)
    }
    return(result_fun)
}

#' @title Train SITS classifiction models
#' @name sits_formula_smooth
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description A function to be used as a symbolic description of some fitting models such as gam.
#' `predictors_index` parameter informs the positions of `tb` fields corresponding to formula independent variables.
#' If no value is given, the default is NULL, a value indicating that all fields will be used as predictors.
#'
#' @param predictors_index  the index of the valid columns whose names are used to compose formula (default: NULL)
#' @return result_fun       a function that computes a valid formula
#' @export
#'
sits_formula_smooth <- function(predictors_index = -2:0){

    # this function returns a formula like 'factor(reference~log(f1)+log(f2)+...+log(fn)' where f1, f2, ..., fn are,
    # respectivelly, the reference and predictors fields of tibble in `tb` parameter.
    result_fun <- function(tb){

        # if no predictors_index are given, assume that all tb's fields are used
        if (is.null(predictors_index))
            predictors_index <- 1:NROW(tb)

        # get predictors names
        categories <- names(tb)[c(predictors_index)]

        # compute formula result
        result_for <- stats::as.formula(paste0("factor(reference)~", paste0(paste0('s(`', categories, '`)'), collapse = "+")))
        return(result_for)
    }
    return(result_fun)
}

#' @title Predict class based on the trained models
#' @name sits_predict
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Given a SITS tibble time series and a model trained by \code{\link[sits]{sits_train}},
#' returns a SITS tibble with the classification.
#'
#' @param data.tb       a SITS tibble time series
#' @param distances.tb  a tibble with a set of distance metrics to each of the classes
#' @param ml_model      a model trained by \code{\link[sits]{sits_train}}
#' @param ...           other parameters to be passed to the model function
#' @return data.tb      a SITS tibble with the predicted label
#'
#' @export
sits_predict <- function(data.tb = NULL, distances.tb = NULL, ml_model, ...){

    # is the input data the result of a TWDTW matching function?
    ensurer::ensure_that(distances.tb, "reference" %in% names (.), err_desc = "sits_train_svm: input data does not contain TWDTW matches")

    # is the input model a model function?
    ensurer::ensure_that(ml_model, class (.) == "function", err_desc = "sits_predict: model parameter is not a function model returned by sits_train.")

    data.tb$predicted <- as.character(ml_model(distances.tb))

    return(data.tb)
}
