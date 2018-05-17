#' @title Train SITS classification models
#' @name sits_train
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#'
#' @description Given a tibble with a set of distance measures,
#'    returns trained models. Currenly, sits supports the following models:
#' 'svm' (see \code{\link[sits]{sits_svm}}), 'random forest' (see \code{\link[sits]{sits_rfor}}),
#' 'boosting' (see \code{\link[sits]{sits_gbm}}), 'lda' (see \code{\link[sits]{sits_lda}}),
#' 'qda' (see \code{\link[sits]{sits_qda}}), multinomial logit' (see \code{\link[sits]{sits_mlr}}),
#' 'lasso' (see \code{\link[sits]{sits_mlr}}), 'ridge' (see \code{\link[sits]{sits_mlr}}),
#' and "deep learning" (see \code{\link[sits]{sits_deeplearning}})
#' The sits_train function is called inside \code{\link[sits]{sits_classify}}
#' and \code{\link[sits]{sits_classify_raster}}, so the user does not need
#' to explicitly use this function. Please see the above-mention classification functions.
#'
#' @param  data.tb          time series with the training samples
#' @param  ml_method        machine learning method that returns a model for prediction
#' @return result           model fitted to input data given by train_method parameter
#'
#' @examples
#'
#'\donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' # find a training model based on the distances and default values (SVM model)
#' samples.tb <- sits_select(samples_MT_9classes, bands = c("ndvi", "evi", "nir", "mir"))
#' ml_model <- sits_train (samples.tb, sits_rfor())
#' # get a point and classify the point with the ml_model
#' point.tb <- sits_select(point_MT_6bands, bands = c("ndvi", "evi", "nir", "mir"))
#' class.tb <- sits_classify(point_MT_6bands, ml_model)
#' sits_plot(class.tb)
#' }
#' @export
sits_train <- function(data.tb, ml_method = sits_svm()) {

    # is the input data a valid SITS tibble?
    ensurer::ensure_that(data.tb, "label" %in% names(.),
                         err_desc = "sits_train: input data does not contain a valid SITS tibble")

    # is the train method a function?
    ensurer::ensure_that(ml_method, class(.) == "function",
                         err_desc = "sits_train: ml_method is not a valid function")

    # compute the training method by the given data
    result <- ml_method(data.tb)

    # return a valid machine learning method
    return(result)
}

#' @title Train a SITS classifiction model using the keras deep learning
#' @name sits_deeplearning
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Use a deeplearning algorithm to classify data.
#' This function is a front-end to the "keras" method R package.
#' Please refer to the documentation in that package for more details.
#'
#' @param data.tb           time series with the training samples
#' @param normalize         FALSE = no normalization, TRUE = normalize per band
#' @param units             vector with the number of hidden nodes in each hidden layer
#' @param activation        vector with the names of activation functions. Valid values are {'relu', 'elu', 'selu', 'sigmoid'}
#' @param dropout_rates     vector with the dropout rates (0,1) for each layer to the next layer
#' @param optimizer         function with a pointer to the optimizer function (default is optimization_adam()).
#'                          Options are optimizer_adadelta(), optimizer_adagrad(), optimizer_adam(),
#'                          optimizer_adamax(), optimizer_nadam(), optimizer_rmsprop(), optimizer_sgd()
#' @param epochs            number of iterations to train the model.
#' @param batch_size        number of samples per gradient update.
#' @param validation_split  number between 0 and 1. Fraction of the training data to be used as validation data.
#'                          The model will set apart this fraction of the training data, will not train on it,
#'                          and will evaluate the loss and any model metrics on this data at the end of each epoch.
#'                          The validation data is selected from the last samples in the x and y data provided,
#'                          before shuffling.
#' @param binary_classification a lenght-one logical indicating if this is a binary classification. If it is so,
#'                          the number of unique labels in the training data must be two as well.
#' @return result           either an model function to be passed in sits_predict or an function prepared that can be called further to compute multinom training model
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(samples_MT_ndvi)
#' # Build a machine learning model based on deep learning
#' dl_model <- sits_train (samples_MT_ndvi, sits_deeplearning(epochs = 50))
#' # get a point with a 16 year time series
#' data(point_ndvi)
#' # classify the point
#' class.tb <- sits_classify (point_ndvi, dl_model)
#' }
#' @export
sits_deeplearning <- function(data.tb          = NULL,
                              normalize        = TRUE,
                              units            = c(512, 512, 512, 512, 512),
                              activation       = 'elu',
                              dropout_rates    = c(0.50, 0.40, 0.35, 0.30, 0.20),
                              optimizer        = keras::optimizer_adam(lr = 0.001),
                              epochs           = 500,
                              batch_size       = 128,
                              validation_split = 0.2,
                              binary_classification = FALSE)
{

    # function that returns keras model based on a sits sample data.table
    result_fun <- function(data.tb){

        if (normalize) {
            stats.tb <- .sits_normalization_param(data.tb)
            train_data_DT <- sits_distances(.sits_normalize_data(data.tb, stats.tb))
        }
        else
            train_data_DT <- sits_distances(data.tb)

        # is the train data correct?
        ensurer::ensure_that(train_data_DT, "reference" %in% names(.),
                             err_desc = "sits_deeplearning: input data does not contain distance")

        ensurer::ensure_that(units, length(.) == length(dropout_rates),
                             err_desc = "sits_deeplearning: number of units must match number of dropout rates")

        ensurer::ensure_that(activation, length(.) == length(dropout_rates) || length(.) == 1,
                             err_desc = "sits_deeplearning: activation vectors should be one string or a
                             set of strings that match the number of units")

        # get the labels of the data
        labels <- as.vector(unique(train_data_DT$reference))

        # create a named vector with integers match the class labels
        int_labels <- c(1:length(labels))
        names(int_labels) <- labels
        n_labels <- length(labels)

        # split the data into training and validation
        # create partitions different splits of the input data
        test_data_DT <- .sits_sample_distances(train_data_DT, frac = validation_split)

        # remove the lines used for validation
        train_data_DT <- train_data_DT[!test_data_DT, on = "original_row"]

        # shuffle the data
        train_data_DT <- train_data_DT[sample(nrow(train_data_DT), nrow(train_data_DT)),]
        test_data_DT  <- test_data_DT[sample(nrow(test_data_DT), nrow(test_data_DT)),]

        # organize data for model training
        train.x <- data.matrix(train_data_DT[, -(1:2)])
        train.y <- unname(int_labels[as.vector(train_data_DT$reference)]) - 1


        # create the test data for keras
        test.x <- data.matrix(test_data_DT[, -(1:2)])
        test.y <- unname(int_labels[as.vector(test_data_DT$reference)]) - 1


        # set the activation vector
        act_vec <- vector()
        #
        for (i in 1:length(units)) {
            if (length(activation) == 1)
                act_vec[i] <- activation
            else
                act_vec <- activation
        }

        # build the model step by step
        # create the input_tensor
        input_tensor <- keras::layer_input(shape = c(NCOL(train.x)))
        output_tensor <-  input_tensor

        # build the nodes
        for (i in 1:length(units)) {
            output_tensor <- keras::layer_dense(output_tensor, units = units[i], activation = act_vec[i])
            output_tensor <- keras::layer_dropout(output_tensor, rate = dropout_rates[i])
            output_tensor <- keras::layer_batch_normalization(output_tensor)
        }
        # create the final tensor
        model_loss <- ""
        if (binary_classification && n_labels == 2) {
            output_tensor <- keras::layer_dense(output_tensor, units = 1, activation = "sigmoid")
            model_loss <- "binary_crossentropy"
        }else{
            output_tensor <- keras::layer_dense(output_tensor, units = n_labels, activation = "softmax")
            model_loss <- "categorical_crossentropy"
            # keras requires categorical data to be put in a matrix
            train.y <- keras::to_categorical(train.y, n_labels)
            test.y <- keras::to_categorical(test.y, n_labels)
        }
        # create the model
        model.keras <- keras::keras_model(input_tensor, output_tensor)
        # compile the model
        model.keras %>% keras::compile(
            loss = model_loss,
            optimizer = optimizer,
            metrics = c("accuracy")
        )
        # fit the model
        history <- model.keras %>% keras::fit(
            train.x, train.y,
            epochs = epochs, batch_size = batch_size,
            validation_data = list(test.x, test.y)
        )

        # construct model predict enclosure function and returns
        model_predict <- function(values.tb){
            values.x    <- data.matrix(values.tb[, -(1:2)])
            preds       <- stats::predict(model.keras, values.x)
            pred.labels <- names(int_labels[max.col(preds)])
            return(pred.labels)
        }
        return(model_predict)
    }

    result <- .sits_factory_function(data.tb, result_fun)
    return(result)
}
#' @title Train a SITS classification model with a gradient boosting machine
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
#' @param data.tb          time series with the training samples
#' @param normalize        (integer) 0 = no normalization, 1 = normalize per band, 2 = normalize per dimension
#' @param formula          a symbolic description of the model to be fit. SITS offers a set of such formulas (default: sits_formula_logref)
#' @param distribution     name of the distribution - use "multinomial" for classification
#' @param n.trees          Number of trees to fit. This should not be set to too small a number,
#'                         to ensure that every input row gets predicted at least a few times. (default: 5000)
#' @param interaction.depth  The maximum depth of variable interactions. 1 implies an additive model, 2 implies a model with up to 2-way interactions.
#' @param shrinkage        a shrinkage parameter applied to each tree in the expansion.
#'                         Also known as the learning rate or step-size reduction.
#' @param cv.folds         number of cross-validations to run
#' @param ...              other parameters to be passed to `gbm::gbm` function
#' @return result          a model function to be passed in sits_predict or an function prepared that can be called further to compute multinom training model
#'
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(samples_MT_ndvi)
#' # Build a GBM model
#' gbm_model <- sits_train(data.tb, sits_gbm())
#' # get a point with a 16 year time series
#' data(point_ndvi)
#' # classify the point
#' class.tb <- sits_classify (point_ndvi, samples_MT_ndvi, gbm_model)
#' }
#' @export
sits_gbm <- function(data.tb = NULL, normalize = TRUE, formula = sits_formula_logref(), distribution = "multinomial",
                     n.trees = 500, interaction.depth = 2, shrinkage = 0.001, cv.folds = 5, ...) {

    # function that returns glmnet::multinom model based on a sits sample tibble
    result_fun <- function(data.tb){

        if (normalize) {
            stats.tb <- .sits_normalization_param(data.tb)
            train_data_DT <- sits_distances(.sits_normalize_data(data.tb, stats.tb))
        }
        else
            train_data_DT <- sits_distances(data.tb)

        # if parameter formula is a function call it passing as argument the input data sample. The function must return a valid formula.
        if (class(formula) == "function")
            formula <- formula(train_data_DT)

        # if the formula is log and data has not been normalized, adjust the values to avoid invalid logs
        sits.env$adjust <- FALSE
        if ( normalize == FALSE )
            train_data_DT <- .sits_formula_adjust(train_data_DT)

        # find the number of cores
        multicores <- parallel::detectCores(logical = FALSE)

        # call gbm::gbm method and return the trained multinom model

        result_gbm <- gbm::gbm(formula = formula, data = train_data_DT[, original_row := NULL],
                               distribution = distribution, n.trees = n.trees, interaction.depth = interaction.depth,
                               shrinkage = shrinkage, cv.folds = cv.folds, n.cores = multicores,...)

        # check performance using 5-fold cross-validation
        best.iter <- gbm::gbm.perf(result_gbm, method = "cv")

        # construct model predict enclosure function and returns
        model_predict <- function(values_DT){
            # shift the values by a fixed amount if required
            if (sits.env$adjust <- TRUE)
                values_DT <- .sits_shift_DT(values_DT, shift = sits.env$adjustment_shift)

            result <- stats::predict(result_gbm, newdata = values_DT, best.iter)
            return(colnames(result)[max.col(data.frame(result))])
        }
        return(model_predict)
    }

    result <- .sits_factory_function(data.tb, result_fun)
    return(result)
}

#' @title Train a SITS classification model using linear discriminant analysis
#' @name sits_lda
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function receives a tibble with a set of attributes X for each observation Y
#' These attributes are distance metrics between patterns and observations, obtained by a distance
#' function in SITS (see \code{\link[sits]{sits_distances}}).
#' The method performs a linear discriminant analysis (lda) to obtain a predictive model.
#' This function is a front-end to the "lda" method in the "MASS" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param data.tb          time series with the training samples
#' @param normalize        (boolean) 0 = no normalization, 1 = normalize per band,
#' @param formula          a symbolic description of the model to be fit. SITS offers a set of such formulas (default: sits_formula_logref)
#' @param ...              other parameters to be passed to MASS::lda function
#' @return result          a model function to be passed in sits_predict
#'
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(samples_MT_ndvi)
#' # Build an LDA model
#' lda_model <- sits_train(samples_MT_ndvi, sits_lda())
#' # get a point with a 16 year time series
#' data(point_ndvi)
#' # classify the point
#' class.tb <- sits_classify (point_ndvi, samples_MT_ndvi, lda_model)
#' }
#' @export
sits_lda <- function(data.tb = NULL, normalize = TRUE, formula = sits_formula_logref(), ...) {

    # function that returns MASS::lda model based on a sits sample tibble
    result_fun <- function(data.tb){

        # data normalization
        if (normalize) {
            stats.tb <- .sits_normalization_param(data.tb)
            train_data_DT <- sits_distances(.sits_normalize_data(data.tb, stats.tb))
        }
        else
            train_data_DT <- sits_distances(data.tb)

        # is the input data the result of a TWDTW matching function?
        ensurer::ensure_that(train_data_DT, "reference" %in% names(.),
                             err_desc = "sits_lda: input data does not contain distance")

        # if parameter formula is a function call it passing as argument the input data sample. The function must return a valid formula.
        if (class(formula) == "function")
            formula <- formula(train_data_DT)

        # if the formula is log and data has not been normalized, adjust the values to avoid invalid logs
        sits.env$adjust <- FALSE
        if (normalize == FALSE)
            train_data_DT <- .sits_formula_adjust(train_data_DT)

        # call MASS::lda method and return the trained lda model
        result_lda <- MASS::lda(formula = formula, data = train_data_DT, ..., na.action = stats::na.fail)

        # construct model predict enclosure function and returns
        model_predict <- function(values_DT){
            # shift the values by a fixed amount if using a log formula
            if (sits.env$adjust == TRUE)
                values_DT <- .sits_shift_DT(values_DT, shift = sits.env$adjustment_shift)

            return(stats::predict(result_lda, newdata = values_DT)$class)
        }
        return(model_predict)
    }

    result <- .sits_factory_function(data.tb, result_fun)
    return(result)
}
#' @title Train a SITS classification model using a liquidSVM package
#' @name sits_liquid_svm
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function receives a tibble with a set of attributes X for each observation Y
#' These attributes are distance metrics between patterns and observations, obtained by a distance
#' function in SITS (see \code{\link[sits]{sits_distances}}).
#' The SVM algorithm is used for multiclass-classification.
#' For this purpose, it uses the "one-against-one" approach, in which k(k-1)/2 binary
#' classifiers are trained; the appropriate class is found by a voting scheme.
#' This function is a front-end to the "svm" method in the "liquidSVM" package.
#' Please refer to the documentation in that package for more details.
#'
#' The grid_choice parameter is the most important one, since it controls the choice of hyperparameters
#' gamma (bandwidth of the kernel) and lambda (regularization).
#'
#' grid_choice   0         1        2
#' gamma_steps   10       15       20
#' lambda_steps  10       15       20
#' min_gamma     0.2     0.1     0.05
#' max_gamma     5.0     10.0    20.0
#' min_lambda    0.001   0.0001  0.00001
#' max_lambda    0.01    0.01    0.01
#'
#' Multiclass classification has to be reduced to binary classification.
#' There are two strategies for this:
#' all-vs-all (AvA): for every pairing of classes a binary SVM is trained
#' one-vs-all (OvA): for every class a binary SVM is trained with that class as one label
#'                   and all other classes are clumped together to another label
#' different combinations of AvA and OvA can be used with decision functions "hinge" and "ls" (least squares)

#'
#' @param data.tb          time series with the training samples
#' @param normalize        (boolean) 0 = no normalization, 1 = normalize per band
#' @param formula          symbolic description of the model to be fit. SITS offers a set of such formulas (default: sits_svm)
#' @param threads          number of cores for computing the kernel matrices (0 = uses all cores)
#' @param partition_choice determines the way the input space is partitioned (0 = disables, 5 = usually best)
#' @param gammas           vector of hyper-parameter used during training for width of kernel
#' @param c_values         vector of hyper-parameter used during training for regularization
#' @param folds            number of folds for model validation (default = 5)
#' @param mc_type          strategy for multiclass classification ("OvA_ls", "AvA_ls", "AvA_hinge")
#' @param predict.prob     if TRUE a LS-svm will be trained and conditional probs will be estimated
#' @param adaptivity_control  use adaptive search heuristic (default 0 - disables heuristic)
#' @param random_seed      seed for the random generator (default -1 uses the internal timer)
#' @param do.select        if TRUE also does the whole selection for this model
#' @param ...              other parameters to be passed to liquidSVM::svmMulticlass function
#' @return result          fitted model function to be passed to sits_predict
#'
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(samples_MT_ndvi)
#' # Build an SVM model
#' svm_model <- sits_train(samples_MT_ndvi, sits_liquid_svm())
#' # get a point
#' data(point_ndvi)
#' # classify the point
#' class.tb <- sits_classify (point_ndvi, svm_model)
#' # plot the classification
#' sits_plot(class.tb)
#'}
#' @export
sits_liquid_svm <- function(data.tb = NULL, normalize = TRUE, formula = sits_formula_logref(),
                            threads = 0, partition_choice = 0,
                            gammas = c(1, 0.04, 0.02, 0.01, 0.005, 0.0025),
                            c_values = c(1, 10, 100, 200, 500, 1000),
                            folds = 5, adaptivity_control = 0, random_seed = -1,
                            mc_type = "AvA_hinge", predict.prob = FALSE, useCells = TRUE, do.select = TRUE, ...) {

    # function that returns liquidSVM::svm model based on a sits sample tibble
    result_fun <- function(data.tb){

        # data normalization
        if (normalize) {
            stats.tb <- .sits_normalization_param(data.tb)
            train_data_DT <- sits_distances(.sits_normalize_data(data.tb, stats.tb))
        }
        else
            train_data_DT <- sits_distances(data.tb)

        # if parameter formula is a function call it passing as argument the input data sample.
        # The function must return a valid formula.
        if (class(formula) == "function")
            formula <- formula(train_data_DT)

        # liquidSVM requires cols to be labelled as Y, X1, X2,...Xn
        ncols_X <- ncol(train_data_DT) - 2
        attr_names_X <- c(paste0("X", as.character(c(1:ncols_X))))
        attr_names <- c("original_row", "Y", attr_names_X)
        colnames(train_data_DT) <- attr_names

        # we need to adjust the formula to account for liquidSVM's format
        if (sits.env$model_formula == "log")
            formula_svm <- stats::as.formula(paste0("factor(Y)~", paste0(paste0('log(`', attr_names_X, '`)'), collapse = "+")))
        else if (sits.env$model_formula == "linear")
            formula_svm <- stats::as.formula(paste0("factor(Y)~", paste0(paste0(attr_names_X), collapse = "+")))
        else
            formula_svm <- stats::as.formula(paste0("factor(Y)~", paste0(paste0('s(`', attr_names_X, '`)'), collapse = "+")))

        # if the formula is log and data has not been normalized, adjust the values to avoid invalid logs
        sits.env$adjust <- FALSE
        if (normalize == FALSE)
            train_data_DT <- .sits_formula_adjust(train_data_DT)

        # call liquidSVM::svmMulticlass method and return the trained svm model
        result_svm <- liquidSVM::svmMulticlass(x = formula_svm, y = train_data_DT[,2:ncol(train_data_DT)],
                                               threads = threads, partition_choice = partition_choice,
                                               gammas = gammas, c_values = c_values,
                                               folds = folds, mc_type = mc_type,
                                               adaptivity_control = adaptivity_control, random_seed =  random_seed,
                                               predict.prob = predict.prob, do.select = do.select, useCells = useCells)

        # construct model predict enclosure function and returns
        model_predict <- function(values_DT){
            if (sits.env$adjust == TRUE)
                values_DT <- .sits_shift_DT(values_DT, shift = sits.env$adjustment_shift)
            return(stats::predict(result_svm, values_DT))
        }
        return(model_predict)
    }

    result <- .sits_factory_function(data.tb, result_fun)
    return(result)
}

#' @title Train a SITS classification model using quadratic discriminant analysis
#' @name sits_qda
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function receives a tibble with a set of attributes X for each observation Y
#' These attributes are distance metrics between patterns and observations, obtained by a distance
#' function in SITS (see \code{\link[sits]{sits_distances}}).
#' The function performs a quadratic discriminant analysis (qda) to obtain a predictive model.
#' This function is a front-end to the "qda" method in the "MASS" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param data.tb          time series with the training samples
#' @param normalize        (integer) 0 = no normalization, 1 = normalize per band, 2 = normalize per dimension
#' @param formula          symbolic description of the model to be fit. SITS offers a set of such formulas (default: sits_formula_logref)
#' @param ...              other parameters to be passed to MASS::qda function
#' @return result          a model function to be passed in sits_predict
#'
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(samples_MT_ndvi)
#' # Build a QDA model
#' qda_model <- sits_train(samples_MT_ndvi, sits_qda())
#' # get a point with a 16 year time series
#' data(point_ndvi)
#' # classify the point
#' class.tb <- sits_classify (point_ndvi, qda_model)
#' }
#' @export
sits_qda <- function(data.tb = NULL, normalize = TRUE, formula = sits_formula_logref(), ...) {

    # function that returns MASS::qda model based on a sits sample tibble
    result_fun <- function(data.tb){

        # data normalization
        if (normalize) {
            stats.tb <- .sits_normalization_param(data.tb)
            train_data_DT <- sits_distances(.sits_normalize_data(data.tb, stats.tb))
        }
        else
            train_data_DT <- sits_distances(data.tb)

        # if parameter formula is a function call it passing as argument the input data sample. The function must return a valid formula.
        if (class(formula) == "function")
            formula <- formula(train_data_DT)

        # if the formula is log and data has not been normalized, adjust the values to avoid invalid logs
        sits.env$adjust <- FALSE
        if (normalize == 0)
            train_data_DT <- .sits_formula_adjust(train_data_DT)

        # call MASS::qda method and return the trained lda model
        result_qda <- MASS::qda(formula = formula, data = train_data_DT, ..., na.action = stats::na.fail)

        # construct model predict enclosure function and returns
        model_predict <- function(values_DT){
            # shift the values by a fixed amount if using a log formula
            if (sits.env$adjust == TRUE)
                values_DT <- .sits_shift_DT(values_DT, shift = sits.env$adjustment_shift)

            return(stats::predict(result_qda, newdata = values_DT)$class)
        }
        return(model_predict)
    }

    result <- .sits_factory_function(data.tb, result_fun)
    return(result)
}

#' @title Train a SITS classifiaction model using multinomial log-linear regions via neural networks
#' @name sits_mlr
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Use multinomial log-linear (mlr) fitting model via neural networks to classify data.
#' These attributes are distance metrics between patterns and observations, obtained by a distance
#' function in SITS (see \code{\link[sits]{sits_distances}}).
#' This function is a front-end to the "multinom" method in the "nnet" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param data.tb          time series with the training samples
#' @param normalize        (integer) 0 = no normalization, 1 = normalize per band, 2 = normalize per dimension
#' @param formula          symbolic description of the model to be fit. SITS offers a set of such formulas (default: sits_formula_logref)
#' @param n_weights        maximum number of weights (should be proportional to size of input data)
#' @param maxit            maximum number of iterations (default 300)
#' @param ...              other parameters to be passed to nnet::multinom function
#' @return result          a model function to be passed in sits_predict
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(samples_MT_ndvi)
#' # Build an MLR model
#' mlr_model <- sits_train(samples_MT_ndvi, sits_mlr())
#' # get a point with a 16 year time series
#' data(point_ndvi)
#' # classify the point
#' class.tb <- sits_classify (point_ndvi, mlr_model)
#' }
#' @export
sits_mlr <- function(data.tb = NULL, normalize = TRUE, formula = sits_formula_linear(),
                     n_weights = 20000, maxit = 2000, ...) {

    # function that returns nnet::multinom model based on a sits sample tibble
    result_fun <- function(data.tb) {

        # data normalization
        if (normalize) {
            stats.tb <- .sits_normalization_param(data.tb)
            train_data_DT <- sits_distances(.sits_normalize_data(data.tb, stats.tb))
        }
        else
            train_data_DT <- sits_distances(data.tb)

        # if parameter formula is a function call it passing as argument the input data sample. The function must return a valid formula.
        if (class(formula) == "function")
            formula <- formula(train_data_DT)

        # if the formula is log and data has not been normalized, adjust the values to avoid invalid logs
        sits.env$adjust <- FALSE
        if (normalize == FALSE)
            train_data_DT <- .sits_formula_adjust(train_data_DT)

        # call nnet::multinom method and return the trained multinom model
        result_mlr <- nnet::multinom(formula = formula,
                                     data = train_data_DT,
                                     maxit = maxit,
                                     MaxNWts = n_weights, ..., na.action = stats::na.fail)

        # construct model predict enclosure function and returns
        model_predict <- function(values_DT){
            # adjust the values in the case of a logarithm model formula
            if (sits.env$adjust == TRUE)
                values_DT <- .sits_shift_DT(values_DT, shift = sits.env$adjustment_shift)

            result <- stats::predict(result_mlr, newdata = values_DT)
            return(result)
        }
        return(model_predict)
    }

    result <- .sits_factory_function(data.tb, result_fun)
    return(result)
}
#' @title Train a SITS classifiction model using fast random forest algorithm
#' @name sits_rfor
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Use Fast Random Forest algorithm to classify data.
#' This function is a front-end to the "ranger" method in the "ranger" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param data.tb          time series with the training samples
#' @param num.trees        number of trees to grow. This should not be set to too small a number,
#'                         to ensure that every input row gets predicted at least a few times. (default: 2000)
#' @param ...              other parameters to be passed to `ranger::ranger` function
#' @return result          either an model function to be passed in sits_predict or an function prepared that can be called further to compute multinom training model
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(samples_MT_ndvi)
#' # Build a random forest model
#' rfor_model <- sits_train(samples_MT_ndvi, sits_rfor())
#' # get a point with a 16 year time series
#' data(point_ndvi)
#' # classify the point
#' class.tb <- sits_classify (point_ndvi, rfor_model)
#' }
#' @export
sits_rfor <- function(data.tb = NULL, num.trees = 2000, ...) {

    # function that returns `randomForest::randomForest` model based on a sits sample tibble
    result_fun <- function(data.tb){

        # calculate the distances
        train_data_DT <- sits_distances(data.tb)

        # if parameter formula is a function call it passing as argument the input data sample. The function must return a valid formula.
        formula <- sits_formula_linear()(train_data_DT)

        # call `ranger::ranger` method and return the trained model
        result_ranger <- ranger::ranger(formula = formula,
                                      data = train_data_DT[,2:ncol(train_data_DT)],
                                      num.trees = num.trees, min.node.size = 1, ...)

        # construct model predict enclosure function and returns
        model_predict <- function(values_DT){
            return(stats::predict(result_ranger, data = values_DT, type = "response")$predictions)
        }
        return(model_predict)
    }

    result <- .sits_factory_function(data.tb, result_fun)
    return(result)
}

#' @title Train a SITS classification model using a support vector machine
#' @name sits_svm
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function receives a tibble with a set of attributes X for each observation Y
#' These attributes are distance metrics between patterns and observations, obtained by a distance
#' function in SITS (see \code{\link[sits]{sits_distances}}).
#' The SVM algorithm is used for multiclass-classification.
#' For this purpose, it uses the "one-against-one" approach, in which k(k-1)/2 binary
#' classifiers are trained; the appropriate class is found by a voting scheme.
#' This function is a front-end to the "svm" method in the "e1071" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param data.tb          time series with the training samples
#' @param normalize        (boolean) 0 = no normalization, 1 = normalize per band
#' @param formula          symbolic description of the model to be fit. SITS offers a set of such formulas (default: sits_svm)
#' @param scale            A logical vector indicating the variables to be scaled.
#' @param kernel           kernel used in training and predicting (options = linear, polynomial, radial basis, sigmoid)
#' @param degree           exponential of polynomial type kernel
#' @param coef0	           parameter needed for kernels of type polynomial and sigmoid (default: 0)
#' @param cost             cost of constraints violation
#' @param tolerance	       tolerance of termination criterion (default: 0.001)
#' @param epsilon	       epsilon in the insensitive-loss function (default: 0.1)
#' @param cross            number of cross validation folds applied on the training data to assess the quality of the model,
#' @param ...              other parameters to be passed to e1071::svm function
#' @return result          fitted model function to be passed to sits_predict
#'
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(samples_MT_ndvi)
#' # Build an SVM model
#' svm_model <- sits_train(samples_MT_ndvi, sits_svm(normalize = 0))
#' # get a point
#' data(point_ndvi)
#' # classify the point
#' class.tb <- sits_classify (point_ndvi, svm_model)
#' # plot the classification
#' sits_plot(class.tb)
#'}
#' @export
sits_svm <- function(data.tb = NULL, normalize = TRUE, formula = sits_formula_logref(), scale = FALSE,
                     kernel = "radial", degree = 3, coef0 = 0, cost = 10, tolerance = 0.001, epsilon = 0.1, cross = 0, ...) {

    # function that returns e1071::svm model based on a sits sample tibble
    result_fun <- function(data.tb){

        # data normalization
        if (normalize) {
            stats.tb <- .sits_normalization_param(data.tb)
            train_data_DT <- sits_distances(.sits_normalize_data(data.tb, stats.tb))
        }
        else
            train_data_DT <- sits_distances(data.tb)

        # if parameter formula is a function call it passing as argument the input data sample.
        # The function must return a valid formula.
        if (class(formula) == "function")
            formula <- formula(train_data_DT)

        # if the formula is log and data has not been normalized, adjust the values to avoid invalid logs
        sits.env$adjust <- FALSE
        if (normalize == FALSE)
            train_data_DT <- .sits_formula_adjust(train_data_DT)

        # call e1071::svm method and return the trained svm model
        result_svm <- e1071::svm(formula = formula, data = train_data_DT, kernel = kernel,
                                 degree = degree, cost = cost, coef0 = coef0,
                                 tolerance = tolerance, epsilon = epsilon, cross = cross, ..., na.action = stats::na.fail)

        # construct model predict enclosure function and returns
        model_predict <- function(values_DT){
            if (sits.env$adjust == TRUE)
                values_DT <- .sits_shift_DT(values_DT, shift = sits.env$adjustment_shift)
            return(stats::predict(result_svm, newdata = values_DT))
        }
        return(model_predict)
    }

    result <- .sits_factory_function(data.tb, result_fun)
    return(result)
}


#' @title Provides access to diagnostic information about a Keras deep learning model
#' @name sits_keras_diagnostics
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description After the Keras deeplearning model is compiled and fit, this
#'              function provides access to the history plot and the evaluation results
#'
#' @param dl_model  A valid keras model
#'
#' @return NULL   Prints the model diagnostics
#'
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(cerrado2classes)
#'  # obtain a DL model
#' dl_model <- sits_train(cerrado2classes,
#'      sits_deeplearning(units = c(512, 512),
#'      dropout_rates = c(0.45, 0.25)))
#' # run the keras diagnostics
#' sits_keras_diagnostics(dl_model)
#' }
#' @export
sits_keras_diagnostics <- function(dl_model) {

    if (purrr::is_null(environment(dl_model)$model.keras)) {
        message("Please configure a keras model before running this function")
        return(FALSE)
    }

    message("Plotting history of the model fit")
    graphics::plot(environment(dl_model)$history)

    test_eval <- keras::evaluate(environment(dl_model)$model.keras, environment(dl_model)$test.x, environment(dl_model)$test.y, verbose = 0)
    message("Estimated loss and accuracy based on test data")
    message(paste0("Estimated accuracy: ", round(test_eval$acc, digits = 3),
                   " estimated loss: ", round(test_eval$loss, digits = 3)))
    return(TRUE)
}

#' @title Define a loglinear formula for classification models
#' @name sits_formula_logref
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description A function to be used as a symbolic description of some fitting models such as svm, lda, qda, and gbm.
#' This function instructs the model to do a logarithm transformation of the input values.
#' The `predictors_index` parameter informs the positions of `tb` fields corresponding to formula independent variables.
#' If no value is given, the default is NULL, a value indicating that all fields will be used as predictors.
#'
#' @param predictors_index  index of the valid columns whose names are used to compose formula (default: NULL)
#' @return result_fun       a function that computes a valid formula
#'
#' @export
sits_formula_logref <- function(predictors_index = -2:0){

    # store configuration information about model formula
    sits.env$model_formula <- "log"

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

#' @title Define a linear formula for classification models
#' @name sits_formula_linear
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description A function to be used as a symbolic description of some fitting models such as svm.
#' This function instructs the model to do a linear transformation of the input values.
#' The `predictors_index` parameter informs the positions of `tb` fields corresponding to formula independent variables.
#' If no value is given, the default is NULL, a value indicating that all fields will be used as predictors.
#'
#' @param predictors_index  index of the valid columns whose names are used to compose formula (default: NULL)
#' @return result_fun       a function that computes a valid formula
#'
#' @export
sits_formula_linear <- function(predictors_index = -2:0){

    # store configuration information about model formula
    sits.env$model_formula <- "linear"

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

#' @title Define a smoothing formula for classification models
#' @name sits_formula_smooth
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description A function to be used as a symbolic description of some fitting models such as gam.
#' This function instructs the model to do a smoothing transformation (via splines) of the input values.
#' `predictors_index` parameter informs the positions of `tb` fields corresponding to formula independent variables.
#' If no value is given, the default is NULL, a value indicating that all fields will be used as predictors.
#'
#' @param predictors_index  index of the valid columns whose names are used to compose formula (default: NULL)
#' @return result_fun       a function that computes a valid formula
#'
#' @export
sits_formula_smooth <- function(predictors_index = -2:0){

    # store configuration information about model formula
    sits.env$model_formula <-  "smooth"

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
#' @name .sits_predict
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Given a SITS tibble time series and a model trained by \code{\link[sits]{sits_train}},
#'   returns a predicted label. Note that the \code{\link[sits]{.sits_predict}} function is
#'   called inside \code{\link[sits]{sits_classify}}, \code{\link[sits]{sits_classify_model}}
#'   and \code{\link[sits]{sits_classify_raster}}, so the user does not need
#'   to explicitly use it. Please see the above-mentioned classification functions.
#'
#' @param distances_DT  set of distance metrics to each of the classes
#' @param ml_model      model trained by \code{\link[sits]{sits_train}}
#' @param ...           other parameters to be passed to the model function
#' @return predicted    vector of predicted labels
.sits_predict <- function(distances_DT = NULL, ml_model, ...){

    predicted <- as.character(ml_model(distances_DT))
    return(predicted)
}

#' @title Adjust training values if the model formula uses log
#' @name .sits_formula_adjust
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description If the model formula is using logs, it is important to correct for negative values in the data
#'
#' @param train_data_DT     training data
#' @return train_data_DT    adjusted training data
#'

.sits_formula_adjust <- function(train_data_DT){

    # shift the values by a fixed amount if using a log formula
    if (sits.env$model_formula == "log") {
        # find the minimum value in the training data
        min <- train_data_DT[, min(.SD), .SDcols = colnames(train_data_DT[,3:length(train_data_DT)]) ]
        # estimate a shift
        shift <- ceiling(sits.env$config$default_adjustment_shift - min)
        # store the shift for later use
        sits.env$adjustment_shift <- shift
        # store the information that data has been adjusted
        sits.env$adjust <- TRUE
        # shift the training data
        train_data_DT <- .sits_shift_DT(train_data_DT, shift)
    }
    return(train_data_DT)
}
