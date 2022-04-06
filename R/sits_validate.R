#' @title Cross-validate time series samples
#' @name sits_kfold_validate
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Splits the set of time series into training and validation and
#' perform k-fold cross-validation.
#' Cross-validation is a technique for assessing how the results
#' of a statistical analysis will generalize to an independent data set.
#' It is mainly used in settings where the goal is prediction,
#' and one wants to estimate how accurately a predictive model will perform.
#' One round of cross-validation involves partitioning a sample of data
#' into complementary subsets, performing the analysis on one subset
#' (called the training set), and validating the analysis on the other subset
#' (called the validation set or testing set).
#'
#' The k-fold cross validation method involves splitting the dataset
#' into k-subsets. For each subset is held out while the model is trained
#' on all other subsets. This process is completed until accuracy
#' is determine for each instance in the dataset, and an overall
#' accuracy estimate is provided.
#'
#' This function returns the confusion matrix, and Kappa values.
#'
#' @param data               Time series.
#' @param samples_validation Validation time series.
#' @param folds              Number of partitions to create.
#' @param ml_method          Machine learning method.
#' @param multicores         Number of cores to process in parallel.
#'
#' @return A tibble containing pairs of reference and predicted values.
#'
#' @examples
#' \donttest{
#' # read a set of samples
#' data(cerrado_2classes)
#' # two fold validation with random forest
#' acc <- sits_kfold_validate(cerrado_2classes,
#'   folds = 2,
#'   ml_method = sits_rfor(num_trees = 100)
#' )
#' }
#' @export
#'
sits_kfold_validate <- function(data,
                                folds = 5,
                                ml_method = sits_rfor(),
                                multicores = 2) {

    # set caller to show in errors
    .check_set_caller("sits_kfold_validate")
    # require package
    if (!requireNamespace("caret", quietly = TRUE)) {
        stop("Please install package caret", call. = FALSE)
    }

    # pre-condition
    .check_that(
        inherits(ml_method, "function"),
        local_msg = "ml_method is not a valid sits method",
        msg = "invalid ml_method parameter"
    )

    # pre-condition
    .check_num(multicores, min = 1, is_integer = TRUE,
               msg = "Invalid multicores parameter")

    # get the labels of the data
    labels <- sits_labels(data)

    # create a named vector with integers match the class labels
    n_labels <- length(labels)
    int_labels <- c(1:n_labels)
    names(int_labels) <- labels

    # is the data labelled?
    .check_that(
        x = !("NoClass" %in% sits_labels(data)),
        msg = "requires labelled set of time series"
    )

    # create partitions different splits of the input data
    data <- .sits_create_folds(data, folds = folds)

    # create prediction and reference vector
    pred_vec <- character()
    ref_vec <- character()

    # start parallel process
    if (multicores > folds)
        multicores <- folds

    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop())

    conf_lst <- .sits_parallel_map(seq_len(folds), function(k) {

        # split data into training and test data sets
        data_train <- data[data$folds != k, ]
        data_test <- data[data$folds == k, ]

        # create a machine learning model
        ml_model <- sits_train(data_train, ml_method)

        # has normalization been applied to the data?
        stats <- environment(ml_model)$stats

        # obtain the distances after normalizing data by band
        if (!purrr::is_null(stats)) {
            distances <- .sits_distances(
                .sits_ml_normalize_data(data_test, stats)
            )
        } else {
            distances <- .sits_distances(data_test)
        }

        # classify the test data
        prediction <- ml_model(distances)

        # extract the values
        values <- names(int_labels[max.col(prediction)])

        ref_vec <- c(ref_vec, data_test$label)
        pred_vec <- c(pred_vec, values)
        remove(ml_model)

        return(list(pred = pred_vec, ref = ref_vec))
    }, n_retries = 0, progress = FALSE)

    pred <- unlist(lapply(conf_lst, function(x) x$pred))
    ref <- unlist(lapply(conf_lst, function(x) x$ref))

    # call caret to provide assessment
    unique_ref <- unique(ref)
    pred_fac <- factor(pred, levels = unique_ref)
    ref_fac <- factor(ref, levels = unique_ref)

    # call caret package to the classification statistics
    assess <- caret::confusionMatrix(pred_fac, ref_fac)

    class(assess) <- c("sits_assessment", class(assess))

    return(assess)
}

#' @rdname sits_kfold_validate
#' @export
sits_validate <- function(data, ...,
                          samples_validation = NULL,
                          validation_split = 0.2,
                          ml_method = sits_rfor()) {

    # set caller to show in errors
    .check_set_caller("sits_validate")
    # require package
    if (!requireNamespace("caret", quietly = TRUE)) {
        stop("Please install package caret", call. = FALSE)
    }

    # pre-condition
    .check_that(
        inherits(ml_method, "function"),
        local_msg = "ml_method is not a valid sits method",
        msg = "invalid ml_method parameter"
    )

    # get the labels of the data
    labels <- sits_labels(data)

    # create a named vector with integers match the class labels
    n_labels <- length(labels)
    int_labels <- c(1:n_labels)
    names(int_labels) <- labels

    # is the data labelled?
    .check_that(
        x = !("NoClass" %in% sits_labels(data)),
        msg = "requires labelled set of time series"
    )

    if (is.null(samples_validation)) {
        data <- .sits_samples_split(
            samples = data,
            validation_split = validation_split
        )
        samples_validation <- dplyr::filter(data, !.data[["train"]])
        data <- dplyr::filter(data, .data[["train"]])
    }

    # create a machine learning model
    ml_model <- sits_train(data, ml_method)

    # has normalization been applied to the data?
    stats <- environment(ml_model)$stats

    # obtain the distances after normalizing data by band
    if (!purrr::is_null(stats)) {
        distances <- .sits_distances(
            .sits_ml_normalize_data(samples_validation, stats)
        )
    } else {
        distances <- .sits_distances(samples_validation)
    }

    # classify the test data
    prediction <- ml_model(distances)

    # extract the values
    # create prediction and reference vector
    pred <- names(int_labels[max.col(prediction)])
    ref <- samples_validation[["label"]]

    # call caret to provide assessment
    unique_ref <- unique(ref)
    pred_fac <- factor(pred, levels = unique_ref)
    ref_fac <- factor(ref, levels = unique_ref)

    # call caret package to the classification statistics
    assess <- caret::confusionMatrix(pred_fac, ref_fac)

    class(assess) <- c("sits_assessment", class(assess))

    return(assess)
}
#' @title Create partitions of a data set
#' @name  .sits_create_folds
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Alexandre Ywata, \email{alexandre.ywata@@ipea.gov.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Split a sits tibble into k groups, based on the label.
#'
#' @keywords internal
#' @param data   A sits tibble to be partitioned.
#' @param folds     Number of folds
#'
.sits_create_folds <- function(data, folds = 5) {
    # verify if data exists
    .sits_tibble_test(data)
    # splits the data into k groups
    data$folds <- caret::createFolds(data$label,
                                     k = folds,
                                     returnTrain = FALSE, list = FALSE
    )
    return(data)
}
