#' @title Cross-validate temporal patterns
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
#' @param data            A sits tibble.
#' @param folds           Number of partitions to create.
#' @param ml_method       Machine learning method.
#' @param multicores      Number of cores for processing.
#' @return A tibble containing pairs of reference and predicted values.
#'
#' @examples
#' \donttest{
#' # read a set of samples
#' data(cerrado_2classes)
#' # perform a five fold validation with the SVM machine learning method
#' conf_matrix1.mx <- sits_kfold_validate(cerrado_2classes)
#' # print the confidence matrix
#' sits_conf_matrix(conf_matrix1.mx)
#' }
#' @export
sits_kfold_validate <- function(data, folds = 5,
                                ml_method = sits_rfor(), multicores = 1){

    # backward compatibility
    data <- .sits_tibble_rename(data)

    # get the labels of the data
    labels <- sits_labels(data)$label

    # create a named vector with integers match the class labels
    n_labels <- length(labels)
    int_labels <- c(1:n_labels)
    names(int_labels) <- labels

    # is the data labelled?
    assertthat::assert_that(!("NoClass" %in% sits_labels(data)$label),
        msg = "sits_cross_validate: requires labelled set of time series")

    # create partitions different splits of the input data
    data <- .sits_create_folds(data, folds = folds)

    # create prediction and reference vector
    pred.vec <-  character()
    ref.vec  <-  character()

    conf.lst <- parallel::mclapply(X = 1:folds, FUN = function(k)
    {
        # split data into training and test data sets
        data_train <- data[data$folds != k,]
        data_test  <- data[data$folds == k,]

        # create a machine learning model
        ml_model <- sits_train(data_train, ml_method)

        # has normalization been applied to the data?
        stats   <- environment(ml_model)$stats

        # obtain the distances after normalizing data by band
        if (!purrr::is_null(stats))
            distances_DT <- .sits_distances(
                .sits_normalize_data(data_test, stats, multicores))
        else
            distances_DT <- .sits_distances(data_test)

        # classify the test data
        prediction_DT <- ml_model(distances_DT)
        # extract the values
        values <-  names(int_labels[max.col(prediction_DT)])

        ref.vec  <- c(ref.vec,  data_test$label)
        pred.vec <- c(pred.vec, values)

        return(list(pred = pred.vec, ref = ref.vec))
    }, mc.cores = multicores)

    pred.vec <- unlist(lapply(conf.lst, function(x) x$pred))
    ref.vec  <- unlist(lapply(conf.lst, function(x) x$ref))

    pred_ref.tb <- tibble::tibble("predicted" = pred.vec,
                                  "reference" = ref.vec)
    class(pred_ref.tb) <- append(class(pred_ref.tb),
                                 c("sits", "sits_val_tbl"), after = 0)

    return(pred_ref.tb)
}
