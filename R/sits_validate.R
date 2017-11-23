#' @title Cross-validate temporal patterns
#' @name sits_kfold_validate
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Splits the set of time series into training and validation and
#' perform k-fold cross-validation.
#' Cross-validation is a model validation technique for assessing how the results
#' of a statistical analysis will generalize to an independent data set.
#' It is mainly used in settings where the goal is prediction,
#' and one wants to estimate how accurately a predictive model will perform in practice.
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
#' @param data.tb         a SITS tibble
#' @param folds           number of partitions to create.
#' @param dist_method     method to compute distances (e.g., sits_TWDTW_distances)
#' @param tr_method       machine learning training method
#' @param multicores      number of threads to process the validation (Linux and MacOS only). Each process will run a whole partition validation.
#' @return pred_ref.tb        a tibble containing pairs of reference and predicted values
#' @export

sits_kfold_validate <- function (data.tb, folds = 5,
                                 dist_method = sits_distances_from_data(),
                                 tr_method   = sits_svm(),
                                 multicores = 1){

    # does the input data exist?
    .sits_test_tibble (data.tb)

    # is the data labelled?
    ensurer::ensure_that (data.tb, !("NoClass" %in% sits_labels(.)),
                          err_desc = "sits_cross_validate: please provide a labelled set of time series")

    #is the bands are not provided, deduced them from the data
    bands <- sits_bands (data.tb)

    # create partitions different splits of the input data
    data.tb <- .sits_create_folds (data.tb, folds = folds)

    # create prediction and reference vector
    pred.vec = character()
    ref.vec  = character()

    conf.lst <- parallel::mclapply(X = 1:folds, FUN = function (k)
    {
        # split data into training and test data sets
        data_train.tb <- data.tb[data.tb$folds != k,]
        data_test.tb  <- data.tb[data.tb$folds == k,]

        #
        message("Creating patterns from a data sample...")

        # find the matches on the training data
        distances_train.tb <- dist_method (data_train.tb)

        # find a model on the training data set
        model.ml <- tr_method (distances_train.tb)

        # find the distances in the test data
        distances_test.tb  <- dist_method (data_test.tb)

        # classify the test data
        predicted <- sits_predict(distances_test.tb, model.ml)

        ref.vec  <- c(ref.vec,  data_test.tb$label)
        pred.vec <- c(pred.vec, predicted)

        return (c(pred.vec, ref.vec))
    }, mc.cores = multicores)

    purrr::map(conf.lst, function (e) {
        mid <- length (e)/2
        pred.vec <<- c(pred.vec, e[1:mid])
        ref.vec <<-  c(ref.vec, e[(mid+1):length(e)])
    })

    pred_ref.tb <- tibble::tibble("predicted" = pred.vec, "reference" = ref.vec)

    return (pred_ref.tb)
}
#' @title Cross-validate temporal patterns (faster than sits_kfold_validate)
#' @name sits_kfold_fast_validate
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Splits the set of time series into training and validation and
#' perform k-fold cross-validation. This function is similar to sits_kfold_validate (see above)
#' but it is not as accurate. The patterns and the distance matrices are calculated
#' once for all samples. The distance matrix is then used for kfold validation.
#' In general, if the number of samples per class is not small,
#' the results will be faster than the full validate.
#' This function should be used for a first comparison between different machine learning methods.
#' For reporting in papers, please use the sits_kfold_validate method.
#'
#' This function returns the confusion matrix.
#'
#' @param data.tb         a SITS tibble
#' @param folds           number of partitions to create.
#' @param dist_method     method to compute distances (e.g., sits_TWDTW_distances)
#' @param tr_method       machine learning training method
#' @param multicores      number of threads to process the validation (Linux or MacOSX only). Each process will run a whole partition validation.
#' @return pred_ref.tb    a tibble containing pairs of reference and predicted values
#' @export

sits_kfold_fast_validate <- function (data.tb, folds = 5,
                                      dist_method = sits_distances_from_data(),
                                      tr_method   = sits_svm(),
                                      multicores = 1){

    # does the input data exist?
    .sits_test_tibble (data.tb)
    # is the data labelled?
    ensurer::ensure_that (data.tb, !("NoClass" %in% sits_labels(.)),
                          err_desc = "sits_cross_validate: please provide a labelled set of time series")

    # what are the bands of the data?
    bands <- sits_bands (data.tb)

    # find the matches on the training data
    message("Measuring distances from all samples of the data to the patterns..")
    distances.tb <- dist_method (data.tb)

    # create partitions different splits of the input data
    data.tb <- .sits_create_folds (data.tb, folds = folds)

    # create prediction and reference vector
    pred.vec = character()
    ref.vec  = character()

    conf.lst <- parallel::mclapply(X = 1:folds, FUN = function (k)
    {

        # split input data into training and test data sets
        data_test.tb  <- data.tb[data.tb$folds == k,]

        # split distances into training and test data sets
        dist_train.tb <- distances.tb[data.tb$folds != k,]
        dist_test.tb  <- distances.tb[data.tb$folds == k,]

        # find a model on the training data set
        model.ml <- tr_method (dist_train.tb)

        # classify the test data
        predicted <- sits_predict(dist_test.tb, model.ml)

        ref.vec  <- c(ref.vec,  data_test.tb$label)
        pred.vec <- c(pred.vec, predicted)

        return (c(pred.vec, ref.vec))
    }, mc.cores = multicores)

    purrr::map(conf.lst, function (e) {
        mid <- length (e)/2
        pred.vec <<- c(pred.vec, e[1:mid])
        ref.vec <<-  c(ref.vec, e[(mid+1):length(e)])
    })

    pred_ref.tb <- tibble::tibble("predicted" = pred.vec, "reference" = ref.vec)

    return (pred_ref.tb)
}
