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
#' @param data.tb         sits tibble
#' @param folds           number of partitions to create.
#' @param ml_method       machine learning method
#' @param multicores      number of threads to process the validation (Linux and MacOS only)
#' @return pred_ref.tb    tibble containing pairs of reference and predicted values
#'
#' @examples
#' \donttest{
#' # read a set of samples
#' data (cerrado_2classes)
#'
#' # perform a five fold validation with the SVM machine learning method
#' conf_matrix1.mx <- sits_kfold_validate (cerrado_2classes, ml_method = sits_svm())
#' }
#' @export

sits_kfold_validate <- function(data.tb, folds = 5,
                                ml_method    = sits_svm(),
                                multicores  = 1){

    # does the input data exist?
    .sits_test_tibble(data.tb)

    # is the data labelled?
    ensurer::ensure_that(data.tb, !("NoClass" %in% sits_labels(.)),
                         err_desc = "sits_cross_validate: please provide a labelled set of time series")

    #is the bands are not provided, deduced them from the data
    bands <- sits_bands(data.tb)

    # create partitions different splits of the input data
    data.tb <- .sits_create_folds(data.tb, folds = folds)

    # create prediction and reference vector
    pred.vec = character()
    ref.vec  = character()

    conf.lst <- parallel::mclapply(X = 1:folds, FUN = function(k)
    {
        # split data into training and test data sets
        data_train.tb <- data.tb[data.tb$folds != k,]
        data_test.tb  <- data.tb[data.tb$folds == k,]

        # create a machine learning model
        ml_model <- sits_train(data_train.tb, ml_method)

        # find the distances in the test data
        distances_test.tb  <- sits_distances(data_test.tb)

        # classify the test data
        predicted <- .sits_predict(distances_test.tb, ml_model)

        ref.vec  <- c(ref.vec,  data_test.tb$label)
        pred.vec <- c(pred.vec, predicted)

        return(c(pred.vec, ref.vec))
    }, mc.cores = multicores)

    purrr::map(conf.lst, function(e) {
        mid <- length(e)/2
        pred.vec <<- c(pred.vec, e[1:mid])
        ref.vec  <<-  c(ref.vec, e[(mid + 1):length(e)])
    })

    pred_ref.tb <- tibble::tibble("predicted" = pred.vec, "reference" = ref.vec)

    return(pred_ref.tb)
}
