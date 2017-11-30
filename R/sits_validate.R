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
#' @param ml_method       machine learning training method
#' @param multicores      number of threads to process the validation (Linux and MacOS only).
#' @return pred_ref.tb        a tibble containing pairs of reference and predicted values
#'
#' @examples
#' # read a set of samples
#' samples.tb <- readRDS (system.file("extdata/time_series/cerrado_2classes.rds", package = "sits"))
#'
#' # perform a five fold validation with the SVM machine learning method
#' conf_matrix1.mx <- sits_kfold_validate (samples.tb)
#'
#' \donttest{
#' #load a data set for with samples for EMBRAPA data set
#' embrapa.tb <- readRDS(system.file ("extdata/time_series/embrapa_mt.rds", package = "sits"))
#'
#' embrapa.tb <- sits_select (embrapa.tb, bands = c("ndvi", "evi", "nir", "mir"))
#'
#' # create a list to save the results
#' results <- list()
#'
#' conf_svm1.tb <- sits_kfold_validate(embrapa.tb, folds = 5, multicores = 2,
#'                 ml_method   = sits_svm (kernel = "radial", cost = 10))
#'  print("==================================================")
#'  print ("== Confusion Matrix = SVM =======================")
#'  conf_svm1.mx <- sits_conf_matrix(conf_svm1.tb)
#'  conf_svm1.mx$name <- "svm_10"
#'
#'  # save the results in a list
#'  results[[length(results) + 1]] <- conf_svm1.mx
#'
#'  # =============== GLM ==============================
#'  # generalized liner model (glm)
#'  conf_glm.tb <- sits_kfold_validate(embrapa.tb, folds = 5, multicores = 2,
#'               ml_method   = sits_glm())
#'
#'  # print the accuracy of the generalized liner model (glm)
#'  print("===============================================")
#'  print ("== Confusion Matrix = GLM  =======================")
#'  conf_glm.mx <- sits_conf_matrix(conf_glm.tb)
#'
#'  conf_glm.mx$name <- "glm"
#'  # save the results in a list
#'  results[[length(results) + 1]] <- conf_glm.mx
#'
#'  # =============== RFOR ==============================
#'  # validate random forest model
#'  conf_rfor.tb <- sits_kfold_validate(embrapa.tb, folds = 5, multicores = 2,
#'                  ml_method   = sits_rfor ())
#'  print("==================================================")
#'  print ("== Confusion Matrix = RFOR =======================")
#'
#'  conf_rfor.mx <- sits_conf_matrix(conf_rfor.tb)
#'  conf_rfor.mx$name <- "rfor"
#'  # save the results in a list
#'  results[[length(results) + 1]] <- conf_rfor.mx
#'
#'  # =============== LDA ==============================
#'
#'  # test validation of LDA method
#'  conf_lda.tb <- sits_kfold_validate(embrapa.tb, folds = 5, multicores = 2,
#'                  ml_method   = sits_lda ())
#'
#'  print("==================================================")
#'  print ("== Confusion Matrix = LDA =======================")
#'  conf_lda.mx <- sits_conf_matrix(conf_lda.tb)
#'  conf_lda.mx$name <- "lda"
#'  # save the results in a list
#'  results[[length(results) + 1]] <- conf_lda.mx
#'
#'  # =============== MLR ==============================
#'  # "multinomial log-linear (mlr)
#'  conf_mlr.tb <- sits_kfold_validate(embrapa.tb, folds = 5, multicores = 2,
#'                 ml_method   = sits_mlr())
#'
#' # print the accuracy of the Multinomial log-linear
#' print("===============================================")
#' print ("== Confusion Matrix = MLR =======================")
#'
#' conf_mlr.mx <- sits_conf_matrix(conf_mlr.tb)
#'
#' conf_mlr.mx$name <- "mlr"
#' # save the results in a list
#' results[[length(results) + 1]] <- conf_mlr.mx
#'
#' # =============== GBM ==============================
#' # Gradient Boosting Machine
#'
#' conf_gbm.tb <- sits_kfold_validate(embrapa.tb, folds = 5, multicores = 1,
#'                ml_method   = sits_gbm())
#'
#'  # print the accuracy of the Gradient Boosting Machine
#'  print("===============================================")
#'  print ("== Confusion Matrix = GBM =======================")
#'
#'  conf_gbm.mx <- sits_conf_matrix(conf_gbm.tb)
#'  conf_gbm.mx$name <- "gbm"
#'  # save the results in a list
#'  results[[length(results) + 1]] <- conf_gbm.mx
#'
#'  # Save the results list in a XLSX (Excel file)
#'  WD = getwd()
#'  sits_toXLSX(results, file = paste0(WD, "/accuracy_results.xlsx"))
#'
#' }
#' @export

sits_kfold_validate <- function (data.tb, folds = 5,
                                 ml_method   = sits_svm(),
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

        # find the matches on the training data
        distances_train.tb <- sits_distances (data_train.tb)

        # find a model on the training data set
        model.ml <- ml_method (distances_train.tb)

        # find the distances in the test data
        distances_test.tb  <- sits_distances (data_test.tb)

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
