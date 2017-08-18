# ---------------------------------------------------------------
#
#  This file contain a list of functions to assess the quality of classified time series
#  it includes functions for cross_validation and accuracy
#  It works with SITS tables where the time series have been classified


#' @title Evaluates the accuracy of classification
#' @name sits_accuracy
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#
#' @description Evaluates the accuracy of classification stored in two vectors.
#' Returns the overall accuracy, producers and users accuracy, and confusion matrix.
#' This algorith was inspired by `rfUtilities::accuracy` function, and fix the user and
#' producer accuracy computation inconsistency.
#'
#' @param pred.vec       A vector of all predicted labels.
#' @param ref.vec        A vector of all reference labels.
#' @param pred_sans_ext  (Boolean) remove all label extension (i.e. every string after last '.' character) from predictors before compute assesment.
#' @param conv.lst       A list conversion list of labels. If NULL no conversion is done.
#' @return result.lst     a list with accuracy measures and confusion matrix
#'
#' @export
#'
sits_accuracy <- function(pred.vec, ref.vec, pred_sans_ext = FALSE, conv.lst = NULL){

    # remove predicted labels' extensions
    if (pred_sans_ext)
        pred.vec <- tools::file_path_sans_ext(pred.vec)

    # count all pairs of labels
    # rows: predicted labels; cols: reference labels
    if (is.null(conv.lst))
        conf.mtx <- table(pred.vec, ref.vec)
    else{
        ensurer::ensure_that(c(pred.vec, ref.vec),
                             all(names(.) %in% names(conv.lst)),
                             err_desc = "sits_accuracy: conversion list does not contain all labels provided in `pred.vec` and/or `ref.vec` arguments.")
        conf.mtx <- table(as.character(conv.lst[[pred.vec]]), as.character(conv.lst[[ref.vec]]))
    }

    if (NCOL(conf.mtx) != NROW (conf.mtx)) {
        missing_names = colnames (conf.mtx) [!(colnames(conf.mtx) %in% row.names(conf.mtx))]
        for (i in 1:length (missing_names)) {
            vz <- rep (0, NCOL(conf.mtx))
            conf.mtx <- rbind (conf.mtx, missing_names[i] = vz)
        }
    }
    # ensures that the confusion matrix is square
    ensurer::ensure_that(conf.mtx, NCOL(.) == NROW(.),
                         err_desc = "sits_accuracy: predicted and reference vectors does not produce a squared matrix. Try to convert `pred.vec` entries before compute accuracy.")

    # sort rows (predicted labels) according to collumn names (reference labels)
    conf.mtx <- conf.mtx[colnames(conf.mtx),]

    # get labels' agreement (matrix diagonal)
    agreement <- diag(conf.mtx)

    # get total of predicted labels (to compute users accuracy)
    users <- apply(conf.mtx, 1, sum)

    # get total of reference labels (to compute producers accuracy)
    producers <- apply(conf.mtx, 2, sum)

    # get grand totals
    agreement_total <- sum(agreement)
    grand_total <- sum(conf.mtx)

    # compose result list
    result.lst <- tibble::lst(
        overall.accuracy = round(agreement_total / grand_total * 100, 4),
        producer.accuracy = round(agreement / producers * 100, 4),
        user.accuracy = round(agreement / users * 100, 4),
        confusion = conf.mtx
    )

    return(result.lst)
}
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
#' This function returns the Overall Accuracy, User's Accuracy,
#' Producer's Accuracy, error matrix (confusion matrix), and Kappa values.
#'
#' @param data.tb         a SITS tibble
#' @param bands           the bands used for classification
#' @param folds           number of partitions to create.
#' @param pt_method       method to create patterns (sits_patterns_gam, sits_dendogram)
#' @param tr_method       machine learning training method
#' @param file            file to save the results
#' @param .multicores     number of threads to process the validation (Linux only). Each process will run a whole partition validation (see `times` parameter).
#' @param ...             any additional parameters to be passed to `sits_pattern` function.
#' @return cm             a validation assessment
#' @export

sits_kfold_validate <- function (data.tb, bands = NULL, folds = 5,
                                 pt_method = sits_gam(bands = bands, from = NULL, to = NULL, freq = 8, formula = y ~ s(x)),
                                 tr_method = sits_svm(formula = sits_formula_logref(predictors_index = -2:0), kernel = "linear",
                                                      degree = 3, coef0 = 0, tolerance = 0.001, epsilon = 0.1),
                                 file = "./conf_matrix.json", .multicores = 1, ...){

    # does the input data exist?
    .sits_test_table (data.tb)
    # is the data labelled?
    ensurer::ensure_that (data.tb, !("NoClass" %in% sits_labels(.)$label),
                          err_desc = "sits_cross_validate: please provide a labelled set of time series")

    #is the bands are not provided, deduced them from the data
    if (purrr::is_null (bands))
        bands <- sits_bands (data.tb)

    # are the bands to be classified part of the input data?
    ensurer::ensure_that(data.tb, !(FALSE %in% bands %in% (sits_bands(.))),
                         err_desc = "sits_kfold_validate: invalid input bands")

    #extract the bands to be included in the patterns
    data.tb <- sits_select(data.tb, bands)

    # create partitions different splits of the input data
    data.tb <- sits_create_folds (data.tb, folds = folds)

    # create prediction and reference vector
    pred.vec = character()
    ref.vec = character()

    for (k in 1:folds)
    {
        # split data into training and test data sets
        data_train <- data.tb[data.tb$folds != k,]
        data_test  <- data.tb[data.tb$folds == k,]

        #
        message("Creating patterns from a data sample...")

        # use the extracted partition to create the patterns
        patterns.tb <- pt_method(data_train)

        # find the matches on the training data
        matches_train.tb  <- sits_TWDTW_matches (data.tb = data_train, patterns.tb, bands = bands, ...)

        # find a model on the training data set
        model.ml <- tr_method (matches_train.tb)

        # find the matches in the test data
        matches_test.tb  <- sits_TWDTW_matches (data.tb = data_test, patterns.tb, bands = bands, ...)

        # classify the test data
        predict.tb <- sits_predict(matches_test.tb, model.ml)

        ref.vec <- append (ref.vec, predict.tb$label)
        pred.vec <- append (pred.vec, predict.tb$predicted)
    }

    confusion.vec <- c(pred.vec, ref.vec)
    # save the confusion vector in  a JSON file
    sits_toJSON (confusion.vec, file)

    # Classification accuracy measures
    assessment <- sits_accuracy(pred.vec, ref.vec, pred_sans_ext = TRUE)

    return (assessment)
}
#' @title Area-weighted post-classification accuracy assessment of classified maps
#' @name sits_accuracy_area
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description To use this function, the input table should be a set of results containing
#' both the label assigned by the user and the classification result.
#' Accuracy assessment set us a confusion matrix to determine the accuracy of your classified result.
#' This function uses an area-weighted technique proposed by Olofsson et al. to
#' produce accuracy estimates that are more reliable
#'
#' We plan to do an improved version of this function that includes a Raster R object
#' with the classified map and a vector with the labels of the classified map
#' (Gilberto-Rolf-05-Jun-2017)
#'
#' This function calls \code{\link[dtwSat]{twdtwAssess}} from \pkg{dtwSat}.
#' \code{\link[dtwSat]{twdtwAssess}} performs an accuracy assessment of the classified, including
#' Overall Accuracy, User's Accuracy, Produce's Accuracy, error matrix (confusion matrix),
#' and estimated area according to [1-2].
#'
#' @references
#' [1] Olofsson, P., Foody, G.M., Stehman, S.V., Woodcock, C.E. (2013).
#' Making better use of accuracy data in land change studies: Estimating
#' accuracy and area and quantifying uncertainty using stratified estimation.
#' Remote Sensing of Environment, 129, pp.122-131.
#'
#' @references
#' [2] Olofsson, P., Foody G.M., Herold M., Stehman, S.V., Woodcock, C.E., Wulder, M.A. (2014)
#' Good practices for estimating area and assessing accuracy of land change. Remote Sensing of
#' Environment, 148, pp. 42-57.
#'
#' @param results.tb a sits table with a set of lat/long/time locations  with known and trusted labels and
#' with the result of classification method
#' @param area a list with the area of each label
#' @param conf.int specifies the confidence level (0-1).
#' @param rm.nosample if sum of columns and sum of rows of the error matrix are zero
#' then remove class. Default is TRUE.
#' @export
sits_accuracy_area <- function (results.tb, area, conf.int = 0.95, rm.nosample = FALSE){

     # Get reference classes
     references <- results.tb$label

     # Get mapped classes
     # mapped    <- dplyr::bind_rows(results.tb$distances) %>%
     #                          dplyr::select(dplyr::matches("classification")) %>% unlist

     # create a vector to store the result of the predictions
     mapped <- results.tb$class
     # Get all labels
     classes   <- unique(c(references, mapped))

     # Create error matrix
     error_matrix <- table(factor(mapped,     levels = classes, labels = classes),
                           factor(references, levels = classes, labels = classes))

     # Get area - TO IMPROVE USING THE METADATA FROM SATELLITE PRODUCTS
     if(missing(area))
          area <- rowSums(error_matrix)

     # Compute accuracy metrics using dtwSat::twdtwAssess
     assessment <- dtwSat::twdtwAssess (error_matrix,
                                        area = area,
                                        conf.int = conf.int,
                                        rm.nosample = rm.nosample )

     return (assessment)

}



#' @title Create partitions of a data set
#' @name  sits_create_folds
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Alexandre Ywata, \email{alexandre.ywata@@ipea.gov.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Split a SITS table into k groups, based on the label
#'
#' @param data.tb a SITS table to be partitioned
#' @param folds   number of folds
#' @export
sits_create_folds <- function (data.tb, folds = 5) {

    ensurer::ensure_that (data.tb, !("NoClass" %in% sits_labels(.)$label),
                          err_desc = "sits_create_folds: please provide a labelled set of time series")


    set.seed(2104)
    # splits the data into k groups
    data.tb$folds <- caret::createFolds(data.tb$label, k = folds, returnTrain = FALSE, list = FALSE)

    return (data.tb)
}
#' @title Create partitions of a data set
#' @name  sits_create_partitions
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Create a list of partitions of a SITS table, based on a percentage and
#' a number of iterations
#'
#' @param data.tb a SITS table to be partitioned
#' @param times   number of iterations
#' @param frac    fraction of original data to be extracted. Value must be between 0 and 1.
.sits_create_partitions <- function (data.tb, times, frac) {

     # create a list to store the partitions
     partitions.lst <- tibble::lst()

     # iterate and create the partitions
     for (i in 1:times){
          partitions.lst [[i]] <- sits_labels_sample (data.tb, frac)
     }
     return (partitions.lst)
}

#' @title reassess classification results
#' @name sits_reassess
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Given a confusion matrix obtained in the validation
#' procedure, and a conversion list between the original labels and
#' new labels, returns a new confusion matrix
#' where classes have been merged.
#'
#' @param  file           a JSON file contaning the result of a validation procedure
#' @param  conv           a conversion of label names for the classes (optional))
#' @return assess         an assessment of validation
#' @export
sits_reassess <- function (file = NULL, conv = NULL){
     ensurer::ensure_that(file, !purrr::is_null(.),
                          err_desc = "sits_relabel: JSON file not provided")

     # return the confusion matrix
     confusion.vec <- jsonlite::fromJSON (file)
     mid <- length(confusion.vec)/2
     pred.vec <- confusion.vec[1:mid]
     ref.vec  <- confusion.vec[(mid+1):length(confusion.vec)]

     # calculate the accuracy assessment
     assess <- sits_accuracy(pred.vec, ref.vec, pred_sans_ext = TRUE, conv.lst = conv)

     return (assess)
}

#' @title Evaluates the accuracy of a set of patterns
#' @name sits_test_patterns
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#
#' @description Tests the accuracy of TWDTW classification
#' of set of labelled samples using a set of patterns.
#' This function should be used when the patterns are not directly derived from the samples.
#' It provides an initial assessment of the validity of using this set of pattern
#' to classify an area whose samples are given.
#' This function returns the Overall Accuracy, User's Accuracy,
#' Producer's Accuracy, error matrix (confusion matrix), and Kappa values.
#'
#' @param  data.tb       A sits tibble containing a set of samples with known and trusted labels
#' @param  patterns.tb   A sits tibble containing a set of patterns
#' @param  bands         the bands used for classification
#' @param  alpha         (double)  - the steepness of the logistic function used for temporal weighting
#' @param  beta          (integer) - the midpoint (in days) of the logistic function
#' @param  theta         (double)  - the relative weight of the time distance compared to the dtw distance
#' @param  span          (integer) - minimum number of days between two matches of the same pattern in the time series (approximate)
#' @param  start_date    date - the start of the classification period
#' @param  end_date      date - the end of the classification period
#' @param  interval      date - the period between two classifications
#' @param  overlap       (double) minimum overlapping between one match and the interval of classification
#' @return assess         an assessment of validation
#' @export
sits_test_patterns <- function (data.tb, patterns.tb, bands,
                                alpha = -0.1, beta = 100, theta = 0.5, span  = 0,
                                start_date = NULL, end_date = NULL, interval = "12 month", overlap = 0.5) {

    # does the input data exist?
    .sits_test_table (data.tb)
    .sits_test_table (patterns.tb)
     ensurer::ensure_that (bands, !purrr::is_null(.),
                           err_desc = "sits_test_patterns: please provide the bands to be used")
     ensurer::ensure_that (data.tb, !("NoClass" %in% sits_labels(.)$label),
                            err_desc = "sits_test_patterns: please provide a labelled set of time series")


     # classify data
     matches.tb  <- sits_TWDTW_matches (data.tb, patterns.tb, bands = bands, alpha = alpha, beta = beta, theta = theta, span = span)
     class.tb    <- sits_TWDTW_classify (matches.tb, start_date = start_date, end_date = end_date, interval = interval, overlap = overlap)

     # retrieve the reference labels
     ref.vec <- as.character(class.tb$label)
     # retrieve the predicted labels
     pred.vec  <- as.character(purrr::map(class.tb$best_matches, function (e) as.character(e$label)))

     # calculate the accuracy assessment
     assess <- sits_accuracy(pred.vec, ref.vec, pred_sans_ext = TRUE)

     return (assess)
}

