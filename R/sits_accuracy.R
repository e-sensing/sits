#' @title Evaluates the accuracy of classification
#' @name sits_accuracy
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#
#' @description Evaluates the accuracy of classification stored in two vectors.
#' Returns a confusion matrix used by the "caret" package
#'
#' @param conf.tb        A tibble containing pairs of reference and predicted values
#' @param conv.lst       A list conversion list of labels. If NULL no conversion is done.
#' @param pred_sans_ext  (Boolean) remove all label extension (i.e. every string after last '.' character) from predictors before compute assesment.
#' @return caret_assess  a confusion matrix assessment produced by the caret package
#'
#' @export
sits_accuracy <- function(conf.tb, conv.lst = NULL, pred_sans_ext = FALSE){


    # recover predicted and reference vectors from input
    pred.vec <- conf.tb$predicted
    ref.vec  <- conf.tb$reference

    # remove predicted labels' extensions
    if (pred_sans_ext)
        pred.vec <- tools::file_path_sans_ext(pred.vec)

    # convert class names
    if (!purrr::is_null(conv.lst)) {
        names_ref <- dplyr::pull (dplyr::distinct (conf.tb, reference))
        ensurer::ensure_that(names_ref,
                             all(. %in% names(conv.lst)),
                             err_desc = "sits_accuracy: conversion list does not contain all reference labels")
        pred.vec <- as.character(conv.lst[pred.vec])
        ref.vec  <- as.character(conv.lst[ref.vec])
    }

    # call caret package to the classification statistics
    caret_assess <- caret::confusionMatrix(pred.vec, ref.vec)

    # print the result
    .print_confusion_matrix (caret_assess)

    # return invisible
    return (invisible(caret_assess))
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

#' @title Evaluates the accuracy of a labelled set of data
#' @name sits_accuracy_classif
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#
#' @description Tests the accuracy of a classification model by comparing an input data set
#' that has been obtained independently to a the predicted values of the model.
#' This function can be used to test the accuracy of a classification model against a
#' data set that is obtained independently. The quality of the accuracy assessment
#' depends critically of the quality of the input data set, which should be be part of the
#' data set used for training the model.
#' This function should be used when the patterns are not directly derived from the samples.
#' It provides an initial assessment of the validity of using this set of pattern
#' to classify an area whose samples are given.
#' This function returns the Overall Accuracy, User's Accuracy,
#' Producer's Accuracy, error matrix (confusion matrix), and Kappa values.
#'
#' @param  data.tb       A sits tibble containing a set of samples with known and trusted labels
#' @param  patterns.tb   A sits tibble containing a set of patterns (independent of input data)
#' @param  ml_model      A model trained by \code{\link[sits]{sits_train}}
#' @param  dist_method   Method to compute distances (e.g., sits_TWDTW_distances)
#' @param  interval      Period between two classifications
#' @param ...            Other parameters to be passed to the distance function
#' @return assess        Assessment of validation
#' @export
sits_accuracy_classif <- function (data.tb,
                                   patterns.tb,
                                   ml_model,
                                   dist_method = sits_TWDTW_distances(),
                                   interval = "12 month") {

    # does the input data exist?
    .sits_test_tibble (data.tb)
    .sits_test_tibble (patterns.tb)
     ensurer::ensure_that (data.tb, !("NoClass" %in% sits_labels(.)),
                            err_desc = "sits_test_patterns: please provide a labelled set of time series")

     # classify data
     class.tb <- sits_classify (data.tb, patterns.tb, ml_model, dist_method, interval = interval)
     # retrieve the reference labels
     class.tb <- dplyr::mutate (class.tb, reference = label)

     # calculate the accuracy assessment
     assess <- sits_accuracy(class.tb, pred_sans_ext = TRUE)

     return (invisible (assess))
}



