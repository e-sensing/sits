#' @title Evaluates the accuracy of classification
#' @name sits_accuracy
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
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
#' @param folds           number of partitions to create.
#' @param pt_method       method to create patterns (sits_patterns_gam, sits_dendogram)
#' @param dist_method     method to compute distances (e.g., sits_TWDTW_distances)
#' @param tr_method       machine learning training method
#' @param multicores      number of threads to process the validation (Linux only). Each process will run a whole partition validation.
#' @return conf.tb        a tibble containing pairs of reference and predicted values
#' @export

sits_kfold_validate <- function (data.tb, folds = 5,
                                 pt_method   = sits_gam(),
                                 dist_method = sits_TWDTW_distances(),
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
    data.tb <- sits_create_folds (data.tb, folds = folds)

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

        # use the extracted partition to create the patterns
        patterns.tb <- pt_method(data_train.tb)

        # find the matches on the training data
        distances_train.tb <- dist_method (data_train.tb, patterns.tb)

        # find a model on the training data set
        model.ml <- tr_method (distances_train.tb)

        # find the distances in the test data
        distances_test.tb  <- dist_method (data_test.tb, patterns.tb)

        # classify the test data
        predict.tb <- sits_predict(data_test.tb, distances_test.tb, model.ml)

        ref.vec  <- c(ref.vec,  predict.tb$label)
        pred.vec <- c(pred.vec, predict.tb$predicted)

        return (c(pred.vec, ref.vec))
    }, mc.cores = multicores)

    purrr::map(conf.lst, function (e) {
        mid <- length (e)/2
        pred.vec <<- c(pred.vec, e[1:mid])
        ref.vec <<-  c(ref.vec, e[(mid+1):length(e)])
    })

    conf.tb <- tibble::tibble("predicted" = pred.vec, "reference" = ref.vec)

    return (conf.tb)
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
#' This function returns the Overall Accuracy, User's Accuracy,
#' Producer's Accuracy, error matrix (confusion matrix), and Kappa values.
#'
#' @param data.tb         a SITS tibble
#' @param folds           number of partitions to create.
#' @param pt_method       method to create patterns (sits_patterns_gam, sits_dendogram)
#' @param dist_method     method to compute distances (e.g., sits_TWDTW_distances)
#' @param tr_method       machine learning training method
#' @param multicores      number of threads to process the validation (Linux only). Each process will run a whole partition validation.
#' @return conf.tb        a tibble containing pairs of reference and predicted values
#' @export

sits_kfold_fast_validate <- function (data.tb, folds = 5,
                                 pt_method   = sits_gam(),
                                 dist_method = sits_TWDTW_distances(),
                                 tr_method   = sits_svm(),
                                 multicores = 1){

    # does the input data exist?
    .sits_test_tibble (data.tb)
    # is the data labelled?
    ensurer::ensure_that (data.tb, !("NoClass" %in% sits_labels(.)),
                          err_desc = "sits_cross_validate: please provide a labelled set of time series")

    # what are the bands of the data?
    bands <- sits_bands (data.tb)

    # Use all samples to find the patterns
    message("Creating patterns from all samples of the data..")
    patterns.tb <- pt_method(data.tb)

    # find the matches on the training data
    message("Measuring distances from all samples of the data to the patterns..")
    distances.tb <- dist_method (data.tb, patterns.tb)

    # create partitions different splits of the input data
    data.tb <- sits_create_folds (data.tb, folds = folds)

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
        predict.tb <- sits_predict(data_test.tb, dist_test.tb, model.ml)

        ref.vec  <- c(ref.vec,  predict.tb$label)
        pred.vec <- c(pred.vec, predict.tb$predicted)

        return (c(pred.vec, ref.vec))
    }, mc.cores = multicores)

    purrr::map(conf.lst, function (e) {
        mid <- length (e)/2
        pred.vec <<- c(pred.vec, e[1:mid])
        ref.vec <<-  c(ref.vec, e[(mid+1):length(e)])
    })

    conf.tb <- tibble::tibble("predicted" = pred.vec, "reference" = ref.vec)

    return (conf.tb)
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

    # verify if data.tb exists
    .sits_test_tibble (data.tb)

    # splits the data into k groups
    data.tb$folds <- caret::createFolds(data.tb$label, k = folds, returnTrain = FALSE, list = FALSE)

    return (data.tb)
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

     # align all samples to the same time series intervals
     # sample_dates <- lubridate::as_date(patterns.tb[1,]$time_series[[1]]$Index)
     # data.tb      <- sits_align (data.tb, sample_dates)
     #
     # start_date <- patterns.tb[1,]$start_date
     # end_date   <- patterns.tb[1,]$end_date

     # classify data
     class.tb <- sits_classify (data.tb, patterns.tb, ml_model, dist_method,
                       start_date = start_date, end_date = end_date, interval = interval)
     # retrieve the reference labels
     class.tb <- dplyr::mutate (class.tb, reference = label)

     # calculate the accuracy assessment
     assess <- sits_accuracy(class.tb, pred_sans_ext = TRUE)

     return (invisible (assess))
}

#' @title Print the values of a confusion matrix
#' @name .print_confusion_matrix
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#
#' @description This is an adaptation of the print.confusionMatrix method by the "caret" package
#' with some of the descriptions adapted for the more common usage in Earth Observation
#'
#'
#' @param x an object of class \code{confusionMatrix}
#' @param mode a single character string either "sens_spec", "prec_recall", or
#' "everything"
#' @param digits number of significant digits when printed
#' @param \dots optional arguments to pass to \code{print.table}
#' @return \code{x} is invisibly returned
#' @seealso \code{\link{confusionMatrix}}

.print_confusion_matrix <- function(x, mode = "sens_spec", digits = max(3, getOption("digits") - 3), ...){

    cat("Confusion Matrix and Statistics\n\n")
    print(x$table, ...)

    # round the data to the significant digits
    overall <- round(x$overall, digits = digits)

    # get the values of the p-index
    # pIndex <- grep("PValue", names(x$overall))
    # tmp[pIndex] <- format.pval(x$overall[pIndex], digits = digits)
    # overall <- tmp

    accCI <- paste("(",
                   paste( overall[ c("AccuracyLower", "AccuracyUpper")], collapse = ", "), ")",
                   sep = "")

    overallText <- c(paste(overall["Accuracy"]), accCI, "", paste(overall["Kappa"]))

    overallNames <- c("Accuracy", "95% CI", "", "Kappa")

    if(dim(x$table)[1] > 2){
        cat("\nOverall Statistics\n")
        overallNames <- ifelse(overallNames == "",
                               "",
                               paste(overallNames, ":"))
        out <- cbind(format(overallNames, justify = "right"), overallText)
        colnames(out) <- rep("", ncol(out))
        rownames(out) <- rep("", nrow(out))

        print(out, quote = FALSE)

        cat("\nStatistics by Class:\n\n")
        x$byClass <- x$byClass[,grepl("(Sensitivity)|(Specificity)|(Pos Pred Value)|(Neg Pred Value)",
                                      colnames(x$byClass))]
        ass.mx <- t(x$byClass)
        rownames(ass.mx) <- c("Prod Acc (Sensitivity)", "Specificity", "User Acc (Pos Pred Value)", "Neg Pred Value" )
        print(ass.mx, digits = digits)

    } else {
        # this is the case of ony two classes
        # get the values of the User's and Producer's Accuracy for the two classes
        # the names in caret are different from the usual names in Earth observation
        x$byClass <- x$byClass[grepl("(Sensitivity)|(Specificity)|(Pos Pred Value)|(Neg Pred Value)",
                                      names(x$byClass))]
        # get the names of the two classes
        nm <- row.names(x$table)
        # the first class (which is called the "positive" class by caret)
        c1 <- x$positive
        # the second class
        c2 <- nm[!(nm == x$positive)]
        # make up the values of UA and PA for the two classes
        pa1 <- paste("Prod Acc ", c1)
        pa2 <- paste("Prod Acc ", c2)
        ua1 <- paste("User Acc ", c1)
        ua2 <- paste("User Acc ", c2)
        names (x$byClass) <- c(pa1, pa2, ua1, ua2)


        overallText <- c(overallText,
                         "",
                         format(x$byClass, digits = digits))
        overallNames <- c(overallNames, "", names(x$byClass))
        overallNames <- ifelse(overallNames == "", "", paste(overallNames, ":"))

        out <- cbind(format(overallNames, justify = "right"), overallText)
        colnames(out) <- rep("", ncol(out))
        rownames(out) <- rep("", nrow(out))

        out <- rbind(out, rep("", 2))

        print(out, quote = FALSE)
    }

    invisible(x)
}

