#' @title Assessment of the accuracy of classification based on a confusion matrix
#' @name sits_conf_matrix
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#
#' @description Evaluates the confusion matrix based on "reference" and "predicted" values
#' provided in a SITS tibble that has been classified. This function takes two kinds of input:
#' (a) The output of the \code{\link[sits]{sits_classify}} function (a tibble with a list of predicted values)
#' (b) The output of the \code{\link[sits]{sits_kfold_validate}} function (a tibble with two columns - predicted and reference)
#' This function returns the Overall Accuracy, User's Accuracy,
#' Producer's Accuracy, error matrix (confusion matrix), and Kappa value.
#'
#'
#' @param  class.tb        A sits tibble containing a set of classified samples whose labels are known
#' @param  conv.lst        A list conversion list of labels. If NULL no conversion is done.
#' @param  pred_sans_ext  (boolean) remove all label extension (i.e. every string after last '.' character) from predictors before compute assesment.
#' @return caret_assess   a confusion matrix assessment produced by the caret package
#'
#' @examples
#' \donttest{
#' # read a tibble with 400 samples of Cerrado and 346 samples of Pasture
#' data(cerrado_2classes)
#' # perform a 2 fold validation of this sample file
#' pred_ref.tb <-  sits_kfold_validate(cerrado_2classes, folds = 2)
#' # calculate and print the confusion matrix
#' conf.mx <- sits_conf_matrix(pred_ref.tb)
#' }
#' @export
sits_conf_matrix <- function(class.tb, conv.lst = NULL, pred_sans_ext = FALSE) {

    # does the input data contain a set of predicted values?
    ensurer::ensure_that(class.tb, "predicted" %in% names(.), err_desc = "sits_conf_matrix: input data does not contain predicted values")


    # recover predicted and reference vectors from input
    # is the input the result of a sits_classify?
    if ("label" %in% names(class.tb)) {
        pred_ref.tb <- .sits_pred_ref(class.tb)
        pred.vec <- pred_ref.tb$predicted
        ref.vec  <- pred_ref.tb$reference
    }
    # is the input the result of the sits_kfold_validate?
    else{
        pred.vec <- class.tb$predicted
        ref.vec  <- class.tb$reference
    }

    # remove predicted labels' extensions
    if (pred_sans_ext)
        pred.vec <- tools::file_path_sans_ext(pred.vec)

    # convert class names
    if (!purrr::is_null(conv.lst)) {
        # select the label names
        names_ref <- unique(ref.vec)
        # are all input labels in the coversion list?
        ensurer::ensure_that(names_ref,
                             all(. %in% names(conv.lst)),
                             err_desc = "sits_conf_matrix: conversion list does not contain all reference labels")
        pred.vec <- as.character(conv.lst[pred.vec])
        ref.vec  <- as.character(conv.lst[ref.vec])
    }

    # call caret package to the classification statistics
    caret_assess <- caret::confusionMatrix(pred.vec, ref.vec)

    # print the result
    .print_confusion_matrix(caret_assess)

    # return invisible
    return(invisible(caret_assess))
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
#' @param class.tb a sits table with a set of lat/long/time locations  with known and trusted labels and
#' with the result of classification method
#' @param area a list with the area of each label
#' @param conf.int specifies the confidence level (0-1).
#' @param rm.nosample if sum of columns and sum of rows of the error matrix are zero
#' then remove class. Default is TRUE.
#' @export
sits_accuracy_area <- function(class.tb, area = NULL, conf.int = 0.95, rm.nosample = FALSE){

    # verifies if dtwSat package is installed
    if (!requireNamespace("dtwSat", quietly = TRUE)) {
        stop("dtwSat needed for this function to work. Please install it.", call. = FALSE)
    }

    # Get reference classes
    references <- class.tb$label

    # create a vector to store the result of the predictions
    mapped <-
        unlist(purrr::map(class.tb$predicted, function(r)
            r$class))

    # Get all labels
    classes   <- unique(c(references, mapped))

    # Create error matrix
    error_matrix <-
        table(
            factor(mapped,     levels = classes, labels = classes),
            factor(references, levels = classes, labels = classes)
        )

    # Get area - TO IMPROVE USING THE METADATA FROM SATELLITE PRODUCTS
    if (purrr::is_null(area))
        area <- rowSums(error_matrix)

    # Compute accuracy metrics using dtwSat::twdtwAssess
    assessment <- dtwSat::twdtwAssess(
        error_matrix,
        area = area,
        conf.int = conf.int,
        rm.nosample = rm.nosample
    )

    return(assessment)

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

    if (dim(x$table)[1] > 2) {
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
        names(x$byClass) <- c(pa1, pa2, ua1, ua2)


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

#' @title Obtains the predicted value of a reference set
#' @name .sits_pred_ref
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#
#' @description Obtains a tibble of predicted and reference values from a classified data set
#'
#' @param  class.tb       A sits tibble containing a set of classified samples whose labels are known
#' @return pred_ref.tb    A tibble with predicted and reference values
.sits_pred_ref <- function(class.tb) {

    # retrieve the predicted values
    pred.vec <- unlist(purrr::map(class.tb$predicted, function(r) r$class))

    # retrieve the reference labels
    ref.vec <- class.tb$label
    # does the input data contained valid reference labels?
    ensurer::ensure_that(ref.vec, !("NoClass" %in% (.)), err_desc = "sits_accuracy: input data does not contain reference values")

    # build the tibble
    pred_ref.tb <- tibble::tibble("predicted" = pred.vec, "reference" = ref.vec)

    # return the tibble
    return(pred_ref.tb)
}

#' @title Saves the results of accuracy assessments as Excel files
#' @name sits_toXLSX
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#
#' @description Saves confusion matrices as Excel spreadsheets. This function
#' takes the a list of confusion matrices generated by the \code{\link[sits]{sits_conf_matrix}}
#' function and save them in an Excel spreadsheet.
#'
#' @param acc.lst        A list of confusion matrices
#' @param file           The file where the XLSX data is to be saved
#'
#' @examples
#' \donttest{
#' # read a tibble with 400 samples of Cerrado and 346 samples of Pasture
#' data(cerrado_2classes)
#' # perform a 2 fold validation of this sample file
#' pred_ref.tb <-  sits_kfold_validate(cerrado_2classes, folds = 2)
#' # calculate and print the confusion matrix
#' conf.mx <- sits_conf_matrix(pred_ref.tb)
#' # create a list to store the results
#' results <- list()
#' # give a name to the confusion matrix
#' conf.mx$name <- "confusion_matrix"
#' # add the confusion matrix to the results
#' results[[length(results) + 1]] <- conf.mx
#' # save the results to an XLSX file
#' sits_toXLSX(results, file = paste0(getwd(),"/confusion_matrix.xlsx"))
#' }
#'
#' @export
sits_toXLSX <- function(acc.lst, file = NULL){

    ensurer::ensure_that(file, !purrr::is_null(.),
                         err_desc = "sits_toXLSX: please provide the file name")

    # create a workbook to save the results
    wb <- openxlsx::createWorkbook("accuracy")

    ind <- 0

    # save all elements of the list
    acc.lst %>%
        purrr::map(function(acc.mx) {

            # create a sheet name"Conf
            if (purrr::is_null(acc.mx$name)) {
                ind <<- ind + 1
                acc.mx$name <- paste0('sheet', ind)
            }
            sheet_name <- acc.mx$name

            # add a worksheet
            openxlsx::addWorksheet(wb, sheet_name)

            # use only the class names (without the "Class: " prefix)
            new_names <- unlist(strsplit(colnames(acc.mx$table), split = ": "))

            # remove prefix from confusion matrix table
            colnames(acc.mx$table) <- new_names
            # write the confusion matrix table in the worksheet
            openxlsx::writeData(wb, sheet_name, acc.mx$table)

            # overall assessment (accuracy and kappa)
            acc_kappa.mx <- as.matrix(acc.mx$overall[c(1:2)])

            # save the accuracy data in the worksheet
            openxlsx::writeData(wb, sheet_name, acc_kappa.mx,
                                rowNames = TRUE, startRow = NROW(acc.mx$table) + 3, startCol = 1)

            if (dim(acc.mx$table)[1] > 2) {
                # per class accuracy assessment
                acc_bc.mx <- t(acc.mx$byClass[,c(1:4)])
                # remove prefix from confusion matrix table
                colnames(acc_bc.mx)  <- new_names
                row.names(acc_bc.mx) <- c("Sensitivity (PA)", "Specificity", "PosPredValue (UA)", "NegPredValue")
            }
            else {
                # this is the case of ony two classes
                # get the values of the User's and Producer's Accuracy for the two classes
                # the names in caret are different from the usual names in Earth observation
                acc_bc.mx <- acc.mx$byClass[grepl("(Sensitivity)|(Specificity)|(Pos Pred Value)|(Neg Pred Value)",
                                                  names(acc.mx$byClass))]
                # get the names of the two classes
                nm <- row.names(acc.mx$table)
                # the first class (which is called the "positive" class by caret)
                c1 <- acc.mx$positive
                # the second class
                c2 <- nm[!(nm == acc.mx$positive)]
                # make up the values of UA and PA for the two classes
                pa1 <- paste("Prod Acc ", c1)
                pa2 <- paste("Prod Acc ", c2)
                ua1 <- paste("User Acc ", c1)
                ua2 <- paste("User Acc ", c2)
                names(acc_bc.mx) <- c(pa1, pa2, ua1, ua2)
                acc_bc.mx <- as.matrix(acc_bc.mx)
            }
            # save the perclass data in the worksheet
            openxlsx::writeData(wb, sheet_name, acc_bc.mx,
                                rowNames = TRUE, startRow = NROW(acc.mx$table) + 8, startCol = 1)
    })

    # write the worksheets to the XLSX file
    openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)

    return(message(paste("Saved Excel file", file)))
}
