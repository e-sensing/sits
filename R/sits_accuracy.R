#' @title Accuracy assessment of classification based on a confusion matrix
#' @name sits_conf_matrix
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#
#' @description Evaluates the confusion matrix based on
#' "reference" and "predicted" values
#' provided in a sits tibble that has been classified.
#' This function takes two kinds of input:
#' (a) The output of the \code{\link[sits]{sits_classify}} function
#' (a tibble with a list of predicted values);
#' (b) The output of the \code{\link[sits]{sits_kfold_validate}} function
#' (a tibble with two columns - predicted and reference).
#' This function returns the Overall Accuracy, User's Accuracy,
#' Producer's Accuracy, error matrix (confusion matrix), and Kappa value.
#'
#' @param  data            Set of classified samples whose labels are known.
#' @param  conv.lst        List with labels to be converted.
#'                         If NULL no conversion is done.
#' @return A confusion matrix assessment produced by the caret package.
#'
#' @examples
#' \donttest{
#' # read a tibble with 400 samples of Cerrado and 346 samples of Pasture
#' data(cerrado_2classes)
#' # perform a 2 fold validation of this sample file
#' pred_ref.tb <- sits_kfold_validate(cerrado_2classes, folds = 2)
#' # calculate and print the confusion matrix
#' conf.mx <- sits_conf_matrix(pred_ref.tb)
#' }
#' @export
sits_conf_matrix <- function(data, conv.lst = NULL) {

    # require package
    if (!requireNamespace("caret", quietly = TRUE)) {
        stop("Please install package caret.", call. = FALSE)
    }

    # backward compatibility
    data <- .sits_tibble_rename(data)

    # does the input data contain a set of predicted values?
    assertthat::assert_that("predicted" %in% names(data),
        msg = "sits_conf_matrix: input data without predicted values")

    # recover predicted and reference vectors from input
    # is the input the result of a sits_classify?
    if ("label" %in% names(data)) {
        pred_ref.tb <- .sits_pred_ref(data)
        pred.vec <- pred_ref.tb$predicted
        ref.vec  <- pred_ref.tb$reference
    }
    # is the input the result of the sits_kfold_validate?
    else{
        pred.vec <- data$predicted
        ref.vec  <- data$reference
    }

    # convert class names
    if (!purrr::is_null(conv.lst)) {
        # get those labels not in conv.lst names
        conv.lst <- .sits_labels_list(data, conv.lst)
        # select the label names
        names_ref <- unique(ref.vec)
        # are all input labels in the coversion list?
        assertthat::assert_that(all(names_ref %in% names(conv.lst)),
            msg = "sits_conf_matrix: missing reference labels")
        pred.vec <- as.character(conv.lst[pred.vec])
        ref.vec  <- as.character(conv.lst[ref.vec])
    }

    unique_ref <- unique(ref.vec)
    pred.fac <- factor(pred.vec, levels = unique_ref)
    ref.fac  <- factor(ref.vec, levels = unique_ref)
    # call caret package to the classification statistics
    caret_assess <- caret::confusionMatrix(pred.fac, ref.fac)

    # print the result
    .print_confusion_matrix(caret_assess)

    # return invisible
    return(invisible(caret_assess))
}

#' @title Area-weighted classification accuracy assessment
#' @name sits_accuracy
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description To use this function the input table should be
#' a set of results containing
#' both the label assigned by the user and the classification result.
#' Accuracy assessment set us a confusion matrix to determine the accuracy
#' of your classified result.
#' This function uses an area-weighted technique proposed by Olofsson et al. to
#' produce more reliable accuracy estimates at 95% confidence level.
#'
#' This function performs an accuracy assessment of the classified, including
#' Overall Accuracy, User's Accuracy, Producer's Accuracy
#' and error matrix (confusion matrix) according to [1-2].
#'
#' @references
#' [1] Olofsson, P., Foody, G.M., Stehman, S.V., Woodcock, C.E. (2013).
#' Making better use of accuracy data in land change studies: Estimating
#' accuracy and area and quantifying uncertainty using stratified estimation.
#' Remote Sensing of Environment, 129, pp.122-131.
#'
#' @references
#' [2] Olofsson, P., Foody G.M., Herold M., Stehman, S.V.,
#' Woodcock, C.E., Wulder, M.A. (2014)
#' Good practices for estimating area and assessing accuracy of land change.
#' Remote Sensing of
#' Environment, 148, pp. 42-57.
#'
#' @param label_cube       A sits labelled cube
#'                         with the result of classification method.
#' @param validation       A SITS tibble or CSV file with validation data
#'
#' @export
sits_accuracy <- function(label_cube, validation){

    assertthat::assert_that("classified_image" %in% class(label_cube),
                            msg = "sits_accuracy requires a labelled cube")



    # backward compatibility
    class.tb <- .sits_tibble_rename(class.tb)

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

    # Get area
    if (purrr::is_null(area))
        area <- rowSums(error_matrix)

    # Compute accuracy metrics
    assessment <- .sits_assess_accuracy_area(error_matrix, area)

    return(assessment)
}

#' @title Support for Area-weighted post-classification accuracy
#' @name .sits_assess_accuracy_area
#' @keywords internal
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @param error_matrix A matrix given in sample counts.
#'                     Columns represent the reference data and
#'                     rows the results of the classification
#' @param area         A vector of the total area of each class on the map
#'
#' @return             A list of lists: The error_matrix,
#'                     the class_areas,
#'                     confidence interval (confint95, a list of two numerics)
#'                     and the accuracy (accuracy, a list of three numerics:
#'                     overall, user, and producer)

.sits_assess_accuracy_area <- function(error_matrix, area){

    if (any(dim(error_matrix) == 0))
        stop("Invalid dimensions in error matrix.", call. = FALSE)
    if (length(unique(dim(error_matrix))) != 1)
        stop("The error matrix is not square.", call. = FALSE)
    if (!all(colnames(error_matrix) == rownames(error_matrix)))
        stop("Labels mismatch in error matrix.", call. = FALSE)
    if (unique(dim(error_matrix)) != length(area))
        stop("Mismatch between error matrix and area vector.",
             call. = FALSE)
    if (!all(names(area) %in% colnames(error_matrix)))
        stop("Label mismatch between error matrix and area vector.",
             call. = FALSE)

    # Re-order vector elements.
    area <- area[colnames(error_matrix)]

    W <- area/sum(area)
    n <- rowSums(error_matrix)
    if (any(n < 2))
        stop("Undefined accuracy: one pixel in a class (division by zero).",
             call. = FALSE)
    n.mat <- matrix(rep(n, times = ncol(error_matrix)),
                    ncol = ncol(error_matrix))
    p <- W * error_matrix / n.mat
    error_adjusted_area_estimate <- colSums(p) * sum(area)
    Sphat_1 <- vapply(seq_len(ncol(error_matrix)), function(i){
        sqrt(sum(W^2 * error_matrix[, i]/n * (1 - error_matrix[, i]/n)/(n - 1)))
    }, numeric(1))

    SAhat <- sum(area) * Sphat_1
    Ahat_sup <- error_adjusted_area_estimate + 2 * SAhat
    Ahat_inf <- error_adjusted_area_estimate - 2 * SAhat
    Ohat <- sum(diag(p))
    Uhat <- diag(p) / rowSums(p)
    Phat <- diag(p) / colSums(p)

    return(
        list(error_matrix = error_matrix, area = area,
             confint95 = list(superior = Ahat_sup, inferior = Ahat_inf),
             accuracy = list(overall = Ohat, user = Uhat, producer = Phat))
    )
}

#' @title Print the values of a confusion matrix
#' @name .print_confusion_matrix
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#
#' @description Adaptation of the caret::print.confusionMatrix method
#'              for the more common usage in Earth Observation.
#'
#' @param x         An object of class \code{confusionMatrix}.
#' @param mode      A single character string either "sens_spec",
#'                  "prec_recall", or "everything".
#' @param digits    Number of significant digits when printed.
#' @param \dots     Optional arguments to pass to \code{print.table}.
#' @return \code{x}   is invisibly returned.
#'
#' @seealso \code{\link[caret]{print.confusionMatrix}}
#'
.print_confusion_matrix <- function(x, mode = "sens_spec",
                              digits = max(3, getOption("digits") - 3), ...){
    cat("Confusion Matrix and Statistics\n\n")
    print(x$table, ...)

    # round the data to the significant digits
    overall <- round(x$overall, digits = digits)

    # get the values of the p-index
    # pIndex <- grep("PValue", names(x$overall))
    # tmp[pIndex] <- format.pval(x$overall[pIndex], digits = digits)
    # overall <- tmp

    accCI <- paste("(",
                   paste(overall[ c("AccuracyLower", "AccuracyUpper")],
                          collapse = ", "), ")",
                   sep = "")

    overallText <- c(paste(overall["Accuracy"]), accCI, "",
                     paste(overall["Kappa"]))

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
        x$byClass <- x$byClass[,
        grepl("(Sensitivity)|(Specificity)|(Pos Pred Value)|(Neg Pred Value)",
            colnames(x$byClass))]
        ass.mx <- t(x$byClass)
        rownames(ass.mx) <- c("Prod Acc (Sensitivity)", "Specificity",
                              "User Acc (Pos Pred Value)", "Neg Pred Value" )
        print(ass.mx, digits = digits)

    } else {
        # this is the case of ony two classes
        # get the values of the User's and Producer's Accuracy
        # Names in caret are different from the usual names in Earth observation
        x$byClass <- x$byClass[
        grepl("(Sensitivity)|(Specificity)|(Pos Pred Value)|(Neg Pred Value)",
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
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#
#' @description Obtains a tibble of predicted and reference values
#' from a classified data set.
#'
#' @param  class.tb  Set of classified samples whose labels are known.
#' @return           A tibble with predicted and reference values.
.sits_pred_ref <- function(class.tb) {
    # retrieve the predicted values
    pred.vec <- unlist(purrr::map(class.tb$predicted, function(r) r$class))

    # retrieve the reference labels
    ref.vec <- class.tb$label
    # does the input data contained valid reference labels?
    assertthat::assert_that(!("NoClass" %in% (ref.vec)),
        msg = "sits_accuracy: input data without labels")

    # build the tibble
    pred_ref.tb <- tibble::tibble("predicted" = pred.vec, "reference" = ref.vec)


    # return the tibble
    return(pred_ref.tb)
}

#' @title Saves the results of accuracy assessments as Excel files
#' @name sits_to_xlsx
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#
#' @description Saves confusion matrices as Excel spreadsheets. This function
#' takes the a list of confusion matrices generated
#' by the \code{\link[sits]{sits_conf_matrix}}
#' function and save them in an Excel spreadsheet.
#'
#' @param acc.lst        A list of confusion matrices.
#' @param file           The file where the XLSX data is to be saved.
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
#' sits_to_xlsx(results, file = "confusion_matrix.xlsx")
#'
#' # cleanup (optional)
#' file.remove("confusion_matrix.xlsx")
#' }
#' @export
sits_to_xlsx <- function(acc.lst, file){

    # create a workbook to save the results
    workbook <- openxlsx::createWorkbook("accuracy")
    # eo_names of the accuracy assessment parameters
    eo_n <- c("(Sensitivity)|(Specificity)|(Pos Pred Value)|(Neg Pred Value)")

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
            openxlsx::addWorksheet(workbook, sheet_name)

            # use only the class names (without the "Class: " prefix)
            new_names <- unlist(strsplit(colnames(acc.mx$table), split = ": "))

            # remove prefix from confusion matrix table
            colnames(acc.mx$table) <- new_names
            # write the confusion matrix table in the worksheet
            openxlsx::writeData(workbook, sheet_name, acc.mx$table)

            # overall assessment (accuracy and kappa)
            acc_kappa.mx <- as.matrix(acc.mx$overall[c(1:2)])

            # save the accuracy data in the worksheet
            openxlsx::writeData(wb    = workbook,
                                sheet = sheet_name,
                                x     = acc_kappa.mx,
                                rowNames = TRUE,
                                startRow = NROW(acc.mx$table) + 3,
                                startCol = 1)

            if (dim(acc.mx$table)[1] > 2) {
                # per class accuracy assessment
                acc_bc.mx <- t(acc.mx$byClass[,c(1:4)])
                # remove prefix from confusion matrix table
                colnames(acc_bc.mx)  <- new_names
                row.names(acc_bc.mx) <- c("Sensitivity (PA)",
                                          "Specificity",
                                          "PosPredValue (UA)",
                                          "NegPredValue")
            }
            else {
                # this is the case of ony two classes
                # get the values of the User's and Producer's Accuracy

                acc_bc.mx <- acc.mx$byClass[grepl(eo_n, names(acc.mx$byClass))]
                # get the names of the two classes
                nm <- row.names(acc.mx$table)
                # the first class (called the "positive" class by caret)
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
            openxlsx::writeData(wb     = workbook,
                                sheet = sheet_name,
                                x     = acc_bc.mx,
                                rowNames = TRUE,
                                startRow = NROW(acc.mx$table) + 8,
                                startCol = 1)
        })

    # write the worksheets to the XLSX file
    openxlsx::saveWorkbook(workbook, file = file, overwrite = TRUE)

    return(message(paste("Saved Excel file", file)))
}
