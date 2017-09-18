#' @title Obtain a confusion matrix from a compressed JSON file.
#'
#' @name sits_conf_fromGZ
#'
#' @description reads a set of data and metadata for satellite image time series from a compressed JSON file
#'
#' @param file        string  - name of a compressed JSON file with sits data and metadata
#' @return data.tb    tibble  with a confusion matrix
#' @export
sits_conf_fromGZ <- function (file) {

    # uncompress the file
    json_file <- R.utils::gunzip (file, remove = FALSE)
    # retrieve the data
    conf.tb <- sits_conf_fromJSON (json_file)
    # remove the uncompressed file
    file.remove (json_file)

    # return the JSON file
    return (conf.tb)
}

#' @title Obtain a confusion matrix from a JSON file.
#'
#' @name sits_conf_fromJSON
#'
#' @description reads a set of data and metadata for satellite image time series from a JSON file
#'
#' @param  file       string  - name of a JSON file with sits data and metadata
#' @return data.tb    tibble  with a confusion matrix
#' @export
sits_conf_fromJSON <- function (file) {
    # add the contents of the JSON file to a SITS tibble
    data.tb <- tibble::as_tibble (jsonlite::fromJSON (file))

    return (data.tb)
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
