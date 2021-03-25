#' @title Print the values of a confusion matrix
#' @name .sits_conf_matrix_show
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#
#' @description Adaptation of the caret::print.confusionMatrix method
#'              for the more common usage in Earth Observation.
#'
#' @param x         An object of class \code{confusionMatrix}.
#' @param mode      A single character string either "sens_spec",
#'                  "prec_recall", or "everything".
#' @param digits    Number of significant digits when printed.
#' @return           \code{x}   is invisibly returned.
#'
#' @keywords internal
.sits_conf_matrix_show <- function(x, mode = "sens_spec",
                                   digits = max(3, getOption("digits") - 3)
                                   ) {
    cat("Confusion Matrix and Statistics\n\n")
    print(x$table)

    # round the data to the significant digits
    overall <- round(x$overall, digits = digits)

    accuracy_ci <- paste("(",
                         paste(overall[c("AccuracyLower", "AccuracyUpper")],
                               collapse = ", "
                         ), ")",
                         sep = ""
    )

    overall_text <- c(
        paste(overall["Accuracy"]), accuracy_ci, "",
        paste(overall["Kappa"])
    )

    overall_names <- c("Accuracy", "95% CI", "", "Kappa")

    if (dim(x$table)[1] > 2) {
        cat("\nOverall Statistics\n")
        overall_names <- ifelse(overall_names == "",
                                "",
                                paste(overall_names, ":")
        )
        out <- cbind(format(overall_names, justify = "right"), overall_text)
        colnames(out) <- rep("", ncol(out))
        rownames(out) <- rep("", nrow(out))

        print(out, quote = FALSE)

        cat("\nStatistics by Class:\n\n")
        x$byClass <- x$byClass[, grepl(
            "(Sensitivity)|(Specificity)|(Pos Pred Value)|(Neg Pred Value)",
            colnames(x$byClass)
        )]
        measures <- t(x$byClass)
        rownames(measures) <- c(
            "Prod Acc (Sensitivity)", "Specificity",
            "User Acc (Pos Pred Value)", "Neg Pred Value"
        )
        print(measures, digits = digits)
    } else {
        # this is the case of only two classes
        # get the values of the User's and Producer's Accuracy
        # Names in caret are different from the usual names in Earth observation
        x$byClass <- x$byClass[
            grepl(
                "(Sensitivity)|(Specificity)|(Pos Pred Value)|(Neg Pred Value)",
                names(x$byClass)
            )
        ]
        # get the names of the two classes
        names_classes <- row.names(x$table)
        # the first class (which is called the "positive" class by caret)
        c1 <- x$positive
        # the second class
        c2 <- names_classes[!(names_classes == x$positive)]
        # make up the values of UA and PA for the two classes
        pa1 <- paste("Prod Acc ", c1)
        pa2 <- paste("Prod Acc ", c2)
        ua1 <- paste("User Acc ", c1)
        ua2 <- paste("User Acc ", c2)
        names(x$byClass) <- c(pa1, pa2, ua1, ua2)

        overall_text <- c(
            overall_text,
            "",
            format(x$byClass, digits = digits)
        )
        overall_names <- c(overall_names, "", names(x$byClass))
        overall_names <- ifelse(overall_names == "", "",
                                paste(overall_names, ":"))

        out <- cbind(format(overall_names, justify = "right"), overall_text)
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
#' @param  class     Tibble with classified samples whose labels are known.
#' @return           A tibble with predicted and reference values.
.sits_pred_ref <- function(class) {
    # retrieve the predicted values
    pred <- unlist(purrr::map(class$predicted, function(r) r$class))

    # retrieve the reference labels
    ref <- class$label
    # does the input data contained valid reference labels?
    assertthat::assert_that(
        !("NoClass" %in% (ref)),
        msg = "sits_accuracy: input data without labels"
    )

    # build the tibble
    pred_ref <- tibble::tibble("predicted" = pred, "reference" = ref)
    # return the tibble
    return(pred_ref)
}
