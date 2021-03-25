#' @title Saves the results of accuracy assessments as Excel files
#' @name sits_to_xlsx
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#
#' @description Saves confusion matrices as Excel spreadsheets. This function
#' takes the a list of accuracy assessments generated
#' by \code{\link[sits]{sits_accuracy}}
#' and saves them in an Excel spreadsheet.
#'
#' @param acc_lst        A list of accuracy statistics
#' @param file           The file where the XLSX data is to be saved.
#'
#' @examples
#' \dontrun{
#' # read a tibble with 400 samples of Cerrado and 346 samples of Pasture
#' data(cerrado_2classes)
#' # perform a 2 fold validation of this sample file
#' accuracy <- sits_kfold_validate(cerrado_2classes,
#'     folds = 2,
#'     ml_method = sits_rfor(num_trees = 300)
#' )
#' # create a list to store the results
#' results <- list()
#' # give a name to the accuracy assessment
#' accuracy$name <- "cerrado_2classes"
#' # add the confusion matrix to the results
#' results[[length(results) + 1]] <- accuracy
#' # save the results to an XLSX file
#' xlsx_file <- paste0(tempdir(), "accuracy.xlsx")
#' sits_to_xlsx(results, file = xlsx_file)
#' }
#'
#' @export
#'
sits_to_xlsx <- function(acc_lst, file) {

    # create a workbook to save the results
    workbook <- openxlsx::createWorkbook("accuracy")
    # eo_names of the accuracy assessment parameters
    eo_n <- c("(Sensitivity)|(Specificity)|(Pos Pred Value)|(Neg Pred Value)")

    num_sheets <- length(acc_lst)
    assertthat::assert_that(
        length(num_sheets) > 0,
        msg = "sits_to_xlsx: number of sheets should be at least one")

    # save all elements of the list
    purrr::map2(acc_lst, 1:num_sheets, function(cf_mat, ind) {

        # create a worksheet for each confusion matrix
        if (purrr::is_null(cf_mat$name)) {
            cf_mat$name <- paste0("sheet", ind)
        }
        sheet_name <- cf_mat$name

        # add a worksheet
        openxlsx::addWorksheet(workbook, sheet_name)

        # use only the class names (without the "Class: " prefix)
        new_names <- unlist(strsplit(colnames(cf_mat$table), split = ": "))

        # remove prefix from confusion matrix table
        colnames(cf_mat$table) <- new_names
        # write the confusion matrix table in the worksheet
        openxlsx::writeData(workbook, sheet_name, cf_mat$table)

        # overall assessment (accuracy and kappa)
        acc_kappa <- as.matrix(cf_mat$overall[c(1:2)])

        # save the accuracy data in the worksheet
        openxlsx::writeData(
            wb = workbook,
            sheet = sheet_name,
            x = acc_kappa,
            rowNames = TRUE,
            startRow = nrow(cf_mat$table) + 3,
            startCol = 1
        )

        if (dim(cf_mat$table)[1] > 2) {
            # per class accuracy assessment
            acc_bc <- t(cf_mat$byClass[, c(1:4)])
            # remove prefix from confusion matrix table
            colnames(acc_bc) <- new_names
            row.names(acc_bc) <- c(
                "Sensitivity (PA)",
                "Specificity",
                "PosPredValue (UA)",
                "NegPredValue"
            )
        }
        else {
            # this is the case of ony two classes
            # get the values of the User's and Producer's Accuracy

            acc_bc <- cf_mat$byClass[grepl(eo_n, names(cf_mat$byClass))]
            # get the names of the two classes
            nm <- row.names(cf_mat$table)
            # the first class (called the "positive" class by caret)
            c1 <- cf_mat$positive
            # the second class
            c2 <- nm[!(nm == cf_mat$positive)]
            # make up the values of UA and PA for the two classes
            pa1 <- paste("Prod Acc ", c1)
            pa2 <- paste("Prod Acc ", c2)
            ua1 <- paste("User Acc ", c1)
            ua2 <- paste("User Acc ", c2)
            names(acc_bc) <- c(pa1, pa2, ua1, ua2)
            acc_bc <- as.matrix(acc_bc)
        }
        # save the per class data in the worksheet
        openxlsx::writeData(
            wb = workbook,
            sheet = sheet_name,
            x = acc_bc,
            rowNames = TRUE,
            startRow = nrow(cf_mat$table) + 8,
            startCol = 1
        )
    })

    # write the worksheets to the XLSX file
    openxlsx::saveWorkbook(workbook, file = file, overwrite = TRUE)

    return(message(paste("Saved Excel file", file)))
}
