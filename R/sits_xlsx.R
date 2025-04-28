#' @title Save accuracy assessments as Excel files
#' @name sits_to_xlsx
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#
#' @description Saves confusion matrices as Excel spreadsheets. This function
#' takes the a list of accuracy assessments generated
#' by \code{\link[sits]{sits_accuracy}}
#' and saves them in an Excel spreadsheet.
#'
#' @param acc       Accuracy statistics (either an output of sits_accuracy
#'                  or a list of those)
#' @param file      The file where the XLSX data is to be saved.
#'
#' @return No return value, called for side effects.
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#' @examples
#' if (sits_run_examples()) {
#'     # A dataset containing a tibble with time series samples
#'     # for the Mato Grosso state in Brasil
#'     # create a list to store the results
#'     results <- list()
#'
#'     # accuracy assessment lightTAE
#'     acc_ltae <- sits_kfold_validate(samples_modis_ndvi,
#'         folds = 5,
#'         multicores = 1,
#'         ml_method = sits_lighttae()
#'     )
#'     # use a name
#'     acc_ltae$name <- "LightTAE"
#'
#'     # put the result in a list
#'     results[[length(results) + 1]] <- acc_ltae
#'
#'     # save to xlsx file
#'     sits_to_xlsx(
#'         results,
#'         file = tempfile("accuracy_mato_grosso_dl_", fileext = ".xlsx")
#'     )
#' }
#' @export
#'
#'
sits_to_xlsx <- function(acc, file) {
    UseMethod("sits_to_xlsx", acc)
}
#' @rdname sits_to_xlsx
#' @export
#'
sits_to_xlsx.sits_accuracy <- function(acc, file) {
    acc_lst <- list(acc)
    sits_to_xlsx(acc_lst, file)
}
#' @rdname sits_to_xlsx
#' @export
#'
sits_to_xlsx.list <- function(acc, file) {
    # set caller to show in errors
    .check_set_caller("sits_to_xlsx")
    # create a workbook to save the results
    workbook <- openxlsx::createWorkbook("accuracy")
    # eo_names of the accuracy assessment parameters
    eo_n <- "(Sensitivity)|(Specificity)|(Pos Pred Value)|(Neg Pred Value)|(F1)"
    # defined the number of sheets
    num_sheets <- length(acc)
    .check_that(length(num_sheets) >= 1L)
    # save all elements of the list
    purrr::map2(acc, seq_len(num_sheets), function(cf_mat, ind) {
        # create a worksheet for each confusion matrix
        if (!.has(cf_mat[["name"]])) {
            cf_mat[["name"]] <- paste0("sheet", ind)
        }
        sheet_name <- cf_mat[["name"]]
        # add a worksheet
        openxlsx::addWorksheet(workbook, sheet_name)
        # use only the class names (without the "Class: " prefix)
        new_names <- unlist(strsplit(colnames(cf_mat[["table"]]), split = ": "))
        # remove prefix from confusion matrix table
        colnames(cf_mat[["table"]]) <- new_names
        # write the confusion matrix table in the worksheet
        openxlsx::writeData(workbook, sheet_name, cf_mat[["table"]])
        # overall assessment (accuracy and kappa)
        acc_kappa <- as.matrix(cf_mat[["overall"]][1L:2L])
        # save the accuracy data in the worksheet
        openxlsx::writeData(
            wb = workbook,
            sheet = sheet_name,
            x = acc_kappa,
            rowNames = TRUE,
            startRow = nrow(cf_mat[["table"]]) + 3L,
            startCol = 1L
        )
        # obtain the per class accuracy assessment
        if (dim(cf_mat[["table"]])[[1L]] > 2L) {
            # per class accuracy assessment
            acc_bc <- t(cf_mat[["byClass"]][, c(1L:4L, 7L)])
            # remove prefix from confusion matrix table
            colnames(acc_bc) <- new_names
            row.names(acc_bc) <- c(
                "Sensitivity (PA)",
                "Specificity",
                "PosPredValue (UA)",
                "NegPredValue",
                "F1 score"
            )
        } else {
            # this is the case of ony two classes
            # get the values of the User's and Producer's Accuracy
            acc_bc <- cf_mat[["byClass"]][grepl(
                eo_n, names(cf_mat[["byClass"]])
            )]
            # get the names of the two classes
            nm <- row.names(cf_mat[["table"]])
            # the first class (called the "positive" class by caret)
            c1 <- cf_mat[["positive"]]
            # the second class
            c2 <- nm[(nm != cf_mat[["positive"]])]
            # make up the values of UA and PA for the two classes
            pa1 <- paste("Prod Acc ", c1)
            pa2 <- paste("Prod Acc ", c2)
            ua1 <- paste("User Acc ", c1)
            ua2 <- paste("User Acc ", c2)
            names(acc_bc) <- c(pa1, pa2, ua1, ua2)
            acc_bc <- as.matrix(acc_bc)
        }
        # save the per class data in the worksheet
        start_row <- nrow(cf_mat[["table"]]) + 8L
        openxlsx::writeData(
            wb = workbook,
            sheet = sheet_name,
            x = acc_bc,
            rowNames = TRUE,
            startRow = start_row,
            startCol = 1L
        )
    })
    # write the worksheets to the XLSX file
    openxlsx::saveWorkbook(workbook, file = file, overwrite = TRUE)
    message(.conf("messages", "sits_to_xlsx_save"), file)
}
