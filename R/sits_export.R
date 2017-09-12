#' @title Export the contents of a SITS tibble in a compressed JSON file
#' @name sits_toGZ
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description stores a SITS tibble in a compressed JSON file
#' The set of time series from a SITS tibble can be saved in a compressed JSON format
#' and later retrieved for further use
#'
#' @param  data.tb      a SITS tibble
#' @param  file         string - file name where compressed data will be saved
#' @return data.tb      the input tibble (for chaining functions)
#' @export
#'
#'
sits_toGZ <- function (data.tb, file = NULL) {
    # test the input data set
    .sits_test_tibble (data.tb)

    ensurer::ensure_that (file, !purrr::is_null(.),
                          err_desc = "sits_toGZ: please provide the file name")

    # save to JSON
    json_file <- .sits_toJSON_tmp (data.tb, file)

    # compress the file and remove the uncompresed JSON file
    R.utils::gzip (json_file, remove = TRUE)

    return (invisible (data.tb))
}

#' @title Save data in a compressed JSON file
#' @name sits_toJSON
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description stores data in a JSON file
#'
#' @param data.tb      a SITS tibble
#' @param file         string - name of the JSON file to be written
#' @export
#'
#'
sits_toJSON <- function (data.tb, file = NULL) {
    sits_toGZ (data.tb, file)
}

#' @title Save data in a temporary JSON file for later compression
#' @name .sits_toJSON_tmp
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description stores data in a JSON file as an intermediate step
#' for compression in a GZ file. This function is called from by sits_toGZ and
#' by sits_toJSON
#'
#' @param data.tb      a SITS tibble
#' @param file         name of the temp JSON file to be written
#'
.sits_toJSON_tmp <- function (data.tb, file = NULL) {

     if (purrr::is_null(file)){
          name <- deparse(substitute(data.tb))
          file = paste("./",name,".json", sep = "")
     }
     # store the contents of tibble in a JSON file
     data.tb %>%
          jsonlite::toJSON (pretty = TRUE) %>%
          readr::write_lines (file)
     # return the file for later compression
     return (file)
}

#' @title Export data to be used to the zoo format
#' @name sits_toZOO
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts data from a SITS tibble to an instance of a zoo series,
#'
#' @param  ts.tb    a tibble with the indexes and values of a SITS time series
#'                  the tibble should have only the time series and should not be a full tibble
#' @param  band     the name of the band to be exported (if NULL all bands are exported)
#' @return ts.zoo   a time series in zoo format
#' @export
sits_toZOO <- function (ts.tb, band = NULL){
    if (purrr::is_null(band))
        band <-  colnames(ts.tb[-1:0])
    # transform each sits time series into a list of zoo
    return (zoo::zoo(ts.tb[,band, drop=FALSE], ts.tb$Index))
}

#' @title Saves the results of accuracy assessment as Excel files
#' @name sits_toXLSX
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#
#' @description Saves the the accuracy of classifications as Excel spreadsheets
#' Returns a confusion matrix used by the "caret" package
#'
#' @param acc.lst        A list of caret S4 object with accuracy assesments
#' @param file           The file where the CSV data is to be saved
#' @return conf.mx       The input confusion matrix
#'
#' @export
sits_toXLSX <- function(acc.lst, file = NULL){

    ensurer::ensure_that (file, !purrr::is_null(.),
                          err_desc = "sits_toXLSX: please provide the file name")

    # create a workbook to save the results
    wb <- openxlsx::createWorkbook ("accuracy")

    ind <- 0

    # save all elements of the list
    purrr::map (acc.lst, function (acc.mx){
        # create a sheet name

        if (purrr::is_null(acc.mx$name)) {
            ind <<- ind + 1
            acc.mx$name <- paste0('sheet', ind)
        }
        sheet_name <- acc.mx$name

        # add a worksheet
        openxlsx::addWorksheet(wb, sheet_name)

        # use only the class names (without the "Class: " prefix)
        new_names <- unlist(strsplit(colnames(acc.mx$table), split =": "))

        # remove prefix from confusion matrix table
        colnames (acc.mx$table) <- new_names
        # write the confusion matrix table in the worksheet
        openxlsx::writeData (wb, sheet_name, acc.mx$table)

        # overall assessment (accuracy and kappa)
        acc_kappa.mx <- as.matrix(acc.mx$overall[c(1:2)])

        # save the accuracy data in the worksheet
        openxlsx::writeData (wb, sheet_name, acc_kappa.mx, rowNames = TRUE, startRow = NROW(acc.mx$table) + 3, startCol = 1)

        if (dim(acc.mx$table)[1] > 2) {
            # per class accuracy assessment
            acc_bc.mx <- t(acc.mx$byClass[,c(1:4)])
            # remove prefix from confusion matrix table
            colnames (acc_bc.mx) <- new_names
            row.names(acc_bc.mx)<- c("Sensitivity (PA)", "Specificity", "PosPredValue (UA)", "NegPredValue")
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
            names (acc_bc.mx) <- c(pa1, pa2, ua1, ua2)
            acc_bc.mx <- as.matrix (acc_bc.mx)
        }
        # save the perclass data in the worksheet
        openxlsx::writeData (wb, sheet_name, acc_bc.mx, rowNames = TRUE, startRow = NROW(acc.mx$table) + 8, startCol = 1)
    })

    # write the worksheets to the XLSX file
    openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)

    return (NULL)
}
#' @title Export data to be used by the dtwSat package
#' @name sits_toTWDTW
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts data from a SITS table to an instance of a TWDTW time series class,
#' Returns a twdtwTimeSeries object (S4)
#'
#'
#' @param  data.tb       a table in SITS format with time series to be converted to TWTDW time series
#' @return ts.twdtw      a time series in TWDTW format (an object of the twdtwTimeSeries class)
sits_toTWDTW <- function (data.tb){
    # transform each sits time series into a list of zoo
    ts <- data.tb$time_series %>%
        purrr::map(function (ts) zoo::zoo(ts[,2:ncol(ts), drop=FALSE], ts$Index))

    # create a new twdtwTimeSeries object from list above
    ts.twdtw <- methods::new("twdtwTimeSeries", timeseries = ts,
                             labels = as.character(data.tb$label))
    return (ts.twdtw)
}
#' @title Export data to be used by the dtwSat package
#' @name sits_toTWDTW_matches
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Converts data from a SITS table to an instance of a TWDTW matches class,
#' Returns a dtwSat::twdtwMatches object (S4)
#'
#' @param  data.tb       a table in SITS format with time series to be converted to TWTDW time series
#' @param  patterns.tb   patterns SITS tibble used to matching
#' @return ts.twdtw      a time series in TWDTW format (an object of the twdtwTimeSeries class)
#'
sits_toTWDTW_matches <- function(data.tb, patterns.tb){
    # compute patterns dtwSat::twdtwTimeSeries object
    pat.twdtw <- patterns.tb %>%
        sits_toTWDTW()

    # traverse data.tb and, for each row, create a list of dtwSat::twdtwMatches objects
    data.tb %>%
        purrrlyr::by_row(function (row.tb){
            # get predicted labels (pattern labels in matches)
            labels <- base::unique(row.tb$matches[[1]]$predicted)

            # traverse predicted labels and, for each entry, generate the alignments' information
            # required by dtwSat::twdtwMatches@alignments
            align.lst <- labels %>%
                purrr::map(function (lb){
                    entry.lst <- list(label = c(lb))
                    entry.lst <- c(entry.lst, row.tb$matches[[1]] %>%
                                       dplyr::filter(predicted == lb) %>%
                                       dplyr::select(-predicted) %>%
                                       purrr::map(function (col) col))
                    entry.lst <- c(entry.lst, list(K = length(entry.lst$from),
                                                   matching = list(), internals = list()))
                    entry.lst
                })

            # names of each entry in list of alignments
            names(align.lst) <- labels

            # generate a dtwSat::twdtwTimeSeries object for the correspondent time series matched by patterns
            ts.twdtw <- row.tb %>%
                sits_toTWDTW()

            # with all required information, creates a new dtwSat::twdtwMatches object for this row
            ts.twdtw <- methods::new("twdtwMatches", timeseries = ts.twdtw,
                                     patterns = pat.twdtw, alignments = list(align.lst))
        }, .to = "matches", .labels = FALSE) %>%
        .$matches
}
