#' @title Export the contents of a SITS table in a compress JSON file
#' @name sits_toGZ
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description stores a SITS table in a compressed JSON file
#' The set of time series from a SITS table can be saved in a compressed JSON format
#' and later retrieved for further use
#'
#' @param source       tibble - an existing SITS table
#' @param file_name    string - file name where compressed data will be saved
#' @return table       the same table (for chaining functions)
#' @export
#'
#'
sits_toGZ <- function (source, file_name = NULL) {
    # test the input data set
    .sits_test_table(source)

    ensurer::ensure_that (file_name, !purrr::is_null(.),
                          err_desc = "sits_save: please provide the file name")

    # save to JSON
    file_name <- sits_toJSON (source, file_name)

    # compress the file and remove the uncompresed JSON file
    R.utils::gzip (file_name, remove = TRUE)

    return (invisible (source))
}

#' @title Save data in a JSON file
#' @name sits_toJSON
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description stores data in a JSON file
#'
#' @param source       an R object
#' @param file         string - name of the JSON file to be written
#' @export
#'
#'
sits_toJSON <- function (source, file = NULL) {

     if (purrr::is_null(file)){
          name <- deparse(substitute(source))
          file = paste("./",name,".json", sep = "")
     }
     # store the contents of table in a JSON file
     source %>%
          jsonlite::toJSON (pretty = TRUE) %>%
          readr::write_lines (file)
     # return the table for chaining sits functions
     return (file)
}

#' @title Export data to be used to the zoo format
#' @name sits_toZOO
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts data from a SITS table to an instance of a zoo series,
#'
#' @param  ts.tb    a tibble with the indexes and values of a SITS time series
#' @param  band     the name of the band to be exported (if NULL all bands are exported)
#' @return ts.zoo   a time series in zoo format
#' @export
sits_toZOO <- function (ts.tb, band = NULL){
    if (purrr::is_null(band))
        band <-  colnames(ts.tb[-1:0])
    # transform each sits time series into a list of zoo
    return (zoo::zoo(ts.tb[,band, drop=FALSE], ts.tb$Index))
}
