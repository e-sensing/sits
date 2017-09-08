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
