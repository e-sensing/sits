#' @title Save the contents of a SITS table in a JSON file
#' @name sits_save
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description stores a SITS table in a JSON file
#' The set of time series from a SITS table can be saved in JSON format
#' and later retrieved for further use
#'
#' @param source       tibble - an existing SITS table
#' @param json_file    string - name of the JSON file to be written
#' @return table       the same table (for chaining functions)
#' @export
#'
#'
sits_save <- function (source, json_file) {
     if ("sits_table" %in% class (source) || "tbl" %in% class (source) ){
          data.tb <- sits_toJSON (source = source, file = json_file)
          return (invisible (source))
     }
     message (paste ("No valid input to save time series data!!","\n",sep=""))
     stop(cond)
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
     return (invisible (source))
}
