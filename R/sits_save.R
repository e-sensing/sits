#' Save the contents of an in-memory database in a JSON file
#'
#' \code{sits_toJSON} stores a SITS table in a JSON file
#'
#' The set of time series from a SITS table can be saved in JSON format
#' and later retrieved for further use
#'
#' @param source       tibble - an existing SITS table
#' @param json_file    string - name of the JSON file to be written
#' @return table       the same table (for chaining functions)
#' @keywords STIS
#' @family   STIS main functions
#' @export
#'
#'
sits_save <- function (source, json_file = NULL) {
     if ("sits_table" %in% class (source) || "tbl" %in% class (source) ){

          if (purrr::is_null(json_file)){
               table_name <- deparse(substitute(source))
               json_file = paste("./",table_name,".json", sep = "")
          }

          data.tb <- .sits_toJSON (source = source, json_file = json_file)
          return (invisible (source))
     }
     message (paste ("No valid input to save time series data!!","\n",sep=""))
     stop(cond)
}

.sits_toJSON <- function (source, json_file) {
     # store the contents of table in a JSON file
     source %>%
          jsonlite::toJSON (pretty = TRUE) %>%
          readr::write_lines (json_file)
     # return the table for chaining sits functions
     return (source)
}
