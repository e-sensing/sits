
#' Store the contents of an in-memory database in a JSON file
#'
#' \code{sits_toJSON} stores a SITS table in a JSON file
#'
#' The set of time series from a SITS table can be saved in JSON format
#' and later retrieved for further use
#'
#' @param json_file   string - name of the JSON file to be written
#' @param table       tibble - an existing SITS table
#' @return table      the same table (for chaining functions)
#' @keywords STIS
#' @family   STIS main functions
#' @examples table <- sits_toJSON (json_file = "myjson_file.json", table = "my.db")
#' @export
sits_toJSON <- function (json_file, table) {

     # does the database exist?
     sits_assert(table)
     # store the contents of table in a JSON file
     table %>%
          jsonlite::toJSON (pretty = TRUE) %>%
          write_lines (json_file)
     # return the table for chaining sits functions
     return (table)
}

# -----------------------------------------------------------
#' Retrieve a set of time series from a JSON file
#'
#' \code{sits_from_JSON} gets a SITS database from a JSON file
#'
#' The set of time series from the SITS database can be saved in JSON format
#' and later retrieved for further use
#'
#' @param json_file   string - name of the JSON file
#' @return  table     tibble - a new SITS table
#' @keywords STIS
#' @family   STIS main functions
#' @export
#' @examples table <- sits_fromJSON ("myjson_file.json")
sits_fromJSON <- function (json_file) {

     # create a new table to store the contents of the JSON file
     table <- sits_table()
     # add the contents of the JSON file to the in-memory database
     table <- as_tibble (jsonlite::fromJSON (json_file))
     # convert Indexes in time series to dates
     for (i in 1:nrow(table)) {
          tb <- as_tibble(table[i,]$time_series[[1]])
          tb$Index <- as_date(tb$Index)
          table[i,]$time_series[[1]] <- tb
     }
     # convert start and end date to Date format
     table <- dplyr::mutate (table, start_date = as.Date(start_date))
     table <- dplyr::mutate (table, end_date   = as.Date(end_date))
     return (table)
}
