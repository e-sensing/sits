
# -----------------------------------------------------------
#' Store the contents of an in-memory database in a JSON file
#'
#' \code{sits_toJSON} gets a SITS table from a JSON file
#'
#' The set of time series from a SITS table can be saved in JSON format
#' and later retrieved for further use
#'
#' @param json_file   name of the JSON file
#' @param table       a string with the name of a global database (previously created)
#' @return            an updated in-memory database
#' @keywords STIS
#' @family   STIS
#' @examples sits_toJSON ("myjson_file.json", database = "my.db")
#' @export
sits_toJSON <- function (json_file, table) {

     # does the database exist?
     sits_assert_table(table)
     # add the contents of the JSON file to the in-memory database
     table %>%
          toJSON (pretty = TRUE) %>%
          write_lines (json_file)
}


# -----------------------------------------------------------
#' Retrieve a set of time series from a JSON file
#'
#' \code{sits_from_JSON} gets a SITS database from a JSON file
#'
#' The set of time series from the SITS database can be saved in JSON format
#' and later retrieved for further use
#'
#' @param json_file   name of the JSON file
#' @param table      a string with the name of a global sits table (created if not existent)
#' @return            an updated in-memory database
#' @keywords STIS
#' @family   STIS
#' @examples sits_fromJSON ("myjson_file.json", table = "my.tb")
sits_fromJSON <- function (json_file) {

     # does the database exist?
     #sits_check_table (table)
     # retrieve the in-memory database specified by the user
     table <- sits_table()
     # add the contents of the JSON file to the in-memory database
     table <- as_tibble (fromJSON (json_file))
     for (i in 1:nrow(table)) {
          tb <- as_tibble(table[i,]$time_series[[1]])
          tb$Index <- as_date(tb$Index)
          table[i,]$time_series[[1]] <- tb
     }
     table <- mutate (table, from = as.Date(from))
     table <- mutate (table, to = as.Date(to))
     # assign the in-memory database to the global enviroment
     # assign(table, global.db, envir=globalenv())
     return (table)
}
