#' Create a sits table to store the time series information
#'
#' \code{sits_table} returns an empty sits table
#' A sits table has the metadata and data for each time series
#' <longitude, latitude, from, to, label, coverage, time_series>
#'
#' @return table  a tibble in SITS format
#' @export

sits_table <- function () {
     df <- data.frame(longitude   = double(),
                      latitude    = double (),
                      from        = as.Date(character()),
                      to          = as.Date(character()),
                      label       = character(),
                      coverage    = character(),
                      stringsAsFactors = FALSE
     )
     tb <- as_tibble (df)
     tb <- add_column (tb, time_series = list())
     # assign the table to the global enviroment
     #assign(table_name, tb, envir=globalenv())
     return (tb)
}

sits_add_row <- function (table, row){
     table <- add_row (table,
                       longitude    = row$longitude,
                       latitude     = row$latitude,
                       from         = row$from,
                       to           = row$to,
                       label        = row$label,
                       coverage     = row$coverage,
                       time_series  = row$time_series
     )
     return (table)
}


sits_tibble_values_only <- function (data.tb, band) {
     values.tb <- data.tb$time_series %>%
          data.frame() %>%
          as_tibble() %>%
          select (starts_with (band)) %>%
          t()
     return (values.tb)
}
#
#' Finds the names of the bands of a time series
#'
#' \code{sits_band_names} tests if the sits table exists and has data
#' @param ts     a time series extracted from a sits table
#' @return names a string vector with the names of the bands
#'
#'
sits_band_names <- function (ts) {
     mynames <- ts %>%
          data.frame() %>%
          colnames() %>%
          . [2:length(.)]
     return (mynames)
}
#
#' Verify if the sits table has been created
#'
#' \code{sits_assert_table} tests if the sits table exists
#' @param table_name  a string - name of table (must exist)
#'
#'
sits_assert_table <- function (table_obj) {
     tryCatch (
          exists(deparse(substitute(table_obj))),
          error = function (cond) {
               message (paste ("Missing SITS table name!!","\n",
                               "Please provide the name of an existing sits table","\n",
                               sep=""))
               stop(cond)
          }
     )
}
#
#' Check if a table exists in the global environment
#'
#' \code{sits_check_table} tests if the sits table exists and creates a new one if the response is negative
#' The table is created in the global environment
#' @param table_name  a string - name of table (created if not existent)
#'
#'
sits_check_table <- function (table_obj) {
     table_name <-
          if (!exists(deparse(substitute(table_obj)))) {
               cat (paste ("Creating a new SITS table - ", table_name,"\n", sep=""))
               table_obj <- sits_table
          }
}
#
#' Check if a table is empty
#'
#' \code{sits_is_empty} tests if the sits table exists and has data
#' @param table_obj  an object (the sits table)
#'
#'
sits_is_empty <- function (table_obj) {
     if (nrow (table_obj) == 0 ) {
          cat (paste ("table ",deparse(substitute(table_obj)," is empty!!" ,"\n", sep="")))
     }
}

