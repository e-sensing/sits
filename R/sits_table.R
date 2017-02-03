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
