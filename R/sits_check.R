#' Checks if the data is a valid SITS tibble
#'
#' \code{sits_check} tests the names of the columns of a tibble to
#' verifiy if they include the mandatory columns
#' <longitude>, <latitude>, <start_date>, <end_date>, <label>, <coverage>
#' @param sits.tb    a table (tibble) in SITS formal
#' @return TRUE/FALSE
#' @export
#'
sits_check <- function (sits.tb) {

     if (c("longitude", "latitude", "start_date", "end_date", "label", "coverage")
         %in% names (sits.tb)) {
          return (TRUE)
     }
     else {
          message (paste ("Input table is not a valid sits table!!","\n",
                          "A sits table has the following mandatory columns","\n",
                          "<longitude>, <latitude>, <start_date>, <end_date>, <label>, <coverage>","\n",
                          sep=""))
          stop(cond)
     }
}
