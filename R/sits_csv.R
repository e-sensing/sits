#' @title Export a tibble data to the CSV format
#' @name sits_toCSV
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts data from a SITS tibble to a CSV file
#'
#' @param  data.tb    a SITS time series
#' @param  file       the name of the CSV file to be exported
#' @return status     the status of the operation
#' @examples
#' \donttest{
#' # read a tibble with 400 samples of Cerrado and 346 samples of Pasture
#' data(cerrado_2classes)
#' # export a time series to zoo
#' sits_toCSV (cerrado_2classes, file = "./cerrado_2classes.csv")
#' }
#' @export
sits_toCSV <- function(data.tb, file){


    csv_columns <- c("longitude", "latitude", "start_date", "end_date", "label")

    #select the parts of the tibble to be saved
    csv.tb <- dplyr::select(data.tb, csv_columns)

    # create a column with the id
    id.tb <- tibble::tibble(id = 1:NROW(csv.tb))

    # join the two tibbles
    csv.tb <- dplyr::bind_cols(id.tb, csv.tb)

    # write the CSV file
    utils::write.csv(csv.tb, file, row.names = FALSE, quote = FALSE)

    return(invisible(TRUE))
}


