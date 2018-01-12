#' @title Obtain timeSeries from time series server, based on a CSV file.
#' @name sits_fromCSV
#'
#' @description reads descriptive information about a set of
#' spatio-temporal locations from a CSV file. Then, it uses the WTSS time series service
#' to retrieve the time series, and stores the time series on a SITS tibble for later use.
#' The CSV file should have the following column names:
#' "longitude", "latitude", "start_date", "end_date", "label"
#'
#' @param csv_file        string  - name of a CSV file with information <id, latitude, longitude, from, end, label>
#' @param service         string - name of the time series service (options are "WTSS" or "SATVEG")
#' @param product         string - the name of the product (e.g., "MOD13Q1")
#' @param coverage        string - the name of the coverage to be retrieved
#' @param bands           string vector - the names of the bands to be retrieved
#' @param prefilter       string ("0" - none, "1" - no data correction, "2" - cloud correction, "3" - no data and cloud correction)
#' @param n_save         number of samples to save as intermediate files (used for long reads)
#' @param n_max          the maximum number of samples to be read
#' @return data.tb        a SITS tibble
#'
#' @examples
#' \donttest{
#' #' # Read a set of points defined in a CSV file from a WTSS server
#' csv_file <- system.file ("extdata/samples/samples_import.csv", package = "sits")
#' points.tb <- sits_fromCSV (file = csv_file)
#' }
#' @export

sits_fromCSV <-  function(csv_file,
                          service   = "WTSS",
                          product   = "MOD13Q1",
                          coverage  = "mod13q1_512",
                          bands     = NULL,
                          prefilter = "1",
                          n_save    = 0,
                          n_max     = Inf) {


    # check that the input is a CSV file
    ensurer::ensure_that(csv_file, !purrr::is_null(.) && tolower(tools::file_ext(.)) == "csv",
                         err_desc = "sits_fromCSV: please provide a valid CSV file")

    # Ensure that the service is available
    .sits_check_service(service)

    # configure the format of the CSV file to be read
    cols_csv <- readr::cols(id          = readr::col_integer(),
                            longitude   = readr::col_double(),
                            latitude    = readr::col_double(),
                            start_date  = readr::col_date(),
                            end_date    = readr::col_date(),
                            label       = readr::col_character())
    # read sample information from CSV file and put it in a tibble
    csv.tb <- readr::read_csv(csv_file, n_max = n_max, col_types = cols_csv)

    # find how many samples are to be read
    n_rows_csv <- NROW(csv.tb)
    # create a variable to store the number of rows
    nrow <- 0
    # create the tibble
    data.tb <- sits_tibble()
    # create a file to store the unread rows
    csv_unread.tb <- .sits_tibble_csv()
    # for each row of the input, retrieve the time series
    csv.tb %>%
        purrrlyr::by_row(function(r){
            row <- .sits_from_service(service, product, coverage, r$longitude, r$latitude, r$start_date, r$end_date,
                                      bands, prefilter, r$label)
            # did we get the data?
            if (!purrr::is_null(row)) {
                nrow <-  nrow + 1

                # add the new point to the SITS tibble
                data.tb <<- dplyr::bind_rows(data.tb, row)

                # optional - save the results to an intermediate file
                if (n_save != 0 && !(nrow %% n_save)) {
                    .sits_log_data(data.tb)
                }
            }
            # the point could not be read
            else {
                csv_unread_row.tb <- tibble::tibble(
                    longitude  = r$longitude,
                    latitude   = r$latitude,
                    start_date = r$start_date,
                    end_date   = r$end_date,
                    label      = r$label
                )
                csv_unread.tb <<- dplyr::bind_rows(csv_unread.tb, csv_unread_row.tb)
            }
        })

    # Have all input rows being read?
    if (nrow != n_rows_csv) {
        message("Some points could not be retrieved - see log file and csv_unread_file")
        .sits_log_CSV(csv_unread.tb, "unread_samples.csv")
    }
    # check that all time series have the same number of samples
    data.tb <- sits_prune(data.tb)

    return(data.tb)
}
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


