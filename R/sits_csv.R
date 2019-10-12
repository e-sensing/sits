#' @title Obtain timeSeries from a data cube, based on a CSV file.
#' @name .sits_from_csv
#'
#' @description reads descriptive information about a set of
#' spatio-temporal locations from a CSV file. Then, it retrieve the time series from a data cube,
#' and stores the time series on a sits tibble for later use.
#' The CSV file should have the following column names:
#' "longitude", "latitude", "start_date", "end_date", "label"
#'
#' @param csv_file        Name of a CSV file with information <id, latitude, longitude, from, end, label>.
#' @param cube            A tibble with metadata about the data cube which contains data to be retrieved.
#' @param bands           A string vector with the names of the bands to be retrieved.
#' @param .prefilter      String ("0" - none, "1" - no data correction, "2" - cloud correction, "3" - no data and cloud correction).
#' @param .n_start_csv    Row on the CSV file to start reading (optional).
#' @param .n_max_csv      Maximum number of samples to be read.
#' @param .n_save         Number of samples to save as intermediate files (used for long reads).
#' @return A sits tibble.
.sits_from_csv <-  function(csv_file, cube, bands,
                            .prefilter, .n_start_csv, .n_max_csv, .n_save) {
    # configure the format of the CSV file to be read
    cols_csv <- readr::cols(id          = readr::col_integer(),
                            longitude   = readr::col_double(),
                            latitude    = readr::col_double(),
                            start_date  = readr::col_date(),
                            end_date    = readr::col_date(),
                            label       = readr::col_character())

    # pre-condition - does the csvfile exist?
    ensurer::ensure_that(csv_file, file.exists(.),
                         err_desc = "sits_from_csv: csv file does not exist")

    # read sample information from CSV file and put it in a tibble
    csv.tb <- readr::read_csv(csv_file, n_max = Inf, col_types = cols_csv)

    # select a subset
    if (.n_max_csv == Inf)
        .n_max_csv <-  NROW(csv.tb)
    csv.tb <- csv.tb[.n_start_csv:.n_max_csv, ]

    # find how many samples are to be read
    n_rows_csv <- NROW(csv.tb)
    # create a variable to store the number of rows
    nrow <- 0
    # create the tibble
    data <- .sits_tibble()
    # create a file to store the unread rows
    csv_unread.tb <- .sits_tibble_csv()
    # for each row of the input, retrieve the time series
    purrr::pmap(list(csv.tb$longitude, csv.tb$latitude, csv.tb$start_date,
                     csv.tb$end_date, csv.tb$label),
                function(longitude, latitude, start_date, end_date, label){
                    row <- .sits_from_service(cube = cube,
                                        longitude = longitude,
                                        latitude = latitude,
                                        start_date = lubridate::as_date(start_date),
                                        end_date = lubridate::as_date(end_date),
                                        bands = bands,
                                        label = label,
                                        .prefilter = .prefilter)
                    # did we get the data?
                    if (!purrr::is_null(row)) {
                        nrow <<-  nrow + 1

                        # add the new point to the sits tibble
                        data <<- dplyr::bind_rows(data, row)

                        # optional - save the results to an intermediate file
                        if (.n_save != 0 && !(nrow %% .n_save)) {
                            .sits_log_data(data)
                        }
                    }
                    # the point could not be read - save it in the log file
                    else {
                        csv_unread_row.tb <- tibble::tibble(
                            longitude  = longitude,
                            latitude   = latitude,
                            start_date = lubridate::as_date(start_date),
                            end_date   = lubridate::as_date(end_date),
                            label      = label
                        )
                        csv_unread.tb <<- dplyr::bind_rows(csv_unread.tb,
                                                           csv_unread_row.tb)
                    }
                })


    # Have all input rows being read?
    if (nrow != n_rows_csv) {
        message("Some points could not be retrieved -
                see log file and csv_unread_file")
        .sits_log_csv(csv_unread.tb, "unread_samples.csv")
    }

    return(data)
}

#' @title Export a sits tibble metadata to the CSV format
#' @name sits_metadata_to_csv
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts metadata from a sits tibble to a CSV file. The CSV file will not contain the actual time
#'              series. Its columns will be the same as those of a CSV file used to retrieve data from
#'              ground information ("latitude", "longitude", "start_date", "end_date", "cube", "label").
#'
#' @param  data       A sits time series.
#' @param  file       Name of the exported CSV file.
#' @return The status of the operation.
#' @examples
#' \donttest{
#' # read a tibble with 400 samples of Cerrado and 346 samples of Pasture
#' data(cerrado_2classes)
#' # export a time series to zoo
#' sits_metadata_to_csv (cerrado_2classes, file = "./cerrado_2classes.csv")
#' }
#' @export
sits_metadata_to_csv <- function(data, file){
    # backward compatibility
    if ("coverage" %in% names(data))
        data <- .sits_tibble_rename(data)
    ensurer::ensure_that(file, suppressWarnings(file.create(.)),
                         err_desc = "sits_metadata_to_csv - file is not writable")

    csv_columns <- c("longitude", "latitude", "start_date", "end_date", "label")

    #select the parts of the tibble to be saved
    csv.tb <- dplyr::select(data, csv_columns)

    ensurer::ensure_that(csv.tb, NROW(.) > 0,
                         err_desc = "sits_metadata_to_csv: invalid csv file")
    n_rows_csv <- NROW(csv.tb)
    # create a column with the id
    id.tb <- tibble::tibble(id = 1:n_rows_csv)

    # join the two tibbles
    csv.tb <- dplyr::bind_cols(id.tb, csv.tb)

    # write the CSV file
    utils::write.csv(csv.tb, file, row.names = FALSE, quote = FALSE)

    return(invisible(TRUE))
}

#' @title Export a sits tibble data to the CSV format
#' @name sits_data_to_csv
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts time series data from a sits tibble to a CSV file. The CSV file will not contain the metadata,
#' but will have the actual time series, with a reference value. This function is useful to
#' export the data for external applications
#'
#' @param  data       A tibble with time series data and metadata.
#' @param  file       Name of the exported CSV file.
#' @return Status of the operation.
#' @examples
#' \donttest{
#' # read a tibble with 400 samples of Cerrado and 346 samples of Pasture
#' data(cerrado_2classes)
#' # export a time series to zoo
#' sits_data_to_csv(cerrado_2classes, file = "cerrado_2classes.csv")
#' }
#' @export
sits_data_to_csv <- function(data, file){
    # backward compatibility
    if ("coverage" %in% names(data))
        data <- .sits_tibble_rename(data)
    # check if data is valid
    .sits_test_tibble(data)

    ensurer::ensure_that(file, suppressWarnings(file.create(.)),
                         err_desc = "sits_data_to_csv - file is not writable")

    distances_DT <- .sits_distances(data)

    # write the CSV file
    utils::write.csv(distances_DT, file, row.names = FALSE, quote = FALSE)

    return(invisible(TRUE))
}
