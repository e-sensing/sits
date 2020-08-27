#' @title Obtain timeSeries from a data cube, based on a CSV file.
#' @name .sits_from_csv
#'
#' @description reads descriptive information about a set of
#' spatio-temporal locations from a CSV file.
#' Then, it retrieve the time series from a data cube,
#' and stores the time series on a sits tibble for later use.
#' The CSV file should have the following column names:
#' "longitude", "latitude", "start_date", "end_date", "label"
#'
#' @param csv_file        Name of a CSV file with information
#'                        <id, latitude, longitude, from, end, label>.
#' @param cube            Data cube which contains data to be retrieved.
#' @param bands           Names of the bands to be retrieved.
#' @param .prefilter      Prefilter condition for SATVEG service
#'                        "0" - none,
#'                        "1" - no data correction,
#'                        "2" - cloud correction,
#'                        "3" - no data and cloud correction.
#' @param .n_start_csv    Row on the CSV file to start reading (optional).
#' @param .n_max_csv      Maximum number of samples to be read.
#' @param .n_save         Number of samples to save as intermediate files
#'                        (used for long reads).
#' @return A sits tibble.
.sits_from_csv <-  function(csv_file,
                            cube,
                            bands,
                            .prefilter,
                            .n_start_csv,
                            .n_max_csv,
                            .n_save) {

    # pre-condition - does the csvfile exist?
    assertthat::assert_that(file.exists(csv_file),
                         msg = "sits_from_csv: csv file does not exist")
    # read sample information from CSV file and put it in a tibble
    csv.tb <- tibble::as_tibble(utils::read.csv(csv_file))

    # select a subset
    if (.n_max_csv == Inf)
        .n_max_csv <-  NROW(csv.tb)
    csv.tb <- csv.tb[.n_start_csv:.n_max_csv, ]

    if (.sits_config_cube_service(cube)) {
        data.tb <- .sits_csv_from_service(cube = cube, csv.tb = csv.tb,
                                          bands = bands, .prefilter = .prefilter)
    }
    else
        data.tb <- .sits_csv_from_raster(cube = cube, csv.tb = csv.tb,
                                         bands = bands)

    return(data.tb)
}

#' @title Read a CSV from a webservice
#' @name .sits_csv_from_service
#'
#' @param cube         Data cube
#' @param csv.tb       Tibble with CSV information
#' @param bands        Vector with bands to be retrieved
#' @param .prefilter   Prefilter info (used for SATVEG)
#'
.sits_csv_from_service <- function (cube, csv.tb, bands, .prefilter){

    # find how many samples are to be read
    n_rows_csv <- NROW(csv.tb)
    # for each row of the input, retrieve the time series
    data.lst <- purrr::pmap(list(csv.tb$longitude,
                                 csv.tb$latitude,
                                 csv.tb$start_date,
                                 csv.tb$end_date,
                                 csv.tb$label),
                            function(longitude, latitude, start_date, end_date, label){
                                row <- .sits_ts_from_web(cube = cube,
                                                          longitude = longitude,
                                                          latitude = latitude,
                                                          start_date = lubridate::as_date(start_date),
                                                          end_date = lubridate::as_date(end_date),
                                                          bands = bands,
                                                          label = label,
                                                          .prefilter = .prefilter)
                                return(row)
                            })
    # unroll the list
    data.tb <- dplyr::bind_rows(data.lst)
    # find out how many rows have been read
    nrows_read <- nrow(data.tb)
    # Have all input rows being read?
    if (nrows_read < n_rows_csv)
        message("Some points could not be retrieved from WTSS service -
                see log file and csv_unread_file")
    else if (nrows_read == 0)
        message("No points were retrieved from WTSS service -
                see log file and csv_unread_file")
    else
        message("All points have been retrieved from WTSS service")

    return(data.tb)

}
#' @title Read a CSV from a raster
#' @name .sits_csv_from_raster
#'
#' @param cube         Data cube
#' @param csv.tb       Tibble with CSV information
#' @param bands        Vector with bands to be retrieved
#' @return             Tibble with requested samples
.sits_csv_from_raster <- function(cube, csv.tb, bands) {

    # transform csv.tb into a spatial points object
    lat_long <- sf::st_as_sf(csv.tb, coords = c("longitude", "latitude"), crs = 4326)

    data.tb <- .sits_ts_from_raster(cube = cube,
                                    sf_object = lat_long,
                                    bands = bands)
}

#' @title Export a sits tibble metadata to the CSV format
#' @name sits_metadata_to_csv
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts metadata from a sits tibble to a CSV file.
#'              The CSV file will not contain the actual time
#'              series. Its columns will be the same as those of a
#'              CSV file used to retrieve data from
#'              ground information ("latitude", "longitude", "start_date",
#'              "end_date", "cube", "label").
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
#' # cleanup (optional)
#' file.remove("./cerrado_2classes.csv")
#' }
#' @export
sits_metadata_to_csv <- function(data, file){
    # backward compatibility
    data <- .sits_tibble_rename(data)
    assertthat::assert_that(suppressWarnings(file.create(file)),
                         msg = "sits_metadata_to_csv - file is not writable")

    csv_columns <- c("longitude", "latitude", "start_date", "end_date", "label")

    #select the parts of the tibble to be saved
    csv.tb <- dplyr::select(data, csv_columns)

    assertthat::assert_that(NROW(csv.tb) > 0,
                         msg = "sits_metadata_to_csv: invalid csv file")
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
#' @description Converts time series data from a sits tibble to a CSV file.
#'              The CSV file will not contain the metadata,
#'              but only the actual time series, with a reference value.
#'              This function is useful to export the data to external apps.
#'
#' @param  data       A tibble with time series data and metadata.
#' @param  file       Name of the exported CSV file.
#' @return            Status of the operation.
#' @examples
#' \donttest{
#' # read a tibble with 400 samples of Cerrado and 346 samples of Pasture
#' data(cerrado_2classes)
#' # export a time series
#' sits_data_to_csv(cerrado_2classes, file = "cerrado_2classes.csv")
#'
#' # cleanup (optional)
#' file.remove("./cerrado_2classes.csv")
#' }
#' @export
sits_data_to_csv <- function(data, file){
    # backward compatibility
    data <- .sits_tibble_rename(data)
    # check if data is valid
    .sits_test_tibble(data)

    assertthat::assert_that(suppressWarnings(file.create(file)),
                         msg = "sits_data_to_csv - file is not writable")

    distances_DT <- .sits_distances(data)

    # write the CSV file
    utils::write.csv(distances_DT, file, row.names = FALSE, quote = FALSE)

    return(invisible(TRUE))
}
