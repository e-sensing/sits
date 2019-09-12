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
#' @param prefilter       String ("0" - none, "1" - no data correction, "2" - cloud correction, "3" - no data and cloud correction).
#' @param .n_start        Row on the CSV file to start reading (optional).
#' @param .n_max          Maximum number of samples to be read.
#' @param .n_save         Number of samples to save as intermediate files (used for long reads).
#' @return A sits tibble.
.sits_from_csv <-  function(csv_file, cube, bands, prefilter, .n_start, .n_max, .n_save) {
    # configure the format of the CSV file to be read
    cols_csv <- readr::cols(id          = readr::col_integer(),
                            longitude   = readr::col_double(),
                            latitude    = readr::col_double(),
                            start_date  = readr::col_date(),
                            end_date    = readr::col_date(),
                            label       = readr::col_character())
    # read sample information from CSV file and put it in a tibble
    csv.tb <- readr::read_csv(csv_file, n_max = Inf, col_types = cols_csv)

    # select a subset
    if (.n_max == Inf)
        .n_max = NROW(csv.tb)
    csv.tb <- csv.tb[.n_start:.n_max, ]

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
                    row <- .sits_from_service(cube, longitude, latitude,
                                              lubridate::as_date(start_date),
                                              lubridate::as_date(end_date),
                                              bands, prefilter, label)
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
                        csv_unread.tb <<- dplyr::bind_rows(csv_unread.tb, csv_unread_row.tb)
                    }
                })


    # Have all input rows being read?
    if (nrow != n_rows_csv) {
        message("Some points could not be retrieved - see log file and csv_unread_file")
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

    csv_columns <- c("longitude", "latitude", "start_date", "end_date", "label")

    #select the parts of the tibble to be saved
    csv.tb <- dplyr::select(data, csv_columns)

    # create a column with the id
    id.tb <- tibble::tibble(id = 1:NROW(csv.tb))

    # join the two tibbles
    csv.tb <- dplyr::bind_cols(id.tb, csv.tb)

    tryCatch({utils::write.csv(csv.tb, file, row.names = FALSE, quote = FALSE)},
             error = function(e){
                 msg <- paste0("CSV - unable to save data in file ", file)
                 .sits_log_error(msg)
                 message("WTSS - unable to retrieve point - see log file for details" )
                 return(invisible(FALSE))})

    # write the CSV file
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

    distances_DT <- .sits_distances(data)

    tryCatch({utils::write.csv(distances_DT, file, row.names = FALSE, quote = FALSE)},
             error = function(e){
                 msg <- paste0("CSV - unable to save data in file ", file)
                 .sits_log_error(msg)
                 message("WTSS - unable to retrieve point - see log file for details" )
                 return(invisible(FALSE))})

    # write the CSV file
    return(invisible(TRUE))
}

#' @title Export a shapefile with points to a CSV file for later processing
#' @name sits_shp_to_csv
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts points from a shapefile to a CSV file. The CSV file will not contain the actual time
#'              series. Its columns will be the same as those of a CSV file used to retrieve data from
#'              ground information ("latitude", "longitude", "start_date", "end_date", "label").
#'
#' @param  shpfile    A point shapefile.
#' @param  csvfile    The name of the exported CSV file.
#' @param  label      Label associated to the samples.
#' @param  timeline   The timeline of the data set.
#' @param  start_date Starting date for which the samples are valid.
#' @param  end_date   End date for which the samples are valid.
#' @param  interval   Interval between two samples of the same place.
#' @return Status of the operation.
#'
#' @examples
#' \donttest{
#' # select the cube
#' wtss_cube <- sits_cube(service = "WTSS", name    = "MOD13Q1")
#' #  get the timeline from the cube
#' cube_timeline <- sits_timeline(wtss_cube)
#' # define the input shapefile (consisting of POINTS)
#' shpfile <- system.file("extdata/shapefiles/cerrado_forested.shp", package = "sits")
#' # set the start and end dates for the validity of the labels of the points in the shapefile
#' start_date <- lubridate::ymd("2002-08-29")
#' end_date   <- lubridate::ymd("2013-08-13")
#' # define the output csv file
#' csvfile <- paste0("cerrado_forested.csv")
#' #' # define the label
#' label <- "Cerrado_Forested"
#' # read the points in the shapefile and produce a CSV file
#' sits_shp_to_csv(shpfile, csvfile, label, cube_timeline, start_date, end_date, interval = "12 month")
#' # read the first three samples from the CSV file
#' csv_data <- sits_get_data(cube = wtss_cube, file = csvfile, .n_max = 3)
#' csv_data
#' }
#' @export
sits_shp_to_csv <- function(shpfile, csvfile, label, timeline, start_date, end_date, interval = "12 month") {
    # test parameters
    ensurer::ensure_that(shpfile, !purrr::is_null(.) && tolower(tools::file_ext(.)) == "shp",
                         err_desc = "sits_from_shp: please provide a valid SHP file")

    # read the shapefile
    sf_shape <- sf::read_sf(shpfile)

    # find out what is the projection of the shape file
    crs1 <- sf::st_crs(sf_shape)
    # if the shapefile is not in EPSG:4326 and WGS84, transform shape into WGS84
    if (crs1$epsg != 4326) {
        sf_shape <- sf::st_transform(sf_shape, crs = 4326)
    }

    # extract the lat/long coords from the shapefile
    coords <- do.call(rbind, sf::st_geometry(sf_shape)) %>%
        tibble::as_tibble() %>%
        stats::setNames(c("longitude","latitude"))

    # create a tibble to store the samples
    csv.tb <- tibble::tibble(
        id         = integer(),
        longitude  = double(),
        latitude   = double(),
        start_date = as.Date(character()),
        end_date   = as.Date(character()),
        label      = character())

    id <- 0
    # limit the timeline btw start and end_date
    timeline <- timeline[timeline >= start_date]
    timeline <- timeline[timeline <= end_date]

    # obtain pairs of (start, end) dates for each interval
    subset_dates.lst <- sits_timeline_match(timeline, start_date, end_date, interval)

    # generate the output tibble
    purrr::pmap(list(coords$longitude, coords$latitude), function(long, lat){
        rows.lst <- subset_dates.lst %>%
            purrr::map(function(date_pair) {
                id <<- id + 1
                tibble::tibble(
                    id         = id,
                    longitude  = long,
                    latitude   = lat,
                    start_date = date_pair[1],
                    end_date   = date_pair[2],
                    label      = label)
            })
        csv.tb <<- dplyr::bind_rows(csv.tb, rows.lst)
    })
    # write the CSV file
    tryCatch({utils::write.csv(csv.tb, csvfile, row.names = FALSE, quote = FALSE)},
             error = function(e){
                 message(paste0("CSV - unable to save data in file ", csvfile))
                 return(invisible(FALSE))})

    return(invisible(TRUE))
}
