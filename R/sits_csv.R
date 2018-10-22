#' @title Export a sits tibble metadata to the CSV format
#' @name sits_metadata_toCSV
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts metadata from a sits tibble to a CSV file. The CSV file will not contain the actual time
#'              series. Its columns will be the same as those of a CSV file used to retrieve data from
#'              ground information ("latitude", "longitude", "start_date", "end_date", "coverage", "label").
#'
#' @param  data.tb    A sits time series.
#' @param  file       Name of the exported CSV file.
#' @return The status of the operation.
#' @examples
#' \donttest{
#' # read a tibble with 400 samples of Cerrado and 346 samples of Pasture
#' data(cerrado_2classes)
#' # export a time series to zoo
#' sits_metadata_toCSV (cerrado_2classes, file = "./cerrado_2classes.csv")
#' }
#' @export
sits_metadata_toCSV <- function(data.tb, file){
    csv_columns <- c("longitude", "latitude", "start_date", "end_date", "label")

    #select the parts of the tibble to be saved
    csv.tb <- dplyr::select(data.tb, csv_columns)

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
#' @name sits_data_toCSV
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts time series data from a sits tibble to a CSV file. The CSV file will not contain the metadata,
#' but will have the actual time series, with a reference value. This function is useful to
#' export the data for external applications
#'
#' @param  data.tb    A tibble with time series data and metadata.
#' @param  file       Name of the exported CSV file.
#' @return Status of the operation.
#' @examples
#' \donttest{
#' # read a tibble with 400 samples of Cerrado and 346 samples of Pasture
#' data(cerrado_2classes)
#' # export a time series to zoo
#' sits_data_toCSV(cerrado_2classes, file = "cerrado_2classes.csv")
#' }
#' @export
sits_data_toCSV <- function(data.tb, file){
    .sits_test_tibble(data.tb)

    distances_DT <- sits_distances(data.tb)

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
#' @name sits_shp_toCSV
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts points from a shapefile to a CSV file. The CSV file will not contain the actual time
#'              series. Its columns will be the same as those of a CSV file used to retrieve data from
#'              ground information ("latitude", "longitude", "start_date", "end_date", "coverage", "label").
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
#' # set the timeline
#' data("timeline_2000_2017")
#' # set the start and end dates
#' start_date <- lubridate::ymd("2002-08-29")
#' end_date   <- lubridate::ymd("2013-08-13")
#' # define the input shapefile
#' shpfile <- system.file ("extdata/shapefiles/cerrado_forested.shp", package = "sits")
#' # define the output csv file
#' csvfile <- paste0("cerrado_forested.csv")
#' # define the label
#' label <- "Cerrado_Forested"
#' # read the points in the shapefile and produce a CSV file
#' sits_shp_toCSV(shpfile, csvfile, label, timeline_2000_2017, start_date, end_date)
#' }
#' @export
sits_shp_toCSV <- function(shpfile, csvfile, label, timeline, start_date, end_date, interval = "12 month") {
    # test parameters
    ensurer::ensure_that(shpfile, !purrr::is_null(.) && tolower(tools::file_ext(.)) == "shp",
                         err_desc = "sits_fromSHP: please provide a valid SHP file")

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
    subset_dates.lst <- sits_match_timeline(timeline, start_date, end_date, interval)

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
