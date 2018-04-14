#' @title Extract a time series from a ST raster data set
#' @name .sits_fromRaster
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Reads metadata about a raster data set to retrieve a set of
#' time series.
#'
#' @param coverage        A tibble with metadata describing a raster coverage
#' @param file            A CSV file with lat/long locations to be retrieve
#' @param longitude       double - the longitude of the chosen location
#' @param latitude        double - the latitude of the chosen location
#' @param start_date      date - the start of the period
#' @param end_date        date - the end of the period
#' @param label           string - the label to attach to the time series
#' @return data.tb        a SITS tibble with the time series
#'
.sits_fromRaster <- function(coverage,
                             file = NULL,
                             longitude = NULL,
                             latitude = NULL,
                             start_date = NULL,
                             end_date  = NULL,
                             label = "NoClass"){

    # ensure metadata tibble exists
    ensurer::ensure_that(coverage, NROW(.) >= 1,
                         err_desc = "sits_classify_raster: need a valid metadata for coverage")

    # get data based on CSV file
    if (!purrr::is_null(file) && tolower(tools::file_ext(file)) == "csv") {
        data.tb <- .sits_ts_fromRasterCSV(coverage, file)
    }

    if (!purrr::is_null(longitude) && !purrr::is_null(latitude)) {
        data.tb <- .sits_ts_fromRaster(coverage, longitude, latitude, start_date, end_date, label)
    }
    return(data.tb)
}

#' @title Extract a time series for a set of Raster Layers
#' @name .sits_ts_fromRaster
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description  This function extracts a SITS time series from a set of
#'               Raster Layers whose metadata is stored in a tibble
#'               created by the sits_STraster function
#'
#' @param raster.tb        A tibble with metadata information about a raster data set
#' @param longitude        Longitude of the chosen location
#' @param latitude         Latitude of the chosen location
#' @param start_date      date - the start of the period
#' @param end_date        date - the end of the period
#' @param label            Label to attach to the time series
#' @return data.tb         SITS tibble with the time series
.sits_ts_fromRaster <- function(raster.tb, longitude, latitude, start_date, end_date, label = "NoClass"){

    # ensure metadata tibble exists
    ensurer::ensure_that(raster.tb, NROW(.) >= 1,
                         err_desc = "sits_ts_fromRasterXY: need a valid metadata for coverage")

    timeline <- raster.tb$timeline[[1]]

    start_idx <- 1
    end_idx   <- length(timeline)

    if (!purrr::is_null(start_date)) {
        start_idx <- which.min(abs(lubridate::as_date(start_date) - timeline))
    }
    if (!purrr::is_null(end_date)) {
        end_idx <- which.min(abs(lubridate::as_date(end_date) - timeline))
    }
    timeline <- timeline[start_idx:end_idx]

    ts.tb <- tibble::tibble(Index = timeline)

    # get the bands, scale factors and missing values
    bands <- unlist(raster.tb$bands)
    missing_values <- unlist(raster.tb$missing_values)
    scale_factors  <- unlist(raster.tb$scale_factors)
    nband <- 0

    # transform longitude and latitude to an sp Spatial Points* (understood by raster)
    st_point <- sf::st_point(c(longitude, latitude))
    ll_sfc <- sf::st_sfc(st_point, crs = "+init=epsg:4326")
    ll_sp <- sf::as_Spatial(ll_sfc)

    # An input raster brick contains several files, each corresponds to a band
    values.lst <- unlist(raster.tb$r_objs) %>%
        purrr::map(function(r_brick) {
            # eack brick is a band
            nband <<- nband + 1
            # get the values of the time series
            values <- suppressWarnings(as.vector(raster::extract(r_brick, ll_sp)))
            # is the data valid?
            if (all(is.na(values))) {
                message("point outside the raster extent - NULL returned")
                return(NULL)
            }
            # create a tibble to store the values
            values.tb <- tibble::tibble(values[start_idx:end_idx])
            # find the names of the tibble column
            band <- bands[nband]
            names(values.tb) <- band
            # correct the values using the scale factor
            values.tb <- values.tb[,1]*scale_factors[band]
            return(values.tb)
        })

    ts.tb <- dplyr::bind_cols(ts.tb, values.lst)

    # create a list to store the time series coming from the set of Raster Layers
    ts.lst <- list()
    # transform the list into a tibble to store in memory
    ts.lst[[1]] <- ts.tb

    # create a tibble to store the WTSS data
    data.tb <- sits_tibble()
    # add one row to the tibble
    data.tb <- tibble::add_row(data.tb,
                               longitude    = longitude,
                               latitude     = latitude,
                               start_date   = as.Date(timeline[1]),
                               end_date     = as.Date(timeline[length(timeline)]),
                               label        = label,
                               coverage     = raster.tb$name,
                               time_series  = ts.lst
    )
    return(data.tb)
}
#' @title Extract a time series for a set of Raster Layers
#' @name .sits_ts_fromRasterXY
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description  This function extracts a SITS time series from a set of
#'               Raster Layers whose metadata is stored in a tibble
#'               created by the sits_STraster function
#'
#' @param raster.tb        A tibble with metadata information about a raster data set
#' @param xy               A matrix with X/Y coordinates
#' @param longitude        Longitude of the chosen location
#' @param latitude         Latitude of the chosen location
#' @param label            Label to attach to the time series
#' @return data.tb         SITS tibble with the time series
.sits_ts_fromRasterXY <- function(raster.tb, xy, longitude, latitude, label = "NoClass"){

    # ensure metadata tibble exists
    ensurer::ensure_that(raster.tb, NROW(.) >= 1,
                         err_desc = "sits_ts_fromRasterXY: need a valid metadata for coverage")

    timeline <- raster.tb$timeline[[1]]

    ts.tb <- tibble::tibble(Index = timeline)

    # get the bands, scale factors and missing values
    bands <- unlist(raster.tb$bands)
    missing_values <- unlist(raster.tb$missing_values)
    scale_factors  <- unlist(raster.tb$scale_factors)
    nband <- 0

    # An input raster brick contains several files, each corresponds to a band
    bricks.lst <- raster.tb$r_objs
    values.lst <- bricks.lst %>%
        purrr::map(function(r_brick) {
            # eack brick is a band
            nband <<- nband + 1
            # get the values of the time series
            values <- as.vector(raster::extract(r_brick, xy))
            # is the data valid?
            if (all(is.na(values)))
                return(NULL)
            # create a tibble to store the values
            values.tb <- tibble::tibble(values)
            # find the names of the tibble column
            band <- bands[nband]
            names(values.tb) <- band
            # correct the values using the scale factor
            values.tb <- values.tb[,1]*scale_factors[band]
            return(values.tb)
        })

    ts.tb <- dplyr::bind_cols(ts.tb, values.lst)

    # create a list to store the time series coming from the set of Raster Layers
    ts.lst <- list()
    # transform the list into a tibble to store in memory
    ts.lst[[1]] <- ts.tb

    # create a tibble to store the WTSS data
    data.tb <- sits_tibble()
    # add one row to the tibble
    data.tb <- tibble::add_row(data.tb,
                               longitude    = longitude,
                               latitude     = latitude,
                               start_date   = as.Date(timeline[1]),
                               end_date     = as.Date(timeline[length(timeline)]),
                               label        = label,
                               coverage     = raster.tb$name,
                               time_series  = ts.lst
    )
    return(data.tb)
}

#' @title Extract a time series for a set of Raster Layers
#' @name .sits_ts_fromRasterCSV
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description  This function extracts a SITS time series from a set of
#'               Raster Layers whose metadata is stored in a tibble
#'               created by the sits_STraster function
#'
#' @param raster.tb       A tibble with metadata describing a spatio-temporal data set
#' @param file            A CSV file with lat/long locations to be retrieved
#' @return data.tb         SITS tibble with the time series

.sits_ts_fromRasterCSV <- function(raster.tb, file) {

    # ensure metadata tibble exists
    ensurer::ensure_that(raster.tb, NROW(.) == 1,
                         err_desc = "sits_classify_raster: need a valid metadata for coverage")

    # configure the format of the CSV file to be read
    cols_csv <- readr::cols(id          = readr::col_integer(),
                            longitude   = readr::col_double(),
                            latitude    = readr::col_double(),
                            start_date  = readr::col_date(),
                            end_date    = readr::col_date(),
                            label       = readr::col_character())

    # read sample information from CSV file and put it in a tibble
    csv.tb <- readr::read_csv(file, col_types = cols_csv)
    # create a tibble for the time series
    data.tb <- sits_tibble()
    # create a tibble  to store the unread rows
    csv_unread.tb <- .sits_tibble_csv()

    # for each row of the input, retrieve the time series
    purrr::pmap(list(csv.tb$longitude, csv.tb$latitude,
                     csv.tb$start_date, csv.tb$end_date, csv.tb$label),
                function (r_longitude, r_latitude, r_start_date, r_end_date,
                          r_label){
                    xy <- .sits_latlong_to_proj(r_longitude, r_latitude, raster.tb$crs)

                    if (!.sits_XY_inside_raster(xy, raster.tb)) {
                        csv_unread_row.tb <- tibble::tibble(
                            longitude  = r_longitude,
                            latitude   = r_latitude,
                            start_date = lubridate::as_date(r_start_date),
                            end_date   = lubridate::as_date(r_end_date),
                            label      = r_label)
                        csv_unread.tb <<- dplyr::bind_rows(csv_unread.tb, csv_unread_row.tb)
                    }
                    # read the time series
                    row.tb <- .sits_ts_fromRasterXY(raster.tb, xy, r_longitude, r_latitude, r_label)
                    # extract the time interval
                    row.tb <- .sits_extract(row.tb, lubridate::as_date(r_start_date), lubridate::as_date(r_end_date))
                    # put one more row in the output tibble
                    data.tb <<- dplyr::bind_rows(data.tb, row.tb)
                })
    if (NROW(csv_unread.tb) > 0) {
        message("Some points could not be retrieved - see log file and csv_unread_file")
        .sits_log_CSV(csv_unread.tb, "unread_samples.csv")
    }



    return(data.tb)
}
