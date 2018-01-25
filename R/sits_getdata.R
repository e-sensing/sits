#' @title Obtain time series from different sources
#' @name sits_getdata
#' @author Gilberto Camara
#'
#' @description Retrieve a set of time series. There are two main ways of retrieving time series:
#' using a time series service and from a Raster Brick. Two time series services are available:
#' (a) the Web Time Series Service (WTSS) by INPE; (b) the SATVEG service from EMBRAPA.
#' Please see \code{link[sits]{sits_infoWTSS}} for more information on thw WTSS service.
#' Please see \code{link[sits]{sits_fromSATVEG}} for more information on SATVEG.
#' The URL and other parameters for access to the time series services are defined in the package
#' configuration file. This file is called "config.yml". Please see the \code{link[sits]{sits_config}} for
#' more information.
#'
#' Before using this service, the user should create a valid coverage tibble
#' using the \code{link[sits]{sits_coverage}} function.
#'
#' The following options are available:
#' \enumerate{
#' \item No input file is given - it retrieves the data and metadata based on the latitude/longitude location
#' and on the information provided by the WTSS server.
#' \item The source is a CSV file - retrieves the metadata from the CSV file and the time series
#' from the WTSS service.
#' \item The source is a SHP file - retrives all points inside the shapefile from the WTSS service.
#' \item The source is a RasterBrick - retrieves the point based on lat/long from the RasterBrick.
#' }
#  The results is a SITS tibble, which  has the metadata and data for each time series
#' <longitude, latitude, start_date, end_date, label, coverage, time_series>
#'
#' @references
#' Lubia Vinhas, Gilberto Queiroz, Karine Ferreira, Gilberto Camara,
#' Web Services for Big Earth Observation Data.
#' In: XVII Brazilian Symposium on Geoinformatics, 2016, Campos do Jordao.
#' Proceedings of GeoInfo 2016. Sao Jose dos Campos: INPE/SBC, 2016. v.1. p.166-177.
#'
#' @param coverage        (Mandatory) A tibble with information about the coverage.
#' @param file            (optional) the name of a file with information on the data to be retrieved (options - CSV, SHP).
#' @param longitude       Longitude of the chosen location.
#' @param latitude        Latitude of the chosen location.
#' @param start_date      (optional) Start of the interval for the time series in Date format ("YYYY-MM-DD")
#' @param end_date        (optional) End of the interval for the time series in Date format ("YYYY-MM-DD")
#' @param bands           (optional) vector - the names of the bands to be retrieved.
#' @param prefilter       string ("0" - none, "1" - no data correction, "2" - cloud correction, "3" - no data and cloud correction).
#' @param label           (optional) string - the label to be assigned to the time series
#' @param .n_start        (optional) integer - Row on the CSV file to start reading
#' @param .n_max          (optional) integer - maximum number of CSV samples to be read (set to Inf to read all)
#' @param .n_save         (optional) number of samples to save as intermediate files (used for long reads)
#' @return data.tb        a SITS tibble
#'
#' @examples
#' \donttest{
#' # Read a single lat long point from a WTSS server
#' wtss_coverage <- sits_coverage(service = "WTSS-INPE-1",
#'                     product = "MOD13Q1", name = "mod13q1_512")
#' point.tb <- sits_getdata (wtss_coverage, longitude = -55.50563, latitude = -11.71557)
#' show(point.tb)
#'
#' # Read a set of points defined in a CSV file from a WTSS server
#' csv_file <- system.file ("extdata/samples/samples_import.csv", package = "sits")
#' points.tb <- sits_getdata (wtss_coverage, file = csv_file)
#' # show the points retrieved for the WTSS server
#' show (points.tb)
#'
#' # Read a single lat long point from the SATVEG server
#' satveg_coverage <- sits_coverage(service = "SATVEG", product = "MOD13Q1", name = "terra")
#' point_satveg.tb <- sits_fromSATVEG (satveg_coverage, longitude = -55.50563, latitude = -11.71557)
#' show (point_satveg.tb)
#'
#' # define a shapefile and read from the points inside it from the WTSS service
#' shp_file <- system.file("extdata/shapefiles/madre_de_deus/madre_de_deus.shp", package = "sits")
#' munic.tb <- sits_fromSHP(shp_file, satveg_coverage)
#'
#' # Read a point in a Raster Brick
#' # define the file that has the raster brick
#' files  <- c(system.file ("extdata/raster/mod13q1/sinop-crop-ndvi.tif", package = "sits"))
#' # define the timeline
#' data(timeline_mod13q1)
#' timeline <- lubridate::as_date(timeline_mod13q1$V1)
#' # create a raster metadata file based on the information about the files
#' raster_cov <- sits_coverageRaster(files, name = "Sinop-crop", timeline, bands = c("ndvi"))
#' # read the point from the raster
#' point_raster.tb <- sits_getdata(raster_cov, longitude = -55.50563, latitude = -11.71557)
#' show(point_raster.tb)
#' }
#' @export
sits_getdata <- function(coverage    = NULL,
                         file        = NULL,
                         longitude   = NULL,
                         latitude    = NULL,
                         start_date  = NULL,
                         end_date    = NULL,
                         bands       = NULL,
                         prefilter   = "1",
                         label       = "NoClass",
                         .n_start    = 1,
                         .n_max      = Inf,
                         .n_save     = 0) {

    # a JSON file has all the data and metadata
    if  (!purrr::is_null(file) && tolower(tools::file_ext(file)) == "json") {
          data.tb <- .sits_fromJSON(file)
          return(data.tb)
     }
    # get data based on gz (compressed JSON) file
    if (!purrr::is_null(file) && tolower(tools::file_ext(file)) == "gz") {
        data.tb <- .sits_fromGZ(file)
        return(data.tb)
    }

    # Ensure that the service is available
    .sits_check_service(coverage[1,]$service)

    # get data based on latitude and longitude
    if (purrr::is_null(file) &&
        !purrr::is_null(latitude) && !purrr::is_null(longitude)) {
        data.tb <- .sits_from_service(coverage = coverage,
                                      longitude = longitude,
                                      latitude = latitude,
                                      start_date = start_date,
                                      end_date = end_date,
                                      bands = bands,
                                      prefilter = prefilter,
                                      label = label)
        return(data.tb)
    }
    # get data based on CSV file
    if (!purrr::is_null(file) && tolower(tools::file_ext(file)) == "csv") {
        data.tb <- .sits_fromCSV(csv_file  = file,
                                 coverage  = coverage,
                                 bands     = bands,
                                 prefilter = prefilter,
                                .n_start   = .n_start,
                                .n_max     = .n_max,
                                .n_save    = .n_save)
        return(data.tb)
    }
    # get data based on SHP file
    if (!purrr::is_null(file) && tolower(tools::file_ext(file)) == "shp") {
        data.tb <- .sits_fromSHP(file, coverage, start_date, end_date, bands, prefilter, label)
        return(data.tb)
    }
    message(paste("No valid input to retrieve time series data!!", "\n", sep = ""))
    stop()
}

#' @title Obtain timeSeries from time series service
#' @name .sits_from_service
#'
#' @description obtains a time series from a time series service
#'
#' @param coverage        coverage metadata
#' @param longitude       double - the longitude of the chosen location
#' @param latitude        double - the latitude of the chosen location)
#' @param start_date      (optional) date - the start of the period
#' @param end_date        (optional) date - the end of the period
#' @param bands           (optional) string vector - the names of the bands to be retrieved
#' @param satellite       (optional) - the same of the satellite (options - "terra", "aqua", "comb")
#' @param prefilter       string ("0" - none, "1" - no data correction, "2" - cloud correction, "3" - no data and cloud correction)
#' @param label           string - the label to attach to the time series
#' @return table          a SITS tibble
#'
.sits_from_service <- function(coverage,
                               longitude,
                               latitude,
                               start_date,
                               end_date,
                               bands,
                               prefilter  = "1",
                               label = "NoClass") {


    protocol <- .sits_get_protocol(coverage[1,]$service)

    if (protocol == "WTSS") {
        data.tb <- .sits_fromWTSS(coverage = coverage,
                                  longitude = longitude,
                                  latitude = latitude,
                                  start_date = start_date,
                                  end_date = end_date,
                                  bands = bands,
                                  label = label)
        return(data.tb)
    }
    if (protocol == "SATVEG") {
        data.tb <- .sits_fromSATVEG(coverage = coverage,
                                   longitude = longitude,
                                   latitude = latitude,
                                   start_date = start_date,
                                   end_date = end_date,
                                   prefilter = prefilter,
                                   label = label)

        return(data.tb)
    }
    if (protocol == "RASTER") {
        data.tb <- .sits_fromRaster(coverage = coverage,
                                    longitude = longitude,
                                    latitude = latitude,
                                    start_date = start_date,
                                    end_date = end_date,
                                    label = label)

        return(data.tb)
    }
    return(NULL)
}
#' @title Obtain one timeSeries from WTSS server and load it on a sits tibble
#' @name .sits_fromWTSS
#'
#' @description Returns one set of time series provided by a WTSS server
#' Given a location (lat/long), and start/end period, and the WTSS server information
#' retrieve a time series and include it on a stis tibble.
#' A Web Time Series Service (WTSS) is a light-weight service that
#' retrieves one or more time series in JSON format from a data base.
#' @references
#' Lubia Vinhas, Gilberto Queiroz, Karine Ferreira, Gilberto Camara,
#' Web Services for Big Earth Observation Data.
#' In: XVII Brazilian Symposium on Geoinformatics, 2016, Campos do Jordao.
#' Proceedings of GeoInfo 2016. Sao Jose dos Campos: INPE/SBC, 2016. v.1. p.166-177
#'
#' @param coverage        metadata about the coverage where the data is to be retrived
#' @param longitude       double - the longitude of the chosen location
#' @param latitude        double - the latitude of the chosen location
#' @param start_date      date - the start of the period
#' @param end_date        date - the end of the period
#' @param bands           list of string - a list of the names of the bands of the coverage
#' @param label           string - the label to attach to the time series (optional)
#' @return data.tb        a SITS tibble
#'
.sits_fromWTSS <- function(coverage,
                           longitude,
                           latitude,
                           start_date = NULL,
                           end_date   = NULL,
                           bands      = NULL,
                           label      = "NoClass") {

    # if bands are not provided, use all bands available in the coverage
    # check the bands are available
    cov_bands <- coverage$bands[[1]]
    if (purrr::is_null(bands))
        bands <- cov_bands
    else
        ensurer::ensure_that(bands, all((.) %in% cov_bands),
                             err_desc = "sits_fromWTSS: requested bands are not available in the coverage")

    # check start and end dates
    if (purrr::is_null(start_date))
        start_date <- coverage$start_date
    if (purrr::is_null(end_date))
        end_date <- coverage$end_date

    # try to get a time series from the WTSS server
    tryCatch({
        # get the WTSS object associated to the URL
        wtss.obj <- coverage$r_obj[[1]]
        # retrieve the time series from the service
        ts <- wtss::timeSeries(object     = wtss.obj,
                               coverages  = coverage$name,
                               attributes = bands,
                               longitude  = longitude,
                               latitude   = latitude,
                               start_date = start_date,
                               end_date   = end_date)

        # retrieve the time series information
        time_series <- ts[[coverage$name]]$attributes

        # determine the missing value for each band
        miss_value <- vector()
        for (b in bands)
            miss_value[b] <- .sits_get_missing_value(coverage$product, b)

        # update missing values to NA
        for (b in bands) {
            time_series[, b][time_series[, b] == miss_value[b]] <- NA
        }

        # interpolate missing values
        time_series[, bands] <- zoo::na.spline(time_series[, bands])

        scale_factor <- vector()
        for (b in bands)
            scale_factor[b] <- .sits_get_scale_factor(coverage$product, b)

        # scale the time series
        bands %>%
            purrr::map(function(b) {
                time_series[, b] <<- time_series[, b]*scale_factor[b]
            })

        # convert the series to a tibble
        ts.tb <- tibble::as_tibble(zoo::fortify.zoo(time_series))

        # create a list to store the time series coming from the WTSS service
        ts.lst <- list()
        ts.lst[[1]] <- ts.tb

        # create a tibble to store the WTSS data
        data.tb <- sits_tibble()
        # add one row to the tibble
        data.tb <- tibble::add_row(data.tb,
                                   longitude,
                                   latitude,
                                   start_date  = start_date,
                                   end_date    = end_date,
                                   label       = label,
                                   coverage    = coverage$name,
                                   time_series = ts.lst)

        # return the tibble with the time series
        return(data.tb)

    }, error = function(e){
        msg <- paste0("WTSS - unable to retrieve point (", longitude, ", ", latitude, ", ", start_date," ,", end_date,")")
        .sits_log_error(msg)
        message("WTSS - unable to retrieve point - see log file for details" )
        return(NULL)
    })
}

#' @title Obtain one timeSeries from the EMBRAPA SATVEG server and load it on a sits tibble
#' @name .sits_fromSATVEG
#' @author Julio Esquerdo, \email{julio.esquerdo@@embrapa.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns one set of MODIS time series provided by the EMBRAPA server (SATVEG)
#' Given a location (lat/long), the function retrieves the "ndvi" and "evi" bands from SATVEG
#' and inclues the data on a stis tibble. If start and end date are given, the function
#' filter the data to limit the temporal interval.
#'
#' @param coverage        the coverage metadata with the SATVEG information
#' @param longitude       double - the longitude of the chosen location
#' @param latitude        double - the latitude of the chosen location
#' @param start_date      date - the start of the period
#' @param end_date        date - the end of the period
#' @param prefilter       string ("0" - none, "1" - no data correction, "2" - cloud correction, "3" - no data and cloud correction)
#' @param label           string - the label to attach to the time series (optional)
#' @return data.tb        a SITS tibble
#'
.sits_fromSATVEG <- function(coverage,
                             longitude,
                             latitude,
                             start_date  = NULL,
                             end_date    = NULL,
                             prefilter   = "1",
                             label       = "NoClass") {

    # check parameters
    ensurer::ensure_that(longitude, !purrr::is_null(.),
                         err_desc = "sits_fromSATVEG: Missing longitude info")
    ensurer::ensure_that(latitude,  !purrr::is_null(.),
                         err_desc = "sits_fromSATVEG: Missing latitude info")

    # retrieve the time series
    ts.tb <- .sits_ts_from_SATVEG(longitude, latitude, coverage$name, prefilter)

    # filter the dates
    if (!purrr::is_null(start_date) && !purrr::is_null(end_date))
        ts.tb <- dplyr::filter(ts.tb, dplyr::between(ts.tb$Index, start_date, end_date))
    else {
        start_date <- as.Date(ts.tb$Index[1])
        end_date   <- as.Date(ts.tb$Index[NROW(ts.tb)])
    }

    # use a list to store the time series
    ts.lst <- list()
    ts.lst[[1]] <- ts.tb

    # create a tibble to store the SATVEG data
    data.tb <- sits_tibble()
    # add one row to the tibble
    data.tb <- tibble::add_row(data.tb,
                               longitude    = longitude,
                               latitude     = latitude,
                               start_date   = start_date,
                               end_date     = end_date,
                               label        = label,
                               coverage     = coverage$name,
                               time_series  = ts.lst
    )
    return(data.tb)
}

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
        xy <- .sits_latlong_to_proj(longitude, latitude, coverage[1, ]$crs)
        data.tb <- .sits_ts_fromRasterXY(coverage, xy, longitude, latitude, label)
    }
    return(data.tb)
}

#' @title Obtain timeSeries from time series server, based on a CSV file.
#' @name .sits_fromCSV
#'
#' @description reads descriptive information about a set of
#' spatio-temporal locations from a CSV file. Then, it uses the WTSS time series service
#' to retrieve the time series, and stores the time series on a SITS tibble for later use.
#' The CSV file should have the following column names:
#' "longitude", "latitude", "start_date", "end_date", "label"
#'
#' @param csv_file        string  - name of a CSV file with information <id, latitude, longitude, from, end, label>
#' @param coverage        tibble - metadata about coverage which contains data to be retrieved
#' @param bands           string vector - the names of the bands to be retrieved
#' @param prefilter       string ("0" - none, "1" - no data correction, "2" - cloud correction, "3" - no data and cloud correction)
#' @param .n_start        (optional) integer - Row on the CSV file to start reading
#' @param .n_max          the maximum number of samples to be read
#' @param .n_save         number of samples to save as intermediate files (used for long reads)
#' @return data.tb        a SITS tibble
#'
.sits_fromCSV <-  function(csv_file, coverage, bands, prefilter, .n_start, .n_max, .n_save) {

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
    data.tb <- sits_tibble()
    # create a file to store the unread rows
    csv_unread.tb <- .sits_tibble_csv()
    # for each row of the input, retrieve the time series
    csv.tb %>%
        purrrlyr::by_row(function(r){
            row <- .sits_from_service(coverage, r$longitude, r$latitude, r$start_date, r$end_date,
                                      bands, prefilter, r$label)
            # did we get the data?
            if (!purrr::is_null(row)) {
                nrow <<-  nrow + 1

                # add the new point to the SITS tibble
                data.tb <<- dplyr::bind_rows(data.tb, row)

                # optional - save the results to an intermediate file
                if (.n_save != 0 && !(nrow %% .n_save)) {
                    .sits_log_data(data.tb)
                }
            }
            # the point could not be read - save it in the log file
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
#' @title Obtain timeSeries from WTSS server, based on a SHP file.
#' @name .sits_fromSHP
#'
#' @description reads a shapefile and retrieves a SITS tibble
#' containing time series from a coverage that are inside the SHP file.
#' The script uses the WTSS service, taking information about coverage, spatial and
#' temporal resolution from the WTSS configuration.
#'
#'
#' @param shp_file        string  - name of a SHP file which provides the boundaries of a region of interest
#' @param coverage        tibble  - metadata about the coverage
#' @param bands           string vector - the names of the bands to be retrieved
#' @param start_date      date - the start of the period
#' @param end_date        date - the end of the period
#' @param prefilter       string ("0" - none, "1" - no data correction, "2" - cloud correction, "3" - no data and cloud correction)
#' @param label           string - the label to attach to the time series
#' @return table          a SITS tibble
#'
.sits_fromSHP <- function(shp_file,
                          coverage,
                          start_date = NULL,
                          end_date   = NULL,
                          bands      = NULL,
                          prefilter  = "1",
                          label      = "NoClass") {


    # test parameters
    ensurer::ensure_that(shp_file, !purrr::is_null(.) && tolower(tools::file_ext(.)) == "shp",
                         err_desc = "sits_fromSHP: please provide a valid SHP file")
    # Ensure that the service is available
    .sits_check_service(coverage$service)


    # recover the coverage resolution
    resolution <- .sits_get_resolution(coverage$product)
    # read the shapefile
    sf_shape <- sf::read_sf(shp_file)
    # get the bounding box
    bbox <- sf::st_bbox(sf_shape)
    # create an empty sits tibble
    shape.tb <- sits_tibble()

    # setup the sequence of latitudes and longitudes to be searched
    longitudes <- seq(from = bbox["xmin"], to = bbox["xmax"], by = resolution["xres"])
    latitudes  <- seq(from = bbox["ymin"], to = bbox["ymax"], by = resolution["yres"])

    longitudes %>%
        purrr::map(function(long){
            latitudes %>%
                purrr::map(function(lat){
                    ll <- sf::st_point(c(long, lat))
                    if (1 %in% as.logical(unlist(sf::st_within(ll, sf_shape)))) {
                        row <- .sits_from_service(coverage, long, lat, start_date, end_date,
                                                  bands, prefilter, label)
                        shape.tb <<- dplyr::bind_rows(shape.tb, row)
                    }
                })
        })
    return(shape.tb)
}

