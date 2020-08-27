#' @title Obtain time series from different sources
#' @name sits_get_data
#' @author Gilberto Camara
#'
#' @description Retrieve a set of time series and puts it in a "sits tibble".
#' Sits tibbles are the main structures of sits package.
#' They contain both the satellite image time series and their metadata.
#' A sits tibble is a tibble with pre-defined columns that
#' has the metadata and data for each time series. The columns are
#' <longitude, latitude, start_date, end_date, label, cube, time_series>.
#' There are two main ways of retrieving time series:
#' 1. Using a time series service and from a data cube defined
#' based on a set of Raster Bricks. Two time series services are available:
#' (a) Web Time Series Service (WTSS) by INPE; (b) SATVEG service from EMBRAPA.
#' for more information on the WTSS service.
#' The URL and other parameters for accessing the time series services
#' are defined in the package
#' configuration file. This file is called "config.yml".
#' Please see the \code{\link[sits]{sits_config}} for more information.
#'
#' Before using this service, the user should create a valid description
#' of a data cube using the \code{\link[sits]{sits_cube}} function.
#'
#' The following options are available:
#' \enumerate{
#' \item No input file is given - it retrieves the data and metadata
#' based on the latitude/longitude location
#' and on the information provided by the WTSS server.
#' \item The source is a CSV file - retrieves the metadata from the CSV file
#' and the time series from the WTSS service.
#' \item The source is a SHP file - retrives all points inside the shapefile
#' from the WTSS service.
#' \item The source is a RasterBrick - retrieves the point based on lat/long
#' from the RasterBrick.
#' }
#'  The result is atibble with the metadata and data for each time series
#' <longitude, latitude, start_date, end_date, label, cube, time_series>
#'
#' @references
#' Lubia Vinhas, Gilberto Queiroz, Karine Ferreira, Gilberto Camara,
#' Web Services for Big Earth Observation Data.
#' In: XVII Brazilian Symposium on Geoinformatics, 2016, Campos do Jordao.
#' Proceedings of GeoInfo 2016. Sao Jose dos Campos: INPE/SBC, 2016. p.166-177.
#'
#' @param cube            Data cube from where data is to be retrived.
#' @param file            File with information on the data to be retrieved
#'                        (options - CSV, SHP).
#' @param longitude       Longitude of the chosen location.
#' @param latitude        Latitude of the chosen location.
#' @param start_date      Start of the interval for the time series
#'                        in "YYYY-MM-DD" format (optional)
#' @param end_date        End of the interval for the time series in
#'                        "YYYY-MM-DD" format (optional).
#' @param bands           Bands to be retrieved (optional)
#' @param label           Label to be assigned to the time series (optional)
#' @param shp_attr        Attribute in the shapefile to be used
#'                        as a polygon label (for shapefiles only.
#' @param .n_shp_pol      Number of samples per polygon to be read
#'                        (for POLYGON or MULTIPOLYGON shapes).
#' @param .n_shp_pts      Number of points to be read (for POINT shapes).
#' @param .prefilter      Prefilter for SATVEG cube
#'                        ("0" - none, "1" - no data correction,
#'                        "2" - cloud correction,
#'                        "3" - no data and cloud correction).
#' @param .n_start_csv    Row on the CSV file to start reading.
#' @param .n_max_csv      Maximum number of CSV samples to be read
#'                        (set to Inf to read all).
#' @param .n_save         Number of samples to save as intermediate files
#'                        (used for long reads).
#' @return                A tibble with time series data and metadata.
#'
#' @examples
#' \donttest{
#' # Read a single lat long point from a WTSS server
#'
#' wtss_cube <- sits_cube(type = "WTSS",
#'                        URL = "http://www.esensing.dpi.inpe.br/wtss/",
#'                        name = "MOD13Q1")
#' point.tb <- sits_get_data (wtss_cube, longitude = -55.50563,
#'                                       latitude = -11.71557)
#' plot(point.tb)
#'
#' # Read a set of points defined in a CSV file from a WTSS server
#' csv_file <- system.file ("extdata/samples/samples_matogrosso.csv",
#'                           package = "sits")
#' points.tb <- sits_get_data (wtss_cube, file = csv_file)
#' # show the points retrieved for the WTSS server
#' plot(points.tb[1:3,])
#'
#'
#' # define a shapefile and read from the points inside it from WTSS
#' shp_file <- system.file("extdata/shapefiles/parcel_agriculture.shp",
#'                          package = "sits")
#' parcel.tb <- sits_get_data(wtss_cube, file = shp_file, .n_shp_pol = 5)
#'
#' # Read a point in a Raster Brick
#' # define the file that has the raster brick
#' files  <- c(system.file ("extdata/raster/mod13q1/sinop-crop-ndvi.tif",
#'                          package = "sits"))
#' # define the timeline
#' data(timeline_modis_392)
#' # create a data cube based on the information about the files
#' raster_cube <- sits_cube(type = "BRICK", satellite = "TERRA",
#'                          sensor = "MODIS", name = "Sinop-crop",
#'                          timeline = timeline_modis_392,
#'                          bands = c("ndvi"), files = files)
#'
#' # read the time series of the point from the raster
#' point_ts <- sits_get_data(raster_cube, longitude = -55.554,
#'                                        latitude = -11.525)
#' plot(point_ts)
#'
#' #' # Read a CSV file in a Raster Brick
#' csv_file <- system.file ("extdata/samples/samples_sinop_crop.csv",
#'                          package = "sits")
#' points.tb <- sits_get_data (raster_cube, file = csv_file)
#' # show the points retrieved for the RASTER images
#' plot(points.tb)
#' }
#' @export
sits_get_data <- function(cube,
                         file         = NULL,
                         longitude    = NULL,
                         latitude     = NULL,
                         start_date   = NULL,
                         end_date     = NULL,
                         bands        = NULL,
                         label        = "NoClass",
                         shp_attr     = NULL,
                         .n_shp_pol   = 20,
                         .n_shp_pts   = Inf,
                         .prefilter   = "1",
                         .n_start_csv = 1,
                         .n_max_csv   = Inf,
                         .n_save      = 0) {

    # Ensure that the cube is valid
    check <- .sits_cube_check_validity(cube)
    assertthat::assert_that(check == TRUE,
               msg = "sits_get_data: cube is not valid or not accessible")

    # No file is given - lat/long must be provided
    if (purrr::is_null(file)) {
        #precondition
        assertthat::assert_that(!purrr::is_null(latitude) &&
                                !purrr::is_null(longitude),
                    msg = "sits_get_data - latitude/longitude must be provided")

        data    <- .sits_ts_from_cube(cube = cube,
                                      longitude = longitude,
                                      latitude = latitude,
                                      start_date = start_date,
                                      end_date = end_date,
                                      bands = bands,
                                      label = label,
                                      .prefilter = .prefilter)
    }
    # file is given - must be either CSV or SHP
    else {
        # precondition
        assertthat::assert_that(tolower(tools::file_ext(file)) == "csv"
                             || tolower(tools::file_ext(file)) == "shp",
              msg = "sits_get_data - file must either be a CSV or a shapefile")

        # get data based on CSV file
        if (tolower(.sits_get_extension(file)) == "csv")
            data  <- .sits_from_csv(csv_file = file,
                                    cube = cube,
                                    bands = bands,
                                    .prefilter = .prefilter,
                                    .n_start_csv = .n_start_csv,
                                    .n_max_csv = .n_max_csv,
                                    .n_save = .n_save)

        # get data based on SHP file
        if (tolower(tools::file_ext(file)) == "shp")
            data  <- .sits_from_shp(shp_file = file,
                                    cube = cube,
                                    start_date = start_date,
                                    end_date = end_date,
                                    bands = bands,
                                    label = label,
                                    shp_attr = shp_attr,
                                    .n_shp_pol = .n_shp_pol,
                                    .n_shp_pts = .n_shp_pts,
                                    .prefilter = .prefilter)
    }
    if (!("sits" %in% class(data)))
        class(data) <- c("sits", class(data))
    return(data)
}
#' @title Extract a time series from a cube
#' @name .sits_ts_from_cube
#'
#' @param  cube        Data cube
#' @param  longitude   Longitude of point
#' @param  latitude    Latitude of point
#' @param  start_date  starting date for the time series
#' @param  end_date    end date for the time series
#' @param  bands       Bands to be retrieved
#' @param  label       Label to be assigned to the series
#' @param  .prefilter  Prefilter (used for SATVEG)
#' @return             A valid sits tibble
#'
.sits_ts_from_cube <- function(cube, longitude, latitude,
                              start_date, end_date, bands, label, .prefilter){

    if (.sits_config_cube_service(cube)) {
        data.tb <- .sits_ts_from_web(cube, longitude, latitude,
                                     start_date, end_date, bands, label, .prefilter)
    }
    else {
        timeline <- sits_timeline(cube)
        if (purrr::is_null(start_date))
            start_date <- timeline[1]
        if (purrr::is_null(end_date))
            end_date <- timeline[length(timeline)]
        ll.tb <- tibble::tibble(id = 1, longitude = longitude, latitude = latitude,
                                start_date = start_date, end_date = end_date, label = label)
        # transform ll.tb into a spatial points object
        lat_long <- sf::st_as_sf(ll.tb, coords = c("longitude", "latitude"), crs = 4326)

        data.tb <- .sits_ts_from_raster(cube       = cube,
                                        sf_object  = lat_long,
                                        bands      = bands)
    }
    return(data.tb)
}

#' @title Extract a time series from a ST raster data set
#' @name .sits_ts_from_raster
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Retrieve a set of time series for a raster data cube.
#'
#' @param cube              Metadata describing a raster data cube.
#' @param sf_object         sf object.
#' @param bands             Bands to be retrieved.
#' @return                  A sits tibble with the time series.
.sits_ts_from_raster <- function(cube,
                                 sf_object,
                                 bands){

    # ensure metadata tibble exists
    assertthat::assert_that(NROW(cube) >= 1,
            msg = "sits_ts_from_raster: need a valid metadata for data cube")

    spatial_points <- sf::as_Spatial(sf_object)

    # ensure metadata tibble exists
    assertthat::assert_that(nrow(spatial_points) >= 1,
            msg = "sits_ts_from_raster: need a valid sf_object")

    # get the timeline
    timeline <- sits_timeline(cube)

    # get the bands, scale factors and missing values
    bands <- unlist(cube$bands)
    missing_values <- unlist(cube$missing_values)
    scale_factors  <- unlist(cube$scale_factors)

    # An input raster brick contains several files, each corresponds to a band
    ts_bands.lst <- bands %>%
        purrr::map(function(band) {
            # create a tibble to store the data for each band
            ts_band.tb <- .sits_tibble()
            # get the values of the time series
            r_obj <- .sits_cube_robj_band(cube, band)
            values.mx <- suppressWarnings(raster::extract(r_obj,spatial_points))
            rm(r_obj)
            # is the data valid?
            assertthat::assert_that(nrow(values.mx) > 0,
                        msg = "sits_ts_from_raster - no data retrieved")
            if (all(is.na(values.mx))) {
                message("point outside the raster extent - NULL returned")
                return(NULL)
            }

            # each row of the values matrix is a spatial point
            for (i in 1:nrow(values.mx)) {
                time_idx <- .sits_timeline_indexes(timeline = timeline,
                                                   start_date = spatial_points$start_date[i],
                                                   end_date = spatial_points$end_date[i])
                # select the valid dates in the timeline
                timeline <- timeline[time_idx["start_idx"]:time_idx["end_idx"]]
                # get only valid values for the timeline
                values.vec <- as.vector(values.mx[i, time_idx["start_idx"]:time_idx["end_idx"]])
                # correct the values using the scale factor
                values.vec <- values.vec*scale_factors[band]
                # create a tibble for each band
                ts.tb <- tibble::tibble(Index = timeline)
                # put the values in the time series tibble together t
                ts.tb$values <- values.vec
                colnames(ts.tb) <- c("Index", band)

                # insert a row on the tibble with the values for lat/long and the band
                ts_band.tb <- tibble::add_row(ts_band.tb,
                    longitude    = as.vector(sp::coordinates(spatial_points)[i,1]),
                    latitude     = as.vector(sp::coordinates(spatial_points)[i,2]),
                    start_date   = timeline[time_idx["start_idx"]],
                    end_date     = timeline[time_idx["end_idx"]],
                    label        = spatial_points$label[i],
                    cube         = cube$name,
                    time_series  = list(ts.tb)
                )
            }
            return(ts_band.tb)
        })

    # merge the bands
    data.tb <- .sits_tibble()
    l <- length(ts_bands.lst)
    for (i in 1:l) {
        data.tb <- sits_merge(data.tb, ts_bands.lst[[i]])
    }


    return(data.tb)
}
#' @title Obtain timeSeries from a web service associated to data cubes
#' @name .sits_ts_from_web
#'
#' @description Obtains a time series from a time series service.
#'
#' @param cube            Data cube metadata.
#' @param longitude       Longitude of the chosen location.
#' @param latitude        Latitude of the chosen location).
#' @param start_date      Start date of the period.
#' @param end_date        End date of the period.
#' @param bands           Bands to be retrieved (optional).
#' @param label           Label to attach to the time series.
#' @param .prefilter      String (only for SATVEG)
#'                        ("0" - none, "1" - no data correction,
#'                        "2" - cloud correction,
#'                        "3" - no data and cloud correction).
#' @return A sits tibble.
#'
.sits_ts_from_web  <- function(cube,
                               longitude,
                               latitude,
                               start_date,
                               end_date,
                               bands,
                               label = "NoClass",
                               .prefilter  = "1") {

    # find out which is the service associate to the cube

    if (cube$type == "WTSS") {
        data <- .sits_from_wtss(cube = cube,
                                longitude = longitude,
                                latitude = latitude,
                                start_date = start_date,
                                end_date = end_date,
                                bands = bands,
                                label = label)
        return(data)
    }
    if (cube$type == "SATVEG") {
        data <- .sits_from_satveg(cube = cube,
                                  longitude = longitude,
                                  latitude = latitude,
                                  start_date = start_date,
                                  end_date = end_date,
                                  bands = bands,
                                  label = label,
                                  .prefilter = .prefilter)

        return(data)
    }
    return(NULL)
}

# function
.sits_get_extension <- function(file){
    ex <- strsplit(basename(file), split="\\.")[[1]]
    return(ex[-1])
}

