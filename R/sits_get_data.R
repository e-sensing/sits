#' @title Obtain time series from different sources
#' @name sits_get_data
#' @author Gilberto Camara
#'
#' @description Retrieve a set of time series from a data cube or from
#' a time series service. Data cubes
#'  and puts it in a "sits tibble".
#' Sits tibbles are the main structures of sits package.
#' They contain both the satellite image time series and their metadata.
#' A sits tibble is a tibble with pre-defined columns that
#' has the metadata and data for each time series. The columns are
#' <longitude, latitude, start_date, end_date, label, cube, time_series>.
#' There are many ways of retrieving time series:
#' \itemize{
#' \item{SATVEG:}{Retrieve data from SATVEG service using a lat/long point
#'  (see S3 method for class 'satveg_cube'),
#'  a CSV file (see S3 method for class 'csv_satveg_cube')
#'  or a SHP file (see S3 method for class 'shp_satveg_cube')}
#'
#' \item{RASTER: }{Retrieve data from any raster cube using a lat/long point
#'  (see S3 method for class 'raster_cube'),
#'  a CSV file (see S3 method for class 'csv_raster_cube')
#'  or a SHP file (see S3 method for class 'shp_raster_cube')}
#'
#' }
#'
#' The URL and other parameters for accessing the time series services
#' are defined in the package configuration file. This file is "config.yml".
#' Please see the \code{\link[sits]{sits_config}} for more information.
#'
#' Before using this service, the user should create a valid description
#' of a data cube using the \code{\link[sits]{sits_cube}} function.
#'
#' @references
#' Lubia Vinhas, Gilberto Queiroz, Karine Ferreira, Gilberto Camara,
#' Web Services for Big Earth Observation Data.
#' In: XVII Brazilian Symposium on Geoinformatics, 2016, Campos do Jordao.
#' Proceedings of GeoInfo 2016. Sao Jose dos Campos: INPE/SBC, 2016. p.166-177.
#
#' @param cube            Data cube from where data is to be retrieved.
#' @param file            File with information on the data to be retrieved.
#' @param ...             Other parameters to be passed for specific types.
#' @param multicores      Number of threads to process the time series.
#' @param longitude       Longitude of the chosen location.
#' @param latitude        Latitude of the chosen location.
#' @param start_date      Start of the interval for the time series
#'                        in "YYYY-MM-DD" format (optional).
#' @param end_date        End of the interval for the time series in
#'                        "YYYY-MM-DD" format (optional).
#' @param label           Label to be assigned to the time series (optional).
#' @param bands           Bands to be retrieved (optional).
#' @param impute_fn       Imputation function for NA values.
#' @param shp_attr        Attribute in the shapefile to be used
#'                        as a polygon label.
#' @param .n_pts_csv      Number of points from CSV file to be retrieved.
#' @param .n_shp_pol      Number of samples per polygon to be read
#'                        (for POLYGON or MULTIPOLYGON shapefile).
#'
#' @return A tibble with the metadata and data for each time series
#' <longitude, latitude, start_date, end_date, label, cube, time_series>.
#'
#' @examples
#' \donttest{
#' # -- Read a point in a raster data cube
#'
#' # Create a data cube based on files
#' data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#' raster_cube <- sits_cube(
#'     source = "LOCAL",
#'     name = "sinop-2014",
#'     satellite = "TERRA",
#'     sensor = "MODIS",
#'     data_dir = data_dir,
#'     delim = "_",
#'     parse_info = c("X1", "X2", "tile", "band", "date")
#' )
#'
#' # read the time series of the point from the raster
#' point_ts <- sits_get_data(raster_cube,
#'     longitude = -55.554,
#'     latitude = -11.525
#' )
#'
#' # --- Read a set of points described by a CSV file
#'
#' # read data from a CSV file
#' csv_file <- system.file("extdata/samples/samples_sinop_crop.csv",
#'     package = "sits"
#' )
#' points_csv <- sits_get_data(raster_cube, file = csv_file)
#' }
#' @export
#'
sits_get_data <- function(cube,
                          file = NULL,
                          ...,
                          multicores = 1) {

    # is there a shapefile or a CSV file?
    if (!purrr::is_null(file)) {
        # get the file extension
        file_ext <- tolower(tools::file_ext(file))
        # sits only accepts "csv" or "shp" files
        assertthat::assert_that(
            file_ext %in% c("csv", "shp"),
            msg = "sits_get_data: accepts only csv and shp files"
        )
        # append "csv" or "shp" to the cube class to call the correct function
        class(cube) <- c(paste0(file_ext, "_", class(cube)[1]),
                         paste0(file_ext, "_raster_cube"), class(cube))
    }
    # Dispatch
    UseMethod("sits_get_data", cube)
}

#' @rdname sits_get_data
#'
#' @export
#'
sits_get_data.wtss_cube <- function(cube, file = NULL, ...,
                                    longitude = NULL,
                                    latitude = NULL,
                                    start_date = NULL,
                                    end_date = NULL,
                                    bands = NULL,
                                    label = "NoClass") {

    # Precondition - lat/long must be provided
    assertthat::assert_that(!purrr::is_null(latitude) &
                                !purrr::is_null(longitude),
                            msg = paste("sits_get_data: latitude/longitude",
                                        "must be provided")
    )

    # Precondition - check bands
    bands <- .sits_cube_bands_check(cube, bands)

    # Precondition - check and get start and end dates
    start_end <- .sits_timeline_check_cube(cube, start_date, end_date)

    data <- .sits_from_wtss(
        cube = cube,
        longitude = longitude,
        latitude = latitude,
        start_date = start_end["start_date"],
        end_date = start_end["end_date"],
        bands = bands,
        label = label
    )
    if (!purrr::is_null(data) && !("sits" %in% class(data))) {
        class(data) <- c("sits", class(data))
    }
    return(data)
}

#' @rdname sits_get_data
#'
#' @export
#'
sits_get_data.satveg_cube <- function(cube,
                                      file = NULL,
                                      ...,
                                      longitude = NULL,
                                      latitude = NULL,
                                      start_date = NULL,
                                      end_date = NULL,
                                      label = "NoClass") {
    # Precondition - is the SATVEG cube available?
    # Retrieve the URL to test for SATVEG access
    url <- .sits_config_satveg_access()

    # test if SATVEG is accessible
    if (!(.sits_config_cube_access(url, "SATVEG")))
        return(NULL)

    # Precondition - lat/long must be provided
    assertthat::assert_that(
        !purrr::is_null(latitude) && !purrr::is_null(longitude),
        msg = "sits_get_data: latitude/longitude must be provided"
    )

    data <- .sits_from_satveg(
        cube = cube,
        longitude = longitude,
        latitude = latitude,
        start_date = start_date,
        end_date = end_date,
        label = label
    )

    if (!inherits(data, "sits")) {
        class(data) <- c("sits", class(data))
    }
    return(data)
}
#' @rdname sits_get_data
#'
#' @export
#'
sits_get_data.csv_wtss_cube <- function(cube, file, ..., bands = NULL) {

    # read sample information from CSV file and put it in a tibble
    csv <- tibble::as_tibble(utils::read.csv(file))

    # Precondition - check if CSV file is correct
    .sits_csv_check(csv)

    # Precondition - check bands
    bands <- .sits_cube_bands_check(cube, bands)

    # for each row of the input, retrieve the time series
    data_lst <- purrr::pmap(
        list(
            csv$longitude,
            csv$latitude,
            csv$start_date,
            csv$end_date,
            csv$label
        ),
        function(longitude, latitude, start_date, end_date, label) {
            row <- .sits_from_wtss(
                cube = cube,
                longitude = longitude,
                latitude = latitude,
                start_date = lubridate::as_date(start_date),
                end_date = lubridate::as_date(end_date),
                bands = bands,
                label = label
            )
            return(row)
        }
    )
    # unroll the list
    data <- dplyr::bind_rows(data_lst)
    # check if data has been retrieved
    .sits_get_data_check(nrow(csv), nrow(data))

    return(data)
}

#' @rdname sits_get_data
#'
#' @export
#'
sits_get_data.csv_satveg_cube <- function(cube, file, ...) {

    # Precondition - is the SATVEG cube available?
    # Retrieve the URL to test for SATVEG access
    url <- .sits_config_satveg_access()

    # test if SATVEG is accessible
    if (!(.sits_config_cube_access(url, "SATVEG")))
        return(NULL)

    # read sample information from CSV file and put it in a tibble
    csv <- tibble::as_tibble(utils::read.csv(file,
                                             stringsAsFactors = FALSE))

    # Precondition - check if CSV file is correct
    .sits_csv_check(csv)
    # get the cube timeline
    timeline <- sits_timeline(cube)
    start_date_cube <- timeline[1]
    end_date_cube <- timeline[length(timeline)]

    # for each row of the input, retrieve the time series
    data_lst <- purrr::pmap(
        list(
            csv$longitude,
            csv$latitude,
            csv$start_date,
            csv$end_date,
            csv$label
        ),
        function(long, lat, st_date, en_date, lab) {
            if (as.Date(st_date) < start_date_cube)
                st_date <- start_date_cube
            if (as.Date(en_date) > end_date_cube)
                en_date <- end_date_cube
            row <- .sits_from_satveg(
                cube = cube,
                longitude = long,
                latitude = lat,
                start_date = lubridate::as_date(st_date),
                end_date = lubridate::as_date(en_date),
                label = lab
            )
            return(row)
        }
    )
    # unroll the list
    data <- dplyr::bind_rows(data_lst)

    # check if data has been retrieved
    .sits_get_data_check(nrow(csv), nrow(data))

    return(data)
}

#' @rdname sits_get_data
#'
#' @export
#'
sits_get_data.shp_wtss_cube <- function(cube, file, ...,
                                        start_date = NULL,
                                        end_date = NULL,
                                        bands = NULL,
                                        label = "NoClass",
                                        shp_attr = NULL,
                                        .n_shp_pol = 30) {

    # Precondition - check that the timelines are compatible with the cube
    start_end <- .sits_timeline_check_cube(cube, start_date, end_date)

    # Precondition - check bands
    bands <- .sits_cube_bands_check(cube, bands)

    # precondition - check the shape file and its attribute
    sf_shape <- .sits_shp_check_validity(
        shp_file = file, shp_attr = shp_attr,
        label = label
    )
    # get the points to be read
    points <- .sits_points_from_shp(
        sf_shape = sf_shape, shp_attr = shp_attr,
        label = label, .n_shp_pol = .n_shp_pol
    )

    # read the points
    # for each row of the input, retrieve the time series
    data_lst <- purrr::pmap(
        list(
            points$longitude,
            points$latitude,
            points$label
        ),
        function(long, lat, lab) {
            row <- .sits_from_wtss(
                cube = cube,
                longitude = long,
                latitude = lat,
                start_date = start_end["start_date"],
                end_date = start_end["end_date"],
                bands = bands,
                label = lab
            )
            return(row)
        }
    )
    # unroll the list
    data <- dplyr::bind_rows(data_lst)

    return(data)
}

#' @rdname sits_get_data
#'
#' @export
#'
sits_get_data.shp_satveg_cube <- function(cube, file, ...,
                                          start_date = NULL,
                                          end_date = NULL,
                                          label = "NoClass",
                                          shp_attr = NULL,
                                          .n_shp_pol = 30) {

    # Precondition - is the SATVEG cube available?
    # Retrieve the URL to test for SATVEG access
    url <- .sits_config_satveg_access()

    # test if SATVEG is accessible
    if (!(.sits_config_cube_access(url, "SATVEG")))
        return(NULL)

    # precondition - check the shape file and its attribute
    sf_shape <- .sits_shp_check_validity(
        shp_file = file, shp_attr = shp_attr,
        label = label
    )

    # get the points to be read
    points <- .sits_points_from_shp(
        sf_shape = sf_shape,
        shp_attr = shp_attr,
        label = label,
        .n_shp_pol = .n_shp_pol
    )

    # read the points
    # for each row of the input, retrieve the time series
    data_lst <- purrr::pmap(
        list(
            points$longitude,
            points$latitude,
            points$label
        ),
        function(long, lat, lab) {
            row <- .sits_from_satveg(
                cube = cube,
                longitude = long,
                latitude = lat,
                start_date = start_date,
                end_date = end_date,
                label = lab
            )
            return(row)
        }
    )
    # unroll the list
    data <- dplyr::bind_rows(data_lst)

    return(data)
}

#' @rdname sits_get_data
#'
#' @export
#'
sits_get_data.raster_cube <- function(cube, file = NULL, ...,
                                      longitude = NULL,
                                      latitude = NULL,
                                      start_date = NULL,
                                      end_date = NULL,
                                      bands = NULL,
                                      label = "NoClass",
                                      impute_fn = sits_impute_linear()) {

    # precondition - are the files in the cube accessible?
    if (!(.sits_config_cube_file_access(cube)))
        return(NULL)

    # Precondition - lat/long must be provided
    assertthat::assert_that(
        !purrr::is_null(latitude) && !purrr::is_null(longitude),
        msg = "sits_get_data: latitude/longitude must be provided"
    )

    # Precondition - check and get start and end dates
    start_end <- .sits_timeline_check_cube(cube, start_date, end_date)

    # Precondition - check bands
    bands <- .sits_cube_bands_check(cube, bands)

    ll <- tibble::tibble(
        id = 1,
        longitude = longitude,
        latitude = latitude,
        start_date = start_end["start_date"],
        end_date = start_end["end_date"],
        label = label
    )

    # is the cloud band available?
    cld_band <- .config_cloud()

    if (cld_band %in% bands) {
        bands <- bands[bands != cld_band]
    } else {
        cld_band <- NULL
    }

    ts_rows <- slider::slide(cube, function(row) {
        # get the data
        ts <- .sits_raster_data_get_ts(
            cube = row,
            points = ll,
            bands = bands,
            cld_band = cld_band,
            impute_fn = impute_fn
        )
        return(ts)
    })
    data <- dplyr::bind_rows(ts_rows)

    if (!inherits(data, "sits")) {
        class(data) <- c("sits", class(data))
    }
    return(data)
}

#' @rdname sits_get_data
#'
#' @export
#'
sits_get_data.csv_raster_cube <- function(cube, file, ...,
                                          bands = NULL,
                                          impute_fn = sits_impute_linear(),
                                          multicores = 1,
                                          .n_pts_csv = NULL) {

    # precondition - are the files in the cube accessible?
    if (!(.sits_config_cube_file_access(cube)))
        return(NULL)

    # read sample information from CSV file and put it in a tibble
    csv <- tibble::as_tibble(utils::read.csv(file,
                                             stringsAsFactors = FALSE))

    # check if user has requested fewer points than full csv file
    if (!purrr::is_null(.n_pts_csv) && .n_pts_csv <= nrow(csv)) {
        csv <- csv[1:.n_pts_csv, ]
    }

    # precondition - csv has to contain valid columns
    .sits_csv_check(csv)

    # precondition - check bands
    bands <- .sits_cube_bands_check(cube, bands)

    # convert to date
    csv$start_date <- lubridate::as_date(csv$start_date)
    csv$end_date <- lubridate::as_date(csv$end_date)

    # is the cloud band available?
    cld_band <- .config_cloud()
    if (cld_band %in% bands) {
        bands <- bands[bands != cld_band]
    } else {
        cld_band <- NULL
    }

    # prepare parallelization
    .sits_parallel_start(workers = multicores)
    on.exit(.sits_parallel_stop(), add = TRUE)

    ts_rows <- slider::slide(cube, function(tile) {
        # get the data
        ts <- .sits_raster_data_get_ts(
            cube = tile,
            points = csv,
            bands = bands,
            cld_band = cld_band,
            impute_fn = impute_fn
        )
        return(ts)
    })
    data <- dplyr::bind_rows(ts_rows)

    # check if data has been retrieved
    .sits_get_data_check(nrow(csv), nrow(data))

    if (!inherits(data, "sits")) {
        class(data) <- c("sits", class(data))
    }
    return(data)
}

#' @rdname sits_get_data
#'
#' @export
#'
sits_get_data.shp_raster_cube <- function(cube, file, ...,
                                          start_date = NULL,
                                          end_date = NULL,
                                          bands = NULL,
                                          label = "NoClass",
                                          impute_fn = sits_impute_linear(),
                                          multicores = 1,
                                          shp_attr = NULL,
                                          .n_shp_pol = 30) {


    # precondition - are the files in the cube accessible?
    if (!(.sits_config_cube_file_access(cube)))
        return(NULL)
    # precondition - check the validity of the shape file
    sf_shape <- .sits_shp_check_validity(
        shp_file = file,
        shp_attr = shp_attr,
        label = label
    )

    # precondition - check the start and end date
    start_end <- .sits_timeline_check_cube(cube, start_date, end_date)

    # precondition - check bands
    bands <- .sits_cube_bands_check(cube, bands)

    # get the points to be read
    points <- .sits_points_from_shp(
        sf_shape = sf_shape,
        shp_attr = shp_attr,
        label = label,
        .n_shp_pol = .n_shp_pol
    )

    # include the start and end dates
    points$start_date <- start_end["start_date"]
    points$end_date <- start_end["end_date"]

    # is the cloud band available?
    cld_band <- .config_cloud()
    if (cld_band %in% bands) {
        bands <- bands[bands != cld_band]
    } else {
        cld_band <- NULL
    }

    # prepare parallelization
    .sits_parallel_start(workers = multicores)
    on.exit(.sits_parallel_stop(), add = TRUE)

    # for each row of the cube, get the points inside
    ts_rows <- slider::slide(cube, function(row) {
        # retrieve the data from raster
        ts <- .sits_raster_data_get_ts(
            cube = row,
            points = points,
            bands = bands,
            cld_band = cld_band,
            impute_fn = impute_fn
        )
        return(ts)
    })
    # join the results
    data <- dplyr::bind_rows(ts_rows)
    # adjust for the class of the data
    if (!inherits(data, "sits")) {
        class(data) <- c("sits", class(data))
    }
    return(data)
}

#' @title check if all points have been retrieved
#'
#' @name .sits_get_data_check
#'
#' @keywords internal
#'
#' @param n_rows_input     Number of rows in input
#' @param n_rows_output    Number of rows in output
#'
#' @return A logical value
#'
.sits_get_data_check <- function(n_rows_input, n_rows_output) {

    # Have all input rows being read?
    if (n_rows_output == 0) {
        message("No points have been retrieved")
        return(invisible(FALSE))
    }

    if (n_rows_output < n_rows_input) {
        message("Some points could not be retrieved")
    } else {
        message("All points have been retrieved")
    }

    return(invisible(TRUE))
}
