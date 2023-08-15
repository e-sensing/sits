#' @title Get time series from data cubes and cloud services
#' @name sits_get_data
#' @author Gilberto Camara
#'
#' @description Retrieve a set of time series from a data cube or from
#' a time series service. Data cubes and puts it in a "sits tibble".
#' Sits tibbles are the main structures of sits package.
#' They contain both the satellite image time series and their metadata.
#'
#' @note
#' There are four ways of specifying data to be retrieved using the
#' "samples" parameter:
#' \itemize{
#' \item{CSV file: }{Provide a CSV file with columns
#' "longitude", "latitude", "start_date", "end_date" and "label" for
#' each sample}
#' \item{SHP file: }{Provide a shapefile in POINT or POLYGON geometry
#' containing the location of the samples and an attribute to be
#' used as label. Also, provide start and end date for the time series.}
#' \item{sits object: }{A sits tibble.}
#' \item{sf object: }{An "sf" object with POINT or POLYGON geometry.}
#' \item{data.frame: }{A data.frame with with mandatory columns
#' "longitude", "latitude".}
#' }
#
#' @param cube            Data cube from where data is to be retrieved.
#'                        (tibble of class "raster_cube").
#' @param samples         Location of the samples to be retrieved.
#'                        Either a tibble of class "sits", an "sf" object,
#'                        the name of a shapefile or csv file, or
#'                        a data.frame with columns "longitude" and "latitude".
#' @param ...             Specific parameters for specific cases.
#' @param start_date      Start of the interval for the time series - optional
#'                        (Date in "YYYY-MM-DD" format).
#' @param end_date        End of the interval for the time series - optional
#'                        (Date in "YYYY-MM-DD" format).
#' @param label           Label to be assigned to the time series (optional)
#'                        (character vector of length 1).
#' @param bands           Bands to be retrieved - optional
#'                        (character vector).
#' @param crs             Default crs for the samples
#'                        (character vector of length 1).
#' @param label_attr      Attribute in the shapefile or sf object to be used
#'                        as a polygon label.
#'                        (character vector of length 1).
#' @param n_sam_pol       Number of samples per polygon to be read
#'                        for POLYGON or MULTIPOLYGON shapefiles or sf objects
#'                        (single integer).
#' @param pol_avg         Logical: summarize samples for each polygon?
#' @param pol_id          ID attribute for polygons
#'                        (character vector of length 1)
#' @param aggreg_fn       Function to compute a summary of each segment
#'                        (object of class "function").
#' @param multicores      Number of threads to process the time series
#'                        (integer, with min = 1 and max = 2048).
#' @param progress        Logical: show progress bar?
#'
#' @return A tibble of class "sits" with set of time series
#' <longitude, latitude, start_date, end_date, label, cube, time_series>.
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # reading a lat/long from a local cube
#'     # create a cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     raster_cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
#'         data_dir = data_dir
#'     )
#'     samples <- tibble::tibble(longitude = -55.66738, latitude = -11.76990)
#'     point_ndvi <- sits_get_data(raster_cube, samples)
#'     #
#'     # reading samples from a cube based on a  CSV file
#'     csv_file <- system.file("extdata/samples/samples_sinop_crop.csv",
#'         package = "sits"
#'     )
#'     points <- sits_get_data(cube = raster_cube, samples = csv_file)
#'
#'     # reading a shapefile from BDC (Brazil Data Cube)
#'     bdc_cube <- sits_cube(
#'             source = "BDC",
#'             collection = "CBERS-WFI-16D",
#'             bands = c("NDVI", "EVI"),
#'             tiles = c("007004", "007005"),
#'             start_date = "2018-09-01",
#'             end_date = "2018-10-28"
#'     )
#'     # define a shapefile to be read from the cube
#'     shp_file <- system.file("extdata/shapefiles/bdc-test/samples.shp",
#'             package = "sits"
#'     )
#'     # get samples from the BDC based on the shapefile
#'     time_series_bdc <- sits_get_data(
#'         cube = bdc_cube,
#'         samples = shp_file
#'     )
#' }
#'
#' @export
sits_get_data <- function(cube,
                          samples = NULL,
                          ...,
                          start_date = as.Date(sits_timeline(cube)[1]),
                          end_date = as.Date(
                              sits_timeline(cube)[length(sits_timeline(cube))]
                          ),
                          label = "NoClass",
                          bands = sits_bands(cube),
                          crs = 4326L,
                          label_attr = NULL,
                          n_sam_pol = 30L,
                          pol_avg = FALSE,
                          pol_id = NULL,
                          multicores = 2L,
                          progress = TRUE) {
    # Pre-conditions
    .check_is_raster_cube(cube)
    .check_is_regular(cube)
    .check_cube_files(cube)
    .check_cube_bands(cube, bands = bands)
    .check_crs(crs)
    .check_multicores(multicores, min = 1, max = 2048)
    .check_progress(progress)
    if (is.character(samples)) {
        class(samples) <- c(.file_ext(samples), class(samples))
    }
    UseMethod("sits_get_data", samples)
}
#' @rdname sits_get_data
#'
#' @export
sits_get_data.default <- function(cube, samples, ...) {
    stop("Invalid samples parameter for sits_get_data")
}

#' @rdname sits_get_data
#' @export
sits_get_data.csv <- function(cube,
                              samples,
                              ...,
                              bands = sits_bands(cube),
                              crs = 4326L,
                              multicores = 2,
                              progress = FALSE) {
    # Get samples
    samples <- .csv_get_samples(samples)
    data <- .data_get_ts(
        cube       = cube,
        samples    = samples,
        bands      = bands,
        crs        = crs,
        multicores = multicores,
        progress   = progress
    )
    return(data)
}
#' @rdname sits_get_data
#' @export
sits_get_data.shp <- function(cube,
                              samples,
                              ...,
                              label = "NoClass",
                              start_date = as.Date(sits_timeline(cube)[1]),
                              end_date = as.Date(sits_timeline(cube)
                                                 [length(sits_timeline(cube))]),
                              bands = sits_bands(cube),
                              label_attr = NULL,
                              n_sam_pol = 30,
                              pol_avg = FALSE,
                              pol_id = NULL,
                              multicores = 2,
                              progress = FALSE) {
    # pre-condition - shapefile should have an id parameter
    .check_that(
        !(pol_avg && purrr::is_null(pol_id)),
        msg = "invalid 'pol_id' parameter."
    )
    # extract a data frame from shapefile
    samples <- .shp_get_samples(
        shp_file    = samples,
        label       = label,
        shp_attr    = label_attr,
        start_date  = start_date,
        end_date    = end_date,
        n_shp_pol   = n_sam_pol,
        shp_id      = pol_id
    )
    # extract time series from a cube given a data.frame
    data <- .data_get_ts(
        cube       = cube,
        samples    = samples,
        bands      = bands,
        multicores = multicores,
        progress   = progress
    )
    if (pol_avg && "polygon_id" %in% colnames(data)) {
        data <- .data_avg_polygon(data = data)
    }
    return(data)
}

#' @rdname sits_get_data
#' @export
sits_get_data.sf <- function(cube,
                             samples,
                             ...,
                             bands = sits_bands(cube),
                             start_date = as.Date(sits_timeline(cube)[1]),
                             end_date = as.Date(sits_timeline(cube)
                                                [length(sits_timeline(cube))]),
                             label = "NoClass",
                             label_attr = NULL,
                             n_sam_pol = 30,
                             pol_avg = FALSE,
                             pol_id = NULL,
                             multicores = 2,
                             progress = FALSE) {
    .check_that(
        !(pol_avg && purrr::is_null(pol_id)),
        msg = "Please provide an sf object with a column
        with the id for each polygon and include
        this column name in the 'pol_id' parameter."
    )
    # extract a samples data.frame from sf object
    samples <- .sf_get_samples(
        sf_object  = samples,
        label      = label,
        label_attr = label_attr,
        start_date = start_date,
        end_date   = end_date,
        n_sam_pol  = n_sam_pol,
        pol_id     = pol_id
    )
    # extract time series from a cube given a data.frame
    data <- .data_get_ts(
        cube       = cube,
        samples    = samples,
        bands      = bands,
        multicores = multicores,
        progress   = progress
    )
    # Do we want an average value per polygon?
    if (pol_avg && "polygon_id" %in% colnames(data)) {
        data <- .data_avg_polygon(data = data)
    }
    return(data)
}
#' @rdname sits_get_data
#' @export
sits_get_data.sits <- function(cube,
                               samples,
                               ...,
                               bands = sits_bands(cube),
                               multicores = 2,
                               progress = FALSE) {
    # extract time series from a cube given a data.frame
    data <- .data_get_ts(
        cube       = cube,
        samples    = samples,
        bands      = bands,
        multicores = multicores,
        progress   = progress
    )
    return(data)
}
#' @rdname sits_get_data
#'
#' @export
#'
sits_get_data.data.frame <- function(cube,
                                     samples,
                                     ...,
                                     start_date = as.Date(
                                         sits_timeline(cube)[1]
                                     ),
                                     end_date = as.Date(
                                         sits_timeline(cube)[
                                             length(sits_timeline(cube))
                                         ]
                                     ),
                                     label = "NoClass",
                                     bands = sits_bands(cube),
                                     crs = 4326,
                                     multicores = 2,
                                     progress = FALSE) {
    # check if samples contains all the required columns
    .check_chr_contains(
        x = colnames(samples),
        contains = c("latitude", "longitude"),
        discriminator = "all_of",
        msg = "missing lat/long information in data frame"
    )
    # fill missing columns
    if (!("label" %in% colnames(samples))) {
        samples$label <- label
    }
    if (!("start_date" %in% colnames(samples))) {
        samples$start_date <- start_date
    }
    if (!("end_date" %in% colnames(samples))) {
        samples$end_date <- end_date
    }
    class(samples) <- c("sits", class(samples))
    # extract time series from a cube given a data.frame
    data <- .data_get_ts(
        cube       = cube,
        samples    = samples,
        bands      = bands,
        crs        = crs,
        multicores = multicores,
        progress   = progress
    )
    return(data)
}

#' @rdname sits_get_data
#' @export
sits_get_data.segs_cube <- function(cube,
                                    samples = NULL,
                                    ...,
                                    bands = sits_bands(cube),
                                    aggreg_fn = "mean",
                                    pol_id = "pol_id",
                                    n_sam_pol = NULL,
                                    multicores = 1,
                                    progress = FALSE) {
    .check_that(
        x = !(purrr::is_null(aggreg_fn) && purrr::is_null(n_sam_pol)),
        msg = "either aggreg_fun or n_sam_pol must not be both NULL"
    )
    # TODO: refactor
    # extract time series from a cube from a set of segments
    # case 1 - one point per polygon with an aggregation function
    if (purrr::is_null(n_sam_pol)) {
        data <- .segments_get_summary(
            cube = cube,
            segments = samples,
            bands = bands,
            aggreg_fn = aggreg_fn,
            pol_id = pol_id,
            multicores = multicores,
            progress = progress
        )
    } else {
        data <- .segments_get_data(
            cube = cube,
            bands = bands,
            aggreg_fn = aggreg_fn,
            pol_id = pol_id,
            multicores = multicores,
            progress = progress
        )
    }

    return(data)
}
