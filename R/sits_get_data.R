#' @title Get time series from data cubes and cloud services
#' @name sits_get_data
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @description Retrieve a set of time series from a data cube and
#' and put the result in a \code{sits tibble}, which
#' contains both the satellite image time series and their metadata.
#'
#' There are five options for the specifying the input
#' \code{samples} parameter:
#' \itemize{
#' \item{A CSV file: see \code{\link[sits]{sits_get_data.csv}}.}
#' \item{A shapefile: see \code{\link[sits]{sits_get_data.shp}}. }
#' \item{An \code{sf} object: see \code{\link[sits]{sits_get_data.sf}}.}
#' \item{A \code{sits} tibble: see \code{\link[sits]{sits_get_data.sits}}. }
#' \item{A data.frame: see see \code{\link[sits]{sits_get_data.data.frame}}.}
#' }
#'
#' @note
#' The main \code{sits} classification workflow has the following steps:
#' \enumerate{
#'      \item{\code{\link[sits]{sits_cube}}: selects a ARD image collection from
#'          a cloud provider.}
#'      \item{\code{\link[sits]{sits_cube_copy}}: copies an ARD image collection
#'          from a cloud provider to a local directory for faster processing.}
#'      \item{\code{\link[sits]{sits_regularize}}: create a regular data cube
#'          from an ARD image collection.}
#'      \item{\code{\link[sits]{sits_apply}}: create new indices by combining
#'          bands of a  regular data cube (optional).}
#'      \item{\code{\link[sits]{sits_get_data}}: extract time series
#'          from a regular data cube based on user-provided labelled samples.}
#'      \item{\code{\link[sits]{sits_train}}: train a machine learning
#'          model based on image time series.}
#'      \item{\code{\link[sits]{sits_classify}}: classify a data cube
#'          using a machine learning model and obtain a probability cube.}
#'      \item{\code{\link[sits]{sits_smooth}}: post-process a probability cube
#'          using a spatial smoother to remove outliers and
#'          increase spatial consistency.}
#'      \item{\code{\link[sits]{sits_label_classification}}: produce a
#'          classified map by selecting the label with the highest probability
#'          from a smoothed cube.}
#' }
#'
#' To be able to build a machine learning model to classify a data cube,
#' one needs to use a set of labelled time series. These time series
#' are created by taking a set of known samples, expressed as
#' labelled points or polygons. This \code{sits_get_data} function
#'  uses these samples to
#' extract time series from a data cube. It needs a \code{cube} parameter
#' which points to a regularized data cube, and a \code{samples} parameter
#' that describes the locations of the training set.
#'
#' @param cube            Data cube from where data is to be retrieved.
#'                        (tibble of class "raster_cube").
#' @param samples         Location of the samples to be retrieved.
#'                        Either a tibble of class "sits", an "sf" object,
#'                        the name of a shapefile or csv file, or
#'                        a data.frame with columns "longitude" and "latitude".
#' @param ...             Specific parameters for each input.
#'
#' @return A tibble of class "sits" with set of time series
#' <longitude, latitude, start_date, end_date, label, time_series>.
#'
#'
#' @examples
#' if (sits_run_examples()) {
#'     # reading a lat/long from a local cube
#'     # create a cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     raster_cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
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
#'         source = "BDC",
#'         collection = "CBERS-WFI-16D",
#'         bands = c("NDVI", "EVI"),
#'         tiles = c("007004", "007005"),
#'         start_date = "2018-09-01",
#'         end_date = "2018-10-28"
#'     )
#'     # define a shapefile to be read from the cube
#'     shp_file <- system.file("extdata/shapefiles/bdc-test/samples.shp",
#'         package = "sits"
#'     )
#'     # get samples from the BDC based on the shapefile
#'     time_series_bdc <- sits_get_data(
#'         cube = bdc_cube,
#'         samples = shp_file
#'     )
#' }
#'
#' @export
sits_get_data <- function(cube, samples, ...) {
    .check_set_caller("sits_get_data")
    # Pre-conditions
    .check_is_raster_cube(cube)
    .check_cube_is_regular(cube)
    .check_raster_cube_files(cube)
    if (is.character(samples)) {
        class(samples) <- c(.file_ext(samples), class(samples))
    }
    UseMethod("sits_get_data", samples)
}

#' @title Get time series using CSV files
#' @name sits_get_data.csv
#'
#' @description Retrieve a set of time series from a data cube and
#' and put the result in a "sits tibble", which
#' contains both the satellite image time series and their metadata.
#' The \code{samples} parameter must point to a file with extension ".csv",
#' with mandatory columns \code{longitude}, \code{latitude}, \code{label},
#' \code{start_date} and  \code{end_date}.
#'
#' @param cube            Data cube from where data is to be retrieved.
#'                        (tibble of class "raster_cube").
#' @param samples         Location of a csv file.
#' @param ...             Specific parameters for each kind of input.
#' @param bands           Bands to be retrieved - optional.
#' @param crs             A character with the samples crs.
#'                        Default is "EPSG:4326".
#' @param impute_fn       Imputation function to remove NA.
#' @param multicores      Number of threads to process the time series
#'                        (integer, with min = 1 and max = 2048).
#' @param progress        Logical: show progress bar?
#'
#' @return A tibble of class "sits" with set of time series and metadata with
#' <longitude, latitude, start_date, end_date, label, time_series>.
#' @examples
#' if (sits_run_examples()) {
#'     # reading a lat/long from a local cube
#'     # create a cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     raster_cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     # reading samples from a cube based on a  CSV file
#'     csv_file <- system.file("extdata/samples/samples_sinop_crop.csv",
#'         package = "sits"
#'     )
#'     points <- sits_get_data(cube = raster_cube, samples = csv_file)
#' }
#' @export
sits_get_data.csv <- function(cube,
                              samples, ...,
                              bands = NULL,
                              crs = "EPSG:4326",
                              impute_fn = impute_linear(),
                              multicores = 2L,
                              progress = FALSE) {
    # Pre-conditions
    bands <- .default(bands, .cube_bands(cube))
    .check_cube_bands(cube, bands = bands)
    .check_crs(crs)
    .check_int_parameter(multicores, min = 1L, max = 2048L)
    progress <- .message_progress(progress)
    .check_function(impute_fn)
    .check_int_parameter(multicores, min = 1)
    progress <- .message_progress(progress)
    # Extract a data frame from csv
    samples <- .csv_get_samples(samples, crs)
    # Extract time series from a cube given a data.frame
    data <- .data_get_ts(
        cube       = cube,
        samples    = samples,
        bands      = bands,
        impute_fn  = impute_fn,
        multicores = multicores,
        progress   = progress
    )
    return(data)
}
#' @title Get time series using shapefiles
#' @name sits_get_data.shp
#'
#' @description Retrieve a set of time series from a data cube and
#' and put the result in a \code{sits tibble}, which
#' contains both the satellite image time series and their metadata.
#' The \code{samples} parameter must point to a file with extension ".shp"
#' which should be a valid shapefile in POINT or POLYGON geometry.
#' If \code{start_date} and \code{end_date} are not informed, the function
#' uses these data from the cube.
#'
#' @param cube            Data cube from where data is to be retrieved.
#'                        (tibble of class "raster_cube").
#' @param samples         The name of a shapefile.
#' @param ...             Specific parameters for specific cases.
#' @param start_date      Start of the interval for the time series - optional
#'                        (Date in "YYYY-MM-DD" format).
#' @param end_date        End of the interval for the time series - optional
#'                        (Date in "YYYY-MM-DD" format).
#' @param bands           Bands to be retrieved - optional
#' @param impute_fn       Imputation function to remove NA.
#' @param label           Label to be assigned to all time series - optional
#' @param label_attr      Attribute in the shapefile to be used
#'                        as a polygon label.
#' @param n_sam_pol       Number of samples per polygon to be read
#'                        for POLYGON or MULTIPOLYGON shapefiles.
#' @param pol_avg         Logical: summarize samples for each polygon?
#' @param sampling_type   Spatial sampling type: random, hexagonal,
#'                        regular, or Fibonacci.
#' @param multicores      Number of threads to process the time series
#'                        (integer, with min = 1 and max = 2048).
#' @param progress        Logical: show progress bar?
#'
#' @return A tibble of class "sits" with set of time series and metadata
#' <longitude, latitude, start_date, end_date, label, time_series>.
#'
#' @note
#' For shapefiles, the following parameters are relevant:
#' \itemize{
#' \item{\code{label}: label to be assigned to the samples.
#' Should only be used if all geometries have a single label.}
#' \item{\code{label_attr}: defines which attribute should be
#' used as a label, required for POINT and POLYGON geometries if
#' \code{label} has not been set.}
#' \item{\code{n_sam_pol}: indicates how many points are
#' extracted from each polygon, required for POLYGON geometry (default = 15).}
#' \item{\code{sampling_type}: defines how sampling is done, required
#' for POLYGON geometry (default = "random").}
#' \item{\code{pol_avg}: indicates if average of values for POLYGON
#' geometry should be computed (default = "FALSE").}
#' }
#' @examples
#' if (sits_run_examples()) {
#'     # reading a shapefile from BDC (Brazil Data Cube)
#'     bdc_cube <- sits_cube(
#'         source = "BDC",
#'         collection = "CBERS-WFI-16D",
#'         bands = c("NDVI", "EVI"),
#'         tiles = c("007004", "007005"),
#'         start_date = "2018-09-01",
#'         end_date = "2018-10-28"
#'     )
#'     # define a shapefile to be read from the cube
#'     shp_file <- system.file("extdata/shapefiles/bdc-test/samples.shp",
#'         package = "sits"
#'     )
#'     # get samples from the BDC based on the shapefile
#'     time_series_bdc <- sits_get_data(
#'         cube = bdc_cube,
#'         samples = shp_file
#'     )
#' }
#'
#' @export
sits_get_data.shp <- function(cube,
                              samples, ...,
                              start_date = NULL,
                              end_date = NULL,
                              bands = NULL,
                              impute_fn = impute_linear(),
                              label = "NoClass",
                              label_attr = NULL,
                              n_sam_pol = 30L,
                              pol_avg = FALSE,
                              sampling_type = "random",
                              multicores = 2L,
                              progress = FALSE) {
    # Set caller for error messages
    .check_set_caller("sits_get_data_shp")
    # Pre-conditions
    bands <- .default(bands, .cube_bands(cube))
    .check_cube_bands(cube, bands = bands)
    .check_function(impute_fn)
    .check_chr_parameter(label, allow_null = TRUE)
    .check_chr_parameter(label_attr, allow_null = TRUE)
    .check_int_parameter(n_sam_pol, min = 1, max = 2048)
    .check_lgl_parameter(pol_avg)
    .check_chr_parameter(sampling_type)
    .check_int_parameter(multicores, min = 1)
    progress <- .message_progress(progress)
    # Get default start and end date
    start_date <- .default(start_date, .cube_start_date(cube))
    end_date <- .default(end_date, .cube_end_date(cube))
    cube <- .cube_filter_interval(
        cube = cube, start_date = start_date, end_date = end_date
    )
    # Extract a data frame from shapefile
    samples <- .shp_get_samples(
        shp_file = samples,
        label = label,
        shp_attr = label_attr,
        start_date = start_date,
        end_date = end_date,
        n_shp_pol = n_sam_pol,
        sampling_type = sampling_type
    )
    # Extract time series from a cube given a data.frame
    data <- .data_get_ts(
        cube       = cube,
        samples    = samples,
        bands      = bands,
        impute_fn  = impute_fn,
        multicores = multicores,
        progress   = progress
    )
    if (pol_avg && "polygon_id" %in% colnames(data)) {
        data <- .data_avg_polygon(data = data)
    }
    return(data)
}

#' @title Get time series using sf objects
#' @name sits_get_data.sf
#'
#' @description Retrieve a set of time series from a data cube and
#' and put the result in a "sits tibble", which
#' contains both the satellite image time series and their metadata.
#' The \code{samples} parameter must be a \code{sf} object
#' in POINT or POLYGON geometry.
#' If \code{start_date} and \code{end_date} are not informed, the function
#' uses these data from the cube.
#'
#' @param cube            Data cube from where data is to be retrieved.
#'                        (tibble of class "raster_cube").
#' @param samples         The name of a shapefile.
#' @param ...             Specific parameters for specific cases.
#' @param start_date      Start of the interval for the time series - optional
#'                        (Date in "YYYY-MM-DD" format).
#' @param end_date        End of the interval for the time series - optional
#'                        (Date in "YYYY-MM-DD" format).
#' @param bands           Bands to be retrieved - optional
#'                        (character vector).
#' @param impute_fn       Imputation function to remove NA.
#' @param label           Label to be assigned to all time series - optional
#' @param label_attr      Attribute in the sf object to be used
#'                        as a polygon label.
#' @param n_sam_pol       Number of samples per polygon to be read
#'                        for POLYGON or MULTIPOLYGON objects.
#' @param pol_avg         Logical: summarize samples for each polygon?
#' @param sampling_type   Spatial sampling type: random, hexagonal,
#'                        regular, or Fibonacci.
#' @param multicores      Number of threads to process the time series
#'                        (integer, with min = 1 and max = 2048).
#' @param progress        Logical: show progress bar?
#'
#' @return A tibble of class "sits" with set of time series
#' <longitude, latitude, start_date, end_date, label>.
#'
#' @note
#' #' For sf objects, the following parameters are relevant:
#' \itemize{
#' \item{\code{label}: label to be assigned to the samples.
#' Should only be used if all geometries have a single label.}
#' \item{\code{label_attr}: defines which attribute should be
#' used as a label, required for POINT and POLYGON geometries if
#' \code{label} has not been set.}
#' \item{\code{n_sam_pol}: indicates how many points are
#' extracted from each polygon, required for POLYGON geometry (default = 15).}
#' \item{\code{sampling_type}: defines how sampling is done, required
#' for POLYGON geometry (default = "random").}
#' \item{\code{pol_avg}: indicates if average of values for POLYGON
#' geometry should be computed (default = "FALSE").}
#' }
#' @examples
#' if (sits_run_examples()) {
#'     # reading a shapefile from BDC (Brazil Data Cube)
#'     bdc_cube <- sits_cube(
#'         source = "BDC",
#'         collection = "CBERS-WFI-16D",
#'         bands = c("NDVI", "EVI"),
#'         tiles = c("007004", "007005"),
#'         start_date = "2018-09-01",
#'         end_date = "2018-10-28"
#'     )
#'     # define a shapefile to be read from the cube
#'     shp_file <- system.file("extdata/shapefiles/bdc-test/samples.shp",
#'         package = "sits"
#'     )
#'     # read a shapefile into an sf object
#'     sf_object <- sf::st_read(shp_file)
#'     # get samples from the BDC using an sf object
#'     time_series_bdc <- sits_get_data(
#'         cube = bdc_cube,
#'         samples = sf_object
#'     )
#' }
#' @export
sits_get_data.sf <- function(cube,
                             samples,
                             ...,
                             start_date = NULL,
                             end_date = NULL,
                             bands = NULL,
                             impute_fn = impute_linear(),
                             label = "NoClass",
                             label_attr = NULL,
                             n_sam_pol = 30L,
                             pol_avg = FALSE,
                             sampling_type = "random",
                             multicores = 2L,
                             progress = FALSE) {
    # Set caller for error messages
    .check_set_caller("sits_get_data_sf")
    # Pre-conditions
    bands <- .default(bands, .cube_bands(cube))
    .check_cube_bands(cube, bands = bands)
    .check_function(impute_fn)
    .check_chr_parameter(label, allow_null = TRUE)
    .check_chr_parameter(label_attr, allow_null = TRUE)
    .check_int_parameter(n_sam_pol, min = 1, max = 2048)
    .check_lgl_parameter(pol_avg)
    .check_chr_parameter(sampling_type)
    .check_int_parameter(multicores, min = 1)
    progress <- .message_progress(progress)
    # Get default start and end date
    start_date <- .default(start_date, .cube_start_date(cube))
    end_date <- .default(end_date, .cube_end_date(cube))
    cube <- .cube_filter_interval(
        cube = cube, start_date = start_date, end_date = end_date
    )
    # Extract a samples data.frame from sf object
    samples <- .sf_get_samples(
        sf_object = samples,
        label = label,
        label_attr = label_attr,
        start_date = start_date,
        end_date = end_date,
        n_sam_pol = n_sam_pol,
        sampling_type = sampling_type
    )
    # Extract time series from a cube given a data.frame
    data <- .data_get_ts(
        cube       = cube,
        samples    = samples,
        bands      = bands,
        impute_fn  = impute_fn,
        multicores = multicores,
        progress   = progress
    )
    # Do we want an average value per polygon?
    if (pol_avg && "polygon_id" %in% colnames(data)) {
        data <- .data_avg_polygon(data = data)
    }
    return(data)
}
#' @title Get time series using sits objects
#' @name sits_get_data.sits
#'
#' @description Retrieve a set of time series from a data cube and
#' and put the result in a \code{sits tibble}. The \code{samples}
#' parameter should be a valid \code{sits tibble} which
#' which contains columns
#' \code{longitude}, \code{latitude},
#' \code{start_date}, \code{end_date} and \code{label} for each sample.
#'
#' @param cube            Data cube from where data is to be retrieved.
#'                        (tibble of class "raster_cube").
#' @param samples         Location of the samples to be retrieved.
#'                        Either a tibble of class "sits", an "sf" object,
#'                        the name of a shapefile or csv file, or
#'                        a data.frame with columns "longitude" and "latitude".
#' @param ...             Specific parameters for specific cases.
#' @param bands           Bands to be retrieved - optional.
#' @param crs             A character with the samples crs.
#'                        Default is "EPSG:4326".
#' @param impute_fn       Imputation function to remove NA.
#' @param multicores      Number of threads to process the time series
#'                        (integer, with min = 1 and max = 2048).
#' @param progress        Logical: show progress bar?
#'
#' @return A tibble of class "sits" with set of time series
#' <longitude, latitude, start_date, end_date, label>.
#'
#' @export
sits_get_data.sits <- function(cube,
                               samples,
                               ...,
                               bands = NULL,
                               crs = "EPSG:4326",
                               impute_fn = impute_linear(),
                               multicores = 2L,
                               progress = FALSE) {
    # Set caller for error messages
    .check_set_caller("sits_get_data")
    # Pre-conditions
    bands <- .default(bands, .cube_bands(cube))
    .check_cube_bands(cube, bands = bands)
    .check_crs(crs)
    .check_function(impute_fn)
    .check_int_parameter(multicores, min = 1)
    progress <- .message_progress(progress)
    # Convert to WGS84
    if (!.is_crs_wgs84(crs)) {
        samples <- .samples_transform(
            samples = samples, crs = crs, as_crs = "EPSG:4326"
        )
    }
    # Extract time series from a cube given a data.frame
    data <- .data_get_ts(
        cube       = cube,
        samples    = samples,
        bands      = bands,
        impute_fn  = impute_fn,
        multicores = multicores,
        progress   = progress
    )
    return(data)
}
#' @title Get time series using sits objects
#' @name sits_get_data.data.frame
#'
#' @description Retrieve a set of time series from a data cube and
#' and put the result in a \code{sits tibble}. The \code{samples}
#' parameter should be a \code{data.frame} which
#' which contains mandatory columns
#' \code{longitude} and \code{latitude}, and optional columns
#' \code{start_date}, \code{end_date} and \code{label} for each sample.
#'
#' @param cube            Data cube from where data is to be retrieved.
#'                        (tibble of class "raster_cube").
#' @param samples         A data.frame with mandatory columns \code{longitude},
#'                        and \code{latitude}, and optional columns
#'                        \code{start_date}, \code{end_date}, \code{label}.
#' @param ...             Specific parameters for specific cases.
#' @param start_date      Start of the interval for the time series - optional
#'                        (Date in "YYYY-MM-DD" format).
#' @param end_date        End of the interval for the time series - optional
#'                        (Date in "YYYY-MM-DD" format).
#' @param bands           Bands to be retrieved - optional.
#' @param label           Label to be assigned to all time series if
#'                        column \code{label} is not provided in the
#'                        data.frame.
#' @param crs             A character with the samples crs.
#'                        Default is "EPSG:4326".
#' @param impute_fn       Imputation function to remove NA.
#' @param multicores      Number of threads to process the time series
#'                        (integer, with min = 1 and max = 2048).
#' @param progress        Logical: show progress bar?
#'
#' @return A tibble of class "sits" with set of time series
#' <longitude, latitude, start_date, end_date, label>.
#' @examples
#' if (sits_run_examples()) {
#'     # create a cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     raster_cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     # read a lat/long from a local cube
#'     samples <- data.frame(longitude = -55.66738, latitude = -11.76990)
#'     point_ndvi <- sits_get_data(raster_cube, samples)
#' }
#' @export
#'
sits_get_data.data.frame <- function(cube,
                                     samples,
                                     ...,
                                     start_date = NULL,
                                     end_date = NULL,
                                     bands = NULL,
                                     impute_fn = impute_linear(),
                                     label = "NoClass",
                                     crs = "EPSG:4326",
                                     multicores = 2,
                                     progress = FALSE) {
    # Set caller for error messages
    .check_set_caller("sits_get_data_data_frame")
    # Pre-conditions
    bands <- .default(bands, .cube_bands(cube))
    .check_cube_bands(cube, bands = bands)
    .check_function(impute_fn)
    .check_chr_parameter(label, allow_null = TRUE)
    .check_crs(crs)
    .check_int_parameter(multicores, min = 1)
    # Check if samples contains all the required columns
    .check_chr_contains(
        x = colnames(samples),
        contains = c("latitude", "longitude"),
        discriminator = "all_of"
    )
    progress <- .message_progress(progress)
    # Get default start and end date
    start_date <- .default(start_date, .cube_start_date(cube))
    end_date <- .default(end_date, .cube_end_date(cube))
    cube <- .cube_filter_interval(
        cube = cube, start_date = start_date, end_date = end_date
    )
    # Fill missing columns
    if (!.has_column(samples, "label")) {
        samples[["label"]] <- label
    }
    if (!.has_column(samples, "start_date")) {
        samples[["start_date"]] <- start_date
    }
    if (!.has_column(samples, "end_date")) {
        samples[["end_date"]] <- end_date
    }
    # Set samples class
    samples <- .set_class(samples, c("sits", class(samples)))
    # Convert to WGS84
    if (!.is_crs_wgs84(crs)) {
        samples <- .samples_transform(
            samples = samples, crs = crs, as_crs = "EPSG:4326"
        )
    }
    # Extract time series from a cube given a data.frame
    data <- .data_get_ts(
        cube       = cube,
        samples    = samples,
        bands      = bands,
        impute_fn  = impute_fn,
        multicores = multicores,
        progress   = progress
    )
    return(data)
}

#' @rdname sits_get_data
#'
#' @export
sits_get_data.default <- function(cube, samples, ...) {
    stop(.conf("messages", "sits_get_data_default"))
}
