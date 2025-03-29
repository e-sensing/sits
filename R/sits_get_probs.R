#' @title Get values from probability maps
#' @name sits_get_probs
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Given a set of lat/long locations and a probability cube,
#' retrieve the prob values of each point. This function is useful
#' to estimate probability distributions and to assess the differences
#' between classifiers.
#'
#' @note
#' There are four ways of specifying data to be retrieved using the
#' \code{samples} parameter:
#' \itemize{
#' \item{CSV: a CSV file with columns \code{longitude}, \code{latitude}.}
#' \item{SHP: a shapefile in POINT geometry.}
#' \item{sf object:  An \code{link[sf]{sf}} object with POINT geometry.}
#' \item{sits object: A valid tibble with \code{sits} timeseries.}
#' \item{data.frame: A data.frame with \code{longitude} and \code{latitude}.}
#' }
#'
#' @param cube            Probability data cube.
#' @param samples         Location of the samples to be retrieved.
#'                        Either a tibble of class "sits",
#'                        an "sf" object with POINT geometry,
#'                        the location of a POINT shapefile,
#'                        the location of csv file  with columns
#'                        "longitude" and "latitude", or
#'                        a data.frame with columns "longitude" and "latitude"
#' @param window_size     Size of window around pixel (optional)
#' @return                A tibble of with columns
#'                        <longitude, latitude, values> in case no windows
#'                        are requested and <longitude, latitude, neighbors>
#'                        in case windows are requested
#' @examples
#' if (sits_run_examples()) {
#'     # create a random forest model
#'     rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     # classify a data cube
#'     probs_cube <- sits_classify(
#'         data = cube, ml_model = rfor_model, output_dir = tempdir()
#'     )
#'     # obtain the a set of points for sampling
#'     ground_truth <- system.file("extdata/samples/samples_sinop_crop.csv",
#'         package = "sits"
#'     )
#'     # get the classification values for a selected set of locations
#'     probs_samples <- sits_get_probs(probs_cube, ground_truth)
#' }
#'
#' @export
sits_get_probs <- function(cube, samples, window_size = NULL){
    .check_set_caller("sits_get_probs")
    # Pre-conditions
    .check_is_probs_cube(cube)
    .check_raster_cube_files(cube)
    if (is.character(samples)) {
        class(samples) <- c(.file_ext(samples), class(samples))
    }
    UseMethod("sits_get_probs", samples)
}
#' @rdname sits_get_probs
#'
#' @export
sits_get_probs.csv <- function(cube, samples, window_size = NULL){
    # Extract a data frame from csv
    samples <- .csv_get_lat_lon(samples)
    # get the data
    data <- .data_get_probs(
        cube       = cube,
        samples    = samples,
        window_size = window_size
    )
    return(data)
}
#' @rdname sits_get_probs
#' @export
sits_get_probs.shp <- function(cube, samples, window_size = NULL){
    .check_set_caller("sits_get_probs")
    # transform from shapefile to sf
    sf_shape <- .shp_transform_to_sf(shp_file = samples)
    # Get the geometry type
    geom_type <- as.character(sf::st_geometry_type(sf_shape)[[1]])
    if (!geom_type == "POINT")
        stop(.conf("messages", "sits_get_probs_not_point"))

    # Get a tibble with points
    samples <- .sf_point_to_latlong(sf_object  = sf_shape)
    # get the data
    data <- .data_get_probs(
        cube       = cube,
        samples    = samples,
        window_size = window_size
    )
    return(data)
}
#' @rdname sits_get_probs
#' @export
sits_get_probs.sf <- function(cube, samples, window_size = NULL){
    .check_set_caller("sits_get_probs")
    # Get the geometry type
    geom_type <- as.character(sf::st_geometry_type(samples)[[1]])
    if (!geom_type == "POINT")
        stop(.conf("messages", "sits_get_probs_not_point"))

    # Get a tibble with points
    samples <- .sf_point_to_latlong(sf_object  = samples)
    # get the data
    data <- .data_get_probs(
        cube       = cube,
        samples    = samples,
        window_size = window_size
    )
    return(data)
}
#' @rdname sits_get_probs
#' @export
sits_get_probs.sits <- function(cube, samples, window_size = NULL){
    .check_set_caller("sits_get_probs")
    # get the data
    data <- .data_get_probs(
        cube       = cube,
        samples    = samples,
        window_size = window_size
    )
    return(data)
}
#' @rdname sits_get_probs
#' @export
sits_get_probs.data.frame <- function(cube, samples, window_size = NULL){
    .check_set_caller("sits_get_probs")
    # get the data
    data <- .data_get_probs(
        cube       = cube,
        samples    = samples,
        window_size = window_size
    )
    return(data)
}
#' @rdname sits_get_probs
#'
#' @export
sits_get_probs.default <- function(cube, samples, window_size = NULL){
    stop(.conf("messages", "sits_get_probs"))
}
