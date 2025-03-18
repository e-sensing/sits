#' @title Get values from probability maps
#' @name sits_get_probs
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Given a set of lat/long locations and a probability cube,
#' retrieve the prob values of each point.
#' @note
#' There are four ways of specifying data to be retrieved using the
#' \code{samples} parameter:
#' (a) CSV file: a CSV file with columns \code{longitude}, \code{latitude};
#' (b) SHP file: a shapefile in POINT geometry;
#' (c) sits object: A sits tibble;
#' (d) sf object: An \code{link[sf]{sf}} object with POINT or geometry;
#' (e) data.frame: A data.frame with \code{longitude} and \code{latitude}.
#'
#'
#' @param cube            Probability data cube.
#' @param samples         Location of the samples to be retrieved.
#'                        Either a tibble of class "sits", an "sf" object,
#'                        the name of a shapefile or csv file, or
#'                        a data.frame with columns "longitude" and "latitude"
#' @param window_size     Size of window around pixel (optional)
#' @return                A tibble of with columns
#'                        <longitude, latitude, values> in case no windows
#'                        are requested and <longitude, latitude, neighbors>
#'                        in case windows are requested
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
