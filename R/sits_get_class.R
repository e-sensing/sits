#' @title Get values from classified maps
#' @name sits_get_class
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Given a set of lat/long locations and a classified cube,
#' retrieve the class of each point.
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
#' @param cube            Classified data cube from where data is to be retrieved.
#'                        (class "class_cube").
#' @param samples         Location of the samples to be retrieved.
#'                        Either a tibble of class "sits", an "sf" object,
#'                        the name of a shapefile or csv file, or
#'                        a data.frame with columns "longitude" and "latitude"
#' @return                A tibble of with columns
#'                        <longitude, latitude, start_date, end_date, label>.
#' @export
sits_get_class <- function(cube, samples){
    .check_set_caller("sits_get_data")
    # Pre-conditions
    .check_is_class_cube(cube)
    .check_raster_cube_files(cube)
    if (is.character(samples)) {
        class(samples) <- c(.file_ext(samples), class(samples))
    }
    UseMethod("sits_get_class", samples)
}
#' @rdname sits_get_class
#'
#' @export
sits_get_class.default <- function(cube, samples){
    stop(.conf("messages", "sits_get_class_default"))
}
#' @rdname sits_get_class
#'
#' @export
sits_get_class.csv <- function(cube, samples){
    # Extract a data frame from csv
    samples <- .csv_get_lat_lon(samples)
    data <- .data_get_class(
        cube       = cube,
        samples    = samples
    )
    return(data)
}
#' @rdname sits_get_class
#' @export
sits_get_class.shp <- function(cube, samples){
    .check_set_caller("sits_get_class")
    # transform from shapefile to sf
    sf_shape <- .shp_transform_to_sf(shp_file = samples)
    # Get the geometry type
    geom_type <- as.character(sf::st_geometry_type(sf_shape)[[1]])
    if (!geom_type == "POINT")
        stop(.conf("messages", "sits_get_class_not_point"))

    # Get a tibble with points
    samples <- .sf_point_to_latlong(sf_object  = sf_shape)
    # get the data
    data <- .data_get_class(
        cube       = cube,
        samples    = samples
    )
    return(data)
}
#' @rdname sits_get_class
#' @export
sits_get_class.sf <- function(cube, samples){
    .check_set_caller("sits_get_class")
    # Get the geometry type
    geom_type <- as.character(sf::st_geometry_type(samples)[[1]])
    if (!geom_type == "POINT")
        stop(.conf("messages", "sits_get_class_not_point"))

    # Get a tibble with points
    samples <- .sf_point_to_latlong(sf_object  = samples)
    # get the data
    data <- .data_get_class(
        cube       = cube,
        samples    = samples
    )
    return(data)
}
#' @rdname sits_get_class
#' @export
sits_get_class.sits <- function(cube, samples){
    .check_set_caller("sits_get_class")
    # get the data
    data <- .data_get_class(
        cube       = cube,
        samples    = samples
    )
    return(data)
}
#' @rdname sits_get_class
#' @export
sits_get_class.data.frame <- function(cube, samples){
    .check_set_caller("sits_get_class")
    # get the data
    data <- .data_get_class(
        cube       = cube,
        samples    = samples
    )
    return(data)
}
