#' @title Transform a shapefile into a samples file
#' @name .shp_get_samples
#' @author Gilberto Camara
#' @keywords internal
#' @noRd
#' @param shp_file        Shapefile that describes the data to be retrieved.
#' @param label           Default label for samples.
#' @param shp_attr        Shapefile attribute that describes the label.
#' @param start_date      Start date for the data set.
#' @param end_date        End date for the data set.
#' @param n_shp_pol       Number of samples per polygon to be read.
#' @param shp_id          ID attribute which contains the label
#'                        (for POLYGON or MULTIPOLYGON shapefile).
#' @param sampling_type   Spatial sampling type: random, hexagonal,
#'                        regular, or Fibonacci.
#' @return                A sits tibble with samples to be retrieved.
#'
.shp_get_samples <- function(shp_file,
                             label,
                             shp_attr,
                             start_date,
                             end_date,
                             n_shp_pol,
                             shp_id,
                             sampling_type) {
    # set caller to show in errors
    .check_set_caller(".shp_get_samples")
    # Pre-condition - check the shape file and its attribute
    .check_that(.has(label) || .has(shp_attr))
    sf_shape <- .shp_transform_to_sf(
        shp_file = shp_file,
        shp_attr = shp_attr,
        label = label
    )
    # Get the points to be read
    samples <- .sf_to_tibble(
        sf_object  = sf_shape,
        label_attr = shp_attr,
        label      = label,
        n_sam_pol  = n_shp_pol,
        pol_id     = shp_id,
        sampling_type = sampling_type,
        start_date = start_date,
        end_date   = end_date
    )

    class(samples) <- c("sits", class(samples))

    return(samples)
}

#' @title Check the validity of the shape file and return an sf object
#' @name .shp_transform_to_sf
#' @keywords internal
#' @noRd
#'
#' @param shp_file        SHP file - boundaries of a region.
#' @param shp_attr        attribute  that contains the label
#' @param label           Label to be used instead of shp_attr
#'
#' @return A valid sf object of POINT or POLYGON geometry.
.shp_transform_to_sf <- function(shp_file, shp_attr = NULL, label = NULL) {
    # set caller to show in errors
    .check_set_caller(".shp_transform_to_sf")
    # precondition - does the shapefile exist?
    .check_file(shp_file, extensions = "shp")

    # read the shapefile
    sf_shape <- sf::read_sf(shp_file)
    # postcondition - is the shape file valid?
    .check_that(nrow(sf_shape) > 0)

    # get the geometry type
    geom_type <- sf::st_geometry_type(sf_shape)[1]

    # postcondition - are all geometries compatible?
    .check_that(all(sf::st_geometry_type(sf_shape) == geom_type))
    # postcondition - can the function deal with the geometry_type?
    .check_that(as.character(geom_type) %in%  .conf("sf_geom_types_supported"))
    # postcondition - is the shape attribute valid?
    .check_shp_attribute(sf_shape, shp_attr)

    return(sf_shape)
}
