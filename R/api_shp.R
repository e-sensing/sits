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
#' @param .n_shp_pol      Number of samples per polygon to be read.
#' @param .shp_id         ID attribute for polygons shapefile.
#'                        (for POLYGON or MULTIPOLYGON shapefile).
#' @return                A tibble with samples to be retrieved.
#'
.shp_get_samples <- function(shp_file,
                             label,
                             shp_attr,
                             start_date,
                             end_date,
                             n_shp_pol,
                             shp_id) {

    # pre-condition - check the shape file and its attribute
    sf_shape <- .shp_check_validity(
        shp_file = shp_file,
        shp_attr = shp_attr,
        label = label
    )
    # get the points to be read
    samples <- .sf_to_tibble(
        sf_object   = sf_shape,
        label_attr  = shp_attr,
        label       = label,
        n_sam_pol   = n_shp_pol,
        pol_id      = shp_id
    )

    samples <- dplyr::mutate(samples,
                             start_date = as.Date(start_date),
                             end_date = as.Date(end_date)
    )

    class(samples) <- c("sits", class(samples))

    return(samples)
}

#' @title Check the validity of the shape file and return an sf object
#' @name .shp_check_validity
#' @keywords internal
#' @noRd
#'
#' @param shp_file        SHP file - boundaries of a region.
#' @param shp_attr        attribute  that contains the label
#' @param label           Label to be used instead of shp_attr
#'
#' @return A valid sf object of POINT or POLYGON geometry.
.shp_check_validity <- function(shp_file, shp_attr = NULL, label = NULL) {

    # pre-condition - does the shapefile exist?
    .check_file(x = shp_file, msg = "shapefile does not exist")
    # read the shapefile
    sf_shape <- sf::read_sf(shp_file)

    # pre-condition - is the default label valid?
    .check_that(
        x = nrow(sf_shape) > 0,
        msg = "shapefile has no content"
    )
    # get the geometry type
    geom_type <- sf::st_geometry_type(sf_shape)[1]

    # precondition - are all geometries compatible?
    .check_that(
        x = all(sf::st_geometry_type(sf_shape) == geom_type),
        msg = "shapefile has different geometries"
    )
    # precondition - can the function deal with the geometry_type?
    .check_chr_within(
        x = as.character(geom_type),
        within = .conf("sf_geom_types_supported"),
        discriminator = "one_of",
        msg = paste0(
            "only handles shapefiles of types",
            .conf("sf_geom_types_supported")
        )
    )
    # precondition - is the default label valid?
    .check_that(
        x = !purrr::is_null(label) || !purrr::is_null(shp_attr),
        msg = "label or shape attribute should be valid"
    )

    # precondition - is the shape attribute valid?
    # get the data frame associated to the shapefile
    shp_df <- sf::st_drop_geometry(sf_shape)
    if (!purrr::is_null(shp_attr)) {
        .check_that(
            x = length(as.character(shp_df[1, (shp_attr)])) > 0,
            msg = "invalid shapefile attribute"
        )
    }
    return(sf_shape)
}
