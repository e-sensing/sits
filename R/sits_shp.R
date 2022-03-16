#' @title Obtain a tibble with lat/long points to be retrieved from a SHP
#' @name .sits_shp_to_tibble
#' @keywords internal
#'
#' @description reads a shapefile and retrieves a sits tibble
#' containing a set of lat/long points for data retrieval
#'
#' @param sf_shape        sf object that contains a SHP file.
#' @param shp_attr        Attribute in the shapefile used as a polygon label.
#' @param label           Label to be assigned to points.
#' @param .n_shp_pol      Number of samples per polygon to be read
#'                        (for POLYGON or MULTIPOLYGON shapes).
#' @return                A sits tibble with points to to be read.
.sits_shp_to_tibble <- function(sf_shape,
                                shp_attr,
                                label,
                                .n_shp_pol) {

    # get the geometry type
    geom_type <- sf::st_geometry_type(sf_shape)[1]

    # if the shapefile is not in planar coordinates, convert it
    sf_shape <- suppressWarnings(sf::st_transform(sf_shape, crs = 4326))

    # get a tibble with points and labels
    if (geom_type == "POINT") {
        points_tbl <- .sits_shp_point_to_tibble(sf_shape,
                                                shp_attr,
                                                label)
    } else {
        points_tbl <- .sits_shp_polygon_to_tibble(sf_shape,
                                                  shp_attr,
                                                  label,
                                                  .n_shp_pol)
    }

    return(points_tbl)
}

#' @title Obtain a tibble with latitude and longitude points from POINT geometry
#' @name .sits_shp_point_to_tibble
#' @keywords internal
#'
#' @param sf_shape        sf object linked to a shapefile.
#' @param shp_attr        Attribute in the shapefile used as a polygon label
#' @param label           Label to be assigned to points
#'
.sits_shp_point_to_tibble <- function(sf_shape, shp_attr, label) {

    # get the db file
    shp_df <- sf::st_drop_geometry(sf_shape)

    # if geom_type is POINT, use the points provided in the shapefile
    points <- sf::st_coordinates(sf_shape$geometry)
    if (!purrr::is_null(shp_attr)) {
        l1_lst <- as.list(shp_df[, shp_attr])
        labels <- as.vector(l1_lst[[1]])
    } else {
        labels <- rep(label, times = nrow(points))
    }
    # build a tibble with lat/long and label
    points_tbl <- tibble::tibble(
        longitude = points[, 1],
        latitude = points[, 2],
        label = labels
    )

    return(points_tbl)
}

#' @title Obtain a tibble from POLYGON geometry
#' @name .sits_shp_polygon_to_tibble
#' @keywords internal
#'
#' @param sf_shape        sf object linked to a shapefile
#' @param shp_attr        Attribute in the shapefile used as a polygon label
#' @param label           Label to be assigned to points
#' @param .n_shp_pol      Number of samples per polygon to be read
#'
#'
.sits_shp_polygon_to_tibble <- function(sf_shape, shp_attr, label, .n_shp_pol) {

    # get the db file
    shp_df <- sf::st_drop_geometry(sf_shape)

    points.tb <- seq_len(nrow(sf_shape)) %>%
        purrr::map_dfr(function(i) {
            # retrieve the class from the shape attribute
            if (!purrr::is_null(shp_attr)) {
                label <- unname(as.character(shp_df[i, shp_attr]))
            }
            # obtain a set of samples based on polygons
            points <- list(sf::st_sample(sf_shape[i, ], size = .n_shp_pol))
            # get one time series per sample
            pts.tb <- points %>%
                purrr::pmap_dfr(function(p) {
                    pll <- sf::st_geometry(p)[[1]]
                    row <- tibble::tibble(
                        longitude = pll[1],
                        latitude = pll[2],
                        label = label
                    )
                    return(row)
                })
            return(pts.tb)
        })
    return(points.tb)
}
#' @title Check the validity of the shape file
#' @name .sits_shp_check_validity
#' @keywords internal
#'
#' @param shp_file        SHP file which provides the boundaries of a region.
#' @param shp_attr        attribute in the shapefile that contains the label
#' @param label           Label to be used instead of shp_attr
#'

.sits_shp_check_validity <- function(shp_file, shp_attr = NULL, label = NULL) {

    # set caller to show in errors
    .check_set_caller(".sits_shp_check_validity")
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
        within = .config_get("sf_geom_types_supported"),
        discriminator = "one_of",
        msg = paste0(
            "only handles shapefiles of types",
            .config_get("sf_geom_types_supported")
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
