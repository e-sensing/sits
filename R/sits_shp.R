#' @title Obtain a tibble with lat/long points to be retrieved from a SHP
#' @name .sits_points_from_shp
#' @keywords internal
#'
#' @description reads a shapefile and retrieves a sits tibble
#' containing a set of lat/long points for data retrieval
#'
#' @param sf_shape        sf object that contains a SHP file
#' @param shp_attr        Attribute in the shapefile used as a polygon label
#' @param label           Label to be assigned to points
#' @param .n_shp_pol      Number of samples per polygon to be read
#'                        (for POLYGON or MULTIPOLYGON shapes)
#' @return                A sits tibble with points to to be read.
.sits_points_from_shp <- function(sf_shape,
                                  shp_attr,
                                  label,
                                  .n_shp_pol) {

    # get the geometry type
    geom_type <- sf::st_geometry_type(sf_shape)[1]

    # if the shapefile is not in planar coordinates, convert it
    sf_shape <- suppressWarnings(sf::st_transform(sf_shape, crs = 4326))

    # get a tibble with points and labels
    if (geom_type == "POINT") {
        points.tb <- .sits_from_point_shp(sf_shape,
                                          shp_attr,
                                          label)
    }
    else {
          points.tb <- .sits_from_polygon_shp(sf_shape,
                                              shp_attr,
                                              label,
                                              .n_shp_pol)
      }

    return(points.tb)
}

#' @title Obtain a tibble with latitude and longitude points from POINT geometry
#' @name .sits_from_point_shp
#' @keywords internal
#'
#' @param sf_shape        sf object linked to a shapefile.
#' @param shp_attr        Attribute in the shapefile used as a polygon label
#' @param label           Label to be assigned to points
#'
.sits_from_point_shp <- function(sf_shape, shp_attr, label) {

    # get the db file
    shp_df <- sf::st_drop_geometry(sf_shape)

    # if geom_type is POINT, use the points provided in the shapefile
    points <- sf::st_coordinates(sf_shape$geometry)
    if (!purrr::is_null(shp_attr)) {
        l1_lst <- as.list(shp_df[, shp_attr])
        labels <- as.vector(l1_lst[[1]])
    }
    else {
        labels <- rep(label, times = nrow(points))
    }
    # build a tibble with lat/long and label
    points.tb <- tibble::tibble(
        longitude = points[, 1],
        latitude = points[, 2],
        label = labels
    )

    return(points.tb)
}

#' @title Obtain a tibble from POLYGON geometry
#' @name .sits_from_polygon_shp
#' @keywords internal
#'
#' @param sf_shape        sf object linked to a shapefile
#' @param shp_attr        Attribute in the shapefile used as a polygon label
#' @param label           Label to be assigned to points
#' @param .n_shp_pol      Number of samples per polygon to be read
#'
#'
.sits_from_polygon_shp <- function(sf_shape, shp_attr, label, .n_shp_pol) {

    # get the db file
    shp_df <- sf::st_drop_geometry(sf_shape)

    points_lst <- seq_len(nrow(sf_shape)) %>%
      purrr::map(function(i){
        # retrieve the class from the shape attribute
        if (!purrr::is_null(shp_attr)) {
          label <- as.character(unname(shp_df[i, shp_attr]))
        }
        # obtain a set of samples based on polygons
        points <- list(sf::st_sample(sf_shape[i, ], size = .n_shp_pol))
        # get one time series per sample
        rows <- points %>%
          purrr::pmap(function(p) {
            pll <- sf::st_geometry(p)[[1]]
            row <- tibble::tibble(
              longitude = pll[1],
              latitude = pll[2],
              label = label
            )
            return(row)
          })
        # combine rows to make SITS tibble
        pts <- dplyr::bind_rows(rows)
        return(pts)
      })
    # join all the points for all shapefiles
    points_tb <- dplyr::bind_rows(points_lst)
    return(points_tb)
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

    # pre-condition - does the shapefile exist?
    assertthat::assert_that(file.exists(shp_file),
        msg = "sits_from_shp: shapefile does not exist"
    )

    # read the shapefile
    sf_shape <- sf::read_sf(shp_file)
    # pre-condition - is the default label valid?
    assertthat::assert_that(nrow(sf_shape) > 0,
        msg = "sits_from_shp: shapefile has no content"
    )

    # get the geometry type
    geom_type <- sf::st_geometry_type(sf_shape)[1]
    # get the data frame associated to the shapefile

    # precondition - are all geometries compatible?
    assertthat::assert_that(all(sf::st_geometry_type(sf_shape) == geom_type),
        msg = "sits_from_shp: shapefile has different geometries"
    )

    # precondition - can the function deal with the geometry_type?
    assertthat::assert_that(
        geom_type %in% c("POINT", "POLYGON", "MULTIPOLYGON"),
        msg = "sits_from_shp: only handles
                            POINT, POLYGON or MULTIPOLYGON shapefiles"
    )

    # precondition - is the default label valid?
    assertthat::assert_that(!purrr::is_null(label) || !purrr::is_null(shp_attr),
        msg = "sits_from_shp: label or shape attribute should be valid"
    )


    # precondition - is the shape attribute valid?
    # get the data frame associated to the shapefile
    shp_df <- sf::st_drop_geometry(sf_shape)
    if (!purrr::is_null(shp_attr)) {
          assertthat::assert_that(
              length(as.character(unname(shp_df[1, (shp_attr)]))) > 0,
              msg = "sits_from_shp: invalid shapefile attribute"
          )
      }


    return(sf_shape)
}
