#' @title Obtain timeSeries from WTSS server, based on a SHP file.
#' @name .sits_from_shp
#'
#' @description reads a shapefile and retrieves a sits tibble
#' containing time series from a data cube that are inside the SHP file.
#' The script uses the WTSS service, taking information about spatial and
#' temporal resolution from the WTSS configuration.
#'
#' @param shp_file        SHP file which provides the boundaries of a region.
#' @param cube            Data cube metadata.
#' @param start_date      Start date of the period.
#' @param end_date        End date of the period.
#' @param bands           Names of the bands to be retrieved.
#' @param label           Label to attach to the time series.
#' @param shp_attr        Attribute in the shapefile used as a polygon label
#' @param .n_shp_pol      Number of samples per polygon to be read
#'                        (for POLYGON or MULTIPOLYGON shapes)
#' @param .n_shp_pts      Number of points to be read (for POINT shapes)
#' @param .prefilter      Filtering for SATVEG cube
#'                        ("0" - none, "1" - no data correction,
#'                        "2" - cloud correction,
#'                        "3" - no data and cloud correction).
#' @return                A sits tibble.
.sits_from_shp <- function(shp_file,
                           cube,
                           start_date,
                           end_date,
                           bands,
                           label,
                           shp_attr,
                           .n_shp_pol,
                           .n_shp_pts,
                           .prefilter) {

    # pre-condition - does the shapefile exist?
    assertthat::assert_that(file.exists(shp_file),
                            msg = "sits_from_shp: shapefile does not exist")
    # pre-condition - is the default label valid?
    assertthat::assert_that(!purrr::is_null(label) || !purrr::is_null(shp_attr),
        msg = "sits_from_shp: default label or shape attribute should be valid")

    # read the shapefile
    sf_shape <- sf::read_sf(shp_file)
    n_rows_shp <- nrow(sf_shape)
    # pre-condition - is the default label valid?
    assertthat::assert_that(n_rows_shp > 0,
            msg = "sits_from_shp: shapefile has no content")

    # get the geometry type
    geom_type <-  sf::st_geometry_type(sf_shape)[1]
    # get the data frame associated to the shapefile
    shp_df <- sf::st_drop_geometry(sf_shape)

    # are all geometries compatible?
    assertthat::assert_that(all(sf::st_geometry_type(sf_shape) == geom_type),
           msg = "sits_from_shp: shapefile has different geometries")

    # can the function deal with the geometry_type?
    assertthat::assert_that(
                    geom_type %in% c("POINT", "POLYGON", "MULTIPOLYGON"),
                    msg = "sits_from_shp: only handles
                            POINT, POLYGON or MULTIPOLYGON shapefiles")

    # is the shape attribute valid?
    if (!purrr::is_null(shp_attr))
        assertthat::assert_that(
            length(as.character(unname(shp_df[1, (shp_attr)]))) > 0,
            msg = "sits_from_shp: invalid shapefile attribute")

    # if the shapefile is not in planar coordinates, convert it
    sf_shape <- suppressWarnings(sf::st_transform(sf_shape, crs = 4326))
    # create an empty sits tibble
    shape.tb <- .sits_tibble()

    # if geom_type is POINT, use the points provided in the shapefile
    if (geom_type == "POINT") {
        points.mx <- sf::st_coordinates(sf_shape$geometry)
        if (!purrr::is_null(shp_attr)) {
            l1.lst     <- as.list(shp_df[,shp_attr])
            labels.vec <- as.vector(l1.lst[[1]])
        }

        # reduce the number of points to be read
        if (nrow(points.mx) > .n_shp_pts) {
            points.mx <- points.mx[1:.n_shp_pts,]
            if (!purrr::is_null(shp_attr))
                labels.vec <- labels[1:.n_shp_pts]
        }

        # read the points into a list
        nrows <- nrow(points.mx)
        for (i in 1:nrows) {
            row <- .sits_ts_from_cube(cube = cube,
                                      longitude   = points.mx[i,1],
                                      latitude    = points.mx[i,2],
                                      start_date  = start_date,
                                      end_date    = end_date,
                                      bands       = bands,
                                      label       = label,
                                      .prefilter  = .prefilter)
            shape.tb <- dplyr::bind_rows(shape.tb, row)
        }

        if (!purrr::is_null(shp_attr))
            shape.tb$label <- labels.vec
    }
    # if geom_type is not POINT, we have to sample each polygong
    else {
        for (i in 1:n_rows_shp) {
            # retrieve the class from the shape attribute
            if (!purrr::is_null(shp_attr))
                label <-  as.character(unname(shp_df[i, shp_attr]))
            # obtain a set of samples based on polygons
            points.lst <- list(sf::st_sample(sf_shape[i,], size = .n_shp_pol))
            # get one time series per sample
            rows.lst <- points.lst %>%
                purrr::pmap(function(p) {
                    pll <- sf::st_geometry(p)[[1]]
                    row <- .sits_ts_from_cube(cube = cube,
                                              longitude = pll[1],
                                              latitude = pll[2],
                                              start_date = start_date,
                                              end_date = end_date,
                                              bands = bands,
                                              label = label,
                                              .prefilter = .prefilter)
                    return(row)
                })
            # combine rows to make SITS tibble
            shape.tb <- dplyr::bind_rows(shape.tb, rows.lst)
        }
    }
    return(shape.tb)
}
