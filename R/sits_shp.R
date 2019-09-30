#' @title Obtain timeSeries from WTSS server, based on a SHP file.
#' @name .sits_from_shp
#'
#' @description reads a shapefile and retrieves a sits tibble
#' containing time series from a data cube that are inside the SHP file.
#' The script uses the WTSS service, taking information about spatial and
#' temporal resolution from the WTSS configuration.
#'
#' @param shp_file        Name of a SHP file which provides the boundaries of a region of interest.
#' @param cube            Data cube metadata.
#' @param start_date      The start date of the period.
#' @param end_date        The end date of the period.
#' @param bands           A string vector with the names of the bands to be retrieved.
#' @param label           A string with the label to attach to the time series.
#' @param shp_attr        An optional string to indicate the attribute in the shapefile to be used as a polygon label
#' @param .n_shp_pol      Number of samples per polygon to be read (for POLYGON or MULTIPOLYGON shapes)
#' @param .n_shp_pts      Number of points to be read (for POINT shapes)
#' @param .prefilter      Filtering for SATVEG cube ("0" - none, "1" - no data correction, "2" - cloud correction, "3" - no data and cloud correction).
#' @return A sits tibble.
.sits_from_shp <- function(shp_file, cube, start_date, end_date, bands,
                           label, shp_attr, .n_shp_pol, .n_shp_pts, .prefilter) {

    # pre-condition - does the shapefile exist?
    ensurer::ensure_that(shp_file, file.exists(.),
                         err_desc = "sits_from_shp: shapefile does not exist")
    # pre-condition - is the default label valid?
    ensurer::ensure_that(label, !purrr::is_null(.) || !purrr::is_null(shp_attr) ,
         err_desc = "sits_from_shp: default label or shape attribute should be valid")

    # read the shapefile
    sf_shape <- sf::read_sf(shp_file)
    n_rows_shp <- nrow(sf_shape)
    # pre-condition - is the default label valid?
    ensurer::ensure_that(n_rows_shp, (.) > 0,
            err_desc = "sits_from_shp: shapefile has no content")

    # get the geometry type
    geom_type <-  sf::st_geometry_type(sf_shape)[1]
    # get the data frame associated to the shapefile
    shp_df <- sf::st_drop_geometry(sf_shape)

    # are all geometries compatible?
    ensurer::ensure_that(sf_shape, all(sf::st_geometry_type(.) == geom_type),
                         err_desc = "sits_from_shp: shapefile has different geometries in a single file")

    # can the function deal with the geometry_type?
    ensurer::ensure_that(geom_type, (.) %in% c("POINT", "POLYGON", "MULTIPOLYGON"),
                         err_desc = "sits_from_shp: function only handles POINT, POLYGON or MULTIPOLYGON shapefiles")

    # is the shape attribute valid?
    if (!purrr::is_null(shp_attr))
        ensurer::ensure_that(shp_attr, length(as.character(unname(shp_df[1, (.)]))) > 0,
                             err_desc = "sits_from_shp: invalid shapefile attribute")

    # if the shapefile is not in planar coordinates, convert it
    sf_shape <- suppressWarnings(sf::st_transform(sf_shape, crs = 4326))
    # create an empty sits tibble
    shape.tb <- .sits_tibble()

    # if geom_type is POINT, use the points provided in the shapefile
    if (geom_type == "POINT") {
        points.lst <- as.list(sf_shape$geometry)
        # reduce the number of points to be read
        if (length(points.lst) > .n_shp_pts)
            points.lst <- points.lst[1:.n_shp_pts]
        # read the points
        rows.lst <- points.lst %>%
            purrr::map(function(p) {
                paste0("long = ", p[1])
                row <- .sits_from_service(cube = cube, longitude = p[1], latitude = p[2],
                                          start_date = start_date, end_date = end_date,
                                          bands = bands, label = label, .prefilter = .prefilter)
                return(row)
            })
        shape.tb <- dplyr::bind_rows(shape.tb, rows.lst)
    }
    # if geom_type is not POINT, we have to sample each polygong
    else {
        for (i in 1:n_rows_shp) {
            # retrive the class from the shape attribute
            if (!purrr::is_null(shp_attr))
                label <-  as.character(unname(shp_df[i, shp_attr]))
            # obtain a set of samples based on polygons
            points.lst <- list(sf::st_sample(sf_shape[i,], size = .n_shp_pol))
            # get one time series per sample
            rows.lst <- points.lst %>%
                purrr::pmap(function(p) {
                    pll <- sf::st_geometry(p)[[1]]
                    row <- .sits_from_service(cube = cube, longitude = pll[1], latitude = pll[2],
                                              start_date = start_date, end_date = end_date,
                                              bands = bands, label = label, .prefilter = .prefilter)
                    return(row)
                })
            # combine rows to make SITS tibble
            shape.tb <- dplyr::bind_rows(shape.tb, rows.lst)
        }
    }
    return(shape.tb)
}
