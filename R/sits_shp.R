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

    # get the data frame associated to the shapefile
    shp_df <- sf::st_drop_geometry(sf_shape)

    # get a tibble with points and labels
    if (geom_type == "POINT") {
        points.tb <- .sits_from_point_shp(sf_shape, shp_attr, label)
    }
    else
        points.tb <- .sits_from_polygon_shp(sf_shape, shp_attr, label, .n_shp_pol)

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
.sits_from_point_shp <-  function(sf_shape, shp_attr, label){

    # get the db file
    shp_df <- sf::st_drop_geometry(sf_shape)

    # if geom_type is POINT, use the points provided in the shapefile
    points.mx <- sf::st_coordinates(sf_shape$geometry)
    if (!purrr::is_null(shp_attr)) {
        l1.lst     <- as.list(shp_df[,shp_attr])
        labels.vec <- as.vector(l1.lst[[1]])
    }
    else {
        labels.vec <- vector(length = nrow(points.mx))
        labels.vec <- label
    }
    # build a tibble with lat/long and label
    points.tb <- tibble::tibble(longitude = points.mx[,1],
                                latitude  = points.mx[,1],
                                label     = labels.vec)

    return(points.tb)
}

#' @title Obtain a tibble with latitude and longitude points from POLYGON geometry
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

    for (i in 1:nrow(sf_shape)) {
        # retrieve the class from the shape attribute
        if (!purrr::is_null(shp_attr))
            label <-  as.character(unname(shp_df[i, shp_attr]))
        # obtain a set of samples based on polygons
        points.lst <- list(sf::st_sample(sf_shape[i,], size = .n_shp_pol))
        # get one time series per sample
        rows.lst <- points.lst %>%
            purrr::pmap(function(p) {
                pll <- sf::st_geometry(p)[[1]]
                row <- tibble::tibble(longitude = pll[1],
                                      latitude = pll[2],
                                      label = label)
                return(row)
            })
        # combine rows to make SITS tibble
        points.tb <- dplyr::bind_rows(rows.lst)
    }
    return(points.tb)
}
#' @title Extract a time series from
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Retrieve a set of time series for a raster data cube.
#'
#' @param cube              Metadata describing a raster data cube.
#' @param sf_shape          sf object.
#' @param bands             Bands to be retrieved.
#' @return                  A sits tibble with the time series.
.sits_ts_from_raster_shp <- function(cube,
                                     sf_shape,
                                     bands){

    # ensure metadata tibble exists
    assertthat::assert_that(NROW(cube) >= 1,
            msg = "sits_ts_from_raster_shp: need a valid metadata for data cube")

    spatial_points <- sf::as_Spatial(sf_shape)

    # ensure spatial points are valid
    assertthat::assert_that(nrow(spatial_points) >= 1,
                        msg = "sits_ts_from_raster_shp: need a valid sf_object")

    # get the timeline
    timeline <- sits_timeline(cube)

    # get the scale factors and missing values
    missing_values <- unlist(cube$missing_values)
    scale_factors  <- unlist(cube$scale_factors)

    # An input raster brick contains several files, each corresponds to a band
    ts_bands.lst <- bands %>%
        purrr::map(function(band) {
            # create a tibble to store the data for each band
            ts_band.tb <- .sits_tibble()
            # get the values of the time series
            r_obj <- .sits_cube_robj_band(cube, band)
            values.mx <- suppressWarnings(raster::extract(r_obj,spatial_points))
            rm(r_obj)
            # is the data valid?
            assertthat::assert_that(nrow(values.mx) > 0,
                            msg = "sits_ts_from_raster_shp - no data retrieved")
            if (all(is.na(values.mx))) {
                message("point outside the raster extent - NULL returned")
                return(NULL)
            }

            # each row of the values matrix is a spatial point
            for (i in 1:nrow(values.mx)) {
                time_idx <- .sits_timeline_indexes(timeline = timeline,
                                        start_date = lubridate::as_date(spatial_points$start_date[i]),
                                        end_date   = lubridate::as_date(spatial_points$end_date[i]))
                # select the valid dates in the timeline
                timeline_row <- timeline[time_idx["start_idx"]:time_idx["end_idx"]]
                # get only valid values for the timeline
                values.vec <- as.vector(values.mx[i, time_idx["start_idx"]:time_idx["end_idx"]])
                # correct the values using the scale factor
                values.vec <- values.vec*scale_factors[band]
                # create a tibble for each band
                ts.tb <- tibble::tibble(Index = timeline_row)
                # put the values in the time series tibble together t
                ts.tb$values <- values.vec
                colnames(ts.tb) <- c("Index", band)

                # insert a row on the tibble with the values for lat/long and the band
                ts_band.tb <- tibble::add_row(ts_band.tb,
                                              longitude    = as.vector(sp::coordinates(spatial_points)[i,1]),
                                              latitude     = as.vector(sp::coordinates(spatial_points)[i,2]),
                                              start_date   = timeline[time_idx["start_idx"]],
                                              end_date     = timeline[time_idx["end_idx"]],
                                              label        = spatial_points$label[i],
                                              cube         = cube$name,
                                              time_series  = list(ts.tb)
                )
            }
            return(ts_band.tb)
        })

    # merge the bands
    data.tb <- .sits_tibble()
    l <- length(ts_bands.lst)
    for (i in 1:l) {
        data.tb <- sits_merge(data.tb, ts_bands.lst[[i]])
    }
    return(data.tb)
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
                            msg = "sits_from_shp: shapefile does not exist")

    # read the shapefile
    sf_shape <- sf::read_sf(shp_file)
    # pre-condition - is the default label valid?
    assertthat::assert_that(nrow(sf_shape) > 0,
                            msg = "sits_from_shp: shapefile has no content")

    # get the geometry type
    geom_type <- sf::st_geometry_type(sf_shape)[1]
    # get the data frame associated to the shapefile

    # precondition - are all geometries compatible?
    assertthat::assert_that(all(sf::st_geometry_type(sf_shape) == geom_type),
                            msg = "sits_from_shp: shapefile has different geometries")

    # precondition - can the function deal with the geometry_type?
    assertthat::assert_that(
        geom_type %in% c("POINT", "POLYGON", "MULTIPOLYGON"),
            msg = "sits_from_shp: only handles
                            POINT, POLYGON or MULTIPOLYGON shapefiles")

    # precondition - is the default label valid?
    assertthat::assert_that(!purrr::is_null(label) || !purrr::is_null(shp_attr),
            msg = "sits_from_shp: default label or shape attribute should be valid")


    # precondition - is the shape attribute valid?
    # get the data frame associated to the shapefile
    shp_df <- sf::st_drop_geometry(sf_shape)
    if (!purrr::is_null(shp_attr))
        assertthat::assert_that(
            length(as.character(unname(shp_df[1, (shp_attr)]))) > 0,
            msg = "sits_from_shp: invalid shapefile attribute")


    return(sf_shape)
}

