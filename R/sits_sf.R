#' @title Return a sits_tibble or raster_cube as an sf object.
#' @name sits_as_sf
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Return a sits_tibble or raster_cube as an sf object.
#'
#' @param data   A sits tibble or sits cube.
#' @param as_crs Output coordinate reference system.
#' @param ...    Additional parameters.
#' @param crs    Input coordinate reference system.
#' @return       An sf object of point or polygon geometry.
#' @examples
#' if (sits_run_examples()) {
#'    # convert sits tibble to an sf object (point)
#'    sf_object <- sits_as_sf(cerrado_2classes)
#'
#'    # convert sits cube to an sf object (polygon)
#'    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'    cube <- sits_cube(
#'        source = "BDC",
#'        collection = "MOD13Q1-6",
#'        data_dir = data_dir,
#'        delim = "_",
#'        parse_info = c("X1", "tile", "band", "date")
#'    )
#'    sf_object <- sits_as_sf(cube)
#'}
#' @export
sits_as_sf <- function(data, ..., as_crs = NULL) {
    UseMethod("sits_as_sf", data)
}

#' @export
#' @rdname sits_as_sf
sits_as_sf.sits <- function(data, ..., crs = "EPSG:4326", as_crs = NULL) {

    # Pre-conditions
    .check_samples(data)

    # Convert samples to sf
    geom <- .point_as_sf(.point(data, crs = crs), as_crs = as_crs)

    # Bind columns
    data <- dplyr::bind_cols(geom, .discard(data, "time_series"))

    return(data)
}

#' @export
#' @rdname sits_as_sf
sits_as_sf.raster_cube <- function(data, ..., as_crs = NULL) {

    # Pre-conditions
    .check_is_raster_cube(data)

    # Convert cube bbox to sf
    data_sf <- .cube_as_sf(data, as_crs = as_crs)

    # Bind columns
    data <- dplyr::bind_cols(data_sf, .discard(data, "file_info"))

    return(data)
}
#' @title Transform an sf object into a samples file
#' @name .sf_get_samples
#' @author Gilberto Camara
#' @keywords internal
#' @noRd
#' @param sf_object       sf object that describes the data to be retrieved.
#' @param label           Default label for samples.
#' @param label_attr      sf attribute that describes the label.
#' @param start_date      Start date for the data set.
#' @param end_date        End date for the data set.
#' @param n_sam_pol       Number of samples per polygon to be read.
#' @param pol_id          ID attribute for polygons shapefile.
#'                        (for POLYGON or MULTIPOLYGON shapefile).
#' @return                A tibble with information the samples to be retrieved.
#'
.sf_get_samples <- function(sf_object,
                            label,
                            label_attr,
                            start_date,
                            end_date,
                            n_sam_pol,
                            pol_id) {

    # get the points to be read
    samples <- .sf_to_tibble(
        sf_object   = sf_object,
        label_attr  = label_attr,
        label       = label,
        n_sam_pol   = n_sam_pol,
        pol_id      = pol_id
    )

    samples <- dplyr::mutate(samples,
                             start_date = as.Date(start_date),
                             end_date = as.Date(end_date)
    )

    class(samples) <- c("sits", class(samples))

    return(samples)
}

#' @title Obtain a tibble with lat/long points from an sf object
#' @name .sf_to_tibble
#' @keywords internal
#' @noRd
#' @description reads a shapefile and retrieves a sits tibble
#' containing a set of lat/long points for data retrieval
#'
#' @param sf_object       sf object .
#' @param label_attr      Attribute in sf object used as a polygon label.
#' @param label           Label to be assigned to points.
#' @param n_sam_pol       Number of samples per polygon to be read
#'                        (for POLYGON or MULTIPOLYGON shapes).
#' @param  pol_id         ID attribute for polygons.
#' @return                A sits tibble with points to to be read.
.sf_to_tibble <- function(sf_object,
                          label_attr,
                          label,
                          n_sam_pol,
                          pol_id) {

    # get the geometry type
    geom_type <- sf::st_geometry_type(sf_object)[1]

    # if the sf object is not in planar coordinates, convert it
    sf_object <- suppressWarnings(sf::st_transform(sf_object, crs = 4326))

    # get a tibble with points and labels
    if (geom_type == "POINT") {
        points_tbl <- .sf_point_to_tibble(
            sf_object,
            label_attr,
            label
        )
        return(points_tbl)
    }
    points_tbl <- .sf_polygon_to_tibble(
        sf_object,
        label_attr,
        label,
        n_sam_pol,
        pol_id
    )

    return(points_tbl)
}

#' @title Obtain a tibble with latitude/longitude points from POINT geometry
#' @name .sf_point_to_tibble
#' @keywords internal
#' @noRd
#' @param sf_object       sf object.
#' @param label_attr      Attribute used as a polygon label
#' @param label           Label to be assigned if no attribute is provided
#' @return  A tibble with latitude/longitude points.
#'
.sf_point_to_tibble <- function(sf_object, label_attr, label) {

    # get the db file
    sf_df <- sf::st_drop_geometry(sf_object)

    # if geom_type is POINT, use the points provided in the shapefile
    points <- sf::st_coordinates(sf_object)

    if ("label" %in% colnames(sf_df)) {
        labels <- as.character(unlist(sf_df[, "label"], use.names = FALSE))
    } else if (!purrr::is_null(label_attr)) {
        .check_chr_within(
            x = label_attr,
            within = colnames(sf_df)
        )

        labels <- as.character(unlist(sf_df[, label_attr], use.names = FALSE))
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
#' @name .sf_polygon_to_tibble
#' @keywords internal
#' @noRd
#' @param sf_object       sf object linked to a shapefile
#' @param label_attr      Attribute in the shapefile used as a polygon label
#' @param label           Label to be assigned to points
#' @param n_sam_pol       Number of samples per polygon to be read
#' @param pol_id          ID attribute for polygons shapefile.
#' @return A tibble with latitude/longitude points from POLYGON geometry
#'
.sf_polygon_to_tibble <- function(sf_object,
                                  label_attr,
                                  label,
                                  n_sam_pol,
                                  pol_id) {

    # get the db file
    sf_df <- sf::st_drop_geometry(sf_object)

    if (!purrr::is_null(label_attr)) {
        .check_chr_within(
            x = label_attr,
            within = colnames(sf_df),
            msg = "invalid 'label_attr' parameter."
        )
    }

    if (!purrr::is_null(pol_id)) {
        .check_chr_within(
            x = pol_id,
            within = colnames(sf_df),
            msg = "invalid 'pol_id' parameter."
        )
    }

    points.tb <- seq_len(nrow(sf_object)) %>%
        purrr::map_dfr(function(i) {
            # retrieve the class from the shape attribute

            if ("label" %in% colnames(sf_df)) {
                label <- as.character(
                    unlist(sf_df[i, "label"], use.names = FALSE)
                )
            } else if (!purrr::is_null(label_attr) &&
                       label_attr %in% colnames(sf_df)) {
                label <- as.character(
                    unlist(sf_df[i, label_attr], use.names = FALSE)
                )
            }
            if (!purrr::is_null(pol_id) && pol_id %in% colnames(sf_df)) {
                polygon_id <- unname(as.character(sf_df[i, pol_id]))
            }

            # obtain a set of samples based on polygons
            points <- list(sf::st_sample(sf_object[i, ], size = n_sam_pol))
            # get one time series per sample
            pts.tb <- points %>%
                purrr::pmap_dfr(function(p) {
                    pll <- sf::st_geometry(p)[[1]]
                    row <- tibble::tibble(
                        longitude = pll[1],
                        latitude = pll[2],
                        label = label
                    )

                    if (!purrr::is_null(pol_id) &&
                        pol_id %in% colnames(sf_df)) {
                        row <- tibble::add_column(
                            row,
                            polygon_id = polygon_id
                        )
                    }

                    return(row)
                })
            return(pts.tb)
        })
    return(points.tb)
}
