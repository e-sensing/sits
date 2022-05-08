#' @title Return a sits_tibble as a point sf object.
#' @name sits_as_sf
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Return a sits_tibble as a sf object of point geometry.
#'
#' @param samples A sits tibble with one or more time series.
#' @return        Return a sits_tibble as a sf object of point geometry.
#' @export
sits_as_sf <- function(samples) {
    .check_chr_within(
        x = .config_get("df_sample_columns"),
        within = colnames(samples),
        msg = "data input is not valid"
    )

    samples_sf <- sf::st_as_sf(samples,
        coords = c("longitude", "latitude"),
        crs = 4326,
        remove = FALSE
    )

    return(samples_sf)
}
#' @title Transform an sf object into a samples file
#' @name .sits_get_samples_from_sf
#' @author Gilberto Camara
#' @keywords internal
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
.sits_get_samples_from_sf <- function(sf_object,
                                      label,
                                      label_attr,
                                      start_date,
                                      end_date,
                                      n_sam_pol,
                                      pol_id) {

    # get the points to be read
    samples <- .sits_sf_to_tibble(
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

    return(samples)
}
#' @title Obtain a tibble with lat/long points to be retrieved from an sf object
#'
#' @name .sits_sf_to_tibble
#' @keywords internal
#'
#' @description reads a shapefile and retrieves a sits tibble
#' containing a set of lat/long points for data retrieval
#'
#' @param sf_object       sf object .
#' @param label_attr      Attribute in the sf object used as a polygon label.
#' @param label           Label to be assigned to points.
#' @param n_sam_pol       Number of samples per polygon to be read
#'                        (for POLYGON or MULTIPOLYGON shapes).
#' @param  pol_id         ID attribute for polygons.
#' @return                A sits tibble with points to to be read.
.sits_sf_to_tibble <- function(sf_object,
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
        points_tbl <- .sits_sf_point_to_tibble(
            sf_object,
            label_attr,
            label
        )
    } else {
        points_tbl <- .sits_sf_polygon_to_tibble(
            sf_object,
            label_attr,
            label,
            n_sam_pol,
            pol_id
        )
    }

    return(points_tbl)
}

#' @title Obtain a tibble with latitude/longitude points from POINT geometry
#' @name .sits_sf_point_to_tibble
#' @keywords internal
#'
#' @param sf_object       sf object.
#' @param label_attr      Attribute used as a polygon label
#' @param label           Label to be assigned if no attribute is provided
#'
.sits_sf_point_to_tibble <- function(sf_object, label_attr, label) {

    # get the db file
    sf_df <- sf::st_drop_geometry(sf_object)

    # if geom_type is POINT, use the points provided in the shapefile
    points <- sf::st_coordinates(sf_object$geometry)
    if (!purrr::is_null(label_attr)) {
        l1_lst <- as.list(sf_df[, label_attr])
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
#' @name .sits_sf_polygon_to_tibble
#' @keywords internal
#'
#' @param sf_object       sf object linked to a shapefile
#' @param label_attr      Attribute in the shapefile used as a polygon label
#' @param label           Label to be assigned to points
#' @param n_sam_pol       Number of samples per polygon to be read
#' @param pol_id          ID attribute for polygons shapefile.
#'
.sits_sf_polygon_to_tibble <- function(sf_object,
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
            if (!purrr::is_null(label_attr) &&
                label_attr %in% colnames(sf_df)) {
                label <- unname(as.character(sf_df[i, label_attr]))
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
