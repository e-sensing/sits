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
#' @param sampling_type   Spatial sampling type: random, hexagonal,
#'                        regular, or Fibonacci.
#' @return                A tibble with information the samples to be retrieved.
#'
.sf_get_samples <- function(sf_object,
                            label,
                            label_attr,
                            start_date,
                            end_date,
                            n_sam_pol,
                            sampling_type) {
    # set caller to show in errors
    .check_set_caller(".sf_get_samples")
    # Pre-condition - is the sf object has geometries?
    .check_that(.has(sf_object))
    # Pre-condition - can the function deal with the geometry_type?
    geom_type <- as.character(sf::st_geometry_type(sf_object)[[1L]])
    sf_geom_types_supported <- .conf("sf_geom_types_supported")
    .check_that(geom_type %in% sf_geom_types_supported)
    # Get the points to be read
    samples <- .sf_to_tibble(
        sf_object = sf_object,
        label_attr = label_attr,
        label = label,
        n_sam_pol = n_sam_pol,
        sampling_type = sampling_type,
        start_date = start_date,
        end_date = end_date
    )
    .set_class(samples, "sits", class(samples))
}
#' @title Obtain a tibble with lat/long points from an sf object
#' @name .sf_to_tibble
#' @keywords internal
#' @noRd
#' @description reads a shapefile and retrieves a sits tibble
#' containing a set of lat/long points for data retrieval
#'
#' @param sf_object       sf object.
#' @param label_attr      Attribute in sf object used as a polygon label.
#' @param label           Label to be assigned to points.
#' @param n_sam_pol       Number of samples per polygon to be read
#'                        (for POLYGON or MULTIPOLYGON shapes).
#' @param sampling_type   Spatial sampling type: random, hexagonal,
#'                        regular, or Fibonacci.
#' @param start_date      Start of the interval for the time series
#'                        in "YYYY-MM-DD" format (optional).
#' @param end_date        End of the interval for the time series in
#'                        "YYYY-MM-DD" format (optional).
#' @return  A sits tibble with points to to be read.
.sf_to_tibble <- function(sf_object,
                          label_attr,
                          label,
                          n_sam_pol,
                          sampling_type,
                          start_date,
                          end_date) {
    # Remove invalid geometries (malformed and empty ones)
    sf_object <- .sf_clean(sf_object)
    # If the sf object is not in planar coordinates, convert it
    sf_object <- suppressWarnings(
        sf::st_transform(sf_object, crs = "EPSG:4326")
    )
    # Get the geometry type
    geom_type <- as.character(sf::st_geometry_type(sf_object)[[1L]])
    # Get a tibble with points and labels
    points_tbl <- switch(geom_type,
        POINT = .sf_point_to_tibble(
            sf_object  = sf_object,
            label_attr = label_attr,
            label      = label
        ),
        POLYGON = ,
        MULTIPOLYGON = .sf_polygon_to_tibble(
            sf_object = sf_object,
            label_attr = label_attr,
            label = label,
            n_sam_pol = n_sam_pol,
            sampling_type = sampling_type
        )
    )
    # Transform to type Date
    dplyr::mutate(
        points_tbl,
        start_date = as.Date(start_date),
        end_date = as.Date(end_date)
    )
}

#' @title Obtain a tibble with latitude/longitude points from POINT geometry
#'        including labels
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
    } else if (.has(label_attr)) {
        .check_chr_within(
            x = label_attr,
            within = colnames(sf_df)
        )

        labels <- as.character(unlist(sf_df[, label_attr], use.names = FALSE))
    } else {
        labels <- rep(label, times = nrow(points))
    }
    # build a tibble with lat/long and label
    tibble::tibble(
        longitude = points[, 1L],
        latitude = points[, 2L],
        label = labels
    )
}
#' @title Obtain a tibble with latitude/longitude points from POINT geometry
#' @name .sf_point_to_latlong
#' @keywords internal
#' @noRd
#' @param sf_object       sf object
#' @return  A tibble with latitude/longitude points.
#'
.sf_point_to_latlong <- function(sf_object) {
    # if geom_type is POINT, use the points provided in the shapefile
    points <- sf::st_coordinates(sf_object)
    # build a tibble with lat/long
    tibble::tibble(
        longitude = points[, 1L],
        latitude = points[, 2L]
    )
}
#' @title Obtain a tibble from POLYGON geometry
#' @name .sf_polygon_to_tibble
#' @keywords internal
#' @noRd
#' @param sf_object       sf object linked to a shapefile
#' @param label_attr      Attribute in the shapefile used as a polygon label
#' @param label           Label to be assigned to points
#' @param n_sam_pol       Number of samples per polygon to be read
#' @param sampling_type   Spatial sampling type: random, hexagonal,
#'                        regular, or Fibonacci.
#' @return A tibble with latitude/longitude points from POLYGON geometry
#'
.sf_polygon_to_tibble <- function(sf_object,
                                  label_attr,
                                  label,
                                  n_sam_pol,
                                  sampling_type) {
    .check_set_caller(".sf_polygon_to_tibble")
    # get the db file
    sf_df <- sf::st_drop_geometry(sf_object)

    if (.has(label_attr)) {
        .check_chr_within(
            x = label_attr,
            within = colnames(sf_df)
        )
    }
    seq_len(nrow(sf_object)) |>
        .map_dfr(function(row_id) {
            # retrieve the class from the shape attribute
            if ("label" %in% colnames(sf_df)) {
                label <- as.character(
                    unlist(sf_df[row_id, "label"], use.names = FALSE)
                )
            } else if (.has(label_attr) &&
                label_attr %in% colnames(sf_df)) {
                label <- as.character(
                    unlist(sf_df[row_id, label_attr], use.names = FALSE)
                )
            }
            # obtain a set of samples based on polygons
            points <- list(sf::st_sample(sf_object[row_id, ],
                type = sampling_type,
                size = n_sam_pol
            ))
            # get one time series per sample
            # return a data frame
            purrr::pmap_dfr(points, function(p) {
                pll <- sf::st_geometry(p)[[1L]]
                # return row
                tibble::tibble(
                    longitude = pll[[1L]],
                    latitude = pll[[2L]],
                    label = label,
                    polygon_id = row_id
                )
            })
        })
}

#' @title Clean invalid geometries
#' @name .sf_clean
#' @description Malformed and empty geometries are defined as invalid
#' @keywords internal
#' @noRd
#' @param sf_object sf object to be validated
#' @return sf object with no invalid geometries.
#'
.sf_clean <- function(sf_object) {
    # condition 1 - geometry is valid
    is_geometry_valid <- sf::st_is_valid(sf_object)
    # condition 2 - geometry is not empty
    is_geometry_valid <- is_geometry_valid & !sf::st_is_empty(sf_object)
    # warning user in case of invalid geometries
    if (!all(is_geometry_valid)) {
        warning(.conf("messages", ".sf_clean"))
    }
    # return only valid geometries
    sf_object[is_geometry_valid, ]
}
#' @title Create an sf polygon from a window
#' @name .sf_from_window
#' @keywords internal
#' @noRd
#' @param window  named window in WGS 84 coordinates with
#'                names (xmin, xmax, ymin, xmax)
#' @return sf polygon
#'
.sf_from_window <- function(window) {
    df <- data.frame(
        lon = c(
            window[["xmin"]], window[["xmin"]],
            window[["xmax"]], window[["xmax"]]
        ),
        lat = c(
            window[["ymin"]], window[["ymax"]],
            window[["ymax"]], window[["ymin"]]
        )
    )
    polygon <- df |>
        sf::st_as_sf(coords = c("lon", "lat"), crs = 4326L) |>
        dplyr::summarise(geometry = sf::st_combine(geometry)) |>
        sf::st_cast("POLYGON")
    polygon
}
