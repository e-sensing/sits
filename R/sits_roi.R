#' @title Find the bounding box for a spatial ROI in a data cube
#' @name .sits_roi_bbox
#' @keywords internal
#' @param  cube            input data cube.
#' @param  roi             spatial region of interest
#' @return                 vector with information on the subimage
.sits_roi_bbox <- function(roi, cube) {

    # set caller to show in errors
    .check_set_caller(".sits_roi_bbox")

    if (!(inherits(roi, "sf"))) {

        if (all(c("xmin", "xmax", "ymin", "ymax") %in% names(roi))) {
            class(roi) <- c("xy", class(roi))
        } else if (all(
            c("lon_min", "lon_max", "lat_min", "lat_max") %in% names(roi))) {
            class(roi) <- c("ll", class(roi))
        }
    }

    .check_that(
        x = inherits(roi, c("sf", "xy", "ll")),
        msg = "invalid definition of ROI"
    )

    UseMethod(".sits_roi_bbox", roi)
}

#' @title Find the bounding box for a spatial ROI defined as an sf object
#' @name .sits_roi_bbox.sf
#' @keywords internal
#' @param  roi             spatial region of interest
#' @param  cube            input data cube.
#' @return                 vector with information on the subimage
#' @export
.sits_roi_bbox.sf <- function(roi, cube) {
    bbox <- roi %>%
        sf::st_transform(crs = .cube_crs(cube)) %>%
        suppressWarnings() %>%
        sf::st_bbox()

    return(bbox)
}
#' @title Find the bounding box for a spatial ROI defined as a bounding box
#' @name .sits_roi_bbox.xy
#' @keywords internal
#' @param  cube            input data cube.
#' @param  roi             spatial region of interest
#' @return                 vector with information on the subimage
#' @export
.sits_roi_bbox.xy <- function(roi, cube) {
    return(roi)
}
#' @title Find the bounding box for a spatial ROI defined as a lat/lon box
#' @name .sits_roi_bbox.ll
#' @keywords internal
#' @param  cube            input data cube.
#' @param  roi             spatial region of interest
#' @return                 vector with information on the subimage
#' @export
.sits_roi_bbox.ll <- function(roi, cube) {
    # region of interest defined by two points
    df <- data.frame(
        lon = c(roi["lon_min"], roi["lon_max"], roi["lon_max"], roi["lon_min"]),
        lat = c(roi["lat_min"], roi["lat_min"], roi["lat_max"], roi["lat_max"])
    )

    sf_region <- df %>%
        sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
        dplyr::summarise(geometry = sf::st_combine(geometry)) %>%
        sf::st_cast("POLYGON")

    bbox <- sf::st_bbox(
        suppressWarnings(
            sf::st_transform(sf_region, crs = .cube_crs(cube))))
}
#' @title Parse is a ROI is valid for an existing data cube
#' @name .sits_parse_roi_cube
#' @keywords internal
#' @param  roi             spatial region of interest
#' @return                 roi in WGS84 projection or NULL if error
#' @export
.sits_parse_roi_cube <- function(roi) {
    # set caller to show in errors
    .check_set_caller(".sits_parse_roi_cube")

    if (!(inherits(roi, "sf"))) {

        if (all(c("xmin", "xmax", "ymin", "ymax") %in% names(roi))) {
            class(roi) <- c("xy", class(roi))
        } else if (all(
            c("lon_min", "lon_max", "lat_min", "lat_max") %in% names(roi))) {
            class(roi) <- c("ll", class(roi))
        }
    }

    .check_that(
        x = inherits(roi, c("sf", "xy", "ll")),
        msg = "invalid definition of ROI"
    )

    UseMethod(".sits_parse_roi_cube", roi)
}
#' @title Parse is a ROI defined as sf is valid for an existing data cube
#' @name .sits_parse_roi_cube.sf
#' @keywords internal
#' @param  roi   spatial region of interest
#' @return       roi in WGS84 projection
#' @export
.sits_parse_roi_cube.sf <- function(roi) {

    roi_crs <- sf::st_crs(roi, parameters = TRUE)
    .check_lst(roi_crs, min_len = 1, msg = "invalid crs in provided roi.")

    if (roi_crs[["epsg"]] != 4326) {
        message("The supplied roi will be transformed to the WGS 84.")
        roi <- sf::st_transform(roi, crs = 4326)
    }

    # return converted roi
    return(roi)
}

#' @title Parse a ROI defined as lat-long
#' @name .sits_parse_roi_cube.ll
#' @keywords internal
#' @param  roi     spatial region of interest
#' @return a sf object in WGS84 projection
#' @export
.sits_parse_roi_cube.ll <- function(roi) {
    .check_num(
        roi["lon_min"], min = -180.0, max = 180.00,
        msg = "roi should be provided in WGS84 coordinates"
    )
    .check_num(
        roi["lon_max"], min = -180.0, max = 180.00,
        msg = "roi should be provided in WGS84 coordinates"
    )
    .check_num(
        roi["lat_min"], min = -90.0, max = 90.00,
        msg = "roi should be provided in WGS84 coordinates"
    )
    .check_num(
        roi["lat_max"], min = -90.0, max = 90.00,
        msg = "roi should be provided in WGS84 coordinates"
    )

    # convert to sf object
    sf_obj <- .sits_bbox_to_sf(xmin = roi[["lon_min"]],
                               xmax = roi[["lon_max"]],
                               ymin = roi[["lat_min"]],
                               ymax = roi[["lat_max"]],
                               crs = 4326)

    return(sf_obj)
}
#' @title Parse a ROI defined as XY
#' @name .sits_parse_roi_cube.xy
#' @keywords internal
#' @param  roi   spatial region of interest
#' @return a sf object in WGS84 projection
#' @export
.sits_parse_roi_cube.xy <- function(roi) {
    .check_num(
        roi["xmin"], min = -180.0, max = 180.00,
        msg = "roi should be provided in WGS84 coordinates"
    )
    .check_num(
        roi["xmax"], min = -180.0, max = 180.00,
        msg = "roi should be provided in WGS84 coordinates"
    )
    .check_num(
        roi["ymin"], min = -90.0, max = 90.00,
        msg = "roi should be provided in WGS84 coordinates"
    )
    .check_num(
        roi["ymax"], min = -90.0, max = 90.00,
        msg = "roi should be provided in WGS84 coordinates"
    )

    # convert to sf object
    sf_obj <- .sits_bbox_to_sf(xmin = roi[["xmin"]],
                               xmax = roi[["xmax"]],
                               ymin = roi[["ymin"]],
                               ymax = roi[["ymax"]],
                               crs = 4326)

    return(sf_obj)
}

#' @title Convert a ROI defined as sf object to a geojson polygon geometry
#' @name .sits_roi_sf_to_geojson
#' @keywords internal
#' @param  roi_sf   region of interest as sf object
#' @return a geojson polygon geometry
#' @export
.sits_roi_sf_to_geojson <- function(roi_sf) {

    # pre-conditions
    .check_that(nrow(roi_sf) == 1,
                local_msg = "roi_sf should have only one row",
                msg = "invalid roi_sf value")

    # convert roi_sf to geojson (character)
    geojson <- geojsonsf::sfc_geojson(sf::st_geometry(roi_sf))

    return(geojson)
}
