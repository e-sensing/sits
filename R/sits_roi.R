#' @title Find the bounding box for a spatial ROI in a data cube
#' @name .sits_roi_bbox
#' @keywords internal
#' @param  cube            input data cube.
#' @param  roi             spatial region of interest
#' @return                 vector with information on the subimage
.sits_roi_bbox <- function(roi, cube){
    if (!requireNamespace("plyr", quietly = TRUE)) {
        stop("plyr required for this function to work.
             Please install it.", call. = FALSE)
    }
    # help the user
    if ("long_min" %in% names(roi))
        roi <- plyr::rename(roi, replace = c("long_min" = "lon_min"))
    if ("long_max" %in% names(roi))
        roi <- plyr::rename(roi, replace = c("long_max" = "lon_max"))

    if (!("sf" %in% class(roi))) {
        if (all(c("xmin", "xmax","ymin", "ymax") %in% names(roi)))
           class(roi) <- c("bbox", class(roi))
        else if (all(c("lon_min", "lon_max","lat_min", "lat_max") %in% names(roi)))
           class(roi) <- c("bbox_ll", class(roi))
    }
    assertthat::assert_that(class(roi)[1] %in% c("sf", "bbox", "bbox_ll"),
                            msg = "invalid definition of ROI")

    UseMethod(".sits_roi_bbox", roi)
}
#' @title Find the bounding box for a spatial ROI defined as an sf object
#' @name sits_roi_bbox.sf
#' @keywords internal
#' @param  cube            input data cube.
#' @param  roi             spatial region of interest
#' @return                 vector with information on the subimage
.sits_roi_bbox.sf <- function(roi, cube){
    bbox_roi <- sf::st_bbox(suppressWarnings(sf::st_transform(roi,
                                                crs = cube[1,]$crs)))
    return(bbox_roi)
}
#' @title Find the bounding box for a spatial ROI defined as a bounding box
#' @name sits_roi_bbox.bbox
#' @keywords internal
#' @param  cube            input data cube.
#' @param  roi             spatial region of interest
#' @return                 vector with information on the subimage
.sits_roi_bbox.bbox <- function(roi, cube){
    return(roi)
}
#' @title Find the bounding box for a spatial ROI defined as a lat/lon box
#' @name sits_roi_bbox.bbox_ll
#' @keywords internal
#' @param  cube            input data cube.
#' @param  roi             spatial region of interest
#' @return                 vector with information on the subimage
.sits_roi_bbox.bbox_ll <- function(roi, cube){
    # region of interest
    df <- data.frame(
        lon = c(roi["lon_min"], roi["lon_max"], roi["lon_max"], roi["lon_min"]),
        lat = c(roi["lat_min"], roi["lat_min"], roi["lat_max"], roi["lat_max"])
    )

    sf_region <- df %>%
        sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
        dplyr::summarise(geometry = sf::st_combine(geometry)) %>%
        sf::st_cast("POLYGON")

    bbox <- sf::st_bbox(suppressWarnings(sf::st_transform(sf_region,
                                                          crs = cube[1,]$crs)))
    return(bbox)
}
