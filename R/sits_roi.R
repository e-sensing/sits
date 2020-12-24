#' @title Find the bounding box for a spatial ROI in a data cube
#' @name .sits_roi_bbox
#' @keywords internal
#' @param  cube            input data cube.
#' @param  roi             spatial region of interest
#' @return                 vector with information on the subimage
.sits_roi_bbox <- function(roi, cube) {

    if (!("sf" %in% class(roi))) {
        if (all(c("xmin", "xmax", "ymin", "ymax") %in% names(roi))) {
              class(roi) <- c("xy", class(roi))
          } else if (all(
            c("lon_min", "lon_max", "lat_min", "lat_max") %in% names(roi))) {
              class(roi) <- c("ll", class(roi))
          }
    }
    assertthat::assert_that(class(roi)[1] %in% c("sf", "xy", "ll"),
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
      sf::st_transform(crs = cube[1,]$crs) %>%
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

    bbox <- sf::st_bbox(suppressWarnings(sf::st_transform(sf_region,
                                                          crs = cube[1,]$crs)))
}
