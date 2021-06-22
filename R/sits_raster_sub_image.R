#' @title Informs if a spatial ROI intersects a data cube
#' @name .sits_raster_sub_image_intersects
#' @keywords internal

#' @param  cube            input data cube.
#' @param  roi             spatial region of interest
#' @return                 logical
#'
.sits_raster_sub_image_intersects <- function(cube, roi) {

    # if roi is null, returns TRUE
    if (purrr::is_null(roi)) return(TRUE)

    # check if roi is a sf object
    if (inherits(roi, "sf")) {

        # check for roi crs
        if (is.null(sf::st_crs(roi))) {
            stop(".sits_raster_sub_image_intersects: invalid roi crs",
                 call. = FALSE)
        }

        # reproject roi to cube crs
        roi <- suppressWarnings(sf::st_transform(roi, cube$crs[[1]]))

        # region of cube tile
        df <- data.frame(
            X = c(cube[["xmin"]][[1]], cube[["xmax"]][[1]],
                  cube[["xmax"]][[1]], cube[["xmin"]][[1]]),
            Y = c(cube[["ymin"]][[1]], cube[["ymin"]][[1]],
                  cube[["ymax"]][[1]], cube[["ymax"]][[1]])
        )

        # compute tile polygon
        sf_region <-
            sf::st_as_sf(df, coords = c("X", "Y"), crs = cube$crs[[1]]) %>%
            dplyr::summarise(geometry = sf::st_combine(geometry)) %>%
            sf::st_cast("POLYGON") %>%
            suppressWarnings()

        # check for intersection
        return(apply(sf::st_intersects(sf_region, roi), 1, any))
    }

    # if the ROI is defined, calculate the bounding box
    bbox_roi <- .sits_roi_bbox(roi, cube)

    # calculate the intersection between the bbox of the ROI and the cube
    bbox_in <- .sits_bbox_intersect(bbox_roi, cube)

    return(!purrr::is_null(bbox_in))
}
#' @title Find the dimensions and location of a spatial ROI in a data cube
#' @name .sits_raster_sub_image
#' @keywords internal

#' @param  cube            input data cube.
#' @param  roi             spatial region of interest
#' @return                 vector with information on the subimage
#'
.sits_raster_sub_image <- function(cube, roi) {

    # if the ROI is defined, calculate the bounding box
    bbox_roi <- .sits_roi_bbox(roi, cube)

    # calculate the intersection between the bbox of the ROI and the cube
    bbox_in <- .sits_bbox_intersect(bbox_roi, cube)

    # return the sub_image
    sub_image <- .sits_sub_image_from_bbox(bbox_in, cube)

    return(sub_image)
}
#' @title Find the dimensions and location of a spatial ROI in a data cube
#' @name .sits_raster_sub_image_default
#' @keywords internal

#' @param  cube            input data cube.
#' @param  sf_region       spatial region of interest (sf_object)
#' @return                 vector with information on the subimage
.sits_raster_sub_image_default <- function(cube) {
    # by default, the sub_image has the same dimension as the main cube
    sub_image <- vector("integer", length = 8)
    names(sub_image) <- c(
        "first_row", "first_col", "nrows", "ncols",
        "xmin", "ymin", "xmax", "ymax"
    )

    sub_image["first_row"] <- 1
    sub_image["first_col"] <- 1
    sub_image["nrows"] <- cube[1, ]$nrows
    sub_image["ncols"] <- cube[1, ]$ncols
    sub_image["xmin"] <- cube[1, ]$xmin
    sub_image["xmax"] <- cube[1, ]$xmax
    sub_image["ymin"] <- cube[1, ]$ymin
    sub_image["ymax"] <- cube[1, ]$ymax

    return(sub_image)
}
