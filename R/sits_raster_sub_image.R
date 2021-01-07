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
