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
    sub_image <- .sits_raster_sub_image_from_bbox(bbox_in, cube)

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

#' @title Extract a sub_image from a bounding box and a cube
#' @name .sits_raster_sub_image_from_bbox
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param bbox           bounding box for a region of interest
#' @param cube           data cube
#' @return               sub_image with additional info on first row,
#'                       first col, nrows, ncols
#'
.sits_raster_sub_image_from_bbox <- function(bbox, cube) {
    si <- vector("double", length = 8)
    names(si) <- c("first_row", "first_col", "nrows", "ncols",
                   "xmin", "xmax", "ymin", "ymax")

    si[c("xmin", "xmax", "ymin", "ymax")] <-
        bbox[c("xmin", "xmax", "ymin", "ymax")]

    # find the first row (remember that rows runs from top to bottom and
    # Y coordinates increase from bottom to top)
    if (bbox["ymax"]  == cube$ymax)
        si["first_row"] <- 1
    else {
        si["first_row"] <- unname(
            floor((cube$ymax - bbox["ymax"]) / cube$yres)) + 1
        # adjust to fit bbox in cube resolution
        si["ymax"] <- cube$ymax - cube$yres * (si["first_row"] - 1)
    }
    # find the first col (remember that rows runs from left to right and
    # X coordinates increase from left to right)
    if (bbox["xmin"] == cube$xmin)
        si["first_col"] <- 1
    else {
        si["first_col"] <- unname(
            floor((bbox["xmin"] - cube$xmin) / cube$xres)
        ) + 1
        # adjust to fit bbox in cube resolution
        si["xmin"] <- cube$xmin + cube$xres * (si["first_col"] - 1)
    }

    # find the number of rows (remember that rows runs from top to bottom and
    # Y coordinates increase from bottom to top)
    if (bbox["ymin"] == cube$ymin)
        si["nrows"] <- cube$nrows - unname(si["first_row"]) + 1
    else {
        si["nrows"] <- unname(
            floor((bbox["ymax"] - bbox["ymin"]) / cube$yres)) + 1
        # adjust to fit bbox in cube resolution
        si["ymin"] <- si["ymax"] - cube$yres * si["nrows"]
    }

    if (si["xmax"] == cube$xmax)
        si["ncols"] <- cube$ncols - unname(si["first_col"]) + 1
    else {
        si["ncols"] <- unname(
            floor((bbox["xmax"] - bbox["xmin"]) / cube$xres)) + 1
        # adjust to fit bbox in cube resolution
        si["xmax"] <- si["xmin"] + cube$yres * si["ncols"]
    }

    return(si)
}
