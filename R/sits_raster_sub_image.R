#' @title Find the dimensions and location of a spatial ROI in a data cube
#' @name .sits_raster_sub_image
#' @keywords internal

#' @param  cube            input data cube.
#' @param  sf_region       spatial region of interest (sf_object)
#' @return                 vector with information on the subimage
#'
.sits_raster_sub_image <- function(cube, sf_region = NULL) {

    # create a default sub_image that is the same size as the cube
    sub_image <- .sits_raster_sub_image_default(cube)

    # no sf_region? return sub_image as entire image
    if (purrr::is_null(sf_region))
        return(sub_image)

    # if the sf_region exists
    # Obtain the bounding box of the shape object describing the ROI
    bbox_roi <- sf::st_bbox(suppressWarnings(sf::st_transform(sf_region,
                                                              crs = cube[1,]$crs)))

    # calculate the sub-region of the cube
    # first_row (remember rows are top to bottom and coordinates are bottom to top)
    if (bbox_roi["ymax"] < cube[1,]$ymax) {
        sub_image["ymax"] <- bbox_roi["ymax"]
        sub_image["first_row"] <- unname(floor((cube[1,]$ymax - bbox_roi["ymax"])/cube$yres))
    }
    # last row
    if (bbox_roi["ymin"] > cube[1,]$ymin) {
        sub_image["ymin"] <- bbox_roi["ymin"]
        last_row <- ceiling((cube$ymax - unname(bbox_roi["ymin"]))/cube$yres)
    }

    # first col
    if (bbox_roi["xmin"] > cube[1,]$xmin) {
        sub_image["first_col"] <- floor((bbox_roi["xmin"] - cube[1,]$xmin)/cube$xres)
        sub_image["xmin"] <- bbox_roi["xmin"]
    }
    #last col
    if (bbox_roi["xmax"] < cube[1,]$xmax) {
        last_col <- ceiling((unname(bbox_roi["xmax"]) - cube[1,]$xmin)/cube$xres)
        sub_image["xmax"] <- bbox_roi["xmax"]
    }

    # number of row and cols
    sub_image["nrows"] <- last_row - sub_image["first_row"] + 1
    sub_image["ncols"] <- last_col - sub_image["first_col"] + 1

    return(sub_image)
}
#' @title Find the dimensions and location of a spatial ROI in a data cube
#' @name .sits_raster_sub_image_default
#' @keywords internal

#' @param  cube            input data cube.
#' @param  sf_region       spatial region of interest (sf_object)
#' @return                 vector with information on the subimage
.sits_raster_sub_image_default <- function(cube){
    # by default, the sub_image has the same dimension as the main cube
    sub_image = vector("integer", length = 8)
    names(sub_image) <- c("first_row", "first_col", "nrows", "ncols",
                          "xmin", "ymin", "xmax", "ymax")

    sub_image["first_row"] <- 1
    sub_image["first_col"] <- 1
    sub_image["nrows"]  <- cube[1,]$nrows
    sub_image["ncols"]  <- cube[1,]$ncols
    sub_image["xmin"]   <- cube[1,]$xmin
    sub_image["xmax"]   <- cube[1,]$xmax
    sub_image["ymin"]   <- cube[1,]$ymin
    sub_image["ymax"]   <- cube[1,]$ymax

    return(sub_image)
}
