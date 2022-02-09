#' @title Informs if a spatial ROI intersects a data cube
#' @name .sits_raster_sub_image_intersects
#' @keywords internal

#' @param  cube            data cube (one tile only).
#' @param  roi             spatial region of interest
#' @return                 logical
#'
.sits_raster_sub_image_intersects <- function(cube, roi) {

    # pre-condition
    .check_num(nrow(cube), min = 1, max = 1, is_integer = TRUE,
               msg = "process one tile at a time only")

    # if roi is null, returns TRUE
    if (purrr::is_null(roi)) return(TRUE)

    # check if roi is a sf object
    if (inherits(roi, "sf")) {

        # check for roi crs
        if (is.null(sf::st_crs(roi))) {
            stop(".sits_raster_sub_image_intersects: invalid roi crs",
                 call. = FALSE)
        }

        # reproject roi to tile crs
        roi <- suppressWarnings(sf::st_transform(roi, .cube_crs(cube)))

        # region of cube tile
        df <- data.frame(
            X = c(cube[["xmin"]], cube[["xmax"]],
                  cube[["xmax"]], cube[["xmin"]]),
            Y = c(cube[["ymin"]], cube[["ymin"]],
                  cube[["ymax"]], cube[["ymax"]])
        )

        # compute tile polygon
        sf_region <-
            sf::st_as_sf(df, coords = c("X", "Y"), crs = .cube_crs(cube)) %>%
            dplyr::summarise(geometry = sf::st_combine(geometry)) %>%
            sf::st_cast("POLYGON") %>%
            suppressWarnings()

        # check for intersection
        return(apply(sf::st_intersects(sf_region, roi), 1, any) ||
                   apply(sf::st_within(sf_region, roi), 1, any)
        )
    }

    # if the ROI is defined, calculate the bounding box
    bbox_roi <- .sits_roi_bbox(roi, cube)

    # calculate the intersection between the bbox of the ROI and the cube tile
    bbox_in <- .sits_bbox_intersect(bbox_roi, cube)

    return(!purrr::is_null(bbox_in))
}
#' @title Find the dimensions and location of a spatial ROI in a data cube
#' @name .sits_raster_sub_image
#' @keywords internal

#' @param  tile            tile of data cube.
#' @param  roi             spatial region of interest
#' @return                 vector with information on the subimage
#'
.sits_raster_sub_image <- function(tile, roi) {

    # pre-condition
    .check_num(nrow(tile), min = 1, max = 1, is_integer = TRUE,
               msg = "process one tile only")

    # if the ROI is defined, calculate the bounding box
    bbox_roi <- .sits_roi_bbox(roi, tile)

    # calculate the intersection between the bbox of the ROI and the cube
    bbox_in <- .sits_bbox_intersect(bbox_roi, tile)

    # return the sub_image
    sub_image <- .sits_raster_sub_image_from_bbox(bbox_in, tile)

    return(sub_image)
}
#' @title Find the dimensions of the sub image without ROI
#' @name .sits_raster_sub_image_default
#' @keywords internal

#' @param  tile            tile of data cube.
#' @param  sf_region       spatial region of interest (sf_object)
#' @return                 vector with information on the subimage
.sits_raster_sub_image_default <- function(tile) {

    # pre-condition
    .check_num(nrow(tile), min = 1, max = 1, is_integer = TRUE,
               msg = "process one tile only")

    # by default, the sub_image has the same dimension as the main cube
    size <- .cube_size(tile)
    bbox <- .cube_tile_bbox(tile)

    sub_image <- c(first_row = 1,
                   first_col = 1,
                   nrows = size[["nrows"]],
                   ncols = size[["ncols"]],
                   xmin = bbox[["xmin"]],
                   xmax = bbox[["xmax"]],
                   ymin = bbox[["ymin"]],
                   ymax = bbox[["ymax"]])

    return(sub_image)
}

#' @title Extract a sub_image from a bounding box and a cube
#' @name .sits_raster_sub_image_from_bbox
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param bbox           bounding box for a region of interest
#' @param tile           tile of data cube
#' @return               sub_image with additional info on first row,
#'                       first col, nrows, ncols
#'
.sits_raster_sub_image_from_bbox <- function(bbox, tile) {

    # pre-condition
    .check_num(nrow(tile), min = 1, max = 1, is_integer = TRUE,
               msg = "process one tile only")

    # pre-conditions
    .check_num(bbox[["xmin"]], max = bbox[["xmax"]],
               msg = "invalid bbox value")

    .check_num(bbox[["ymin"]], max = bbox[["ymax"]],
               msg = "invalid bbox value")

    .check_num(bbox[["xmin"]], min = tile[["xmin"]], max = tile[["xmax"]],
               msg = "bbox value is outside the tile")

    .check_num(bbox[["xmax"]], min = tile[["xmin"]], max = tile[["xmax"]],
               msg = "bbox value is outside the cube")

    .check_num(bbox[["ymin"]], min = tile[["ymin"]], max = tile[["ymax"]],
               msg = "bbox value is outside the cube")

    .check_num(bbox[["ymax"]], min = tile[["ymin"]], max = tile[["ymax"]],
               msg = "bbox value is outside the cube")

    # get ncols and nrows
    # throw an error if size are not the same
    size  <- .cube_size(tile)

    # tile template
    r_obj <- .raster_new_rast(
        nrows = size[["nrows"]],
        ncols = size[["ncols"]],
        xmin = tile[["xmin"]],
        xmax = tile[["xmax"]],
        ymin = tile[["ymin"]],
        ymax = tile[["ymax"]],
        nlayers = 1,
        crs = tile[["crs"]]
    )

    # compute block
    r_crop <- .raster_crop(r_obj, bbox = bbox)
    row <- .raster_row(r_obj, y = .raster_ymax(r_crop))
    if (is.na(row)) row <- 1
    col <- .raster_col(r_obj, x = .raster_xmin(r_crop))
    if (is.na(col)) col <- 1

    # set initial values
    si <- c(first_row = row,
            first_col = col,
            nrows = .raster_nrows(r_crop),
            ncols = .raster_ncols(r_crop),
            xmin = .raster_xmin(r_crop),
            xmax = .raster_xmax(r_crop),
            ymin = .raster_ymin(r_crop),
            ymax = .raster_ymax(r_crop))

    # compute first_col (terra code below)
    # if (bbox[["xmin"]] >= tile[["xmin"]] && bbox[["xmin"]] < tile[["xmax"]]) {
    #
    #     first_col <- trunc((bbox[["xmin"]] - tile[["xmin"]]) / res[["xres"]]) + 1
    #
    # } else if (bbox[["xmin"]] == tile[["xmax"]]) {
    #
    #     first_col <- size[["ncols"]]
    # }

    # compute first_row (terra code below)
    # if (bbox[["ymax"]] > tile[["ymin"]] && bbox[["ymax"]] <= tile[["ymax"]]) {
    #
    #     first_row <- trunc((tile[["ymax"]] - bbox[["ymax"]]) / res[["yres"]]) + 1
    #
    # } else if (bbox[["ymax"]] == tile[["ymin"]]) {
    #
    #     first_row <- size[["nrows"]]
    # }

    tolerance <- .config_get(key = c("sources", .cube_source(tile),
                                     "collections", .cube_collection(tile),
                                     "ext_tolerance"))

    # pre-conditions
    .check_num(si[["xmin"]], max = si[["xmax"]],
               msg = "invalid subimage value")

    .check_num(si[["ymin"]], max = si[["ymax"]],
               msg = "invalid subimage value")

    .check_num(si[["xmin"]], min = tile[["xmin"]], max = tile[["xmax"]],
               tolerance = tolerance, msg = "invalid subimage value")

    .check_num(si[["xmax"]], min = tile[["xmin"]], max = tile[["xmax"]],
               tolerance = tolerance, msg = "invalid subimage value")

    .check_num(si[["ymin"]], min = tile[["ymin"]], max = tile[["ymax"]],
               tolerance = tolerance, msg = "invalid subimage value")

    .check_num(si[["ymax"]], min = tile[["ymin"]], max = tile[["ymax"]],
               tolerance = tolerance, msg = "invalid subimage value")

    return(si)
}
