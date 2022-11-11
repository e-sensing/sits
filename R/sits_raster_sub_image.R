#' @title Informs if a spatial ROI intersects a data cube
#' @name .raster_sub_image_intersects
#' @keywords internal
#' @noRd
#' @param  cube            data cube (one tile only).
#' @param  roi             spatial region of interest
#' @return                 does a spatial ROI intersect a data cube?
#'
.raster_sub_image_intersects <- function(cube, roi) {

    # pre-condition
    .check_num(nrow(cube),
        min = 1, max = 1, is_integer = TRUE,
        msg = "process one tile at a time only"
    )
    # if roi is null, returns TRUE
    if (purrr::is_null(roi)) {
        return(TRUE)
    }
    # check if roi is a sf object
    if (inherits(roi, "sf")) {
        # check for roi crs
        if (is.null(sf::st_crs(roi))) {
            stop(".raster_sub_image_intersects: invalid roi crs",
                call. = FALSE
            )
        }
        # reproject roi to tile crs
        roi <- suppressWarnings(sf::st_transform(roi, .cube_crs(cube)))
        # region of cube tile
        df <- data.frame(
            X = c(
                cube[["xmin"]], cube[["xmax"]],
                cube[["xmax"]], cube[["xmin"]]
            ),
            Y = c(
                cube[["ymin"]], cube[["ymin"]],
                cube[["ymax"]], cube[["ymax"]]
            )
        )
        # compute tile polygon
        sf_region <-
            sf::st_as_sf(df, coords = c("X", "Y"), crs = .cube_crs(cube)) %>%
            dplyr::summarise(geometry = sf::st_combine(.data[["geometry"]])) %>%
            sf::st_cast("POLYGON") %>%
            suppressWarnings()

        # check for intersection
        return(apply(sf::st_intersects(sf_region, roi), 1, any) ||
            apply(sf::st_within(sf_region, roi), 1, any))
    }

    # if the ROI is defined, calculate the bounding box
    bbox_roi <- .roi_bbox(roi, cube)

    # calculate the intersection between the bbox of the ROI and the cube tile
    bbox_in <- .bbox_intersect(bbox_roi, cube)

    return(!purrr::is_null(bbox_in))
}
#' @title Find the dimensions and location of a spatial ROI in a data cube
#' @name .raster_sub_image
#' @keywords internal
#' @noRd
#' @param  tile            tile of data cube.
#' @param  roi             spatial region of interest
#' @return                 vector with information on the subimage
#'
.raster_sub_image <- function(tile, roi) {

    # pre-condition
    .check_num(nrow(tile),
        min = 1, max = 1, is_integer = TRUE,
        msg = "process one tile only"
    )

    # if the ROI is defined, calculate the bounding box
    bbox_roi <- .roi_bbox(roi, tile)

    # calculate the intersection between the bbox of the ROI and the cube
    bbox_in <- .bbox_intersect(bbox_roi, tile)

    # return the sub_image
    sub_image <- .raster_sub_image_from_bbox(bbox_in, tile)

    return(sub_image)
}

#' @title Extract a sub_image from a bounding box and a cube
#' @name .raster_sub_image_from_bbox
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param bbox           bounding box for a region of interest
#' @param tile           tile of data cube
#' @return               sub_image with additional info on first row,
#'                       first col, nrows, ncols
#'
.raster_sub_image_from_bbox <- function(bbox, tile) {

    # pre-condition
    .check_num(nrow(tile),
        min = 1, max = 1, is_integer = TRUE,
        msg = "process one tile only"
    )

    # pre-conditions
    .check_num(bbox[["xmin"]],
        max = bbox[["xmax"]],
        msg = "invalid bbox value"
    )

    .check_num(bbox[["ymin"]],
        max = bbox[["ymax"]],
        msg = "invalid bbox value"
    )

    .check_num(bbox[["xmin"]],
        min = tile[["xmin"]], max = tile[["xmax"]],
        msg = "bbox value is outside the tile"
    )

    .check_num(bbox[["xmax"]],
        min = tile[["xmin"]], max = tile[["xmax"]],
        msg = "bbox value is outside the cube"
    )

    .check_num(bbox[["ymin"]],
        min = tile[["ymin"]], max = tile[["ymax"]],
        msg = "bbox value is outside the cube"
    )

    .check_num(bbox[["ymax"]],
        min = tile[["ymin"]], max = tile[["ymax"]],
        msg = "bbox value is outside the cube"
    )

    # tile template
    r_obj <- .raster_new_rast(
        nrows = .tile_nrows(tile),
        ncols = .tile_ncols(tile),
        xmin = tile[["xmin"]],
        xmax = tile[["xmax"]],
        ymin = tile[["ymin"]],
        ymax = tile[["ymax"]],
        nlayers = 1,
        crs = tile[["crs"]]
    )

    # compute block
    r_crop <- .raster_crop_metadata(r_obj, bbox = bbox)
    row <- .raster_row(
        r_obj,
        y = .raster_ymax(r_crop) - 0.5 * .raster_yres(r_crop)
    )

    col <- .raster_col(
        r_obj,
        x = .raster_xmin(r_crop) + 0.5 * .raster_xres(r_crop)
    )

    # set initial values
    si <- list(
        row = row,
        col = col,
        nrows = .raster_nrows(r_crop),
        ncols = .raster_ncols(r_crop),
        xmin = .raster_xmin(r_crop),
        xmax = .raster_xmax(r_crop),
        ymin = .raster_ymin(r_crop),
        ymax = .raster_ymax(r_crop),
        crs = tile[["crs"]]
    )

    tolerance <- .conf(
        "sources", .cube_source(tile),
        "collections", .cube_collection(tile),
        "ext_tolerance"
    )

    # pre-conditions
    .check_num(si[["xmin"]],
        max = si[["xmax"]],
        msg = "invalid subimage value"
    )

    .check_num(si[["ymin"]],
        max = si[["ymax"]],
        msg = "invalid subimage value"
    )

    .check_num(si[["xmin"]],
        min = tile[["xmin"]], max = tile[["xmax"]],
        tolerance = tolerance, msg = "invalid subimage value"
    )

    .check_num(si[["xmax"]],
        min = tile[["xmin"]], max = tile[["xmax"]],
        tolerance = tolerance, msg = "invalid subimage value"
    )

    .check_num(si[["ymin"]],
        min = tile[["ymin"]], max = tile[["ymax"]],
        tolerance = tolerance, msg = "invalid subimage value"
    )

    .check_num(si[["ymax"]],
        min = tile[["ymin"]], max = tile[["ymax"]],
        tolerance = tolerance, msg = "invalid subimage value"
    )

    return(si)
}
