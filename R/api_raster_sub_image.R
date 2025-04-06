#' @title Find the dimensions and location of a spatial ROI in a data cube
#' @name .raster_sub_image
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @param  tile  tile of data cube.
#' @param  roi   sf object with spatial region of interest
#' @return       vector with information on the subimage
#'
.raster_sub_image <- function(tile, roi) {
    .check_set_caller(".raster_sub_image")
    # pre-condition
    .check_int_parameter(nrow(tile), min = 1, max = 1)

    # calculate the intersection between the bbox of the ROI and the cube
    # transform the tile bbox to sf
    sf_tile <- .bbox_as_sf(.tile_bbox(tile))
    if (sf::st_crs(sf_tile) != sf::st_crs(roi)) {
        roi <- sf::st_transform(roi, crs = .tile_crs(tile))
    }
    geom <- sf::st_intersection(sf_tile, roi)
    # get bbox of subimage
    sub_image_bbox <- .bbox(geom)
    # return the sub_image
    sub_image <- .raster_sub_image_from_bbox(sub_image_bbox, tile)
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
    .check_set_caller(".raster_sub_image_from_bbox")
    # pre-condition
    n_tiles <- nrow(tile)
    .check_int_parameter(n_tiles, min = 1, max = 1)

    # tolerance added to handle edge cases
    tolerance <- 0.001

    # pre-conditions
    .check_that(
        bbox[["xmin"]] < bbox[["xmax"]]   &&
            bbox[["ymin"]] < bbox[["ymax"]]  + tolerance &&
            bbox[["xmin"]] >= tile[["xmin"]] - tolerance &&
            bbox[["xmax"]] <= tile[["xmax"]] + tolerance  &&
            bbox[["ymin"]] >= tile[["ymin"]] - tolerance  &&
            bbox[["ymax"]] <= tile[["ymax"]] + tolerance
    )

    # tile template
    rast <- .raster_new_rast(
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
    r_crop <- .raster_crop_metadata(rast, bbox = bbox)
    row <- .raster_row(rast,
        y = .raster_ymax(r_crop) - 0.5 * .raster_yres(r_crop)
    )

    col <- .raster_col(rast,
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
    .check_that(si[["xmin"]] < si[["xmax"]])
    .check_that(si[["ymin"]] < si[["ymax"]])

    subimage_xmin <- si[["xmin"]]
    .check_num_parameter(subimage_xmin,
        min = tile[["xmin"]],
        max = tile[["xmax"]],
        tolerance = tolerance
    )
    subimage_max <- si[["xmax"]]
    .check_num_parameter(subimage_max,
               min = tile[["xmin"]],
               max = tile[["xmax"]],
               tolerance = tolerance
    )
    subimage_ymin <- si[["ymin"]]
    .check_num_parameter(subimage_ymin,
        min = tile[["ymin"]],
        max = tile[["ymax"]],
        tolerance = tolerance
    )
    subimage_ymax <- si[["ymax"]]
    .check_num(subimage_ymax,
        min = tile[["ymin"]],
        max = tile[["ymax"]],
        tolerance = tolerance
    )

    return(si)
}
