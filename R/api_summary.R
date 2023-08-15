#' @title Summary of a tile of a cube
#' @noRd
#' @param tile A \code{tile}.
#' @return Called for side effects
.summary_tile_information <- function(tile) {
    # print the basic tile information
    cat("class       : ", class(tile)[1], "\n")
    cat("dimensions  : ",
        .tile_nrows(tile), ", ",
        .tile_ncols(tile), "  (nrow, ncol)\n",
        sep = ""
    )
    cat("resolution  : ",
        .tile_xres(tile), ", ",
        .tile_yres(tile), "  (x, y)\n",
        sep = ""
    )
    cat("extent      : ",
        .xmin(tile), ", ",
        .xmax(tile), ", ",
        .ymin(tile), ", ",
        .ymax(tile),
        "  (xmin, xmax, ymin, ymax)\n",
        sep = ""
    )
    cat("coord ref   : ", .crs_wkt_to_proj4(tile$crs), "\n")
    return(invisible(tile))
}
#' @title Check in tile is available
#' @noRd
#' @param object data cube
#' @param tile A \code{tile}.
#' @return Tile if available, else report error
.summary_check_tile <- function(object, tile) {
    # only one tile at a time
    .check_chr_parameter(tile)
    # is tile inside the cube?
    .check_chr_contains(
        x = object$tile,
        contains = tile,
        case_sensitive = FALSE,
        discriminator = "one_of",
        can_repeat = FALSE,
        msg = "tile is not included in the cube"
    )
    # filter the tile to be processed
    tile <- .cube_filter_tiles(cube = object, tiles = tile)
    return(tile)
}
