#' @title Check in tile is available
#' @noRd
#' @param cube data cube
#' @param tile A \code{tile}.
#' @return Tile if available, else report error
.summary_check_tile <- function(cube, tile) {
    .check_set_caller(".summary_check_tile")
    # only one tile at a time
    .check_chr_parameter(tile)
    # is tile inside the cube?
    .check_chr_contains(
        x = .cube_tiles(cube),
        contains = tile,
        case_sensitive = FALSE,
        discriminator = "one_of",
        can_repeat = FALSE
    )
    # filter the tile to be processed
    .cube_filter_tiles(cube = cube, tiles = tile)
}
