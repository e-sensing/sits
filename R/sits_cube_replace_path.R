#' @title Replace part of the image file paths of a data cube
#' @name sits_cube_replace_path
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' Rewrites a substring of the image file paths stored in the \code{file_info}
#' of a data cube, returning a cube that is still structurally valid. This is
#' useful when the underlying image files have been moved to a new location
#' (for example, a different storage root) and the paths recorded in the cube
#' need to be updated to point to the new location.
#'
#' Only the requested \code{file_info} column is changed; every tile row, the
#' nested \code{file_info} tables, and the cube's classes are preserved. The
#' substitution is applied with \code{\link[base]{gsub}} to each path.
#'
#' @param cube        A data cube (class \code{"raster_cube"}).
#' @param pattern     Character (length 1). Text to look for inside each
#'                    image path.
#' @param replacement Character (length 1). New text to substitute for
#'                    \code{pattern}.
#' @param column      Character (length 1). Name of the \code{file_info}
#'                    column to edit. Default is \code{"path"}.
#' @param fixed       Logical (length 1). If \code{TRUE} (default),
#'                    \code{pattern} is matched literally; if \code{FALSE},
#'                    it is treated as a regular expression.
#'
#' @return A data cube (class \code{"raster_cube"}) with the updated paths.
#'
#' @note
#' This function only rewrites the recorded paths; it does not move or copy
#' any files. For the resulting cube to be readable, the new paths must point
#' to files that exist on disk. To copy the images themselves, see
#' \code{\link[sits]{sits_cube_copy}}.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     # rewrite the storage root of every image path
#'     new_cube <- sits_cube_replace_path(
#'         cube        = cube,
#'         pattern     = data_dir,
#'         replacement = data_dir
#'     )
#' }
#'
#' @family data cube management
#' @export
sits_cube_replace_path <- function(cube,
                                   pattern,
                                   replacement,
                                   column = "path",
                                   fixed = TRUE) {
    # set caller for error msgs
    .check_set_caller("sits_cube_replace_path")
    # pre-conditions
    .check_is_raster_cube(cube)
    # reset caller (the cube check above sets its own)
    .check_set_caller("sits_cube_replace_path")
    .check_chr_parameter(pattern, len_min = 1L, len_max = 1L)
    .check_chr_parameter(replacement, len_min = 1L, len_max = 1L)
    .check_chr_parameter(column, len_min = 1L, len_max = 1L)
    .check_lgl_parameter(fixed)
    # the target column must exist in the file_info of every tile
    .check_that(
        all(slider::slide_lgl(cube, function(tile) {
            column %in% names(.fi(tile))
        }))
    )
    # Replace the paths
    .cube_replace_path(
        cube        = cube,
        pattern     = pattern,
        replacement = replacement,
        column      = column,
        fixed       = fixed
    )
}
