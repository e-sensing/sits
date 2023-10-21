#' @title Vector info API
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description
#' Set of functions for handling `vector_info`.
#'
NULL

#' @title Get `vector_info` from a given tile.
#' @noRd
#' @param tile  A tile.
#' @returns A `vector_info` tibble.
.vi <- function(tile) {
    vi <- tile[["vector_info"]][[1]]
    vi
}
#' @title Set `vector_info` into a given tile.
#' @noRd
#' @param tile  A tile.
#' @param value  A `vector_info` to be set.
#' @returns An updated tile tibble.
`.vi<-` <- function(tile, value) {
    tile <- .tile(tile)
    tile[["vector_info"]] <- list(value)
    tile
}

.vi_derived <- function(band, start_date, end_date, xmin, xmax, ymin,
                        ymax, path) {
    # Create a new derived file_info
    tibble::tibble(
        band = .band_derived(band),
        start_date = start_date,
        end_date = end_date,
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        path = path
    )
}

.vi_segment_from_file <- function(file, base_tile, band, start_date, end_date) {
    file <- .file_normalize(file)
    v_obj <- .vector_read_vec(file_path = file)
    bbox <- .vector_bbox(v_obj)
    .vi_derived(
        band = band,
        start_date = start_date,
        end_date = end_date,
        xmin = bbox[["xmin"]],
        xmax = bbox[["xmax"]],
        ymin = bbox[["ymin"]],
        ymax = bbox[["ymax"]],
        path = file
    )
}
