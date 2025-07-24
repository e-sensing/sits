#' @title Convert a data cube into a stars object
#' @name sits_as_stars
#' @author Gilberto Camara, \email{gilberto.camara.inpe@@gmail.com}
#'
#' @description Uses the information about files, bands and dates
#' in a data cube to produce an object of class \code{stars}.
#' User has to select a tile from the data cube. By default,
#' all bands and dates are included in the \code{stars} object.
#' Users can select bands and dates.
#'
#' @param cube   A sits cube.
#' @param tile   Tile of the data cube.
#' @param bands  Bands of the data cube to be part of \code{stars} object.
#' @param dates  Dates of the data cube to be part of \code{stars} object.
#' @param proxy  Produce a stars proxy object.
#' @return       An space-time stars object.
#'
#' @note
#' By default, the \code{stars} object will be loaded in memory. This
#' can result in heavy memory usage. To produce a \code{stars.proxy} object,
#' uses have to select a single date, since \code{stars} does not allow
#' proxy objects to be created with two dimensions.
#' @examples
#' if (sits_run_examples()) {
#'     # convert sits cube to an sf object (polygon)
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     stars_object <- sits_as_stars(cube)
#' }
#' @export
sits_as_stars <- function(cube,
                          tile = cube[1L, ]$tile,
                          bands = NULL,
                          dates = NULL,
                          proxy = FALSE) {
    # Pre-conditions
    .check_set_caller("sits_as_stars")
    .check_is_raster_cube(cube)
    .check_chr_parameter(tile, len_max = 1L)
    .check_chr_contains(cube[["tile"]],
        contains = tile,
        discriminator = "any_of",
        msg = .conf("messages", "sits_as_stars_tile")
    )
    .check_lgl_parameter(proxy)

    # extract tile from cube
    tile_cube <- .cube_filter_tiles(cube, tile)
    # get file info for tile
    fi <- .fi(tile_cube)
    # filter bands
    if (.has(bands)) {
        .check_cube_bands(tile_cube, bands)
        fi <- .fi_filter_bands(fi, bands)
    } else {
        bands <- .tile_bands(tile_cube)
    }
    # filter dates
    if (.has(dates)) {
        # proxy? only one date is retrieved
        if (proxy) {
            dates <- dates[[1L]]
        }
        .check_dates_timeline(dates, tile_cube)
        fi <- .fi_filter_dates(fi, dates)
    } else {
        dates <- as.Date(.tile_timeline(tile_cube))
    }


    # retrieve files
    image_files <- .fi_paths(fi)

    # proxy? only one dimension (bands)
    if (proxy) {
        stars_obj <- stars::read_stars(
            image_files,
            along = "band",
            proxy = TRUE
        )
    } else {
        stars_obj <- stars::read_stars(
            image_files,
            along = list(
                band = bands,
                time = dates
            )
        )
    }
    return(stars_obj)
}
