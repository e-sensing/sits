#' @title Convert a data cube into a Spatial Raster object from terra
#' @name sits_as_terra
#' @author Gilberto Camara, \email{gilberto.camara.inpe@@gmail.com}
#'
#' @description Uses the information about files, bands and dates
#' in a data cube to produce an object of class \code{terra}.
#' User has to select a tile and a date from the data cube. By default,
#' all bands are included in the \code{terra} object.
#' Users can select bands.
#'
#' @param cube   A sits cube.
#' @param tile   Tile of the data cube.
#' @param ...    Other parameters for specific types of data cubes.
#' @param bands  Bands of the data cube to be part of \code{terra} object.
#' @param date   Date of the data cube to be part of \code{terra} object.
#' @return       An Spatial Raster object from \code{terra}.
#'
#' @examples
#' if (sits_run_examples()) {
#'
#'     # convert sits cube to an sf object (polygon)
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     spat_raster <- sits_as_terra(cube)
#' }
#' @export
sits_as_terra <- function(cube,
                          tile = cube[1,]$tile,
                          ...){
    # Pre-conditions
    .check_set_caller("sits_as_terra")
    .check_is_raster_cube(cube)
    .check_chr_parameter(tile, len_max = 1)
    .check_chr_contains(cube[["tile"]], contains = tile,
                        discriminator = "any_of",
                        msg = .conf("messages", "sits_as_terra_tile"))

    UseMethod("sits_as_terra", cube)
}
#' @rdname sits_as_terra
#' @export
sits_as_terra.raster_cube <- function(cube,
                                      tile = cube[1,]$tile,
                                      ...,
                                      bands = NULL,
                                      date = NULL){
    # extract tile from cube
    tile_cube <- .cube_filter_tiles(cube, tile)
    # get file info for tile
    fi <- .fi(tile_cube)

    # filter bands
    if (.has(bands)) {
        .check_cube_bands(tile_cube, bands)
        fi <- .fi_filter_bands(fi, bands)
    } else
        bands <- .tile_bands(tile_cube)

    # filter dates
    if (.has(date))
        .check_dates_timeline(date, tile_cube)
    else
        date <- as.Date(.tile_timeline(tile_cube)[[1]])

    fi <- .fi_filter_dates(fi, date)

    # retrieve files
    image_files <- .fi_paths(fi)

    # export spatial raster
    spatial_raster <- terra::rast(image_files)

    return(spatial_raster)
}
#' @rdname sits_as_terra
#' @export
sits_as_terra.probs_cube <- function(cube,
                                     tile = cube[1,]$tile,
                                      ...){
    # extract tile from cube
    tile_cube <- .cube_filter_tiles(cube, tile)
    # get file info for tile
    fi <- .fi(tile_cube)
    # retrieve file
    image_file <- .fi_paths(fi)
    # export spatial raster
    spatial_raster <- terra::rast(image_file)
    # get all labels
    labels <- .tile_labels(tile_cube)
    # save names in terra object
    names(spatial_raster) <- labels
    # return
    return(spatial_raster)
}
#' @rdname sits_as_terra
#' @export
sits_as_terra.class_cube <- function(cube,
                                     tile = cube[1,]$tile,
                                     ...){
    # extract tile from cube
    tile_cube <- .cube_filter_tiles(cube, tile)
    # get file info for tile
    fi <- .fi(tile_cube)
    # retrieve file
    image_file <- .fi_paths(fi)
    # create spatial raster
    spatial_raster <- terra::rast(image_file)
    # get all labels
    labels <- .tile_labels(tile_cube)
    # set levels for raster
    terra_levels <- data.frame(
        id = as.numeric(names(labels)),
        cover = unname(labels)
    )
    levels(spatial_raster) <- terra_levels
    # return
    return(spatial_raster)
}
