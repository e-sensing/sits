#' @title Verify items tiles
#' @name .aws_tiles
#' @keywords internal
#'
#' @param tiles  Tile names to be searched.
#'
#' @return a \code{tibble} with information of tiles to be searched in STAC AWS.
.aws_tiles <- function(tiles) {

    # regex pattern
    pattern_s2 <- "[0-9]{2}[A-Z]{3}"

    # verify tile pattern
    if (!any(grepl(pattern_s2, tiles, perl = TRUE)))
        stop(paste("The specified tiles do not match the Sentinel-2A grid",
                   "pattern. See the user guide for more information."))

    # list to store the info about the tiles to provide the query in STAC
    tiles_tbl <- purrr::map_dfr(tiles, function(tile) {
        tile_aws <- tibble::tibble(
            utm_zone = substring(tile, 1, 2),
            lat_band = substring(tile, 3, 3),
            grid_square = substring(tile, 4, 5)
        )
    })

    return(tiles_tbl)
}

#' @keywords internal
#' @export
.source_items_new.aws_cube <- function(source,
                                       collection,
                                       stac_query, ...,
                                       tiles = NULL) {

    # set caller to show in errors
    .check_set_caller(".source_items_new.aws_cube")

    # if specified, a filter per tile is added to the query
    if (!is.null(tiles)) {
        sep_tile <- .aws_tiles(tiles)

        stac_query <-
            rstac::ext_query(q = stac_query,
                             "sentinel:utm_zone" %in% sep_tile$utm_zone,
                             "sentinel:latitude_band" %in% sep_tile$lat_band,
                             "sentinel:grid_square" %in% sep_tile$grid_square)
    }

    # making the request
    items_info <- rstac::post_request(q = stac_query, ...)

    # check if matched items
    .check_that(
        x = rstac::items_matched(items_info) > 0,
        msg = "no items matched the query criteria."
    )

    # if more than 2 times items pagination are found the progress bar
    # is displayed
    pgr_fetch <- rstac::items_matched(items_info) > 2 * .config_rstac_limit()

    # fetching all the metadata
    items_info <- rstac::items_fetch(items = items_info, progress = pgr_fetch)

    return(items_info)
}

#' @keywords internal
#' @export
.source_items_tile.aws_cube <- function(source,
                                         items, ...,
                                         collection = NULL) {

    # store tile info in items object
    items$features <- purrr::map(items$features, function(feature) {
        feature$properties$tile <- paste0(
            feature$properties[["sentinel:utm_zone"]],
            feature$properties[["sentinel:latitude_band"]],
            feature$properties[["sentinel:grid_square"]])

        feature
    })

    rstac::items_reap(items, field = c("properties", "tile"))
}

#' @keywords internal
#' @export
.source_items_tile_get_crs.aws_cube <- function(source,
                                                tile_items, ...,
                                                collection = NULL) {

    # format collection crs
    crs <- .sits_proj_format_crs(
        tile_items[["features"]][[1]][[c("properties", "proj:epsg")]]
    )

    return(crs)
}
