#' @title Verify items tiles
#' @name .aws_tiles
#' @keywords internal
#' @noRd
#' @param tiles  Tile names to be searched.
#'
#' @return a \code{tibble} with information of tiles to be searched in STAC AWS.
.aws_tiles <- function(tiles) {

    # regex pattern
    pattern_s2 <- "[0-9]{2}[A-Z]{3}"

    # verify tile pattern
    if (!any(grepl(pattern_s2, tiles, perl = TRUE))) {
        stop(paste(
            "The specified tiles do not match the Sentinel-2A grid",
            "pattern. See the user guide for more information."
        ))
    }

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
#' @noRd
#' @export
.source_items_new.aws_cube <- function(source,
                                       collection,
                                       stac_query, ...,
                                       tiles = NULL,
                                       platform = NULL) {

    # set caller to show in errors
    .check_set_caller(".source_items_new.aws_cube")

    if (!is.null(platform)) {
        platform <- .stac_format_platform(
            source = source,
            collection = collection,
            platform = platform
        )

        stac_query <- rstac::ext_query(
            q = stac_query,
            "platform" == platform
        )
    }

    # if specified, a filter per tile is added to the query
    if (!is.null(tiles)) {
        sep_tile <- .aws_tiles(tiles)

        stac_query <-
            rstac::ext_query(
                q = stac_query,
                "sentinel:utm_zone" %in% sep_tile$utm_zone,
                "sentinel:latitude_band" %in% sep_tile$lat_band,
                "sentinel:grid_square" %in% sep_tile$grid_square
            )
    }

    # making the request
    items_info <- rstac::post_request(q = stac_query, ...)
    .check_stac_items(items_info)

    # if more than 2 times items pagination are found the progress bar
    # is displayed
    progress <- rstac::items_matched(items_info) > 2 *
        .conf("rstac_pagination_limit")
    # check documentation mode
    progress <- .check_documentation(progress)
    # fetching all the metadata
    items_info <- rstac::items_fetch(
        items = items_info,
        progress = progress
    )

    return(items_info)
}

#' @keywords internal
#' @noRd
#' @export
.source_items_tile.aws_cube <- function(source,
                                        items, ...,
                                        collection = NULL) {

    # store tile info in items object
    items$features <- purrr::map(items$features, function(feature) {
        feature$properties$tile <- paste0(
            feature$properties[["sentinel:utm_zone"]],
            feature$properties[["sentinel:latitude_band"]],
            feature$properties[["sentinel:grid_square"]]
        )

        feature
    })

    rstac::items_reap(items, field = c("properties", "tile"))
}
