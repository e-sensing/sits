#' @keywords internal
#' @export
.source_items_new.deafrica_cube <- function(source, ...,
                                            collection,
                                            stac_query,
                                            tiles = NULL) {

    # set caller to show in errors
    .check_set_caller(".source_items_new.deafrica_cube")


    # searching for tiles in the items
    if (!is.null(tiles))
        stop(paste("DEAFRICA cubes do not support searching for tiles, use",
                   "'roi' parameter instead.", call. = FALSE)
        )

    # making the request
    items_info <- rstac::post_request(q = stac_query, ...)

    # if more than 2 times items pagination are found the progress bar
    # is displayed
    pgr_fetch <- rstac::items_matched(items_info) > 2 * .config_rstac_limit()

    # fetching all the metadata and updating to upper case instruments
    items_info <- rstac::items_fetch(items = items_info, progress = pgr_fetch)

    # checks if the items returned any items
    .check_that(
        x = rstac::items_length(items_info) != 0,
        msg = paste("the provided search returned 0 items. Please, verify",
                    "the provided parameters.")
    )

    return(items_info)
}

#' @keywords internal
#' @export
.source_items_tiles_group.deafrica_cube <- function(source, ...,
                                                    items,
                                                    collection = NULL) {

    rstac::items_group(items, field = c("properties", "odc:region_code"))
}

#' @keywords internal
#' @export
.source_items_tile_get_crs.deafrica_cube <- function(source, ...,
                                                     tile_items,
                                                     collection = NULL) {

    # format collection crs
    crs <- .sits_proj_format_crs(
        tile_items[["features"]][[1]][[c("properties", "proj:epsg")]]
    )

    return(crs)
}

#' @keywords internal
#' @export
.source_items_tile_get_name.deafrica_cube <- function(source, ...,
                                                      collection,
                                                      tile_items) {

    tile_items[["features"]][[1]][[c("properties", "odc:region_code")]]
}
