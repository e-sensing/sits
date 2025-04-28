#' @title Create an items object in SDC
#' @keywords internal
#' @noRd
#' @description \code{.source_items_new()} this function is called to create
#' an items object. In case of Web services, this function is responsible for
#' making the Web requests to the server.
#' @param source     Name of the STAC provider.
#' @param ...        Other parameters to be passed for specific types.
#' @param collection Collection to be searched in the data source.
#' @param stac_query Query that follows the STAC protocol
#' @param tiles      Selected tiles (optional)
#' @return An object referring the images of a sits cube.
#' @export
.source_items_new.sdc_cube <- function(source, ...,
                                       collection,
                                       stac_query,
                                       tiles = NULL) {
    # making the request
    items_info <- rstac::post_request(q = stac_query, ...)
    .check_stac_items(items_info)
    # if more than 2 times items pagination are found the progress bar
    # is displayed
    progress <- rstac::items_matched(items_info) > 2L *
        .conf("rstac_pagination_limit")
    # check documentation mode
    progress <- .message_progress(progress)
    # fetching all the metadata and updating to upper case instruments
    items_info <- rstac::items_fetch(items = items_info, progress = progress)
    # checks if the items returned any items
    .check_stac_items(items_info)
    return(items_info)
}
#' @title Get cloud cover information in SDC
#' @keywords internal
#' @noRd
#' @description \code{.source_items_new()} this function is called to create
#' an items object. In case of Web services, this function is responsible for
#' making the Web requests to the server.
#' @param source     Name of the STAC provider.
#' @param ...        Other parameters to be passed for specific types.
#' @param item      \code{STACItemcollection} object from rstac package.
#' @param collection Collection to be searched in the data source.
#' @return NA (SDC does not support cloud cover information)
#' @export
#' @keywords internal
#' @export
.source_item_get_cloud_cover.sdc_cube <- function(source, ...,
                                                  item,
                                                  collection = NULL) {
    return(NA)
}
#' @title Organizes items by tiles for SDC  collections
#' @param source     Name of the STAC provider.
#' @param ...        Other parameters to be passed for specific types.
#' @param items      \code{STACItemcollection} object from rstac package.
#' @param collection Collection to be searched in the data source.
#' @return A list of items.
#' @keywords internal
#' @noRd
#' @export
.source_items_tile.sdc_cube <- function(source, ...,
                                        items,
                                        collection = NULL) {
    gsub(
        pattern = "_",
        replacement = "-",
        fixed = TRUE,
        x = rstac::items_reap(items,
            field = c("properties", "cubedash:region_code")
        )
    )
}
#' @title Check if roi or tiles are provided
#' @param source        Data source
#' @param roi           Region of interest
#' @param tiles         Tiles to be included in cube
#' @return Called for side effects.
#' @keywords internal
#' @noRd
#' @export
.source_roi_tiles.sdc_cube <- function(source, roi, tiles) {
    .check_set_caller(".source_roi_tiles_sdc_cube")
    .check_that(.has_not(tiles))
    invisible(source)
}
