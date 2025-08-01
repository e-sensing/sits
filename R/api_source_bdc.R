#' @title Retrieves the paths or URLs of each file bands of an item for BDC
#' @param source     Name of the STAC provider.
#' @param ...        Other parameters to be passed for specific types.
#' @param item       \code{STACItemcollection} object from rstac package.
#' @param collection Collection to be searched in the data source.
#' @return Returns paths to each image band of an item.
#' @keywords internal
#' @noRd
#' @export
.source_item_get_hrefs.bdc_cube <- function(source, ...,
                                            item,
                                            collection = NULL) {
    access_key <- Sys.getenv("BDC_ACCESS_KEY")

    href <- paste0(
        unname(purrr::map_chr(item[["assets"]], `[[`, "href")),
        "?access_token=", access_key
    )
    # add gdal vsi in href urls
    .stac_add_gdal_fs(href)
}
#' @title Create an items object in a BDC cube
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
#' @param platform   Satellite platform (optional).
#' @return An object referring the images of a sits cube.
#'
#' @export
.source_items_new.bdc_cube <- function(source, ...,
                                       collection,
                                       stac_query,
                                       tiles = NULL,
                                       platform = NULL) {
    # if specified, a filter per tile is added to the query
    if (!is.null(tiles)) {
        stac_query <- rstac::ext_query(q = stac_query, "bdc:tile" %in% tiles)
    }
    # making the request
    items_info <- rstac::post_request(q = stac_query, ...)
    .check_stac_items(items_info)
    # if more than 2 times items pagination are found the progress bar
    # is displayed
    progress <- rstac::items_matched(items_info) >
        2L * .conf("rstac_pagination_limit")
    # check documentation mode
    progress <- .message_progress(progress)
    # fetching all the metadata
    rstac::items_fetch(
        items = items_info,
        progress = progress, ...
    )
}
#' @title Organizes items by tiles for BDC collections
#' @param source     Name of the STAC provider.
#' @param ...        Other parameters to be passed for specific types.
#' @param items      \code{STACItemcollection} object from rstac package.
#' @param collection Collection to be searched in the data source.
#' @return A list of STAC items.
#' @keywords internal
#' @noRd
#' @export
.source_items_tile.bdc_cube <- function(source, ...,
                                        items,
                                        collection = NULL) {
    rstac::items_reap(items, field = c("properties", "bdc:tiles"))
}
#' @noRd
#' @title Configure access.
#' @param source  Data source
#' @param collection Image collection
#' @return Called for side effects
.source_configure_access.bdc_cube <- function(source, collection = NULL) {
    bdc_access_key <- Sys.getenv("BDC_ACCESS_KEY")
    if (!nzchar(bdc_access_key)) {
        Sys.setenv(BDC_ACCESS_KEY = .conf("BDC_ACCESS_KEY"))
    }
    return(invisible(source))
}
