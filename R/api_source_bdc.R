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
    return(.stac_add_gdal_fs(href))
}
#' @keywords internal
#' @noRd
#' @export
.source_items_new.bdc_cube <- function(source, ...,
                                       collection,
                                       stac_query,
                                       tiles = NULL,
                                       platform = NULL) {
    # set caller to show in errors
    .check_set_caller(".source_items_new.bdc_cube")
    # if specified, a filter per tile is added to the query
    if (!is.null(tiles)) {
        stac_query <- rstac::ext_query(q = stac_query, "bdc:tile" %in% tiles)
    }
    # making the request
    items_info <- rstac::post_request(q = stac_query, ...)
    .check_stac_items(items_info)
    # if more than 2 times items pagination are found the progress bar
    # is displayed
    progress <- rstac::items_matched(items_info) > 2 * .conf("rstac_pagination_limit")
    # check documentation mode
    progress <- .check_documentation(progress)
    # fetching all the metadata
    items_info <- rstac::items_fetch(
        items = items_info,
        progress = progress, ...
    )

    return(items_info)
}

#' @keywords internal
#' @noRd
#' @export
.source_items_tile.bdc_cube <- function(source, ...,
                                        items,
                                        collection = NULL) {
    rstac::items_reap(items, field = c("properties", "bdc:tiles"))
}
