#' @keywords internal
#' @export
.source_items_new.deafrica_cube <- function(source, ...,
                                            collection,
                                            stac_query,
                                            tiles = NULL) {

    # set caller to show in errors
    .check_set_caller(".source_items_new.deafrica_cube")

    if (!is.null(tiles)) {
        stop(paste("DEAFRICA cubes do not support searching for tiles, use",
                   "'roi' parameter instead.",
                   call. = FALSE
        ))
    }

    # making the request
    items_info <- rstac::post_request(q = stac_query, ...)
    # if more than 2 times items pagination are found the progress bar
    # is displayed
    progress <- rstac::items_matched(items_info) > 2 * .config_rstac_limit()

    # check documentation mode
    progress <- .check_documentation(progress)

    # fetching all the metadata and updating to upper case instruments
    items_info <- rstac::items_fetch(items = items_info, progress = progress)
    # checks if the items returned any items
    .check_that(
        x = rstac::items_length(items_info) != 0,
        msg = paste(
            "the provided search returned 0 items. Please, verify",
            "the provided parameters."
        )
    )
    return(items_info)
}

#' @keywords internal
#' @export
.source_items_tile.deafrica_cube <- function(source, ...,
                                             items,
                                             collection = NULL) {
    rstac::items_reap(items, field = c("properties", "odc:region_code"))
}
