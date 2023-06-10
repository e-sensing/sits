

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
        sep_tile <- paste0("MGRS-", tiles)

        stac_query <-
            rstac::ext_query(
                q = stac_query,
                "grid:code" %in% sep_tile
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
        feature$properties$tile <- feature$properties[["grid:code"]]
        feature$properties$tile <- gsub("MGRS-","",feature$properties$tile)
        feature
    })

    rstac::items_reap(items, field = c("properties", "tile"))
}
