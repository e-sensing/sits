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
    href <- unname(purrr::map_chr(item[["assets"]], `[[`, "href"))
    # BDC refuses an invalid token, so no token is sent when none is set
    access_key <- Sys.getenv("BDC_ACCESS_KEY")
    if (nzchar(access_key)) {
        href <- paste0(href, "?access_token=", access_key)
    }
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
#' @keywords internal
#' @noRd
#' @export
`.source_items_new.bdc_cube_landsat-2m` <- function(source, ...,
                                                    collection,
                                                    stac_query,
                                                    tiles = NULL,
                                                    platform = NULL) {
    # if specified, a filter per tile is added to the query
    if (!is.null(tiles)) {
        roi <- .s2_mgrs_to_roi(tiles)
        stac_query[["params"]][["intersects"]] <- NULL
        stac_query[["params"]][["bbox"]] <- c(
            roi[["lon_min"]],
            roi[["lat_min"]],
            roi[["lon_max"]],
            roi[["lat_max"]]
        )
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
#' @keywords internal
#' @noRd
#' @export
`.source_items_new.bdc_cube_landsat-1y` <- function(source, ...,
                                                    collection,
                                                    stac_query,
                                                    tiles = NULL,
                                                    platform = NULL) {
    # if specified, a filter per tile is added to the query
    if (!is.null(tiles)) {
        roi <- .s2_mgrs_to_roi(tiles)
        stac_query[["params"]][["intersects"]] <- NULL
        stac_query[["params"]][["bbox"]] <- c(
            roi[["lon_min"]],
            roi[["lat_min"]],
            roi[["lon_max"]],
            roi[["lat_max"]]
        )
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
#' @title Check the personal token of BDC
#' @name .source_check_token.bdc_cube
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @description BDC requires a personal token from `token_required_from`
#'   of its configuration. Without a token, a message is shown before that
#'   time and an error from it.
#' @param source     Data source.
#' @param collection Image collection.
#' @param ...        Not used.
#' @param now        Time of the check.
#' @return TRUE if no token is needed or it is set, else FALSE
#'   (invisible).
.source_check_token.bdc_cube <- function(source, collection, ...,
                                         now = Sys.time()) {
    token <- .try(
        .conf("sources", "BDC", "collections", collection, "token_vars"),
        .default = NULL
    )
    if (.has_not(token) || nzchar(Sys.getenv(token))) {
        return(invisible(TRUE))
    }
    token_url <- .conf("sources", "BDC", "token_url")
    required_from <- .conf("sources", "BDC", "token_required_from")
    # base R reads an offset as -0300, not -03:00
    required_time <- as.POSIXct(
        sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", required_from),
        format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"
    )
    if (now >= required_time) {
        stop(sprintf(
            .conf("messages", ".source_check_token_bdc_cube"),
            token_url, token
        ), call. = FALSE)
    }
    message(sprintf(
        .conf("messages", ".source_check_token_bdc_cube_from"),
        substr(required_from, 1L, 10L), token_url, token
    ))
    invisible(FALSE)
}
#' @keywords internal
#' @noRd
#' @export
`.source_items_tile.bdc_cube_amazonia-1` <- function(source, ...,
                                                     items,
                                                     collection = NULL) {
    paste0("NoTilingSystem-", seq_len(rstac::items_length(items)))
}
#' @keywords internal
#' @noRd
#' @export
`.source_items_tile.bdc_cube_landsat-2m` <- function(source, ...,
                                                     items,
                                                     collection = NULL) {
    id <- rstac::items_reap(items = items, field = "id")
    gsub("^.*([0-9]{3}[WE])_([0-9]{2}[NS]).*$", "\\1\\2", id)
}
#' @keywords internal
#' @noRd
#' @export
`.source_items_tile.bdc_cube_landsat-1y` <- function(source, ...,
                                                     items,
                                                     collection = NULL) {
    id <- rstac::items_reap(items = items, field = "id")
    gsub("^.*([0-9]{3}[WE])_([0-9]{2}[NS]).*$", "\\1\\2", id)
}
#' @keywords internal
#' @noRd
#' @export
`.source_filter_tiles.bdc_cube_landsat-2m` <- function(source,
                                                       collection,
                                                       cube,
                                                       tiles) {
    cube
}
#' @keywords internal
#' @noRd
#' @export
`.source_filter_tiles.bdc_cube_landsat-1y` <- function(source,
                                                       collection,
                                                       cube,
                                                       tiles) {
    cube
}
