# ---- source api ----
#' @title Create an items object in an DEAustralia cube
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
#' @export
.source_items_new.deaustralia_cube <- function(source, ...,
                                               collection,
                                               stac_query,
                                               tiles = NULL,
                                               platform = NULL) {
    .check_that(is.null(tiles))
    # Convert roi to bbox
    roi <- .stac_intersects_as_bbox(stac_query)
    stac_query[["params"]][["intersects"]] <- NULL
    stac_query[["params"]][["bbox"]] <- roi$bbox
    # making the request
    items_info <- rstac::post_request(q = stac_query, ...)
    .check_stac_items(items_info)
    # if more than 2 times items pagination are found the progress bar
    # is displayed
    progress <- rstac::items_matched(items_info) >
        2L * .conf("rstac_pagination_limit")
    # check documentation mode
    progress <- .message_progress(progress)
    # fetching all the metadata and updating to upper case instruments
    items_info <- rstac::items_fetch(items = items_info, progress = progress)
    # checks if the items returned any items
    .check_stac_items(items_info)
    return(items_info)
}

#' @title Create an items object in an DEAustralia cube
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
.deaustralia_cube_sentinel_2 <- function(source, ...,
                                         collection,
                                         stac_query,
                                         tiles = NULL) {
    # check spatial extensions
    if (!is.null(tiles)) {
        roi <- .s2_mgrs_to_roi(tiles)
        stac_query[["params"]][["intersects"]] <- NULL
        stac_query[["params"]][["bbox"]] <- c(
            roi[["lon_min"]],
            roi[["lat_min"]],
            roi[["lon_max"]],
            roi[["lat_max"]]
        )
    } else {
        roi <- .stac_intersects_as_bbox(stac_query)
        stac_query[["params"]][["intersects"]] <- NULL
        stac_query[["params"]][["bbox"]] <- roi$bbox
    }
    # make request
    items_info <- rstac::post_request(q = stac_query, ...)
    items_info <- rstac::items_fetch(items = items_info, progress = FALSE)
    # check results
    .check_stac_items(items_info)
    # done
    items_info
}

#' @keywords internal
#' @noRd
#' @export
`.source_items_new.deaustralia_cube_ga_s2am_ard_3` <- function(source, ...,
                                                               collection,
                                                               stac_query,
                                                               tiles = NULL) {
    # set caller to show in errors
    .check_set_caller(".source_items_new")
    # process sentinel-2 data
    .deaustralia_cube_sentinel_2(
        source = source,
        collection = collection,
        stac_query = stac_query,
        tiles = tiles,
        ...
    )
}

#' @keywords internal
#' @noRd
#' @export
`.source_items_new.deaustralia_cube_ga_s2bm_ard_3` <- function(source, ...,
                                                               collection,
                                                               stac_query,
                                                               tiles = NULL) {
    # set caller to show in errors
    .check_set_caller(".source_items_new")
    # process sentinel-2 data
    .deaustralia_cube_sentinel_2(
        source = source,
        collection = collection,
        stac_query = stac_query,
        tiles = tiles,
        ...
    )
}

#' @keywords internal
#' @noRd
#' @export
.source_items_tile.deaustralia_cube <- function(source, ...,
                                                items,
                                                collection = NULL) {
    rstac::items_reap(items, field = c("properties", "odc:region_code"))
}
