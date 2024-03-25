#' @title Create an items object in an DEAfrica cube
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
.source_items_new.deafrica_cube <- function(source, ...,
                                            collection,
                                            stac_query,
                                            tiles = NULL,
                                            platform = NULL) {
    # set caller to show in errors
    .check_set_caller(".source_items_new.deafrica_cube")

    # Convert roi to bbox
    lon <- stac_query$params$intersects$coordinates[, , 1]
    lat <- stac_query$params$intersects$coordinates[, , 2]
    stac_query$params$intersects <- NULL
    stac_query$params$bbox <- c(min(lon), min(lat), max(lon), max(lat))

    # making the request
    items_info <- rstac::post_request(q = stac_query, ...)
    .check_stac_items(items_info)
    # if more than 2 times items pagination are found the progress bar
    # is displayed
    progress <- rstac::items_matched(items_info) >
        2 * .conf("rstac_pagination_limit")
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
#' @title Create an items object in an DEAfrica cube
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
.source_items_new.deafrica_cube_s2_l2a <- function(source, ...,
                                                   collection,
                                                   stac_query,
                                                   tiles = NULL,
                                                   platform = NULL) {
    # set caller to show in errors
    .check_set_caller(".source_items_new.deafrica_cube")

    if (!is.null(tiles)) {
        roi <- .s2_mgrs_to_roi(tiles)
        stac_query$params$intersects <- NULL
        stac_query$params$bbox <- c(roi[["lon_min"]],
                                    roi[["lat_min"]],
                                    roi[["lon_max"]],
                                    roi[["lat_max"]]
        )
    } else {
        # Convert roi to bbox
        lon <- stac_query$params$intersects$coordinates[, , 1]
        lat <- stac_query$params$intersects$coordinates[, , 2]
        stac_query$params$intersects <- NULL
        stac_query$params$bbox <- c(min(lon), min(lat), max(lon), max(lat))
    }
    # making the request
    items_info <- rstac::post_request(q = stac_query, ...)
    .check_stac_items(items_info)
    # if more than 2 times items pagination are found the progress bar
    # is displayed
    progress <- rstac::items_matched(items_info) >
        2 * .conf("rstac_pagination_limit")
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
#' @title Organizes items by tiles for DEAfrica collections
#' @param source     Name of the STAC provider.
#' @param ...        Other parameters to be passed for specific types.
#' @param items      \code{STACItemcollection} object from rstac package.
#' @param collection Collection to be searched in the data source.
#' @return A list of items.
#' @keywords internal
#' @noRd
#' @export
.source_items_tile.deafrica_cube <- function(source, ...,
                                             items,
                                             collection = NULL) {
    rstac::items_reap(items, field = c("properties", "odc:region_code"))
}
