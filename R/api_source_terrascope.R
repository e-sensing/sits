# ---- source api ----
#' @title Transform an items object in an TerraScope (World Cover) cube
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
`.source_items_new.terrascope_cube_world-cover-2021` <- function(source, ...,
                                              collection,
                                              stac_query,
                                              tiles = NULL,
                                              platform = NULL) {
    # set caller to show in errors
    .check_set_caller(".source_items_new_terrascope_cube")
    # convert roi to bbox
    roi <- .stac_intersects_as_bbox(stac_query)
    # update stac query with the new spatial reference
    stac_query[["params"]][["intersects"]] <- NULL
    stac_query[["params"]][["bbox"]] <- roi[["bbox"]]
    # world cover product has all data available for a single date. So, fix the
    # temporal interval from the query
    stac_query[["params"]][["datetime"]] <- "2022-06-01T00:00:00Z"
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
    items_info <- rstac::items_fetch(items = items_info, progress = FALSE)
    # checks if the items returned any items
    .check_stac_items(items_info)
    return(items_info)
}

#' @keywords internal
#' @noRd
#' @export
`.source_items_tile.terrascope_cube_world-cover-2021` <- function(source, ...,
                                               items,
                                               collection = NULL) {
    rstac::items_reap(items, field = c("properties", "title")) |>
        purrr::map_chr(function(property) {
            # extract date from the filename
            stringr::str_split(property, "_")[[1]][[6]]
        })
}
