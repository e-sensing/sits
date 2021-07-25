#' @title Search items tiles
#' @name .deafrica_search_tiles
#' @keywords internal
#'
#' @param items a \code{STACItemCollection} object returned by rstac package.
#'  grouped.
#' @param tiles a \code{character} with the names of the tiles.
#'
#' @return      a \code{STACItemCollection} object representing the search
#'              by rstac.
.deafrica_search_tiles <- function(items, tiles) {

    # checks if the supplied tiles are in the searched items
    index_features <- purrr::map_lgl(items$features, function(feature) {
        region_code <- feature[["properties"]][["odc:region_code"]]
        if (region_code %in% tiles)
            return(TRUE)
        return(FALSE)
    })

    # selects the tiles found in the search
    items$features <- items$features[index_features]

    # checks if the search return zero items
    assertthat::assert_that(
        rstac::items_length(items) != 0,
        msg = paste(".deafrica_search_tiles: the supplied tile(s) were",
                    "not found.")
    )

    return(items)
}

#' @keywords internal
#' @export
.source_item_get_date.deafrica_cube <- function(source,
                                                item, ...,
                                                collection = NULL) {
    item[[c("properties", "datetime")]]
}

#' @keywords internal
#' @export
.source_item_get_hrefs.deafrica_cube <- function(source,
                                                 item, ...,
                                                 collection = NULL) {


    href <- unname(purrr::map_chr(item[["assets"]], `[[`, "href"))

    # add gdal vsi in href urls
    return(.stac_add_gdal_vsi(href))
}

#' @keywords internal
#' @export
.source_item_get_bands.deafrica_cube <- function(source,
                                                 item, ...,
                                                 collection = NULL) {
    names(item[["assets"]])
}

#' @keywords internal
#' @export
.source_item_get_resolutions.deafrica_cube <- function(source,
                                                       item, ...,
                                                       collection = NULL) {
    item[[c("properties", "gsd")]]
}

#' @keywords internal
#' @export
.source_items_new.deafrica_cube <- function(source,
                                            collection, ...,
                                            stac_query,
                                            tiles = NULL) {

    # making the request
    items_info <- rstac::post_request(q = stac_query, ...)

    # if more than 2 times items pagination are found the progress bar
    # is displayed
    pgr_fetch <- rstac::items_matched(items_info) > 2 * .config_rstac_limit()

    # fetching all the metadata and updating to upper case instruments
    items_info <- rstac::items_fetch(items = items_info, progress = pgr_fetch)

    # searching for tiles in the items
    if (!is.null(tiles))
        items_info <- .deafrica_search_tiles(items_info, tiles)

    # checks if the items returned any items
    assertthat::assert_that(
        rstac::items_length(items_info) != 0,
        msg = paste(".source_items_new.deafrica_cube: the provided search",
                    "returned 0 items. Please, verify the provided parameters.")
    )

    return(items_info)
}

#' @keywords internal
#' @export
.source_items_tiles_group.deafrica_cube <- function(source,
                                                    items, ...,
                                                    collection = NULL) {

    rstac::items_group(items, field = c("properties", "odc:region_code"))
}

#' @keywords internal
#' @export
.source_items_get_sensor.deafrica_cube <- function(source,
                                                   items, ...,
                                                   collection = NULL) {

    items[["features"]][[1]][[c("properties", "instruments")]]
}

#' @keywords internal
#' @export
.source_items_get_satellite.deafrica_cube <- function(source,
                                                      items, ...,
                                                      collection = NULL) {

    items[["features"]][[1]][[c("properties", "platform")]]
}

#' @keywords internal
#' @export
.source_items_tile_get_crs.deafrica_cube <- function(source,
                                                     tile_items, ...,
                                                     collection = NULL) {

    tile_items[["features"]][[1]][[c("properties", "proj:epsg")]]
}

#' @keywords internal
#' @export
.source_items_tile_get_name.deafrica_cube <- function(source,
                                                      tile_items, ...,
                                                      collection = NULL) {

    tile_items[["features"]][[1]][[c("properties", "odc:region_code")]]
}

#' @keywords internal
#' @export
.source_items_tile_get_bbox.deafrica_cube <- function(source,
                                                      tile_items, ...,
                                                      collection = NULL) {

    # get collection crs
    crs <- .source_items_tile_get_crs(source = source,
                                      tile_items = tile_items,
                                      collection = collection)

    bbox <- .sits_stac_get_bbox(tile_items, crs)

    return(bbox)
}

#' @keywords internal
#' @export
.source_items_tile_get_size.deafrica_cube <- function(source,
                                                      tile_items, ...,
                                                      collection = NULL) {

    size <- tile_items[["features"]][[1]][[c("properties", "proj:shape")]]

    names(size) <- c("nrows", "ncols")
    return(size)
}
