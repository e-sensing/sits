#' @keywords internal
#' @export
.source_item_get_date.usgs_cube <- function(source,
                                           item, ...,
                                           collection = NULL) {
    item[[c("properties", "datetime")]]
}

#' @keywords internal
#' @export
.source_item_get_hrefs.usgs_cube <- function(source,
                                            item, ...,
                                            collection = NULL) {

    unname(purrr::map_chr(item[["assets"]], `[[`, "href"))
}

#' @keywords internal
#' @export
.source_item_get_bands.usgs_cube <- function(source,
                                            item, ...,
                                            collection = NULL) {
    names(item[["assets"]])
}

#' @keywords internal
#' @export
.source_item_get_resolutions.usgs_cube <- function(source,
                                                  item, ...,
                                                  collection = NULL) {
    item[[c("properties", "eo:gsd")]]
}

#' @keywords internal
#' @export
.source_items_new.usgs_cube <- function(source,
                                       collection,
                                       name,
                                       bands,
                                       tiles,
                                       bbox,
                                       start_date,
                                       end_date, ...) {

    url <- .config_src_url(source = source)
    roi <- list(bbox = NULL, intersects = NULL)

    # obtain the datetime parameter for STAC like parameter
    datetime <- .sits_stac_datetime(start_date, end_date)

    # obtain the bounding box and intersects parameters
    if (!is.null(bbox))
        roi <- .sits_stac_roi(bbox)

    # get the limit items to be returned in each page
    limit_items <- .config_rstac_limit()

    # creating a rstac object
    rstac_query <-  rstac::stac_search(q = rstac::stac(url,
                                                       force_version = "0.9.0"),
                                       collections = collection,
                                       bbox       = roi$bbox,
                                       intersects = roi$intersects,
                                       datetime   = datetime,
                                       limit      = limit_items)

    # adding search filter in query
    rstac_query <- rstac::ext_query(q = rstac_query,
                                    "collection" %in% collection,
                                    "platform" %in% "LANDSAT_8",
                                    "landsat:collection_category" %in% "T1")

    # if specified, a filter per tile is added to the query
    if (!is.null(tiles)) {

        # format tile parameter provided by users
        sep_tile <- .usgs_format_tiles(tiles)

        # add filter by wrs path and row
        rstac_query <- rstac::ext_query(
            q = rstac_query,
            "landsat:wrs_path" %in% sep_tile$wrs_path,
            "landsat:wrs_row" %in% sep_tile$wrs_row
        )
    }

    # making the request
    items <- rstac::post_request(q = rstac_query, ...)

    # checks if the collection returned zero items
    assertthat::assert_that(!(rstac::items_length(items) == 0),
                            msg = paste(".sits_usgs_items: the provided search",
                                        "returned zero items.")
    )

    # filtering images by interval
    items_info <- .usgs_filter_datetime(items = items,
                                        start_date = start_date,
                                        end_date = end_date)

    # progress bar status
    pgr_fetch  <- FALSE

    # if more than 1000 items are found the progress bar is displayed
    if (rstac::items_matched(items = items_info,
                             matched_field = c("meta", "found")) > 1000)
        pgr_fetch <- TRUE

    # fetching all the metadata and updating to upper case instruments
    items_info <- rstac::items_fetch(items = items_info,
                                     progress = pgr_fetch,
                                     matched_field = c("meta", "found"))

    # store tile info in items object
    items_info$features <- purrr::map(items_info$features, function(features) {
        features$properties$tile <- paste0(
            features$properties[["landsat:wrs_path"]],
            features$properties[["landsat:wrs_row"]])

        features
    })

    return(items_info)
}

#' @title Format tile parameter provided by users
#' @name .sits_usgs_format_tiles
#' @keywords internal
#'
#' @param tiles     a \code{character} vector with the tiles provided by users.
#'
#' @return          a \code{tibble} with attributes of wrs path and row.
.usgs_format_tiles <- function(tiles) {

    # regex pattern of wrs_path and wrs_row
    pattern_l8 <- "[0-9]{6}"

    # verify tile pattern
    if (!any(grepl(pattern_l8, tiles, perl = TRUE)))
        stop(paste("The specified tiles do not match the Landsat-8 grid",
                   "pattern. See the user guide for more information."))

    # list to store the info about the tiles to provide the query in STAC
    list_tiles <- purrr::map(tiles, function(tile) {

        c(wrs_path = substring(tile, 1, 3),
          wrs_row = substring(tile, 4, 6))
    })

    # bind into a tibble all tiles
    tiles_tbl <- dplyr::bind_rows(list_tiles)

    return(tiles_tbl)
}

#' @title Filter datetime in STAC items
#' @name .sits_usgs_filter_datetime
#' @keywords internal
#'
#' @param items      a \code{STACItemCollection} object returned by rstac
#' package.
#' @param start_date a \code{character} with the initial date to search in
#' collection.
#' @param end_date   a \code{character} with the final date to searched in
#' collection.
#'
#' @return  a \code{STACItemCollection} object with datetime filtered.
.usgs_filter_datetime <- function(items, start_date, end_date) {

    # checks if the supplied tiles are in the searched items
    index_features <- purrr::map_lgl(items$features, function(feature) {
        datetime <- lubridate::date(feature[["properties"]][["datetime"]])

        if (datetime >= start_date && datetime <= end_date)
            return(TRUE)
        return(FALSE)
    })

    # select the tiles found in the search
    items$features <- items$features[index_features]

    items
}

#' @keywords internal
#' @export
.source_items_tiles_group.usgs_cube <- function(source,
                                               items, ...,
                                               collection = NULL) {

    rstac::items_group(items, field = c("properties", "tile"))
}

#' @keywords internal
#' @export
.source_items_get_sensor.usgs_cube <- function(source,
                                              items, ...,
                                              collection = NULL) {

    # OLI and TIRS returned, taking only OLI
    items[["features"]][[1]][[c("properties", "eo:instrument")]][[1]]
}

#' @keywords internal
#' @export
.source_items_get_satellite.usgs_cube <- function(source,
                                                 items, ...,
                                                 collection = NULL) {

    items[["features"]][[1]][[c("properties", "platform")]]
}

#' @keywords internal
#' @export
.source_items_tile_get_crs.usgs_cube <- function(source,
                                                tile_items, ...,
                                                collection = NULL) {

    # read the first image and obtain crs attribute
    params <- .sits_raster_api_params_file(
        tile_items[["features"]][[1]][["assets"]][[1]][["href"]]
    )

    # format collection crs
    crs <- .sits_format_crs(params[["crs"]])

    return(crs)
}

#' @keywords internal
#' @export
.source_items_tile_get_name.usgs_cube <- function(source,
                                                 tile_items, ...,
                                                 collection = NULL) {

    tile_items[["features"]][[1]][[c("properties", "tile")]]
}

#' @keywords internal
#' @export
.source_items_tile_get_bbox.usgs_cube <- function(source,
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
.source_items_tile_get_size.usgs_cube <- function(source,
                                                 tile_items, ...,
                                                 collection = NULL) {

    # read the first image and obtain the size parameters
    params <- .sits_raster_api_params_file(
        tile_items[["features"]][[1]][["assets"]][[1]][["href"]]
    )

    size <- c(nrows = params[["nrows"]], ncols = params[["ncols"]])

    return(size)
}
