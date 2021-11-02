#' @keywords internal
#' @export
.source_collection_access_test.mspc_cube <- function(source, ...,
                                                     collection, bands,
                                                     dry_run = TRUE) {
    # require package
    if (!requireNamespace("rstac", quietly = TRUE)) {
        stop("Please install package rstac", call. = FALSE
        )
    }

    items_query <- .stac_items_query(source = source,
                                     collection = collection,
                                     limit = 1)

    # assert that service is online
    tryCatch({
        items <- rstac::post_request(items_query, ...)
    }, error = function(e) {
        stop(paste(".source_collection_access_test.stac_cube: service is unreachable\n",
                   e$message), call. = FALSE)
    })

    items <- rstac::items_sign(items,
                               sign_fn = rstac::sign_planetary_computer())

    items <- .source_items_bands_select(source = source, ...,
                                        collection = collection,
                                        items = items,
                                        bands = bands[[1]])

    href <- .source_item_get_hrefs(source = source, ...,
                                   item = items$feature[[1]],
                                   collection = collection)

    # assert that token and/or href is valid
    if (dry_run)
        tryCatch({
            .raster_open_rast(href)
        }, error = function(e) {
            stop(paste(".source_collection_access_test.stac_cube: cannot",
                       "open url\n", href, "\n", e$message), call. = FALSE)
        })


    return(invisible(NULL))
}

#' @keywords internal
#' @export
`.source_items_new.mspc_cube_sentinel-2-l2a` <- function(source, ...,
                                                         collection,
                                                         stac_query,
                                                         tiles = NULL) {

    # set caller to show in errors
    .check_set_caller(".source_items_new.mspc_cube")

    # if specified, a filter per tile is added to the query
    if (!is.null(tiles))

        stac_query <- rstac::ext_query(
            q = stac_query, "s2:mgrs_tile" == tiles
        )

    # making the request
    items_info <- rstac::post_request(q = stac_query, ...)

    # assign href
    items_info <- rstac::items_sign(items_info,
                                    sign_fn = rstac::sign_planetary_computer())

    # check if matched items
    .check_that(
        x = rstac::items_matched(items_info) > 0,
        msg = "no items matched the query criteria."
    )

    # if more than 2 times items pagination are found the progress bar
    # is displayed
    pgr_fetch <- rstac::items_matched(items_info) > 2 * .config_rstac_limit()

    # fetching all the metadata
    #items_info <- rstac::items_fetch(items = items_info, progress = pgr_fetch)

    return(items_info)
}

#' @keywords internal
#' @export
`.source_items_tiles_group.mspc_cube_sentinel-2-l2a` <- function(source, ...,
                                                                 items,
                                                                 collection = NULL) {

    rstac::items_group(items, field = c("properties", "s2:mgrs_tile"))
}

#' @keywords internal
#' @export
`.source_items_tile_get_name.mspc_cube_sentinel-2-l2a` <- function(source, ...,
                                                                   tile_items,
                                                                   collection = NULL) {

    tile_items[["features"]][[1]][[c("properties", "s2:mgrs_tile")]]
}

#' @keywords internal
#' @export
`.source_items_tile_get_crs.mspc_cube_sentinel-2-l2a` <- function(source,...,
                                                                  tile_items,
                                                                  collection = NULL) {

    # format collection crs
    crs <- .sits_proj_format_crs(
        tile_items[["features"]][[1]][[c("properties", "proj:epsg")]]
    )

    return(crs)
}

#' @keywords internal
#' @export
`.source_items_new.mspc_cube_landsat-8-c2-l2` <- function(source, ...,
                                                          collection,
                                                          stac_query,
                                                          tiles = NULL) {

    # set caller to show in errors
    .check_set_caller(".source_items_new.mspc_cube_landsat-8-c2-l2")

    # if specified, a filter per tile is added to the query
    if (!is.null(tiles)) {

        # format tile parameter provided by users
        sep_tile <- .usgs_format_tiles(tiles)

        # add filter by wrs path and row
        stac_query <- rstac::ext_query(
            q = stac_query,
            "landsat:wrs_path" == sep_tile$wrs_path,
            "landsat:wrs_row" == sep_tile$wrs_row
        )
    }

    # making the request
    items <- rstac::post_request(q = stac_query, ...)

    # assign href
    items <- rstac::items_sign(items,
                                    sign_fn = rstac::sign_planetary_computer())

    # checks if the collection returned zero items
    .check_that(
        x = !(rstac::items_length(items) == 0),
        msg = "the provided search returned zero items."
    )

    #pgr_fetch <- rstac::items_matched(items_info) > 2 * .config_rstac_limit()


    # fetching all the metadata and updating to upper case instruments
    #items_info <- rstac::items_fetch(items = items,
    #                                 progress = pgr_fetch,
    #                                 matched_field = c("meta", "found"))
    return(items)
}

#' @keywords internal
#' @export
`.source_items_tiles_group.mspc_cube_landsat-8-c2-l2` <- function(source, ...,
                                                                  items,
                                                                  collection = NULL) {

    # store tile info in items object
    items$features <- purrr::map(items$features, function(feature) {
        feature$properties$tile <- paste0(
            feature$properties[["landsat:wrs_path"]],
            feature$properties[["landsat:wrs_row"]]
        )

        feature
    })

    rstac::items_group(items, field = c("properties", "tile"))
}

#' @keywords internal
#' @export
`.source_items_tile_get_crs.mspc_cube_landsat-8-c2-l2` <- function(source,
                                                                   tile_items, ...,
                                                                   collection = NULL) {

    epsg_code <- tile_items[["features"]][[1]][[c("properties", "proj:epsg")]]
    # format collection crs
    crs <- .sits_proj_format_crs(epsg_code)

    return(crs)
}
