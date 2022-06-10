#' @keywords internal
#' @export
.source_collection_access_test.mspc_cube <- function(source,
                                                     collection,
                                                     bands, ...,
                                                     start_date = NULL,
                                                     end_date = NULL,
                                                     dry_run = TRUE) {
    # require package
    .check_require_packages("rstac")

    items_query <- .stac_create_items_query(
        source = source,
        collection = collection,
        start_date = start_date,
        end_date = end_date,
        limit = 1
    )
    # assert that service is online
    tryCatch(
        {
            items <- rstac::post_request(items_query, ...)
        },
        error = function(e) {
            stop(paste(
                ".source_collection_access_test.stac_cube: service is",
                "unreachable\n", e$message
            ), call. = FALSE)
        }
    )

    # signing the url with the mspc token
    items <- suppressWarnings(
        rstac::items_sign(items, sign_fn = rstac::sign_planetary_computer())
    )

    items <- .source_items_bands_select(
        source = source,
        items = items,
        bands = bands[[1]],
        collection = collection, ...
    )

    href <- .source_item_get_hrefs(
        source = source,
        item = items$feature[[1]],
        collection = collection, ...
    )

    # assert that token and/or href is valid
    if (dry_run) {
        tryCatch(
            {
                .raster_open_rast(href)
            },
            error = function(e) {
                stop(paste(
                    ".source_collection_access_test.stac_cube: cannot",
                    "open url\n", href, "\n", e$message
                ), call. = FALSE)
            }
        )
    }
    return(invisible(NULL))
}

#' @keywords internal
#' @export
`.source_items_new.mspc_cube_sentinel-2-l2a` <- function(source,
                                                         collection,
                                                         stac_query, ...,
                                                         tiles = NULL,
                                                         platform = NULL) {

    # set caller to show in errors
    .check_set_caller(".source_items_new.mspc_cube_sentinel-2-l2a")

    if (!is.null(platform)) {
        platform <- .stac_format_platform(
            source = source,
            collection = collection,
            platform = platform
        )

        stac_query <- rstac::ext_query(
            q = stac_query, "platform" == platform
        )
    }

    # mspc does not support %in% operator, so we have to
    if (!is.null(tiles)) {
        items_list <- lapply(tiles, function(tile) {
            stac_query <- rstac::ext_query(
                q = stac_query, "s2:mgrs_tile" == tile
            )
            # making the request
            items_info <- rstac::post_request(q = stac_query, ...)
            # check if matched items
            .check_that(
                x = rstac::items_length(items_info) > 0,
                msg = "no items matched the query criteria."
            )
            # fetching all the metadata
            suppressWarnings(
                rstac::items_fetch(items = items_info, progress = FALSE)
            )
        })

        # getting the first item info
        items_info <- items_list[[1]]
        # joining the items
        items_info$features <- do.call(
            c,
            args = lapply(items_list, `[[`, "features")
        )
    } else {
        items_info <- rstac::post_request(q = stac_query, ...)
        # check if matched items
        .check_that(
            x = rstac::items_length(items_info) > 0,
            msg = "no items matched the query criteria."
        )
        # fetching all the metadata
        items_info <- suppressWarnings(
            rstac::items_fetch(items = items_info, progress = FALSE)
        )
    }

    # assign href
    items_info <- suppressWarnings(
        rstac::items_sign(items_info,
            sign_fn = rstac::sign_planetary_computer()
        )
    )
    return(items_info)
}

#' @keywords internal
#' @export
`.source_items_tile.mspc_cube_sentinel-2-l2a` <- function(source,
                                                          items, ...,
                                                          collection = NULL) {
    rstac::items_reap(items, field = c("properties", "s2:mgrs_tile"))
}

#' @keywords internal
#' @export
`.source_items_new.mspc_cube_landsat-c2-l2` <- function(source,
                                                          collection,
                                                          stac_query, ...,
                                                          tiles = NULL,
                                                          platform = NULL) {

    # set caller to show in errors
    .check_set_caller(".source_items_new.mspc_cube_landsat-c2-l2")

    if (!is.null(platform)) {

        platform <- .stac_format_platform(
            source = source,
            collection = collection,
            platform = platform
        )

        stac_query <- rstac::ext_query(
            q = stac_query, "platform" == platform
        )
    }

    # if specified, a filter per tile is added to the query
    if (!is.null(tiles)) {
        items_list <- lapply(tiles, function(tile) {
            # format tile parameter provided by users
            sep_tile <- .usgs_format_tiles(tile)
            # add filter by wrs path and row
            stac_query <- rstac::ext_query(
                q = stac_query,
                "landsat:wrs_path" == sep_tile$wrs_path,
                "landsat:wrs_row" == sep_tile$wrs_row
            )
            # making the request
            items <- rstac::post_request(q = stac_query, ...)

            # checks if the collection returned zero items
            .check_that(
                x = !(rstac::items_length(items) == 0),
                msg = "the provided search returned zero items."
            )

            # fetching all the metadata and updating to upper case instruments
            items <- suppressWarnings(
                rstac::items_fetch(items = items, progress = FALSE)
            )
        })
        # getting the first item info
        items <- items_list[[1]]
        # joining the items
        items$features <- do.call(
            c,
            args = lapply(items_list, `[[`, "features")
        )
    } else {
        # making the request
        items <- rstac::post_request(q = stac_query, ...)

        # checks if the collection returned zero items
        .check_that(
            x = !(rstac::items_length(items) == 0),
            msg = "the provided search returned zero items."
        )
        # fetching all the metadata and updating to upper case instruments
        items <- suppressWarnings(
            rstac::items_fetch(items = items, progress = FALSE)
        )
    }

    # assign href
    items <- suppressWarnings(
        rstac::items_sign(items, sign_fn = rstac::sign_planetary_computer())
    )
    return(items)
}

#' @keywords internal
#' @export
`.source_items_tile.mspc_cube_landsat-c2-l2` <- function(source,
                                                           items, ...,
                                                           collection = NULL) {

    # store tile info in items object
    items$features <- purrr::map(items$features, function(feature) {
        feature$properties$tile <- paste0(
            feature$properties[["landsat:wrs_path"]],
            feature$properties[["landsat:wrs_row"]]
        )
        feature
    })
    rstac::items_reap(items, field = c("properties", "tile"))
}
