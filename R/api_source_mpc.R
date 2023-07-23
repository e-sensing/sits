#' @title Test access to collection in MPC
#' @keywords internal
#' @noRd
#' @description
#' These functions provide an API to handle/retrieve data from source's
#' collections.
#'
#' @param source     Data source.
#' @param collection Image collection.
#' @param bands      Band names
#' @param ...        Other parameters to be passed for specific types.
#' @param start_date Start date.
#' @param end_date   End date.
#' @param dry_run    TRUE/FALSE
#' @return           Called for side effects
#' @export
.source_collection_access_test.mpc_cube <- function(source,
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

    .check_stac_items(items)

    # signing the url with the mpc token
    access_key <- Sys.getenv("MPC_TOKEN")
    if (!nzchar(access_key)) {
        access_key <- NULL
    }
    items <- suppressWarnings(
        rstac::items_sign(
            items,
            sign_fn = rstac::sign_planetary_computer(
                httr::add_headers("Ocp-Apim-Subscription-Key" = access_key)
            )
        )
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
    return(invisible(source))
}
#' @title Create an items object in an MPC Sentinel-2 collection
#' @keywords internal
#' @noRd
#' @description \code{.source_items_new()} this function is called to create
#' an items object. In case of Web services, this function is responsible for
#' making the Web requests to the server.
#' @param source     Name of the STAC provider.
#' @param collection Collection to be searched in the data source.
#' @param stac_query Query that follows the STAC protocol
#' @param ...        Other parameters to be passed for specific types.
#' @param tiles      Selected tiles (optional)
#' @param platform   Satellite platform (optional).
#' @return An object referring the images of a sits cube.
#' @export
`.source_items_new.mpc_cube_sentinel-2-l2a` <- function(source,
                                                        collection,
                                                        stac_query, ...,
                                                        tiles = NULL,
                                                        platform = NULL) {
    # set caller to show in errors
    .check_set_caller(".source_items_new.mpc_cube_sentinel-2-l2a")

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

    # mpc does not support %in% operator, so we have to
    if (!is.null(tiles)) {
        items_list <- lapply(tiles, function(tile) {
            stac_query <- rstac::ext_query(
                q = stac_query, "s2:mgrs_tile" == tile
            )
            # making the request
            items_info <- rstac::post_request(q = stac_query, ...)
            .check_stac_items(items_info)
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
        .check_stac_items(items_info)
        # fetching all the metadata
        items_info <- suppressWarnings(
            rstac::items_fetch(items = items_info, progress = FALSE)
        )
    }

    # assign href
    access_key <- Sys.getenv("MPC_TOKEN")
    if (!nzchar(access_key)) {
        access_key <- NULL
    }
    items_info <- suppressWarnings(
        rstac::items_sign(
            items_info,
            sign_fn = rstac::sign_planetary_computer(
                httr::add_headers("Ocp-Apim-Subscription-Key" = access_key)
            )
        )
    )
    return(items_info)
}
#' @title Organizes items for MPC Sentinel-2 collections
#' @param source     Name of the STAC provider.
#' @param items      \code{STACItemcollection} object from rstac package.
#' @param ...        Other parameters to be passed for specific types.
#' @param collection Collection to be searched in the data source.
#' @return A list of items.
#' @keywords internal
#' @noRd
#' @export
`.source_items_tile.mpc_cube_sentinel-2-l2a` <- function(source,
                                                         items, ...,
                                                         collection = NULL) {
    rstac::items_reap(items, field = c("properties", "s2:mgrs_tile"))
}
#' @title Create an items object in MPC Landsat collection
#' @keywords internal
#' @noRd
#' @description \code{.source_items_new()} this function is called to create
#' an items object. In case of Web services, this function is responsible for
#' making the Web requests to the server.
#' @param source     Name of the STAC provider.
#' @param collection Collection to be searched in the data source.
#' @param stac_query Query that follows the STAC protocol
#' @param ...        Other parameters to be passed for specific types.
#' @param tiles      Selected tiles (optional)
#' @param platform   Satellite platform (optional).
#' @return An object referring the images of a sits cube.
#' @export
`.source_items_new.mpc_cube_landsat-c2-l2` <- function(source,
                                                       collection,
                                                       stac_query, ...,
                                                       tiles = NULL,
                                                       platform = NULL) {
    # set caller to show in errors
    .check_set_caller(".source_items_new.mpc_cube_landsat-c2-l2")

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
    .check_that(
        is.null(tiles),
        local_msg = "Error when retrieving Landsat collection",
        msg = "Searching by tiles not allowed, use roi"
    )

    # making the request based on ROI
    items <- rstac::post_request(q = stac_query, ...)
    .check_stac_items(items)
    # fetching all the metadata and updating to upper case instruments
    items <- suppressWarnings(
        rstac::items_fetch(items = items, progress = FALSE)
    )
    # assign href
    access_key <- Sys.getenv("MPC_TOKEN")
    if (!nzchar(access_key)) {
        access_key <- NULL
    }
    items <- suppressWarnings(
        rstac::items_sign(
            items,
            sign_fn = rstac::sign_planetary_computer(
                httr::add_headers("Ocp-Apim-Subscription-Key" = access_key)
            )
        )
    )
    return(items)
}
#' @title Organizes items for MPC Landsat collections
#' @param source     Name of the STAC provider.
#' @param items      \code{STACItemcollection} object from rstac package.
#' @param ...        Other parameters to be passed for specific types.
#' @param collection Collection to be searched in the data source.
#' @return A list of items.
#' @keywords internal
#' @noRd
#' @export
`.source_items_tile.mpc_cube_landsat-c2-l2` <- function(source,
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
