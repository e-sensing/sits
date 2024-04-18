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
    items <- .try({rstac::post_request(items_query, ...)},
        .default = NULL
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
        rast <- .try({.raster_open_rast(href)},
            default = NULL
        )
        .check_null_parameter(rast)
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
#' @param bands      Names of the bands to filter
#' @param ...        Other parameters to be passed for specific types.
#' @param orbit      Name of the orbit (e.g. "ascending" or "descending")
#' @param tiles      Selected tiles (optional)
#' @param platform   Satellite platform (optional).
#' @return An object referring the images of a sits cube.
#' @export
`.source_collection_access_test.mpc_cube_sentinel-1-grd` <- function(
        source,
        collection,
        bands, ...,
        orbit = "descending",
        start_date = NULL,
        end_date = NULL,
        dry_run = TRUE) {

    # require package
    .check_require_packages("rstac")
    orbits <- .conf("sources", source, "collections", collection, "orbits")
    .check_chr_within(x = orbit, within = orbits)

    stac_query <- .stac_create_items_query(
        source = source,
        collection = collection,
        roi = list(
            "xmin" = -50.479,
            "ymin" = -10.1973,
            "xmax" = -50.410,
            "ymax" = -10.1510,
            "crs"  = "EPSG:4326"
        ),
        start_date = start_date,
        end_date = end_date,
        limit = 1
    )
    stac_query <- rstac::ext_filter(
        stac_query,
        `sar:frequency_band` == "C" &&
            `sar:instrument_mode` == "IW" &&
            `sat:orbit_state` == {{orbit}}
    )

    # assert that service is online
    items <- .try({rstac::post_request(stac_query, ...)},
        .default = NULL
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
        rast <- .try({.raster_open_rast(href)},
            default = NULL
        )
        .check_null_parameter(rast)
    }
    return(invisible(NULL))
}

`.source_collection_access_test.mpc_cube_sentinel-1-rtc` <- function(
        source,
        collection,
        bands, ...,
        orbit = "descending",
        start_date = NULL,
        end_date = NULL,
        dry_run = TRUE) {

    `.source_collection_access_test.mpc_cube_sentinel-1-grd`(
        source = source,
        collection = collection,
        bands = bands, ...,
        orbit = orbit,
        start_date = start_date,
        end_date = end_date,
        dry_run = dry_run
    )
}
#' @title Get bbox from file info for Sentinel-1 GRD
#' @keywords internal
#' @noRd
#' @param source     Data source
#' @param file_info  File info
#' @param ...        Additional parameters.
#' @param collection Image collection
#' @return vector (xmin, ymin, xmax, ymax).
#' @export
`.source_tile_get_bbox.mpc_cube_sentinel-1-grd` <- function(source,
                                                            file_info, ...,
                                                            collection = NULL) {

    # pre-condition
    .check_num(nrow(file_info), min = 1, msg = "invalid 'file_info' value")

    # get bbox based on file_info
    xmin <- min(file_info[["xmin"]])
    ymin <- min(file_info[["ymin"]])
    xmax <- max(file_info[["xmax"]])
    ymax <- max(file_info[["ymax"]])

    # post-condition
    .check_that(xmin < xmax && ymin < ymax)
    # create a bbox
    bbox <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
    return(bbox)
}
#' @title Get bbox from file info for Sentinel-1 RTC
#' @keywords internal
#' @noRd
#' @param source     Data source
#' @param file_info  File info
#' @param ...        Additional parameters.
#' @param collection Image collection
#' @return vector (xmin, ymin, xmax, ymax).
#' @export
`.source_tile_get_bbox.mpc_cube_sentinel-1-rtc` <- function(source,
                                                            file_info, ...,
                                                            collection = NULL) {
    `.source_tile_get_bbox.mpc_cube_sentinel-1-grd`(
        source = source,
        file_info = file_info, ...,
        collection = collection
    )
}
#' @keywords internal
#' @noRd
#' @export
`.source_items_new.mpc_cube_sentinel-1-grd` <- function(source,
                                                        collection,
                                                        stac_query, ...,
                                                        tiles = NULL,
                                                        orbit = "descending") {

    orbits <- .conf("sources", source, "collections", collection, "orbits")
    .check_chr_within(
        x = orbit,
        within = orbits,
        msg = "Invalid `orbit` parameter"
    )

    stac_query <- rstac::ext_filter(
        stac_query,
        `sar:frequency_band` == "C" &&
            `sar:instrument_mode` == "IW" &&
            `sat:orbit_state` == {{orbit}}
    )

    # Sentinel-1 does not support tiles - convert to ROI
    if (!is.null(tiles)) {
        roi <- .s2_mgrs_to_roi(tiles)
        stac_query$params$intersects <- NULL
        stac_query$params$bbox <- c(roi[["lon_min"]],
                                    roi[["lat_min"]],
                                    roi[["lon_max"]],
                                    roi[["lat_max"]]
        )
    }
    items_info <- rstac::post_request(q = stac_query, ...)
    .check_stac_items(items_info)
    # fetching all the metadata
    items_info <- suppressWarnings(
        rstac::items_fetch(items = items_info, progress = FALSE)
    )

    # assign href
    access_key <- Sys.getenv("MPC_TOKEN")
    if (!nzchar(access_key)) {
        access_key <- NULL
    }
    items_info <- suppressWarnings(
        rstac::items_sign(
            items_info, sign_fn = rstac::sign_planetary_computer(
                httr::add_headers("Ocp-Apim-Subscription-Key" = access_key)
            )
        )
    )
    return(items_info)
}
#' @keywords internal
#' @noRd
#' @export
`.source_items_new.mpc_cube_sentinel-1-rtc` <- function(source,
                                                        collection,
                                                        stac_query, ...,
                                                        tiles = NULL,
                                                        orbit = "descending") {
    `.source_items_new.mpc_cube_sentinel-1-grd`(
        source = source,
        collection = collection,
        stac_query = stac_query, ...,
        tiles = tiles,
        orbit = orbit
    )
}
#' @keywords internal
#' @noRd
#' @export
`.source_items_new.mpc_cube_sentinel-2-l2a` <- function(source,
                                                        collection,
                                                        stac_query, ...,
                                                        tiles = NULL,
                                                        platform = NULL) {
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
    # mpc does not support %in% operator
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
    if (.has(platform)) {
        platform <- .stac_format_platform(
            source = source,
            collection = collection,
            platform = platform
        )
        stac_query <- rstac::ext_query(
            q = stac_query, "platform" == platform
        )
    }
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
#' @keywords internal
#' @noRd
#' @export
`.source_items_tile.mpc_cube_sentinel-1-grd` <- function(source,
                                                         items, ...,
                                                         collection = NULL) {
    rep("NoTilingSystem", rstac::items_length(items))
}
#' @keywords internal
#' @noRd
#' @export
`.source_items_tile.mpc_cube_sentinel-1-rtc` <- function(source,
                                                         items, ...,
                                                         collection = NULL) {
    `.source_items_tile.mpc_cube_sentinel-1-grd`(
        source = source,
        items = items, ...,
        collection = collection
    )
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
#' @title Filter S1 GRD tiles
#' @noRd
#' @param source  Data source
#' @param cube    Cube to be filtered
#' @param tiles   Tiles to be selected
#' @return Filtered cube
#' @export
`.source_filter_tiles.mpc_cube_sentinel-1-grd` <- function(source,
                                                           collection,
                                                           cube,
                                                           tiles) {
    return(cube)
}
`.source_filter_tiles.mpc_cube_sentinel-1-rtc` <- function(source,
                                                           collection,
                                                           cube,
                                                           tiles) {
    `.source_filter_tiles.mpc_cube_sentinel-1-grd`(
        source = source,
        collection = collection,
        cube = cube,
        tiles = tiles)

}
#' @title Check if roi or tiles are provided
#' @param source        Data source
#' @param roi           Region of interest
#' @param tiles         Tiles to be included in cube
#' @return Called for side effects.
#' @keywords internal
#' @noRd
#' @export
`.source_roi_tiles.mpc_cube_landsat-c2-l2` <- function(source, roi, tiles) {
    # set caller to show in errors
    .check_set_caller(".source_roi_tiles_mpc_cube_landsat_c2_l2")
    .check_that(.has_not(tiles))
    return(invisible(source))
}
