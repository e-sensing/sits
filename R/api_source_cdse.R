# ---- cdse utilities ----
#' @title Extract item type of a given `collection`.
#' @keywords internal
#' @noRd
#'
#' @description
#'  This function read the item type of a `collection` and prepares it for use
#'  in the CDSE Source API internal methods.
#'
#' @param source     Data source.
#' @param collection Image collection.
#' @return           item type of a given `collection`.
.cdse_item_type <- function(source, collection) {
    # Get item type
    item_type <- .conf(
        "sources",
        source,
        "collections",
        collection,
        "item_type"
    )
    # Use item type as class
    class(item_type) <- c("character", item_type)
    # Return!
    item_type
}

# ---- source api ----
#' @title Test access to collection in CDSE
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
.source_collection_access_test.cdse_cube <- function(source,
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
        limit = 1L
    )
    # format query dates
    items_query[["params"]][["datetime"]] <- .stac_dates_as_datetimes(items_query)
    # as CDSE STAC returns many types of items in the same collection,
    # it is required to filter the content by a specific type.
    item_type <- .cdse_item_type(source, collection)
    # update query
    items_query <- rstac::ext_filter(
        items_query,
        `product:type` == {{ item_type }}
    )
    # assert that service is online
    items <- .try(
        {
            rstac::post_request(items_query, ...)
        },
        .default = NULL
    )
    .check_stac_items(items)
    # Add 1s delay to avoid request rate limit
    Sys.sleep(1)
    # select bands in the results
    items <- .source_items_bands_select(
        source = source,
        items = items,
        bands = bands[[1L]],
        collection = collection, ...
    )
    href <- .source_item_get_hrefs(
        source = source,
        item = items[["features"]][[1L]],
        collection = collection, ...
    )
    # assert that token and/or href is valid
    if (dry_run) {
        rast <- .try(
            {
                .raster_open_rast(href)
            },
            default = NULL
        )
        .check_null_parameter(rast)
    }
    return(invisible(source))
}
#' @title Create an items object in CDSE
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
.source_items_new.cdse_cube <- function(source,
                                       collection,
                                       stac_query, ...,
                                       tiles = NULL,
                                       platform = NULL) {
    .check_set_caller(".source_items_new_cdse_cube")
    # Handle platform
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
    # Handle tiles
    # as CDSE STAC returns many types of items in the same collection,
    # it is required to filter the content by a specific type.
    item_type <- .cdse_item_type(source, collection)
    # build base query, including item_type
    query_cdse <- substitute(`product:type` == val, list(val = item_type))
    # if tile is available, use it
    if (!is.null(tiles)) {
        # build tile expressions: grid:code == "MGRS-..."
        tile_exprs <- purrr::map(tiles, function(tile) {
            substitute(`grid:code` == val, list(val = paste0("MGRS-", tile)))
        })

        # combine with OR: grid:code == ... OR ...
        tile_or_expr <- if (length(tile_exprs) == 1) {
            # if is a unique tile, use the expression available
            tile_exprs[[1]]
        } else {
            # for multiple tiles, reduce it to a unique expression
            purrr::reduce(tile_exprs, function(x, y) {
                substitute(a | b, list(a = x, b = y))
            })
        }

        # generate valid expression
        query_cdse <- substitute(
            A & (`product:type` == val),
            list(A = tile_or_expr, val = item_type)
        )
    }
    # format query dates
    stac_query[["params"]][["datetime"]] <- .stac_dates_as_datetimes(stac_query)
    # update query
    stac_query <- rstac::ext_filter(
        stac_query, {{ query_cdse }}
    )
    # making the request based on ROI
    items <- rstac::post_request(q = stac_query, ...)
    .check_stac_items(items)
    # fetching all the metadata and updating to upper case instruments
    items <- suppressWarnings(
        rstac::items_fetch(items = items, progress = FALSE)
    )
    return(items)
}
#' @keywords internal
#' @noRd
#' @export
.source_items_tile.cdse_cube <- function(source,
                                         items, ...,
                                         collection = NULL) {
    stringr::str_replace_all(
        string = rstac::items_reap(items, field = c("properties", "grid:code")),
        pattern = "MGRS-",
        replacement = ""
    )
}
#' @title Test access to Sentinel-1 GRD collection in CDSE
#' @keywords internal
#' @noRd
#' @param source     Name of the STAC provider.
#' @param collection Collection to be searched in the data source.
#' @param bands      Names of the bands to filter
#' @param ...        Other parameters to be passed for specific types
#' @param orbit      Name of the orbit (e.g. "ascending" or "descending")
#' @param start_date Start date.
#' @param end_date   End date.
#' @param dry_run    TRUE/FALSE
#' @return           Called for side effects
#' @export
`.source_collection_access_test.cdse_cube_sentinel-1-grd` <- function(source,
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
    # create items query
    items_query <- .stac_create_items_query(
        source = source,
        collection = collection,
        start_date = start_date,
        end_date = end_date,
        limit = 1L
    )
    # format query dates
    items_query[["params"]][["datetime"]] <- .stac_dates_as_datetimes(items_query)
    # as CDSE STAC returns many types of items in the same collection,
    # it is required to filter the content by a specific type.
    # get item type
    item_type <- .cdse_item_type(source, collection)
    # add cql filter for item type and orbit
    items_query <- rstac::ext_filter(
        items_query,
        `product:type` == {{ item_type }} &&
            `sat:orbit_state` == {{ orbit }}
    )
    # we need to restrict query to the selected polarizations, otherwise the 
    # CDSE stac can return different bands, which causes errors
    # so, first we get the accepted polarizations based on the bands requested
    polarizations <- toupper(.source_bands_to_source(
        source = source,
        collection = collection,
        bands = bands
    ))
    # then, we update the query to only include selected polarizations
    items_query[["params"]][["filter"]][["args"]] <- c(
        items_query[["params"]][["filter"]][["args"]],
        list(list(
            op = "a_contains",
            args = list(
                list(property = "sar:polarizations"),
                as.list(polarizations)
            )
        ))
    )
    # request to the service
    items <- .try(
        {
            rstac::post_request(items_query, ...)
        },
        .default = NULL
    )
    # check if the request was successful
    .check_stac_items(items)
    # select bands in the results
    items <- .source_items_bands_select(
        source = source,
        items = items,
        bands = bands[[1L]],
        collection = collection, ...
    )
    # get the href of the first item
    href <- .source_item_get_hrefs(
        source = source,
        item = items[["features"]][[1L]],
        collection = collection, ...
    )
    # check if the href is valid
    if (dry_run) {
        rast <- .try(
            {
                .raster_open_rast(href)
            },
            default = NULL
        )
        .check_null_parameter(rast)
    }
    # return!
    return(invisible(source))
}
#' @title Create an items object in CDSE for Sentinel-1 GRD
#' @keywords internal
#' @noRd
#' @param source     Name of the STAC provider.
#' @param collection Collection to be searched in the data source.
#' @param stac_query Query that follows the STAC protocol
#' @param ...        Other parameters to be passed for specific types.
#' @param tiles      Selected tiles (optional)
#' @param orbit      Name of the orbit (e.g. "ascending" or "descending")
#' @param platform   Satellite platform (optional).
#' @return An object referring the images of a sits cube.
#' @export
`.source_items_new.cdse_cube_sentinel-1-grd` <- function(source,
                                                         collection,
                                                         stac_query, ...,
                                                         tiles = NULL,
                                                         orbit = "descending",
                                                         platform = NULL) {
    .check_set_caller(".source_items_new_cdse_s1_grd")
    # get the orbits
    orbits <- .conf("sources", source, "collections", collection, "orbits")
    # check if the orbit is valid
    .check_chr_within(x = orbit, within = orbits)
    # as CDSE STAC returns many types of items in the same collection,
    # it is required to filter the content by a specific type.
    # get item type
    item_type <- .cdse_item_type(source, collection)
    # build base query with item_type and orbit
    query_cdse <- substitute(
        `product:type` == type && `sat:orbit_state` == orb,
        list(type = item_type, orb = orbit)
    )
    # handle platform
    # when a cql2 filter is used, CDSE does not accept STAC query extension combined.
    # so we need to add the platform to the query manually
    if (.has(platform)) {
        # format platform
        platform <- .stac_format_platform(
            source = source,
            collection = collection,
            platform = platform
        )
        # add platform to the query
        query_cdse <- substitute(
            A && platform == plat, list(A = query_cdse, plat = platform)
        )
    }
    # sentinel-1 does not support tiles - convert to ROI
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
    # format query dates
    stac_query[["params"]][["datetime"]] <- .stac_dates_as_datetimes(stac_query)
    # update query with extra filters
    stac_query <- rstac::ext_filter(stac_query, {{ query_cdse }})
    # request service
    items <- rstac::post_request(q = stac_query, ...)
    # validate result
    .check_stac_items(items)
    # fetch all the metadata
    items <- suppressWarnings(
        rstac::items_fetch(items = items, progress = FALSE)
    )
    # return!
    return(items)
}
#' @keywords internal
#' @noRd
#' @export
`.source_items_tile.cdse_cube_sentinel-1-grd` <- function(source,
                                                          items, ...,
                                                          collection = NULL) {
    # Create a tile name for each feature
    paste0("NoTilingSystem-", seq_len(rstac::items_length(items)))
}
#' @title Filter S1 GRD tiles
#' @keywords internal
#' @noRd
#' @param source     Data source
#' @param collection Image collection
#' @param cube       Cube to be filtered
#' @param tiles      Tiles to be selected
#' @return Filtered cube
#' @export
`.source_filter_tiles.cdse_cube_sentinel-1-grd` <- function(source,
                                                            collection,
                                                            cube,
                                                            tiles) {
    cube
}
