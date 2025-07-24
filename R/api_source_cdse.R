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
