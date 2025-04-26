#' @title Test access to STAC collection
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
.source_collection_access_test.hls_cube <- function(source, collection,
                                                    bands, ...,
                                                    start_date = NULL,
                                                    end_date = NULL,
                                                    dry_run = FALSE) {
    # require package
    .check_require_packages("rstac")
    # create a query
    items_query <- .stac_create_items_query(
        source = source,
        collection = collection,
        start_date = start_date,
        end_date = end_date,
        limit = 1L
    )
    # format query dates
    items_query[["params"]][["datetime"]] <- .stac_dates_as_datetimes(
        items_query
    )
    # assert that service is online
    items <- .try(
        {
            rstac::post_request(items_query, ...)
        },
        .default = NULL
    )
    .check_stac_items(items)

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

#' @title Create an items object in an HLS cube
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
#' @return An object referring the images of a sits cube.
#' @export
.source_items_new.hls_cube <- function(source, ...,
                                       collection,
                                       stac_query,
                                       tiles = NULL) {
    # set caller to show in errors
    .check_set_caller(".source_items_new_hls_cube")
    # check netrc file
    suppressWarnings(.check_netrc_gdal(attributes = .conf("HLS_ACCESS_URL")))
    # format query dates
    stac_query[["params"]][["datetime"]] <- .stac_dates_as_datetimes(stac_query)
    # convert tiles to a valid STAC query
    if (!is.null(tiles)) {
        roi <- .s2_mgrs_to_roi(tiles)
        stac_query[["params"]][["intersects"]] <- NULL
        stac_query[["params"]][["bbox"]] <- c(
            roi[["lon_min"]],
            roi[["lat_min"]],
            roi[["lon_max"]],
            roi[["lat_max"]]
        )
    } else {
        # Convert roi to bbox
        lon <- stac_query[["params"]][["intersects"]][["coordinates"]][, , 1L]
        lat <- stac_query[["params"]][["intersects"]][["coordinates"]][, , 2L]
        stac_query[["params"]][["intersects"]] <- NULL
        stac_query[["params"]][["bbox"]] <- c(
            min(lon),
            min(lat),
            max(lon),
            max(lat)
        )
    }
    # making the request
    items_info <- rstac::post_request(q = stac_query, ...)
    .check_stac_items(items_info)
    # if more than 2 times items pagination are found the progress bar
    # is displayed
    progress <- rstac::items_matched(items_info) > 2L *
        .conf("rstac_pagination_limit")
    # check documentation mode
    progress <- .message_progress(progress)
    # fetching all the metadata and updating to upper case instruments
    items_info <- rstac::items_fetch(items = items_info, progress = progress)
    # checks if the items returned any items
    .check_stac_items(items_info)
    return(items_info)
}
#' @title Organizes items by tiles for HLS collections
#' @param source     Name of the STAC provider.
#' @param ...        Other parameters to be passed for specific types.
#' @param items      \code{STACItemcollection} object from rstac package.
#' @param collection Collection to be searched in the data source.
#' @return A list of items.
#' @keywords internal
#' @noRd
#' @export
.source_items_tile.hls_cube <- function(source, ...,
                                        items,
                                        collection = NULL) {
    tiles <- strsplit(rstac::items_reap(items, field = "id"), "\\.")
    tiles <- purrr::map_chr(tiles, function(x) x[[3L]])
    substr(tiles, 2L, 6L)
}
#' @noRd
#' @title Configure access.
#' @param source  Data source
#' @param collection Image collection
#' @return Called for side effects
.source_configure_access.hls_cube <- function(source, collection = NULL) {
    .check_set_caller(".source_configure_access_hls_cube")
    # check netrc file
    .check_netrc_gdal(attributes = .conf("HLS_ACCESS_URL"))
    # done!
    return(invisible(source))
}
