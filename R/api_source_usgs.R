#' @title Format tile parameter provided by users
#' @name .usgs_format_tiles
#' @keywords internal
#' @noRd
#'
#' @param tiles     Tiles provided by users.
#' @return          Attributes of wrs path and row.
#'
.usgs_format_tiles <- function(tiles) {
    .check_set_caller(".usgs_format_tiles")
    # regex pattern of wrs_path and wrs_row
    pattern_l8 <- "[0-9]{6}"
    # verify tile pattern
    .check_that(all(grepl(pattern_l8, tiles, perl = TRUE)))

    # prepare the tiles for a valid STAC query to the USGS archive
    .map_dfr(tiles, function(tile) {
        c(wrs_path = substring(tile, 1L, 3L), wrs_row = substring(tile, 4L, 6L))
    })
}
#' @title Test access to collection in USGS
#' @keywords internal
#' @noRd
#' @param source     Data source.
#' @param ...        Other parameters to be passed for specific types.
#' @param collection Image collection.
#' @param bands      Band names
#' @return           Called for side effects
#' @export
.source_collection_access_test.usgs_cube <- function(source, ...,
                                                     collection,
                                                     bands) {
    # require package
    .check_require_packages("rstac")
    # create a STAC query
    items_query <- .stac_create_items_query(
        source = source,
        collection = collection,
        limit = 1L
    )
    # Run a query
    items_query <- rstac::ext_query(
        q = items_query,
        "landsat:correction" == "L2SR",
        "platform" == "LANDSAT_8",
        "landsat:collection_number" == "02"
    )
    # assert that service is online
    items <- .try(
        {
            rstac::post_request(items_query, ...)
        },
        default = NULL
    )
    # check result
    .check_stac_items(items)
    items <- .source_items_bands_select(
        source = source,
        items = items,
        bands = bands[[1L]],
        collection = collection, ...
    )
    # Get HTTP refs
    href <- .source_item_get_hrefs(
        source = source,
        item = items[["features"]][[1L]],
        collection = collection, ...
    )
    # assert that token and/or href is valid
    rast <- .try(
        {
            .raster_open_rast(href)
        },
        default = NULL
    )
    .check_null_parameter(rast)
    invisible(source)
}
#' @title Retrieves the paths or URLs of each file bands of an item for BDC
#' @param source     Name of the STAC provider.
#' @param ...        Other parameters to be passed for specific types.
#' @param item       \code{STACItemcollection} object from rstac package.
#' @param collection Collection to be searched in the data source.
#' @return Returns paths to each image band of an item.
#' @keywords internal
#' @noRd
#' @export
.source_item_get_hrefs.usgs_cube <- function(source, item, ...,
                                             collection = NULL) {
    href <- unname(purrr::map_chr(item[["assets"]], function(x) {
        x[["alternate"]][[c("s3", "href")]]
    }))

    # add gdal vsi in href urls
    return(.stac_add_gdal_fs(href))
}
#' @title Create an items object in a USGS collection
#' @keywords internal
#' @noRd
#' @description \code{.source_items_new()} this function is called to create
#' an items object. In case of Web services, this function is responsible for
#' making the Web requests to the server.
#' @param source     Name of the STAC provider.
#' @param collection Collection to be searched in the data source.
#' @param stac_query Query that follows the STAC protocol
#' @param ...        Additional parameters
#' @param tiles      Selected tiles (optional)
#' @param platform   Satellite platform (optional).
#' @return An object referring the images of an USGS collection
#' @export
.source_items_new.usgs_cube <- function(source,
                                        collection,
                                        stac_query, ...,
                                        tiles = NULL,
                                        platform = NULL) {
    # get start and end date
    date_time <- strsplit(
        stac_query[["params"]][["datetime"]],
        split = "/"
    )
    dates_chr <- date_time[[1L]]
    # USGS stac only accepts RFC 3339 datetime format
    stac_query[["params"]][["datetime"]] <- paste(
        format(as.Date(dates_chr), "%Y-%m-%dT%H:%M:%SZ"),
        collapse = "/"
    )
    # requests with more than searched items throws 502 error
    stac_query[["params"]][["limit"]] <- 300L

    if (!is.null(platform)) {
        platform <- .stac_format_platform(
            source = source,
            collection = collection,
            platform = platform
        )
        stac_query <- rstac::ext_query(
            q = stac_query,
            "platform" == platform
        )
    } else {
        platforms <- unlist(unname(
            .conf(
                "sources", source, "collections", collection, "platforms"
            )
        ))
        stac_query <- rstac::ext_query(
            q = stac_query,
            "platform" %in% platforms
        )
    }
    # adding search filter in query
    stac_query <- rstac::ext_query(
        q = stac_query,
        "landsat:correction" %in% c("L2SR", "L2SP"),
        "landsat:collection_category" %in% c("T1", "T2"),
        "landsat:collection_number" == "02"
    )
    # if specified, a filter per tile is added to the query
    if (!is.null(tiles)) {
        # format tile parameter provided by users
        sep_tile <- .usgs_format_tiles(tiles)

        # add filter by wrs path and row
        stac_query <- rstac::ext_query(
            q = stac_query,
            "landsat:wrs_path" %in% sep_tile[["wrs_path"]],
            "landsat:wrs_row" %in% sep_tile[["wrs_row"]]
        )
    }
    # making the request
    items <- rstac::post_request(q = stac_query, ...)
    .check_stac_items(items)
    # filter only surface reflectance products
    items[["features"]] <- items[["features"]][
        endsWith(rstac::items_reap(items, "id"), suffix = "_SR")
    ]
    # checks if the collection returned zero items
    .check_stac_items(items)
    # if more than 2 times items pagination are found the progress bar
    # is displayed
    matched_items <- rstac::items_matched(items = items)
    # progress bar
    progress <- matched_items > 2L * .conf("rstac_pagination_limit")
    # check documentation mode
    progress <- .message_progress(progress)
    # fetching all the metadata and updating to upper case instruments
    items_info <- suppressWarnings(
        rstac::items_fetch(items = items, progress = progress)
    )
    return(items_info)
}
#' @title Organizes items by tiles for USGS collections
#' @param source     Name of the STAC provider.
#' @param items      \code{STACItemcollection} object from rstac package.
#' @param ...        Other parameters to be passed for specific types.
#' @param collection Collection to be searched in the data source.
#' @return A list of items.
#' @keywords internal
#' @noRd
#' @export
.source_items_tile.usgs_cube <- function(source,
                                         items, ...,
                                         collection = NULL) {
    # store tile info in items object
    items[["features"]] <- purrr::map(items[["features"]], function(feature) {
        feature[["properties"]][["tile"]] <- paste0(
            feature[["properties"]][["landsat:wrs_path"]],
            feature[["properties"]][["landsat:wrs_row"]]
        )
        feature
    })

    rstac::items_reap(items, field = c("properties", "tile"))
}
#' @noRd
#' @title Configure access.
#' @param source  Data source
#' @param collection Image collection
#' @return Called for side effects
.source_configure_access.usgs_cube <- function(source, collection = NULL) {
    .check_set_caller(".source_configure_access_usgs_cube")
    aws_access_key <- Sys.getenv("AWS_SECRET_ACCESS_KEY")
    if (.has(aws_access_key)) {
        stop(.conf("messages", ".source_configure_access_usgs_cube"))
    }
    return(invisible(source))
}
