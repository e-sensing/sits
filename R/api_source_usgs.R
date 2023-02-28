#' @title Format tile parameter provided by users
#' @name .usgs_format_tiles
#' @keywords internal
#' @noRd
#'
#' @param tiles     Tiles provided by users.
#' @return          Attributes of wrs path and row.
#'
.usgs_format_tiles <- function(tiles) {

    # regex pattern of wrs_path and wrs_row
    pattern_l8 <- "[0-9]{6}"

    # verify tile pattern
    if (!any(grepl(pattern_l8, tiles, perl = TRUE))) {
        stop(paste(
            "The specified tiles do not match the Landsat-8 grid",
            "pattern. See the user guide for more information."
        ))
    }

    # list to store the info about the tiles to provide the query in STAC
    tiles_tbl <- purrr::map_dfr(tiles, function(tile) {
        c(
            wrs_path = substring(tile, 1, 3),
            wrs_row = substring(tile, 4, 6)
        )
    })

    return(tiles_tbl)
}

#' @keywords internal
#' @noRd
#' @export
.source_collection_access_test.usgs_cube <- function(source, ...,
                                                     collection,
                                                     bands) {

    # require package
    .check_require_packages("rstac")

    items_query <- .stac_create_items_query(
        source = source,
        collection = collection,
        limit = 1
    )

    items_query <- rstac::ext_query(
        q = items_query,
        "landsat:correction" %in% "L2SR",
        "platform" %in% "LANDSAT_8",
        "landsat:collection_number" %in% "02"
    )

    # assert that service is online
    tryCatch(
        {
            items <- rstac::post_request(items_query)
        },
        error = function(e) {
            stop(paste(
                ".source_collection_access_test.usgs_cube: service is",
                "unreachable\n", e$message
            ), call. = FALSE)
        }
    )

    .check_stac_items(items)
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
    tryCatch(
        {
            .raster_open_rast(href)
        },
        error = function(e) {
            stop(paste(
                ".source_collection_access_test.usgs_cube: cannot open url\n",
                href, "\n", e$message
            ), call. = FALSE)
        }
    )

    return(invisible(NULL))
}

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

#' @keywords internal
#' @noRd
#' @export
.source_items_new.usgs_cube <- function(source,
                                        collection,
                                        stac_query, ...,
                                        tiles = NULL,
                                        platform = NULL) {

    # set caller to show in errors
    .check_set_caller(".source_items_new.usgs_cube")

    # get start and end date
    dates_chr <- strsplit(x = stac_query$params$datetime, split = "/")[[1]]

    # the usgs stac only accepts RFC 3339 datetime format
    stac_query$params$datetime <- paste(
        format(as.Date(dates_chr), "%Y-%m-%dT%H:%M:%SZ"),
        collapse = "/"
    )

    # request with more than searched items throws 502 error
    stac_query$params$limit <- 300

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
        platform <- unlist(unname(
            .conf(
            "sources", source, "collections",  collection, "platforms"
            )
        ))

        stac_query <- rstac::ext_query(
            q = stac_query,
            "platform" %in% platform
        )
    }

    # adding search filter in query
    stac_query <- rstac::ext_query(
        q = stac_query,
        "landsat:correction" %in% c("L2SR", "L2SP"),
        "landsat:collection_category" %in% c("T1", "T2"),
        "landsat:collection_number" %in% "02"
    )

    # if specified, a filter per tile is added to the query
    if (!is.null(tiles)) {

        # format tile parameter provided by users
        sep_tile <- .usgs_format_tiles(tiles)

        # add filter by wrs path and row
        stac_query <- rstac::ext_query(
            q = stac_query,
            "landsat:wrs_path" %in% sep_tile$wrs_path,
            "landsat:wrs_row" %in% sep_tile$wrs_row
        )
    }

    # making the request
    items <- rstac::post_request(q = stac_query, ...)
    .check_stac_items(items)
    # filter only surface reflectance products
    items$features <- items$features[
        grepl("_SR$", rstac::items_reap(items, "id"))
    ]

    # checks if the collection returned zero items
    .check_that(
        x = !(rstac::items_length(items) == 0),
        msg = "the provided search returned zero items."
    )

    # if more than 2 times items pagination are found the progress bar
    # is displayed
    matched_items <- rstac::items_matched(items = items)

    # progress bar
    progress <- matched_items > 2 * .conf("rstac_pagination_limit")
    # check documentation mode
    progress <- .check_documentation(progress)
    # fetching all the metadata and updating to upper case instruments
    items_info <- suppressWarnings(
        rstac::items_fetch(items = items, progress = progress)
    )
    return(items_info)
}

#' @keywords internal
#' @noRd
#' @export
.source_items_tile.usgs_cube <- function(source,
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
