#' @title Create an items object in an AWS cube
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
.source_items_new.aws_cube <- function(source,
                                       collection,
                                       stac_query, ...,
                                       tiles = NULL,
                                       platform = NULL) {
    # check if platform is set
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
    }

    # if specified, a filter per tile is added to the query
    if (!is.null(tiles)) {
        sep_tile <- paste0("MGRS-", tiles)

        stac_query <-
            rstac::ext_query(
                q = stac_query,
                "grid:code" %in% sep_tile
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
    # fetching all the metadata
    items_info <- rstac::items_fetch(
        items = items_info,
        progress = progress
    )

    return(items_info)
}
#' @title Create an items object in an AWS cube collection LANDSAT
#' @keywords internal
#' @noRd
#' @description \code{.source_items_new()} this function is called to create
#' an items object.
#' @param source     Name of the STAC provider.
#' @param collection Collection to be searched in the data source.
#' @param stac_query Query that follows the STAC protocol
#' @param ...        Other parameters to be passed for specific types.
#' @param tiles      Selected tiles (optional)
#' @param platform   Satellite platform (optional).
#' @return An object referring the images of a sits cube.
#' @export
`.source_items_new.aws_cube_landsat-c2-l2` <- function(source,
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
    # making the request based on ROI
    items <- rstac::post_request(q = stac_query, ...)
    .check_stac_items(items)
    # fetching all the metadata and updating to upper case instruments
    items <- suppressWarnings(
        rstac::items_fetch(items = items, progress = FALSE)
    )
    return(items)
}

#' @title Organizes items by tiles for AWS LANDSAT collection
#' @param source     Name of the STAC provider.
#' @param items      \code{STACItemcollection} object from rstac package.
#' @param ...        Other parameters to be passed for specific types.
#' @param collection Collection to be searched in the data source.
#' @return A list of items.
#' @keywords internal
#' @noRd
#' @export
`.source_items_tile.aws_cube_landsat-c2-l2` <- function(source,
                                                        items, ...,
                                                        collection = NULL) {
    # store tile info in items object
    items[["features"]] <- purrr::map(
        items[["features"]], function(feature) {
            feature[["properties"]][["tile"]] <-
                paste0(feature[["properties"]][["landsat:wrs_path"]],
                    feature[["properties"]][["landsat:wrs_row"]],
                    collapse = ""
                )
            feature
        }
    )
    rstac::items_reap(items, field = c("properties", "tile"))
}

#' @title Organizes items by tiles for AWS collections
#' @param source     Name of the STAC provider.
#' @param items      \code{STACItemcollection} object from rstac package.
#' @param ...        Other parameters to be passed for specific types.
#' @param collection Collection to be searched in the data source.
#' @return A list of items.
#' @keywords internal
#' @noRd
#' @export
.source_items_tile.aws_cube <- function(source,
                                        items, ...,
                                        collection = NULL) {
    # store tile info in items object
    items[["features"]] <- purrr::map(items[["features"]], function(feature) {
        feature[["properties"]][["tile"]] <-
            feature[["properties"]][["grid:code"]]
        feature[["properties"]][["tile"]] <-
            gsub("MGRS-", "", feature[["properties"]][["tile"]], fixed = TRUE)
        feature
    })

    rstac::items_reap(items, field = c("properties", "tile"))
}
#' @title Adjusts date-time if required by source
#' @noRd
#' @param source  Data source
#' @param date    Date to be adjusted
#' @return Adjusted date
.source_adjust_date.aws_cube <- function(source, date) {
    if (.has(date)) {
        date <- paste0(date, "T00:00:00Z")
    }
    date
}
#' @noRd
#' @title Configure access.
#' @param source  Data source
#' @param collection Image collection
#' @return Called for side effects
.source_configure_access.aws_cube <- function(source, collection) {
    .check_set_caller(".source_configure_access_aws_cube")
    if (.conf("sources", "AWS", "collections", collection, "open_data")
    == "false") {
        aws_access_key <- Sys.getenv("AWS_SECRET_ACCESS_KEY")
        if (.has_not(aws_access_key)) {
            stop(.conf("messages", ".source_configure_access_aws_cube"))
        }
    }
}
