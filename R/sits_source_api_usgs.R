#' @title Format tile parameter provided by users
#' @name .usgs_format_tiles
#' @keywords internal
#'
#' @param tiles     a \code{character} vector with the tiles provided by users.
#'
#' @return          a \code{tibble} with attributes of wrs path and row.
.usgs_format_tiles <- function(tiles) {

    # regex pattern of wrs_path and wrs_row
    pattern_l8 <- "[0-9]{6}"

    # verify tile pattern
    if (!any(grepl(pattern_l8, tiles, perl = TRUE)))
        stop(paste("The specified tiles do not match the Landsat-8 grid",
                   "pattern. See the user guide for more information."))

    # list to store the info about the tiles to provide the query in STAC
    list_tiles <- purrr::map(tiles, function(tile) {

        c(wrs_path = substring(tile, 1, 3),
          wrs_row = substring(tile, 4, 6))
    })

    # bind into a tibble all tiles
    tiles_tbl <- dplyr::bind_rows(list_tiles)

    return(tiles_tbl)
}

#' @title Filter datetime in STAC items
#' @name .usgs_filter_datetime
#' @keywords internal
#'
#' @param items      a \code{STACItemCollection} object returned by rstac
#' package.
#' @param datetime  a \code{character} ...
#'
#' @return  a \code{STACItemCollection} object with datetime filtered.
.usgs_filter_datetime <- function(items, datetime) {

    split_datetime <- strsplit(x = datetime, split = "/")

    start_date <- split_datetime[[1]][[1]]
    end_date <- split_datetime[[1]][[2]]

    # checks if the supplied tiles are in the searched items
    index_features <- purrr::map_lgl(items$features, function(feature) {
        datetime <- lubridate::date(feature[["properties"]][["datetime"]])

        if (datetime >= start_date && datetime <= end_date)
            return(TRUE)
        return(FALSE)
    })

    # select the tiles found in the search
    items$features <- items$features[index_features]

    items
}

.source_access_test.usgs_cube <- function(source, collection, ..., bands) {

    # require package
    if (!requireNamespace("rstac", quietly = TRUE)) {
        stop(paste("Please install package rstac from CRAN:",
                   "install.packages('rstac')"), call. = FALSE
        )
    }

    items_query <- .stac_items_query(source = source,
                                     collection = collection,
                                     limit = 1, ...)

    items_query$version <- "0.9.0"

    items_query <- rstac::ext_query(q = items_query,
                                    "collection" %in% collection,
                                    "platform" %in% "LANDSAT_8",
                                    "landsat:collection_category" %in% "T1")

    # assert that service is online
    tryCatch({
        items <- rstac::post_request(items_query)
    }, error = function(e) {
        stop(paste(".source_access_test.stac_cube: service is unreachable\n",
                   e$message), call. = FALSE)
    })

    items <- .source_items_bands_select(source = source,
                                        collection = collection,
                                        items = items,
                                        bands = bands[[1]], ...)

    href <- .source_item_get_hrefs(source = source,
                                   item = items$feature[[1]], ...,
                                   collection = collection)

    # assert that token and/or href is valid
    tryCatch({
        .raster_open_rast(href)
    }, error = function(e) {
        stop(paste(".source_access_test.stac_cube: cannot open url\n",
                   href, "\n", e$message), call. = FALSE)
    })

    return(invisible(NULL))
}

#' @keywords internal
#' @export
.source_item_get_date.usgs_cube <- function(source,
                                            item, ...,
                                            collection = NULL) {
    item[[c("properties", "datetime")]]
}

#' @keywords internal
#' @export
.source_item_get_hrefs.usgs_cube <- function(source,
                                             item, ...,
                                             collection = NULL) {

    href <- stringr::str_replace(
        string = unname(purrr::map_chr(item[["assets"]], `[[`, "href")),
        pattern = "^(https://landsatlook.usgs.gov/data)",
        replacement = "s3://usgs-landsat"
    )

    # add gdal vsi in href urls
    return(.stac_add_gdal_vsi(href))
}

#' @keywords internal
#' @export
.source_item_get_bands.usgs_cube <- function(source,
                                             item, ...,
                                             collection = NULL) {
    names(item[["assets"]])
}

#' @keywords internal
#' @export
.source_item_get_resolutions.usgs_cube <- function(source,
                                                   item, ...,
                                                   collection = NULL) {
    item[[c("properties", "eo:gsd")]]
}

#' @keywords internal
#' @export
.source_items_new.usgs_cube <- function(source,
                                        collection, ...,
                                        stac_query,
                                        tiles = NULL) {

    # forcing version
    stac_query$version <- "0.9.0"

    # adding search filter in query
    stac_query <- rstac::ext_query(q = stac_query,
                                   "collection" %in% collection,
                                   "platform" %in% "LANDSAT_8",
                                   "landsat:collection_category" %in% "T1")

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

    # checks if the collection returned zero items
    .check_that(
        x = !(rstac::items_length(items) == 0),
        msg = paste(".source_items_new.usgs_cube: the provided search returned",
                    "zero items.")
    )

    # filtering images by interval
    items_info <- .usgs_filter_datetime(items = items,
                                        datetime = stac_query$params$datetime)

    # if more than 2 times items pagination are found the progress bar
    # is displayed
    matched_items  <- rstac::items_matched(items_info,
                                           matched_field = c("meta", "found"))

    pgr_fetch <- matched_items > 2 * .config_rstac_limit()


    # fetching all the metadata and updating to upper case instruments
    items_info <- rstac::items_fetch(items = items_info,
                                     progress = pgr_fetch,
                                     matched_field = c("meta", "found"))
    return(items_info)
}

#' @keywords internal
#' @export
.source_items_tiles_group.usgs_cube <- function(source,
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

    rstac::items_group(items, field = c("properties", "tile"))
}

#' @keywords internal
#' @export
.source_items_get_sensor.usgs_cube <- function(source,
                                               items, ...,
                                               collection = NULL) {

    # OLI and TIRS returned, taking only OLI
    items[["features"]][[1]][[c("properties", "eo:instrument")]][[1]]
}

#' @keywords internal
#' @export
.source_items_get_satellite.usgs_cube <- function(source,
                                                  items, ...,
                                                  collection = NULL) {

    items[["features"]][[1]][[c("properties", "platform")]]
}

#' @keywords internal
#' @export
.source_items_tile_get_crs.usgs_cube <- function(source,
                                                 tile_items, ...,
                                                 collection = NULL) {

    href <- .source_item_get_hrefs(source = source,
                                   item = tile_items[["features"]][[1]], ...,
                                   collection = collection)

    # read the first image and obtain crs attribute
    params <- .raster_params_file(href)

    # format collection crs
    crs <- .sits_proj_format_crs(params[["crs"]])

    return(crs)
}

#' @keywords internal
#' @export
.source_items_tile_get_name.usgs_cube <- function(source,
                                                  tile_items, ...,
                                                  collection = NULL) {

    tile_items[["features"]][[1]][[c("properties", "tile")]]
}

#' @keywords internal
#' @export
.source_items_tile_get_bbox.usgs_cube <- function(source,
                                                  tile_items, ...,
                                                  collection = NULL) {
    # get collection crs
    crs <- .source_items_tile_get_crs(source = source,
                                      tile_items = tile_items,
                                      collection = collection)

    bbox <- .stac_get_bbox(tile_items, crs)

    return(bbox)
}

#' @keywords internal
#' @export
.source_items_tile_get_size.usgs_cube <- function(source,
                                                  tile_items, ...,
                                                  collection = NULL) {

    href <- .source_item_get_hrefs(source = source,
                                   item = tile_items[["features"]][[1]], ...,
                                   collection = collection)

    # read the first image and obtain the size parameters
    params <- .raster_params_file(href)

    size <- c(nrows = params[["nrows"]], ncols = params[["ncols"]])

    return(size)
}
