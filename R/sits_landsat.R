#' @title Get information from items
#' @name .sits_usgs_items
#' @keywords internal
#'
#' @param url        a \code{character} representing a URL for the USGS catalog.
#' @param collection a \code{character} with the collection to be searched.
#' @param tiles      a \code{character} with the names of the tiles.
#' @param roi        selects images (tiles) that intersect according to the
#'  region of interest provided. Expressed either as an \code{sfc} or \code{sf}
#'  object from sf package, a \code{character} with GeoJSON following the rules
#'  from RFC 7946, or a \code{vector} with bounding box named XY values in
#'  WGS 84 ("xmin", "xmax", "ymin", "ymax").
#' @param start_date a \code{character} corresponds to the initial date
#'                   when the cube will be created.
#' @param end_date   a \code{character} corresponds to the final date when the
#'                   cube will be created.
#' @param bands      a \code{character} vector with the bands name.
#' @param ...        other parameters to be passed for specific types.
#'
#' @return           a \code{STACItemCollection} object representing the search
#'                   by rstac.
.sits_usgs_items <- function(url, collection, tiles, roi,
                             start_date, end_date, bands, ...) {

    # obtain the datetime parameter for STAC like parameter
    datetime <- .sits_stac_datetime(start_date, end_date)

    # obtain the bbox and intersects parameters
    if (!is.null(roi)) {
        roi <- .sits_stac_roi(roi)
    } else {
        roi[c("bbox", "intersects")] <- list(NULL, NULL)
    }

    # get the limit items to be returned in each page
    limit_items <- .sits_config_rstac_limit()

    # creating a rstac object
    rstac_query <- rstac::stac(url, force_version = "0.9.0") %>%
        rstac::stac_search(collection = collection,
                           bbox       = roi$bbox,
                           intersects = roi$intersects,
                           datetime   = datetime,
                           limit      = limit_items) %>%
        rstac::ext_query("collection" %in% collection,
                         "platform" %in% "LANDSAT_8",
                         "landsat:collection_category" %in% "T1")

    # if specified, a filter per tile is added to the query
    if (!is.null(tiles)) {
        sep_tile <- .sits_usgs_format_tiles(tiles)

        rstac_query <- rstac_query %>%
            rstac::ext_query("landsat:wrs_path" %in% sep_tile$wrs_path,
                             "landsat:wrs_row" %in% sep_tile$wrs_row)
    }

    # making the request and filtering images by interval
    items_info <- rstac_query %>%
        rstac::post_request() %>%
        .sits_usgs_filter_datetime(start_date = start_date,
                                   end_date = end_date)

    # checks if the collection returned any items
    assertthat::assert_that(
        !(rstac::items_length(items_info) == 0),
        msg = ".sits_usgs_items: the provided search returned zero items."
    )

    # progress bar status
    pgr_fetch  <- FALSE

    # if more than 1000 items are found the progress bar is displayed
    if (rstac::items_matched(items_info,
                             path_row = c("meta", "found")) > 1000)
        pgr_fetch <- TRUE

    # fetching all the metadata and updating to upper case instruments
    items_info <- rstac::items_fetch(items_info,
                                     progress = pgr_fetch,
                                     path_row = c("meta", "found"))

    # getting sensor name
    sensor <- .sits_config_sensors("LANDSAT-8")

    # getting bands name
    items_info <- .sits_stac_bands(.sits_usgs_fix_href(items_info),
                                   bands,
                                   source = "USGS",
                                   sensor = sensor)

    # store tile info in items object
    items_info$features <- purrr::map(items_info$features, function(features) {
        features$properties$tile <- paste0(
            features$properties[["landsat:wrs_path"]],
            features$properties[["landsat:wrs_row"]])

        features
    })

    return(items_info)
}

#' @title Fix href on URLs provided by USGS
#' @name .sits_usgs_fix_href
#' @keywords internal
#'
#' @param items      a \code{STACItemCollection} object returned by rstac
#' package.
#'
#' @return  a \code{STACItemCollection} object with URLs fixed.
.sits_usgs_fix_href <- function(items) {

    items$features <- purrr::map(items$features, function(item){

        # remove ext from bands names
        names(item$assets) <- tools::file_path_sans_ext(names(item$assets))

        item$assets <- purrr::map(item$assets, function(assets){
            assets$href <- stringr::str_replace(
                assets$href,
                "^(https://landsatlook.usgs.gov/data)",
                "s3://usgs-landsat"
            )
            assets
        })
        item
    })
    items
}

#' @title Filter datetime in STAC items
#' @name .sits_usgs_filter_datetime
#' @keywords internal
#'
#' @param items      a \code{STACItemCollection} object returned by rstac
#' package.
#' @param start_date a \code{character} with the initial date to search in
#' collection.
#' @param end_date   a \code{character} with the final date to searched in
#' collection.
#'
#' @return  a \code{STACItemCollection} object with datetime filtered.
.sits_usgs_filter_datetime <- function(items, start_date, end_date) {

    # checks if the supplied tiles are in the searched items
    index_features <- purrr::map_lgl(items$features, function(feature) {
        datetime <- lubridate::date(feature[["properties"]][["datetime"]])

        if (datetime >= start_date &&  datetime <= end_date)
            return(TRUE)
        return(FALSE)
    })

    # selects the tiles found in the search
    items$features <- items$features[index_features]

    items
}

#' @title Format tile parameter provided by users
#' @name .sits_usgs_format_tiles
#' @keywords internal
#'
#' @param tiles     a \code{character} vector with the tiles provided by users.
#'
#' @return          a \code{tibble} with attributes of wrs path and row.
.sits_usgs_format_tiles <- function(tiles) {
    # regex pattern of wrs_path and wrs_row
    pattern_l8 <- "[0-9]{6}"

    # verify tile pattern
    if (!any(grepl(pattern_l8, tiles, perl = TRUE)))
        stop(paste("The specified tiles do not match the Landsat-8 grid",
                   "pattern. See the user guide for more information."))

    # list to store the info about the tiles to provide the query in STAC
    list_tiles <- list()
    list_tiles <- purrr::map(tiles, function(tile) {
        list_tiles$wrs_path <- substring(tile, 1, 3)
        list_tiles$wrs_row <- substring(tile, 4, 6)

        list_tiles
    })

    tiles_tbl <- dplyr::bind_rows(list_tiles)

    return(tiles_tbl)
}

#' @title Get the STAC information corresponding to a tile.
#' @name .sits_usgs_tile_cube
#' @keywords internal
#'
#' @param name        Name of output data cube.
#' @param items       \code{STACItemCollection} object returned by rstac.
#' @param collection  Image collection in USGS
#' @param file_info   file information with date/time.
#'
#' @return          a \code{tibble} with metadata information about a
#'                  raster data set.
.sits_usgs_tile_cube <- function(name, items, collection, file_info) {

    # store items properties attributes
    item_prop <- items$features[[1]]$properties

    # get stac bands
    bands <- items[["bands"]]

    # get params from an example image
    params <- .sits_raster_api_params_file(file_info$path[1])

    # format image crs
    item_prop[["proj:epsg"]] <- .sits_format_crs(params[["crs"]])

    # obtain bbox extent
    bbox <- .sits_stac_get_bbox(items, item_prop[["proj:epsg"]])

    # get resolution
    res <- list(xres = item_prop[["eo:gsd"]], yres = item_prop[["eo:gsd"]])

    # add resolution to file_info
    file_info <- dplyr::mutate(file_info,
                               res = as.numeric(res[["xres"]]),
                               .before = path)

    # create a tibble to store the metadata
    tile <- .sits_cube_create(
        name       = name,
        source     = "USGS",
        collection = collection,
        satellite  = "LANDSAT-8",
        sensor     = "OLI",
        tile       = item_prop[["tile"]],
        bands      = bands,
        labels     = labels,
        nrows      = params[["nrows"]],
        ncols      = params[["ncols"]],
        xmin       = bbox[["xmin"]],
        xmax       = bbox[["xmax"]],
        ymin       = bbox[["ymin"]],
        ymax       = bbox[["ymax"]],
        xres       = res[["xres"]],
        yres       = res[["yres"]],
        crs        = item_prop[["proj:epsg"]],
        file_info  = file_info)

    tile <- .sits_config_bands_stac_write(tile)

    return(tile)
}
