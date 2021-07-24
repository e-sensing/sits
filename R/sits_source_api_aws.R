#' @keywords internal
#' @export
.source_item_get_date.aws_cube <- function(source,
                                           item, ...,
                                           collection = NULL) {
    item[[c("properties", "datetime")]]
}

#' @keywords internal
#' @export
.source_item_get_hrefs.aws_cube <- function(source,
                                            item, ...,
                                            collection = NULL) {

    # Adding the spatial resolution in the band URL
    href_res <- gsub(pattern = "R[0-9]{2}m",
                     replacement = paste0("R", ..., "m"),
                     x = unname(purrr::map_chr(item[["assets"]], `[[`, "href")))


    return(href_res)
}

#' @keywords internal
#' @export
.source_item_get_bands.aws_cube <- function(source,
                                            item, ...,
                                            collection = NULL) {
    names(item[["assets"]])
}

#' @keywords internal
#' @export
.source_item_get_resolutions.aws_cube <- function(source,
                                                  item, ...,
                                                  collection = NULL) {
    item[[c("properties", "gsd")]]
}

#' @keywords internal
#' @export
.source_items_new.aws_cube <- function(source,
                                       collection,
                                       name,
                                       bands,
                                       tiles,
                                       bbox,
                                       start_date,
                                       end_date, ...) {

    url <- .config_src_url(source = source)
    roi <- list(bbox = NULL, intersects = NULL)

    # obtain the bounding box and intersects parameters
    if (!is.null(bbox))
        roi <- .sits_stac_roi(bbox)

    # obtain the datetime parameter for STAC like parameter
    datetime <- .sits_stac_datetime(start_date, end_date)

    # get the limit items to be returned in each page
    limit_items <- .config_rstac_limit()

    # creating an query object to be search
    rstac_query <-  rstac::stac_search(q = rstac::stac(url),
                                       collections = collection,
                                       bbox        = roi$bbox,
                                       intersects  = roi$intersects,
                                       datetime    = datetime,
                                       limit       = limit_items)

    # if specified, a filter per tile is added to the query
    if (!is.null(tiles)) {
        sep_tile <- .sits_s2_aws_tiles(tiles)

        rstac_query <-
            rstac::ext_query(q = rstac_query,
                             "sentinel:utm_zone" %in% sep_tile$utm_zone,
                             "sentinel:latitude_band" %in% sep_tile$lat_band,
                             "sentinel:grid_square" %in% sep_tile$grid_square)
    }

    # making the request
    items_info <- rstac::post_request(q = rstac_query, ...)

    # check if matched items
    assertthat::assert_that(
        rstac::items_matched(items_info) > 0,
        msg = ".source_items_new.aws_cube: no items matched the query criteria."
    )

    # progress bar status
    pgr_fetch  <- FALSE

    # if more than 1000 items are found the progress bar is displayed
    if (rstac::items_matched(items_info) > 1000)
        pgr_fetch <- TRUE

    # fetching all the metadata
    items_info <- rstac::items_fetch(items = items_info, progress = pgr_fetch)

    # store tile info in items object
    items_info$features <- purrr::map(items_info$features, function(features) {
        features$properties$tile <- paste0(
            features$properties[["sentinel:utm_zone"]],
            features$properties[["sentinel:latitude_band"]],
            features$properties[["sentinel:grid_square"]])

        features
    })

    return(items_info)
}

#' @keywords internal
#' @export
.source_items_bands_select.aws_cube <- function(source,
                                                collection,
                                                items,
                                                bands, ...,
                                                s2_resolution) {

    bands_sits <- .config_bands(source = source, collection = collection)

    bands_converter <- .source_bands_to_source(source, collection, bands_sits)

    names(bands_converter) <- bands_sits

    items <- .sits_stac_bands_select(
        items = items,
        bands_source = .source_bands_to_source(source, collection, bands),
        bands_converter = bands_converter
    )

    return(items)
}

#' @keywords internal
#' @export
.source_items_tiles_group.aws_cube <- function(source,
                                               items, ...,
                                               collection = NULL) {

    rstac::items_group(items, field = c("properties", "tile"))
}

#' @keywords internal
#' @export
.source_items_get_sensor.aws_cube <- function(source,
                                              items, ...,
                                              collection = NULL) {

    items[["features"]][[1]][[c("properties", "instruments")]]
}

#' @keywords internal
#' @export
.source_items_get_satellite.aws_cube <- function(source,
                                                 items, ...,
                                                 collection = NULL) {
    items[["features"]][[1]][[c("properties", "constellation")]]
}

#' @keywords internal
#' @export
.source_items_tile_get_crs.aws_cube <- function(source,
                                                tile_items, ...,
                                                collection = NULL) {

    # format collection crs
    crs <- .sits_format_crs(
        tile_items[["features"]][[1]][[c("properties", "proj:epsg")]]
    )

    return(crs)
}

#' @keywords internal
#' @export
.source_items_tile_get_name.aws_cube <- function(source,
                                                 tile_items, ...,
                                                 collection = NULL) {

    tile_items[["features"]][[1]][[c("properties", "tile")]]
}

#' @keywords internal
#' @export
.source_items_tile_get_bbox.aws_cube <- function(source,
                                                 tile_items, ...,
                                                 collection = NULL) {

    # read the first image and obtain the size parameters
    params <- .sits_raster_api_params_file(
        tile_items[["features"]][[1]][["assets"]][[1]][["href"]]
    )

    bbox <- c(xmin = params[["xmin"]], ymin = params[["ymin"]],
              xmax = params[["xmax"]], ymax = params[["ymax"]])

    return(bbox)
}

#' @keywords internal
#' @export
.source_items_tile_get_size.aws_cube <- function(source,
                                                 tile_items, ...,
                                                 collection = NULL) {

    # read the first image and obtain the size parameters
    params <- .sits_raster_api_params_file(
        tile_items[["features"]][[1]][["assets"]][[1]][["href"]]
    )

    size <- c(nrows = params[["nrows"]], ncols = params[["ncols"]])

    return(size)
}
