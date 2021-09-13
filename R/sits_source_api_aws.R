#' @title Get aws sits bands by resolution
#' @name .aws_bands
#' @keywords internal
#'
#' @description retrieve aws bands by resolution
#'
#' @param source         Data source (one of "SATVEG", "LOCAL", "BDC", "AWS",
#'                       "USGS", "DEAFRICA", "PROBS").
#' @param collection     Collection to be searched in the data source.
#' @param s2_resolution  sentinel band resolution
#'
#' @return sentinel bands that corresponds a specified resolution.
.aws_bands <- function(source, collection, s2_resolution) {

    .source_bands(
        source = source,
        collection = collection,
        fn_filter = function(x) s2_resolution %in% x$resolution
    )
}

#' @title Get aws source bands by resolution
#' @name .aws_bands_band_name
#' @keywords internal
#'
#' @description retrieve aws bands by resolution
#'
#' @param source         Data source (one of "SATVEG", "LOCAL", "BDC", "AWS",
#'                       "USGS", "DEAFRICA", "PROBS").
#' @param collection     Collection to be searched in the data source.
#' @param s2_resolution  sentinel band resolution
#'
#' @return sentinel bands that corresponds a specified resolution.
.aws_bands_band_name <- function(source, collection, s2_resolution) {

    .source_bands_band_name(
        source = source,
        collection = collection,
        fn_filter = function(x) s2_resolution %in% x$resolution
    )
}

#' @title Verify items tiles
#' @name .sits_s2_aws_tiles
#' @keywords internal
#'
#' @param tiles  Tile names to be searched.
#'
#' @return a \code{tibble} with information of tiles to be searched in STAC AWS.
.aws_tiles <- function(tiles) {

    # regex pattern
    pattern_s2 <- "[0-9]{2}[A-Z]{3}"

    # verify tile pattern
    if (!any(grepl(pattern_s2, tiles, perl = TRUE)))
        stop(paste("The specified tiles do not match the Sentinel-2A grid",
                   "pattern. See the user guide for more information."))

    # list to store the info about the tiles to provide the query in STAC
    list_tiles <- list()
    list_tiles <- purrr::map(tiles, function(tile) {
        list_tiles$utm_zone <- substring(tile, 1, 2)
        list_tiles$lat_band <- substring(tile, 3, 3)
        list_tiles$grid_square <- substring(tile, 4, 5)

        list_tiles
    })

    tiles_tbl <- dplyr::bind_rows(list_tiles)

    return(tiles_tbl)
}

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
                                            collection = NULL,
                                            s2_resolution) {

    # Adding the spatial resolution in the band URL
    href_res <- gsub(pattern = "R[0-9]{2}m",
                     replacement = paste0("R", s2_resolution, "m"),
                     x = unname(purrr::map_chr(item[["assets"]], `[[`, "href")))

    # add gdal vsi in href urls
    return(.stac_add_gdal_vsi(href_res))
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
                                                  collection = NULL,
                                                  s2_resolution) {
    return(s2_resolution)
}

#' @keywords internal
#' @export
.source_items_new.aws_cube <- function(source,
                                       collection, ...,
                                       stac_query,
                                       tiles = NULL) {

    # set caller to show in errors
    .check_set_caller(".source_items_new.aws_cube")

    # if specified, a filter per tile is added to the query
    if (!is.null(tiles)) {
        sep_tile <- .aws_tiles(tiles)

        stac_query <-
            rstac::ext_query(q = stac_query,
                             "sentinel:utm_zone" %in% sep_tile$utm_zone,
                             "sentinel:latitude_band" %in% sep_tile$lat_band,
                             "sentinel:grid_square" %in% sep_tile$grid_square)
    }

    # making the request
    items_info <- rstac::post_request(q = stac_query, ...)

    # check if matched items
    .check_that(
        x = rstac::items_matched(items_info) > 0,
        msg = "no items matched the query criteria."
    )

    # if more than 2 times items pagination are found the progress bar
    # is displayed
    pgr_fetch <- rstac::items_matched(items_info) > 2 * .config_rstac_limit()

    # fetching all the metadata
    items_info <- rstac::items_fetch(items = items_info, progress = pgr_fetch)

    return(items_info)
}

#' @keywords internal
#' @export
.source_items_bands_select.aws_cube <- function(source,
                                                collection,
                                                items,
                                                bands, ...,
                                                s2_resolution) {

    # convert sits bands to source bands
    bands_converter <-  .source_bands(
        source = source,
        collection = collection,
        fn_filter = function(x) s2_resolution %in% x$resolution
    )

    items <- .stac_bands_select(
        items = items,
        bands_source = .source_bands_to_source(source = source,
                                               collection = collection,
                                               bands = bands),
        bands_sits = .source_bands_to_sits(source = source,
                                           collection = collection,
                                           bands = bands)
    )

    return(items)
}

#' @keywords internal
#' @export
.source_items_tiles_group.aws_cube <- function(source,
                                               items, ...,
                                               collection = NULL) {

    # store tile info in items object
    items$features <- purrr::map(items$features, function(feature) {
        feature$properties$tile <- paste0(
            feature$properties[["sentinel:utm_zone"]],
            feature$properties[["sentinel:latitude_band"]],
            feature$properties[["sentinel:grid_square"]])

        feature
    })

    rstac::items_group(items, field = c("properties", "tile"))
}

#' @keywords internal
#' @export
.source_items_tile_get_crs.aws_cube <- function(source,
                                                tile_items, ...,
                                                collection = NULL) {

    # format collection crs
    crs <- .sits_proj_format_crs(
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
                                                 collection = NULL,
                                                 s2_resolution) {


    href <- .source_item_get_hrefs(source = source,
                                   item = tile_items[["features"]][[1]], ...,
                                   collection = collection,
                                   s2_resolution = s2_resolution)

    # read the first image and obtain the size parameters
    params <- .raster_params_file(href)

    bbox <- c(xmin = params[["xmin"]], ymin = params[["ymin"]],
              xmax = params[["xmax"]], ymax = params[["ymax"]])

    return(bbox)
}

#' @keywords internal
#' @export
.source_items_tile_get_size.aws_cube <- function(source,
                                                 tile_items, ...,
                                                 collection = NULL,
                                                 s2_resolution) {

    href <- .source_item_get_hrefs(source = source,
                                   item = tile_items[["features"]][[1]], ...,
                                   collection = collection,
                                   s2_resolution = s2_resolution)

    # read the first image and obtain the size parameters
    params <- .raster_params_file(href)

    size <- c(nrows = params[["nrows"]], ncols = params[["ncols"]])

    return(size)
}
