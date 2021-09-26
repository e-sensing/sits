#' @keywords internal
#' @export
.source_item_get_date.opendata_cube <- function(source,
                                             item, ...,
                                             collection = NULL) {
    item[[c("properties", "datetime")]]
}

#' @keywords internal
#' @export
.source_item_get_hrefs.opendata_cube <- function(source,
                                              item, ...,
                                              collection = NULL) {

    href <- unname(purrr::map_chr(item[["assets"]], `[[`, "href"))

    # add gdal vsi in href urls
    return(.stac_add_gdal_vsi(href))
}

#' @keywords internal
#' @export
.source_item_get_bands.opendata_cube <- function(source,
                                              item, ...,
                                              collection = NULL) {
    names(item[["assets"]])
}

#' @keywords internal
#' @export
.source_item_get_resolutions.opendata_cube <- function(source,
                                                    item, ...,
                                                    collection = NULL) {

    res <- .source_bands_resolutions(
        source = source,
        collection = collection,
        bands = .source_item_get_bands(source = source,
                                       item = item)
    )

    unlist(res)
}

#' @keywords internal
#' @export
.source_items_new.opendata_cube <- function(source,
                                         collection, ...,
                                         stac_query,
                                         tiles = NULL) {

    # set caller to show in errors
    .check_set_caller(".source_items_new.opendata_cube")

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
.source_items_tiles_group.opendata_cube <- function(source,
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
.source_items_tile_get_crs.opendata_cube <- function(source,
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
.source_items_tile_get_name.opendata_cube <- function(source,
                                                   tile_items, ...,
                                                   collection = NULL) {

    tile_items[["features"]][[1]][[c("properties", "tile")]]
}

#' @keywords internal
#' @export
.source_items_tile_get_bbox.opendata_cube <- function(source,
                                                   tile_items, ...,
                                                   collection = NULL) {


    href <- .source_item_get_hrefs(source = source,
                                   item = tile_items[["features"]][[1]], ...,
                                   collection = collection)

    # read the first image and obtain the size parameters
    params <- .raster_params_file(href)

    bbox <- c(xmin = params[["xmin"]], ymin = params[["ymin"]],
              xmax = params[["xmax"]], ymax = params[["ymax"]])

    return(bbox)
}

#' @keywords internal
#' @export
.source_items_tile_get_size.opendata_cube <- function(source,
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
