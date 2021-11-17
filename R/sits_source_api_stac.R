#' @keywords internal
#' @export
.source_collection_access_test.stac_cube <- function(source, ...,
                                                     collection, bands,
                                                     dry_run = TRUE) {
    # require package
    if (!requireNamespace("rstac", quietly = TRUE)) {
        stop("Please install package rstac", call. = FALSE
        )
    }

    items_query <- .stac_items_query(source = source,
                                     collection = collection,
                                     limit = 1)

    # assert that service is online
    tryCatch({
        items <- rstac::post_request(items_query, ...)
    }, error = function(e) {
        stop(paste(".source_collection_access_test.stac_cube: service is unreachable\n",
                   e$message), call. = FALSE)
    })

    items <- .source_items_bands_select(source = source, ...,
                                        collection = collection,
                                        items = items,
                                        bands = bands[[1]])

    href <- .source_item_get_hrefs(source = source, ...,
                                   item = items$feature[[1]],
                                   collection = collection)

    # assert that token and/or href is valid
    if (dry_run)
        tryCatch({
            .raster_open_rast(href)
        }, error = function(e) {
            stop(paste(".source_collection_access_test.stac_cube: cannot",
                       "open url\n", href, "\n", e$message), call. = FALSE)
        })


    return(invisible(NULL))
}

#' @keywords internal
#' @export
.source_cube.stac_cube <- function(source, ...,
                                   collection,
                                   bands,
                                   tiles,
                                   bbox,
                                   start_date,
                                   end_date) {

    # set caller to show in errors
    .check_set_caller(".source_cube.stac_cube")

    items_query <- .stac_items_query(source = source,
                                     collection = collection,
                                     bbox = bbox,
                                     start_date = start_date,
                                     end_date = end_date, ...)

    items <- .source_items_new(source = source, ...,
                               collection = collection,
                               stac_query = items_query,
                               tiles = tiles)

    items <- .source_items_bands_select(source = source, ...,
                                        collection = collection,
                                        items = items,
                                        bands = bands)

    items_lst <- .source_items_tiles_group(source = source, ...,
                                           collection = collection,
                                           items = items)

    cube <- purrr::map_dfr(items_lst, function(tile) {

        file_info <- .source_items_fileinfo(source = source, ...,
                                            items = tile,
                                            collection = collection)

        tile_cube <- .source_items_cube(source = source, ...,
                                        collection = collection,
                                        items = tile,
                                        file_info = file_info)

        return(tile_cube)
    })

    class(cube) <- .cube_s3class(cube)

    return(cube)
}

#' @keywords internal
#' @export
.source_items_bands_select.stac_cube <- function(source, ...,
                                                 collection,
                                                 items,
                                                 bands) {

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
.source_items_fileinfo.stac_cube <- function(source, ...,
                                             items,
                                             collection = NULL) {

    # set caller to show in errors
    .check_set_caller(".source_items_fileinfo.stac_cube")

    file_info <- purrr::map_dfr(items$features, function(item){

        date <- suppressWarnings(
            lubridate::as_date(.source_item_get_date(source = source,
                                                     item = item, ...,
                                                     collection = collection))
        )

        bands <- .source_item_get_bands(source = source, ...,
                                        item = item,
                                        collection = collection)

        res <- .source_item_get_resolution(source = source, ...,
                                           item = item,
                                           collection = collection)

        paths <- .source_item_get_hrefs(source = source, ...,
                                        item = item,
                                        collection = collection)

        .check_that(
            x = !is.na(date),
            msg = "invalid date format."
        )

        .check_that(
            x = is.character(bands),
            msg = "invalid band format."
        )

        .check_that(
            x = is.numeric(res),
            msg = "invalid res format."
        )

        .check_that(
            x = is.character(paths),
            msg = "invalid path format."
        )

        tidyr::unnest(
            tibble::tibble(
                date = date,
                band = list(bands),
                res = list(res),
                path = list(paths)
            ), cols = c("band", "res", "path")
        )
    }) %>% dplyr::arrange(date)

    file_info <- dplyr::group_by(file_info, date, band, res) %>%
        dplyr::summarise(
            path = dplyr::first(path, order_by = path),
            .groups = "drop"
        )

    return(file_info)
}

#' @keywords internal
#' @export
.source_items_cube.stac_cube <- function(source, ...,
                                         collection,
                                         items,
                                         file_info) {

    # set caller to show in errors
    .check_set_caller(".source_items_cube.stac_cube")

    t_bbox <- .source_items_tile_get_bbox(source = source, ...,
                                          tile_items = items,
                                          collection = collection)

    .check_chr_within(
        x = names(t_bbox),
        within = c("xmin", "ymin", "xmax", "ymax"),
        msg = paste(".source_items_cube.stac_cube: bbox must be have",
                    "'xmin', 'ymin', 'xmax', and 'ymax' names.")
    )

    .check_num_type(x = t_bbox,
                    msg = "bbox must be numeric.")

    # tile name
    t_name <- .source_items_tile_get_name(source = source, ...,
                                          tile_items = items,
                                          collection = collection)

    .check_chr_type(
        x = t_name,
        msg = "name must be a character value."
    )

    t_crs <- .source_items_tile_get_crs(source = source, ...,
                                        tile_items = items,
                                        collection = collection)
    .check_that(
        x = is.character(t_crs) || is.numeric(t_crs),
        msg = "name must be a character or numeric value."
    )

    tile <- .cube_create(
        source     = source[[1]],
        collection = collection[[1]],
        satellite  = .source_collection_satellite(source, collection),
        sensor     = .source_collection_sensor(source, collection),
        tile       = t_name[[1]],
        xmin       = t_bbox[["xmin"]],
        xmax       = t_bbox[["xmax"]],
        ymin       = t_bbox[["ymin"]],
        ymax       = t_bbox[["ymax"]],
        crs        = t_crs[[1]],
        file_info  = file_info)

    return(tile)
}
#' @keywords internal
#' @export
.source_item_get_date.stac_cube <- function(source, ...,
                                            item,
                                            collection = NULL) {
    item[[c("properties", "datetime")]]
}

#' @keywords internal
#' @export
.source_item_get_hrefs.stac_cube <- function(source, ...,
                                             item,
                                             collection = NULL) {

    href <- unname(purrr::map_chr(item[["assets"]], `[[`, "href"))

    # add gdal vsi in href urls
    return(.stac_add_gdal_vsi(href))
}
#' @keywords internal
#' @export
.source_item_get_bands.stac_cube <- function(source,
                                             item, ...,
                                             collection = NULL) {
    names(item[["assets"]])
}
#' @keywords internal
#' @export
.source_item_get_resolution.stac_cube <- function(source, ...,
                                                   item,
                                                   collection = NULL) {
    # use config information to get resolution
    res <- .source_bands_resolution(
        source = source,
        collection = collection,
        bands = .source_item_get_bands(source = source,
                                       item = item)
    )

    return(unlist(res))
}
#' @keywords internal
#' @export
.source_items_tile_get_name.stac_cube <- function(source, ...,
                                                  tile_items,
                                                  collection = NULL) {

    tile_items[["features"]][[1]][[c("properties", "tile")]]
}

#' @keywords internal
#' @export
.source_items_tile_get_bbox.stac_cube <- function(source, ...,
                                                  tile_items,
                                                  collection = NULL) {

    # get collection crs
    crs <- .source_items_tile_get_crs(source = source, ...,
                                      tile_items = tile_items,
                                      collection = collection)

    bbox <- .stac_get_bbox(tile_items, crs)

    return(bbox)
}
