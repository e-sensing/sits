#' @keywords internal
#' @export
.source_collection_access_test.stac_cube <- function(source, collection,
                                                     bands, ...,
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

    items <- .source_items_bands_select(source = source,
                                        items = items,
                                        bands = bands[[1]],
                                        collection = collection, ...)

    href <- .source_item_get_hrefs(source = source,
                                   item = items$feature[[1]],
                                   collection = collection, ...)

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
.source_cube.stac_cube <- function(source,
                                   collection,
                                   bands,
                                   tiles,
                                   bbox,
                                   start_date,
                                   end_date, ...) {

    # set caller to show in errors
    .check_set_caller(".source_cube.stac_cube")

    # prepares a query object
    items_query <- .stac_items_query(source = source,
                                     collection = collection,
                                     bbox = bbox,
                                     start_date = start_date,
                                     end_date = end_date, ...)

    # make query and retrieve items
    items <- .source_items_new(source = source,
                               collection = collection,
                               stac_query = items_query,
                               tiles = tiles, ...)

    # filter bands in items
    items <- .source_items_bands_select(source = source,
                                        items = items,
                                        bands = bands,
                                        collection = collection, ...)

    # group items by tile
    items_lst <- .source_items_tiles_group(source = source,
                                           collection = collection,
                                           items = items, ...)

    # make a cube for each tile (rows)
    cube <- purrr::map_dfr(items_lst, function(items_tile) {

        # make a new file info for one tile
        file_info <- .source_items_file_info(source = source,
                                             items = items_tile,
                                             collection = collection, ...)

        # make a new cube tile
        tile_cube <- .source_items_cube(source = source,
                                        collection = collection,
                                        items = items_tile,
                                        file_info = file_info, ...)

        return(tile_cube)
    })

    class(cube) <- .cube_s3class(cube)

    return(cube)
}

#' @keywords internal
#' @export
.source_items_bands_select.stac_cube <- function(source,
                                                 items,
                                                 bands,
                                                 collection, ...) {

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
.source_items_file_info.stac_cube <- function(source,
                                              items, ...,
                                              collection = NULL) {

    # set caller to show in errors
    .check_set_caller(".source_items_file_info.stac_cube")

    # start file_info by feature id
    file_info <- purrr::map_dfr(items$features, function(item) {

        fid <- .source_item_get_fid(source = source,
                                    item = item,
                                    collection = collection, ...)

        return(tibble::tibble(fid = fid))
    })

    # post-condition

    .check_that(
        nrow(file_info) == length(unique(file_info$fid)),
        local_msg = "feature id is not unique",
        msg = "invalid feature id values"
    )

    # prepare number of workers
    progress <- TRUE
    n_workers <- .config_gdalcubes_open_connections()
    if (.source_collection_metadata_search(
        source = source,
        collection = collection) == "tile") {

        n_workers <- 1
    } else if (.config_gdalcubes_min_files_for_parallel() >
               length(items$features)) {
        n_workers <- 1
        progress <- FALSE
    }

    # prepare parallel requests
    .sits_parallel_start(n_workers, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)

    # do in case of 'tile' strategy
    if (.source_collection_metadata_search(source = source,
                                           collection = collection) == "tile") {

        # get first item
        item <- items$features[[1]]

        # get file paths
        paths <- .source_item_get_hrefs(source = source,
                                        item = item,
                                        collection = collection, ...)

        # open band rasters
        assets <- purrr::map(paths, .raster_open_rast)

        # get asset info
        asset_info <- purrr::map(assets, function(asset) {
            res <- .raster_res(asset)
            bbox <- .raster_bbox(asset)
            size <- .raster_size(asset)
            tibble::as_tibble_row(c(res, bbox, size))
        })
    }

    # do parallel requests
    file_info$meta_data <- .sits_parallel_map(
        items$features,
        function(item) {

            # get date
            date <- .source_item_get_date(source = source,
                                          item = item,
                                          collection = collection, ...)

            # get bands
            bands <- .source_item_get_bands(source = source,
                                            item = item,
                                            collection = collection, ...)

            # get file paths
            paths <- .source_item_get_hrefs(source = source,
                                            item = item,
                                            collection = collection, ...)

            # add cloud cover statistics
            cloud_cover <- .source_item_get_cc(source = source, ...,
                                               item = item,
                                               collection = collection)

            # do in case of 'tile' strategy
            if (.source_collection_metadata_search(source = source,
                                                   collection = collection) ==
                "feature") {

                # open band rasters
                assets <- purrr::map(paths, .raster_open_rast)

                # get asset info
                asset_info <- purrr::map(assets, function(asset) {
                    res <- .raster_res(asset)
                    bbox <- .raster_bbox(asset)
                    size <- .raster_size(asset)
                    tibble::as_tibble_row(c(res, bbox, size))
                })
            }

            # post-conditions
            .check_na(date, msg = "invalid date value")

            .check_length(date, len_min = 1, len_max = 1,
                          msg = "invalid date value")

            .check_chr(bands, len_min = 1, msg = "invalid band value")

            tidyr::unnest(
                tibble::tibble(
                    date = date,
                    band = bands,
                    asset_info = asset_info,
                    path = paths,
                    cloud_cover = cloud_cover
                ), cols = c("band", "asset_info", "path", "cloud_cover")
            )
        },
        progress = progress
    )

    # arrange
    file_info <- dplyr::arrange(
        tidyr::unnest(file_info, cols = "meta_data"),
        date, fid, band
    )

    return(file_info)
}

#' @keywords internal
#' @export
.source_items_cube.stac_cube <- function(source,
                                         collection,
                                         items,
                                         file_info, ...) {

    # set caller to show in errors
    .check_set_caller(".source_items_cube.stac_cube")

    bbox <- .source_items_tile_get_bbox(source = source,
                                        tile_items = items,
                                        file_info = file_info,
                                        collection = collection, ...)

    # post-conditions
    .check_chr_contains(
        names(bbox),
        contains = c("xmin", "ymin", "xmax", "ymax"),
        msg = "invalid bbox value"
    )

    .check_num(bbox, len_min = 4, len_max = 4, is_named = TRUE,
               msg = "invalid bbox value")

    # tile name
    tile_name <- .source_items_tile_get_name(source = source,
                                             tile_items = items,
                                             collection = collection, ...)

    # post-conditions
    .check_chr(tile_name, allow_empty = FALSE, len_min = 1, len_max = 1,
               msg = "invalid tile name value")

    crs <- .source_items_tile_get_crs(source = source,
                                      tile_items = items,
                                      collection = collection, ...)

    # post-conditions
    .check_that(
        x = is.character(crs) || is.numeric(crs),
        local_msg = "name must be a character or numeric value",
        msg = "invalid CRS value"
    )

    tile <- .cube_create(
        source     = source[[1]],
        collection = collection[[1]],
        satellite  = .source_collection_satellite(source, collection),
        sensor     = .source_collection_sensor(source, collection),
        tile       = tile_name[[1]],
        xmin       = bbox[["xmin"]],
        xmax       = bbox[["xmax"]],
        ymin       = bbox[["ymin"]],
        ymax       = bbox[["ymax"]],
        crs        = crs[[1]],
        file_info  = file_info)

    return(tile)
}

#' @keywords internal
#' @export
.source_item_get_fid.stac_cube <- function(source,
                                           item, ...,
                                           collection = NULL) {

    fid <- item[["id"]]

    # post-conditions
    .check_chr(fid, allow_empty = FALSE, len_min = 1, len_max = 1,
               msg = "invalid feature id value")

    return(fid)
}

#' @keywords internal
#' @export
.source_item_get_date.stac_cube <- function(source,
                                            item, ...,
                                            collection = NULL) {

    suppressWarnings(
        lubridate::as_date(item[[c("properties", "datetime")]])
    )
}

#' @keywords internal
#' @export
.source_item_get_hrefs.stac_cube <- function(source,
                                             item, ...,
                                             collection = NULL) {

    hrefs <- unname(purrr::map_chr(item[["assets"]], `[[`, "href"))

    # post-conditions
    .check_chr(hrefs, allow_empty = FALSE)

    # add gdal vsi in href urls
    hrefs <- .stac_add_gdal_vsi(hrefs)

    return(hrefs)
}

#' @keywords internal
#' @export
.source_item_get_cc.stac_cube <- function(source, ...,
                                          item,
                                          collection = NULL) {

    item[["properties"]][["eo:cloud_cover"]]
}

#' @keywords internal
#' @export
.source_item_get_bands.stac_cube <- function(source,
                                             item, ...,
                                             collection = NULL) {
    names(item[["assets"]])
}

#' @rdname source_cube
#'
#' @description \code{.source_items_tile_get_bbox()} retrieves the bounding
#' box from items of a tile.
#'
#' @return \code{.source_items_tile_get_bbox()} returns a \code{list}
#' vector with 4 elements (xmin, ymin, xmax, ymax).
#'
.source_items_tile_get_bbox.stac_cube <- function(source,
                                                  tile_items,
                                                  file_info, ...,
                                                  collection = NULL) {

    .check_set_caller(".source_items_tile_get_bbox.stac_cube")

    # pre-condition
    .check_num(nrow(file_info), min = 1, msg = "invalid 'file_info' value")

    # get bbox based on file_info
    xmin <- max(file_info[["xmin"]])
    ymin <- max(file_info[["ymin"]])
    xmax <- min(file_info[["xmax"]])
    ymax <- min(file_info[["ymax"]])

    # post-condition
    .check_that(xmin < xmax,
                local_msg = "xmin is greater than xmax",
                msg = "invalid bbox value")

    .check_that(ymin < ymax,
                local_msg = "ymin is greater than ymax",
                msg = "invalid bbox value")

    # create a bbox
    bbox <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)

    return(bbox)
}

#' @keywords internal
#' @export
.source_items_tile_get_name.stac_cube <- function(source,
                                                  tile_items, ...,
                                                  collection = NULL) {

    tile_items[["features"]][[1]][[c("properties", "tile")]]
}
