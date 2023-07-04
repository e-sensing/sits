#' @keywords internal
#' @noRd
#' @export
.source_collection_access_test.stac_cube <- function(source, collection,
                                                     bands, ...,
                                                     start_date = NULL,
                                                     end_date = NULL,
                                                     dry_run = FALSE) {
    # require package
    .check_require_packages("rstac")

    items_query <- .stac_create_items_query(
        source = source,
        collection = collection,
        start_date = start_date,
        end_date = end_date,
        limit = 1
    )

    # assert that service is online
    tryCatch(
        {
            items <- rstac::post_request(items_query, ...)
        },
        error = function(e) {
            stop(
                paste(
                    ".source_collection_access_test.stac_cube: service is",
                    "unreachable\n", e$message
                ),
                call. = FALSE
            )
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
    if (dry_run) {
        tryCatch(
            {
                .raster_open_rast(href)
            },
            error = function(e) {
                stop(paste(
                    ".source_collection_access_test.stac_cube: cannot",
                    "open url\n", href, "\n", e$message
                ), call. = FALSE)
            }
        )
    }
    return(invisible(NULL))
}

#' @keywords internal
#' @noRd
#' @export
.source_cube.stac_cube <- function(source,
                                   collection,
                                   bands,
                                   tiles,
                                   roi,
                                   start_date,
                                   end_date,
                                   platform,
                                   progress, ...) {
    # set caller to show in errors
    .check_set_caller(".source_cube.stac_cube")

    # prepares a query object
    items_query <- .stac_create_items_query(
        source = source,
        collection = collection,
        roi = roi,
        start_date = start_date,
        end_date = end_date, ...
    )
    # make query and retrieve items
    items <- .source_items_new(
        source = source,
        collection = collection,
        stac_query = items_query,
        tiles = tiles,
        platform = platform, ...
    )
    # filter bands in items
    items <- .source_items_bands_select(
        source = source,
        items = items,
        bands = bands,
        collection = collection, ...
    )
    # make a cube
    cube <- .source_items_cube(
        source = source,
        items = items,
        collection = collection,
        progress = progress, ...
    )
    if (is.character(tiles)) {
        # post-condition
        .check_chr_within(.cube_tiles(cube),
            within = tiles,
            can_repeat = FALSE,
            msg = "invalid tile returned in cube"
        )
        # arrange cube tiles according with 'tiles' parameter
        tiles <- tiles[tiles %in% .cube_tiles(cube)]
        cube <- cube[match(.cube_tiles(cube), tiles), ]
    }
    class(cube) <- .cube_s3class(cube)
    return(cube)
}

#' @keywords internal
#' @noRd
#' @export
.source_items_bands_select.stac_cube <- function(source,
                                                 items,
                                                 bands,
                                                 collection, ...) {
    items <- .stac_select_bands(
        items = items,
        bands_source = .source_bands_to_source(
            source = source,
            collection = collection,
            bands = bands
        ),
        bands_sits = .source_bands_to_sits(
            source = source,
            collection = collection,
            bands = bands
        )
    )
    return(items)
}

#' @keywords internal
#' @noRd
#' @export
.source_items_cube.stac_cube <- function(source,
                                         collection = NULL,
                                         items, ...,
                                         multicores = 2,
                                         progress) {
    # set caller to show in errors
    .check_set_caller(".source_items_cube.stac_cube")

    # start by tile and items
    data <- tibble::tibble(
        tile = .source_items_tile(
            source = source,
            collection = collection,
            items = items, ...
        ),
        fid = .source_items_fid(
            source = source,
            collection = collection,
            items = items, ...
        ),
        features = items[["features"]]
    )

    if (.source_collection_metadata_search(
        source = source,
        collection = collection
    ) == "tile") {
        # tile by tile
        data <- data |>
            tidyr::nest(items = c("fid", "features"))
    } else {
        # item by item
        data <- data |>
            dplyr::transmute(
                tile = .data[["tile"]],
                items = purrr::map2(
                    .data[["fid"]], .data[["features"]], function(x, y) {
                        dplyr::tibble(fid = x, features = list(y))
                    }
                )
            )
    }
    # prepare parallel requests
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)

    # do parallel requests
    tiles <- .parallel_map(seq_len(nrow(data)), function(i) {
        # get tile name
        tile <- data[["tile"]][[i]]
        # get fids
        fids <- data[["items"]][[i]][["fid"]]
        # get features
        features <- data[["items"]][[i]][["features"]]
        # post-condition
        .check_num(length(features), min = 1, msg = "invalid features value")
        # get item
        item <- features[[1]]
        # get file paths
        paths <- .source_item_get_hrefs(
            source = source,
            item = item,
            collection = collection, ...
        )
        # post-condition
        .check_num(length(paths), min = 1, msg = "invalid href values")
        # open band rasters and retrieve asset info
        asset_info <- tryCatch(
            {
                purrr::map(paths, function(path) {
                    asset <- .raster_open_rast(path)
                    info <- tibble::as_tibble_row(c(
                        .raster_res(asset),
                        .raster_bbox(asset),
                        .raster_size(asset),
                        list(crs = .raster_crs(asset))
                    ))
                    return(info)
                })
            },
            error = function(e) {
                NULL
            }
        )
        # check if metadata was retrieved
        if (is.null(asset_info)) {
            warning("cannot open files:\n", paste(paths, collapse = ", "),
                call. = FALSE
            )
            return(NULL)
        }
        # generate file_info
        items_info <- purrr::map2_dfr(fids, features, function(fid, item) {
            # get assets name
            bands <- .source_item_get_bands(
                source = source,
                item = item,
                collection = collection, ...
            )
            # get date
            date <- .source_item_get_date(
                source = source,
                item = item,
                collection = collection, ...
            )
            # get file paths
            paths <- .source_item_get_hrefs(
                source = source,
                item = item,
                collection = collection, ...
            )
            # add cloud cover statistics
            cloud_cover <- .source_item_get_cloud_cover(
                source = source,
                item = item,
                collection = collection, ...
            )
            # post-conditions
            .check_na(date, msg = "invalid date value")
            .check_length(date,
                len_min = 1, len_max = 1,
                msg = "invalid date value"
            )
            .check_chr(bands, len_min = 1, msg = "invalid band value")
            .check_chr(paths,
                allow_empty = FALSE, len_min = length(bands),
                len_max = length(bands),
                msg = "invalid path value"
            )
            # do in case of 'feature' strategy
            if (.source_collection_metadata_search(
                source = source,
                collection = collection
            ) == "feature") {
                # open band rasters and retrieve asset info
                asset_info <- tryCatch(
                    {
                        purrr::map(paths, function(path) {
                            asset <- .raster_open_rast(path)
                            info <- tibble::as_tibble_row(c(
                                .raster_res(asset),
                                .raster_bbox(asset),
                                .raster_size(asset),
                                list(crs = .raster_crs(asset))
                            ))
                            return(info)
                        })
                    },
                    error = function(e) {
                        NULL
                    }
                )
                # check if metadata was retrieved
                if (is.null(asset_info)) {
                    warning("cannot open files:\n",
                        paste(paths, collapse = ", "),
                        call. = FALSE
                    )
                    return(NULL)
                }
            }

            # prepare result
            assets_info <- tidyr::unnest(
                tibble::tibble(
                    tile = tile,
                    fid = fid,
                    date = date,
                    band = bands,
                    asset_info = asset_info,
                    path = paths,
                    cloud_cover = cloud_cover
                ),
                cols = c("band", "asset_info", "path", "cloud_cover")
            )
            return(assets_info)
        })

        return(items_info)
    }, progress = progress)

    # bind cube rows
    cube <- dplyr::bind_rows(tiles)

    # post-condition
    .check_that(
        x = nrow(cube) > 0,
        local_msg = "could not retrieve cube metadata",
        msg = "empty cube metadata"
    )

    # review known malformed paths
    review_date <- .try(
        .conf(
            "sources", source,
            "collections", collection,
            "review_dates"
        ),
        .default = NA
    )

    if (!is.na(review_date)) {
        data <- dplyr::filter(cube, .data[["date"]] == !!review_date) |>
            tidyr::nest(assets = -"tile")

        # test paths by open files...
        val <- .parallel_map(seq_len(nrow(data)), function(i) {
            tryCatch(
                {
                    lapply(data$assets[[i]]$path, .raster_open_rast)
                    TRUE
                },
                error = function(e) FALSE
            )
        }, progress = FALSE)

        # which tiles have passed on check
        passed_tiles <- data$tile[unlist(val)]

        # exclude features by date but passed tiles
        cube <- dplyr::filter(
            cube, .data[["date"]] != !!review_date |
                .data[["tile"]] %in% !!passed_tiles
        )
    }

    # prepare cube
    cube <- cube |>
        tidyr::nest(file_info = -dplyr::matches(c("tile", "crs"))) |>
        slider::slide_dfr(function(tile) {
            # get file_info
            file_info <- tile[["file_info"]][[1]]
            # arrange file_info
            file_info <- dplyr::arrange(
                file_info, .data[["date"]],
                .data[["fid"]], .data[["band"]]
            )

            # get tile bbox
            bbox <- .source_tile_get_bbox(
                source = source,
                file_info = file_info,
                collection = collection, ...
            )

            # create cube row
            tile <- .cube_create(
                source     = source,
                collection = collection,
                satellite  = .source_collection_satellite(source, collection),
                sensor     = .source_collection_sensor(source, collection),
                tile       = tile[["tile"]],
                xmin       = bbox[["xmin"]],
                xmax       = bbox[["xmax"]],
                ymin       = bbox[["ymin"]],
                ymax       = bbox[["ymax"]],
                crs        = tile[["crs"]],
                file_info  = file_info
            )
            return(tile)
        })
    return(cube)
}

#' @keywords internal
#' @noRd
#' @export
.source_item_get_date.stac_cube <- function(source,
                                            item, ...,
                                            collection = NULL) {
    suppressWarnings(
        lubridate::as_date(item[[c("properties", "datetime")]])
    )
}

#' @keywords internal
#' @noRd
#' @export
.source_item_get_hrefs.stac_cube <- function(source,
                                             item, ...,
                                             collection = NULL) {
    hrefs <- unname(purrr::map_chr(item[["assets"]], `[[`, "href"))
    # post-conditions
    .check_chr(hrefs, allow_empty = FALSE)
    # add gdal VSI in href urls
    hrefs <- .stac_add_gdal_fs(hrefs)
    return(hrefs)
}

#' @keywords internal
#' @noRd
#' @export
.source_item_get_cloud_cover.stac_cube <- function(source, ...,
                                                   item,
                                                   collection = NULL) {
    item[["properties"]][["eo:cloud_cover"]]
}

#' @keywords internal
#' @noRd
#' @export
.source_item_get_bands.stac_cube <- function(source,
                                             item, ...,
                                             collection = NULL) {
    names(item[["assets"]])
}

#' @rdname source_cube
#' @keywords internal
#' @noRd
#' @description \code{.source_tile_get_bbox()} retrieves the bounding
#' box from items of a tile.
#'
#' @return \code{.source_tile_get_bbox()} returns a \code{list}
#' vector with 4 elements (xmin, ymin, xmax, ymax).
#'
.source_tile_get_bbox.stac_cube <- function(source,
                                            file_info, ...,
                                            collection = NULL) {
    .check_set_caller(".source_tile_get_bbox.stac_cube")

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
        msg = "invalid bbox value"
    )
    .check_that(ymin < ymax,
        local_msg = "ymin is greater than ymax",
        msg = "invalid bbox value"
    )
    # create a bbox
    bbox <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
    return(bbox)
}

#' @keywords internal
#' @noRd
#' @export
.source_items_fid.stac_cube <- function(source,
                                        items, ...,
                                        collection = NULL) {
    fid <- rstac::items_reap(items, field = "id")
    # post-conditions
    .check_length(unique(fid),
        len_min = length(fid), len_max = length(fid),
        msg = "invalid feature id value"
    )

    return(fid)
}
