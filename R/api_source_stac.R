#' @title Test access to STAC collection
#' @keywords internal
#' @noRd
#' @description
#' These functions provide an API to handle/retrieve data from source's
#' collections.
#'
#' @param source     Data source.
#' @param collection Image collection.
#' @param bands      Band names
#' @param ...        Other parameters to be passed for specific types.
#' @param start_date Start date.
#' @param end_date   End date.
#' @param dry_run    TRUE/FALSE
#' @return           Called for side effects
#' @export
.source_collection_access_test.stac_cube <- function(source, collection,
                                                     bands, ...,
                                                     start_date = NULL,
                                                     end_date = NULL,
                                                     dry_run = FALSE) {
    # require package
    .check_require_packages("rstac")
    # create a query
    items_query <- .stac_create_items_query(
        source = source,
        collection = collection,
        start_date = start_date,
        end_date = end_date,
        limit = 1
    )
    # assert that service is online
    items <- .try({rstac::post_request(items_query, ...)},
        default = NULL
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
        # assert that service is online
        items <- .try(
            {
                rstac::post_request(items_query, ...)
            },
            default = NULL
        )
        .check_stac_items(items)
    }
    return(invisible(source))
}
#' @title Function to instantiate a new cube from a source
#' @keywords internal
#' @noRd
#' @description
#' These functions provide an API to instantiate a new cube object and
#' access/retrieve information from services or local files to fill
#' cube attributes.
#'
#' A cube is formed by images (items) organized in tiles. To create a sits
#' cube object (a \code{tibble}), a set of functions are called in order
#' to retrieve metadata.
#'
#' @param source     Data source.
#' @param collection Image collection.
#' @param bands      Bands to be selected in the collection.
#' @param tiles      A set of tiles in the collections reference system
#' @param roi        Region of interest
#' @param start_date Start date.
#' @param end_date   End date.
#' @param platform   Satellite platform
#' @param progress   Show a progress bar?
#' @param ...        Additional parameters.
#' @return           New data cube
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
    # filter tiles
    cube <- .source_filter_tiles(
        source = source,
        collection = collection,
        cube = cube,
        tiles = tiles
    )

    class(cube) <- .cube_s3class(cube)
    return(cube)
}
#' @title Select bands from a STAC item
#' @keywords internal
#' @noRd
#'
#' @param source     Data source
#' @param items      STAC items
#' @param bands      Bands to be selected in the collection.
#' @param collection Image collection
#' @param ...        Additional parameters.
#' @return List of STAC items
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
#' @title Create a new data cube based on STAC item
#' @keywords internal
#' @noRd
#' @param source     Data source
#' @param collection Image collection
#' @param items      STAC items
#' @param ...        Additional parameters.
#' @param multicores Number of cores
#' @param progress   Show progress bar?
#' @return A data cube
#' @export
.source_items_cube.stac_cube <- function(source,
                                         collection = NULL,
                                         items, ...,
                                         multicores = 2,
                                         progress) {
    .check_set_caller(".source_items_cube_stac_cube")

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
        .check_that(length(features) >= 1)
        # get item
        item <- features[[1]]
        # get file paths
        paths <- .source_item_get_hrefs(
            source = source,
            item = item,
            collection = collection, ...
        )
        # post-condition
        .check_that(length(paths) >= 1)
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
            msg <- .conf("messages", ".source_items_cube_stac_cube")
            warning(paste(msg,":\n", paste(paths, collapse = ", ")),
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
            cloud_cover <- .default(cloud_cover, 0)
            # post-conditions
            .check_date_parameter(date)
            .check_chr_parameter(bands, len_min = 1)
            .check_chr_parameter(paths,
                allow_empty = FALSE,
                len_min = length(bands),
                len_max = length(bands)
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
                    msg <- .conf("messages", ".source_items_cube_stac_cube")
                    warning(paste(msg,":\n", paste(paths, collapse = ", ")),
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
    .check_that(nrow(cube) > 0)

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
        dplyr::mutate(crs2 = .data[["crs"]]) |>
        tidyr::nest(file_info = -dplyr::matches(c("tile", "crs2"))) |>
        dplyr::rename(crs = "crs2") |>
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
#' @title Get date from STAC item
#' @keywords internal
#' @noRd
#' @param source     Data source
#' @param item       STAC item
#' @param ...        Additional parameters.
#' @param collection Image collection
#' @return List of dates
#' @export
.source_item_get_date.stac_cube <- function(source,
                                            item, ...,
                                            collection = NULL) {
    suppressWarnings(
        lubridate::as_date(item[[c("properties", "datetime")]])
    )
}
#' @title Get href from STAC item
#' @keywords internal
#' @noRd
#' @param source     Data source
#' @param item       STAC item
#' @param ...        Additional parameters.
#' @param collection Image collection
#' @return HTTP references
#' @export
.source_item_get_hrefs.stac_cube <- function(source,
                                             item, ...,
                                             collection = NULL) {
    .check_set_caller(".source_item_get_hrefs_stac_cube")
    hrefs <- unname(purrr::map_chr(item[["assets"]], `[[`, "href"))
    # post-conditions
    .check_chr_parameter(hrefs, allow_empty = FALSE)
    # add gdal VSI in href urls
    hrefs <- .stac_add_gdal_fs(hrefs)
    return(hrefs)
}
#' @title Get cloud cover from STAC item
#' @keywords internal
#' @noRd
#' @param source     Data source
#' @param ...        Additional parameters.
#' @param item       STAC item
#' @param collection Image collection
#' @return Cloud cover value
#' @export
.source_item_get_cloud_cover.stac_cube <- function(source, ...,
                                                   item,
                                                   collection = NULL) {
    item[["properties"]][["eo:cloud_cover"]]
}
#' @title Get bands from STAC item
#' @keywords internal
#' @noRd
#' @param source     Data source
#' @param item       STAC item
#' @param ...        Additional parameters.
#' @param collection Image collection
#' @return Band names
#' @export
.source_item_get_bands.stac_cube <- function(source,
                                             item, ...,
                                             collection = NULL) {
    names(item[["assets"]])
}
#' @title Get bbox from file info
#' @keywords internal
#' @noRd
#' @param source     Data source
#' @param file_info  File info
#' @param ...        Additional parameters.
#' @param collection Image collection
#' @return vector (xmin, ymin, xmax, ymax).
#' @export
.source_tile_get_bbox.stac_cube <- function(source,
                                            file_info, ...,
                                            collection = NULL) {

    # pre-condition
    .check_that(nrow(file_info) >= 1)

    # get bbox based on file_info
    xmin <- max(file_info[["xmin"]])
    ymin <- max(file_info[["ymin"]])
    xmax <- min(file_info[["xmax"]])
    ymax <- min(file_info[["ymax"]])

    # post-condition
    .check_that(xmin < xmax && ymin < ymax)
    # create a bbox
    bbox <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
    return(bbox)
}
#' @title Get file ID from STAC item
#' @keywords internal
#' @noRd
#' @param source     Data source
#' @param item       STAC item
#' @param ...        Additional parameters.
#' @param collection Image collection
#' @return File IDs
#' @export
.source_items_fid.stac_cube <- function(source,
                                        items, ...,
                                        collection = NULL) {
    .check_set_caller(".source_items_fid_stac_cube")
    fid <- rstac::items_reap(items, field = "id")
    # post-conditions
    exp_length <- length(fid)
    .check_that(length(unique(fid)) == exp_length)
    return(fid)
}
#' @noRd
#' @title Configure access.
#' @param source  Data source
#' @param collection Image collection
#' @return No return, called for side effects
.source_configure_access.stac_cube <- function(source, collection) {
    return(invisible(source))
}
#' @title Adjusts date-time if required by source
#' @noRd
#' @param source  Data source
#' @param date    Date to be adjusted
#' @return Adjusted date
.source_adjust_date.stac_cube <- function(source, date) {
    return(date)
}
#' @title Filter tiles if required by source
#' @noRd
#' @param source  Data source
#' @param cube    Cube to be filtered
#' @param tiles   Tiles to be selected
#' @return Filtered cube
#' @export
.source_filter_tiles.stac_cube <- function(source, collection, cube, tiles) {
    if (is.character(tiles)) {
        # post-condition
        .check_chr_within(.cube_tiles(cube),
                          within = tiles,
                          discriminator = "any_of",
                          can_repeat = FALSE,
                          msg = "cube does not contain requested tiles"
        )
        # filter cube tiles
        cube <- dplyr::filter(cube, .data[["tile"]] %in% tiles)
    }
    return(cube)
}
#' @title Check if roi or tiles are provided
#' @param source        Data source
#' @param roi           Region of interest
#' @param tiles         Tiles to be included in cube
#' @return Called for side effects.
#' @keywords internal
#' @noRd
#' @export
.source_roi_tiles.stac_cube <- function(source, roi, tiles) {
    return(invisible(source))
}
