#' @title Create data cubes using local files
#' @keywords internal
#' @noRd
#' @param source       Data source (one of \code{"AWS"}, \code{"BDC"},
#' \code{"DEAFRICA"}, \code{"MPC"}, \code{"USGS"}).
#' @param collection   Image collection in data source (To find out
#'  the supported collections, use \code{\link{sits_list_collections}()}).
#' @param data_dir     Local directory where images are stored.
#' @param vector_dir    Local director where vector files are stored
#'                     (for local vector cubes - character vector of length 1)
#' @param parse_info   Parsing information for local files.
#' @param version      Version id for local files.
#' @param delim        Delimiter for parsing local files.
#' @param tiles        Tiles from the collection to be included in
#'                     the cube (see details below).
#' @param bands        Spectral bands and indices to be included
#'                     in the cube (optional).
#' @param vector_band  Band for vector data cube
#' @param labels       Labels associated to the classes (only for result cubes)
#' @param start_date,end_date Initial and final dates to include
#'                     images from the collection in the cube (optional).
#' @param multicores   Number of workers for parallel processing
#' @param progress     Show a progress bar?
#' @param ...          Other parameters to be passed for specific types.
#' @return A \code{tibble} describing the contents of a local data cube.
.local_cube <- function(source,
                        collection,
                        data_dir,
                        vector_dir,
                        parse_info,
                        version,
                        delim,
                        tiles,
                        bands,
                        vector_band,
                        labels,
                        start_date,
                        end_date,
                        multicores,
                        progress, ...) {
    # set caller to show in errors
    .check_set_caller(".local_cube")
    # initialize vector items
    vector_items <- NULL
    # is this a cube with results?
    results_cube <- .check_is_results_cube(bands, labels)

    # set the correct parse_info
    parse_info <- .conf_parse_info(parse_info, results_cube)

    # bands in upper case for raw cubes, lower case for results cubes
    bands <- .band_set_case(bands)

    # make query and retrieve items
    raster_items <- .local_cube_items_raster_new(
        data_dir = data_dir,
        parse_info = parse_info,
        version = version,
        delim = delim,
        start_date = start_date,
        end_date = end_date,
        bands = bands
    )
    if (.has(vector_dir)) {
        # set the correct parse_info
        parse_info <- .conf("results_parse_info_def")

        vector_items <- .local_cube_items_vector_new(
            vector_dir = vector_dir,
            parse_info = parse_info,
            version = version,
            delim = delim,
            start_date = start_date,
            end_date = end_date,
            vector_band = vector_band
        )
    }

    # filter bands in items (only for raw image cube)
    if (!results_cube) {
        raster_items <- .local_cube_items_bands_select(
            source = source,
            collection = collection,
            bands = bands,
            items = raster_items
        )
    }
    # filter tiles
    if (.has(tiles)) {
        raster_items <- .local_cube_items_tiles_select(
            tiles = tiles,
            items = raster_items
        )
        if (.has(vector_items)) {
            vector_items <- .local_cube_items_tiles_select(
                tiles = tiles,
                items = vector_items
            )
        }
    }
    # build file_info for the items
    if (results_cube) {
        raster_items <- .local_results_cube_file_info(
            items = raster_items,
            multicores = multicores,
            progress = progress
        )
    } else {
        raster_items <- .local_cube_file_info(
            items = raster_items,
            multicores = multicores,
            progress = progress
        )
    }

    # get all tiles
    tiles <- unique(raster_items[["tile"]])

    # make a cube for each tile (rows)
    cube <- .map_dfr(tiles, function(tile) {
        # filter tile
        items_tile <- dplyr::filter(raster_items, .data[["tile"]] == !!tile)
        # create result cube
        if (results_cube) {
            tile_cube <- .local_results_items_cube(
                source = source,
                collection = collection,
                raster_items = items_tile,
                labels = labels
            )
            return(tile_cube)
        }
        # create EO cube
        tile_cube <- .local_cube_items_cube(
            source = source,
            collection = collection,
            items = items_tile
        )
        # handle special class cube
        if (.has(labels)) {
            is_class_cube <- .try(
                .conf(
                    "sources", source, "collections", collection, "class_cube"
                ),
                .default = FALSE
            )
            # class cubes from STAC/other sources are not handled as "results"
            # results are reserved as a `sits` type
            if (is_class_cube) {
                tile_cube[["labels"]] <- list(labels)
            }
        }
        # return!
        tile_cube
    })
    if (.has(vector_items)) {
        cube <- .local_cube_include_vector_info(cube, vector_items)
    }

    if (results_cube) {
        result_class <- .conf("sits_results_s3_class")[[bands]]
        class(cube) <- c(
            result_class, "derived_cube",
            "raster_cube", class(cube)
        )
    } else {
        class(cube) <- .cube_s3class(cube)
        if (.has(vector_items)) {
            if (vector_band == "segments") {
                class(cube) <- c("segs_cube", "vector_cube", class(cube))
            } else if (vector_band == "probs") {
                class(cube) <- c("probs_vector_cube",
                                 "derived_vector_cube",
                                 "segs_cube",
                                 "vector_cube",
                                 class(cube))
            } else if (vector_band == "class") {
                class(cube) <- c("class_vector_cube",
                                 "derived_vector_cube",
                                 "segs_cube",
                                 "vector_cube",
                                 class(cube))

            }
        }
    }
    # check if labels match in the case of class cube
    if (inherits(cube, "class_cube")) {
        .check_labels_class_cube(cube)
    }

    return(cube)
}

#' @title Return raster items for local data cube
#' @keywords internal
#' @noRd
#' @param data_dir     Local directory where images are stored.
#' @param parse_info   Parsing information for local files.
#' @param version      Version id for local files.
#' @param delim        Delimiter for parsing local files.
#' @param start_date,end_date Initial and final dates to include
#'                     images from the collection in the cube (optional).
#' @param bands        Spectral bands and indices to be included
#'                     in the cube (optional).
#' @return A list of items describing the contents of a local data cube.
.local_cube_items_raster_new <- function(data_dir,
                                         parse_info,
                                         version,
                                         delim,
                                         start_date,
                                         end_date,
                                         bands) {
    # set caller to show in errors
    .check_set_caller(".local_cube_items_raster_new")

    # is this a cube with results?
    if (.has(bands) &&
        bands[[1]] %in% .conf("sits_results_bands")) {
        results_cube <- TRUE
    } else {
        results_cube <- FALSE
    }
    # how many of those files are images?
    # retrieve the known file extensions
    file_ext <- .conf("local_file_extensions")
    # list the files in the data directory
    img_files <- list.files(
        path = data_dir,
        pattern = paste0("\\.(", paste0(file_ext, collapse = "|"), ")$")
    )
    # postcondition
    .check_chr_parameter(img_files, allow_empty = FALSE, len_min = 1)

    # remove the extension
    img_files_noext <- tools::file_path_sans_ext(img_files)
    # split the file names
    img_files_lst <- strsplit(img_files_noext, split = delim, fixed = TRUE)
    # which image files in directory match the parse info?
    are_img_files_ok <- purrr::map_lgl(img_files_lst, function(img_file) {
        if (length(img_file) == length(parse_info))
            return(TRUE)
        else
            return(FALSE)
    })
    # select the images that match the file info
    img_files_ok <- img_files_lst[are_img_files_ok]

    # post condition
    .check_that(length(img_files_ok) > 0)

    # get valid files
    img_files_filt <- img_files[are_img_files_ok]
    # bind rows
    img_files_mx <- do.call(rbind, img_files_ok)
    # read the image files into a tibble with added parse info
    colnames(img_files_mx) <- parse_info
    # joint the list into a tibble and convert bands name to upper case
    items <- suppressMessages(
        tibble::as_tibble(img_files_mx,
            .name_repair = "universal"
        )
    )
    if (.has(bands)) {
        # check if bands exist
        .check_chr_contains(
            x = items[["band"]],
            contains = bands,
            discriminator = "all_of",
            msg = .conf("messages", ".local_cube_items_bands")
        )
    }
    # get the information on the required bands, dates and path
    if (results_cube) {
        # check required version exists
        .check_chr_within(
            x = version,
            within = items[["version"]],
            discriminator = "any_of",
            msg = .conf("messages", ".local_cube_items_version")
        )
        # get only the first band
        band <- bands[[1]]
        # get the information on the required band, dates and path
        items <- items |>
            # bands are case insensitive (converted to lower case)
            dplyr::mutate(band = tolower(.data[["band"]])) |>
            # add path
            dplyr::mutate(path = paste(data_dir,
                                           img_files_filt, sep = "/")) |>
            # filter by the band
            dplyr::filter(.data[["band"]] == !!band) |>
            # filter by the version
            dplyr::filter(.data[["version"]] == !!version) |>
            # select the relevant parts
            dplyr::select(
                "tile",
                "start_date",
                "end_date",
                "band",
                "path"
            ) |>
            # check the start date format
            dplyr::mutate(
                start_date = .timeline_format(.data[["start_date"]])
            ) |>
            # check the end date format
            dplyr::mutate(
                end_date = .timeline_format(.data[["end_date"]])
            ) |>
            # filter to remove duplicate combinations of file and band
            dplyr::distinct(
                .data[["tile"]],
                .data[["start_date"]],
                .data[["end_date"]],
                .data[["band"]],
                .keep_all = TRUE
            ) |>
            # order by dates
            dplyr::arrange(.data[["start_date"]])
    } else {
        # bands are case insensitive (converted to upper case)
        items <- items |>
            dplyr::mutate(band = toupper(.data[["band"]])) |>
            # add path
            dplyr::mutate(
                path = paste(!!data_dir, !!img_files_filt, sep = "/")
            ) |>
            # select the relevant parts
            dplyr::select(
                "tile",
                "date",
                "band",
                "path"
            ) |>
            # check the date format
            dplyr::mutate(date = .timeline_format(.data[["date"]])) |>
            # filter to remove duplicate combinations of file and band
            dplyr::distinct(
                .data[["tile"]],
                .data[["date"]],
                .data[["band"]],
                .keep_all = TRUE
            ) |>
            # order by dates
            dplyr::arrange(.data[["date"]], .data[["band"]])

        # filter start and end dates
        if (.has(start_date)) {
            items <- dplyr::filter(items, .data[["date"]] >= start_date)
        }
        if (.has(end_date)) {
            items <- dplyr::filter(items, .data[["date"]] <= end_date)
        }
    }
    # post-condition
    .check_that(nrow(items) > 0)
    return(items)
}
#' @title Return raster items for local data cube
#' @keywords internal
#' @noRd
#' @param vector_dir     Local directory where images are stored.
#' @param parse_info   Parsing information for local files.
#' @param version      Version id for local files.
#' @param delim        Delimiter for parsing local files.
#' @param start_date,end_date Initial and final dates to include
#'                     images from the collection in the cube (optional).
#' @param vector_band         Vector band.
#' @return A list of items describing the contents of a local data cube.
.local_cube_items_vector_new <- function(vector_dir,
                                         parse_info,
                                         version,
                                         delim,
                                         start_date,
                                         end_date,
                                         vector_band) {
    # set caller to show in errors
    .check_set_caller(".local_cube_items_vector_new")

    # how many of those files are vector?
    # retrieve the known file extensions
    file_ext <- "gpkg"
    # list the vector files in the data directory
    gpkg_files <- list.files(
        path = vector_dir,
        pattern = paste0("\\.(", paste0(file_ext, collapse = "|"), ")$")
    )
    # post-condition
    gpkg_files_path <- paste0(vector_dir, "/", gpkg_files)
    .check_that(all(file.exists(gpkg_files_path)))

    # remove the extension
    gpkg_files_noext <- tools::file_path_sans_ext(gpkg_files)
    # split the file names
    gpkg_files_lst <- strsplit(gpkg_files_noext, split = delim, fixed = TRUE)
    # check gkpg files
    are_gpkg_files_ok <- purrr::map_lgl(gpkg_files_lst, function(gpkg_file) {
        if (length(gpkg_file) == length(parse_info)) {
            return(TRUE)
        }
        return(FALSE)
    })
    # subset gkpg files
    gpkg_files_ok <- gpkg_files_lst[are_gpkg_files_ok]
    # filter only valid files
    gpkg_files_filt <- gpkg_files[are_gpkg_files_ok]
    # bind rows
    gpkg_files_mx <- do.call(rbind, gpkg_files_ok)
    # read the image files into a tibble with added parse info
    colnames(gpkg_files_mx) <- parse_info
    # joint the list into a tibble and convert bands name to upper case
    items <- suppressMessages(
        tibble::as_tibble(gpkg_files_mx,
                          .name_repair = "universal"
        )
    )
    # check if bands exist
    .check_chr_contains(
        x = items[["band"]],
        contains = vector_band,
        discriminator = "any_of",
        msg = .conf("messages", ".local_cube_items_bands")
    )
    # get the information on the required bands, dates and path
    # check required version exists
    .check_chr_within(
        x = version,
        within = items[["version"]],
        discriminator = "any_of",
        msg = .conf("messages", ".local_cube_items_version")
    )
    # get the information on the required band, dates and path
    items <- items |>
        # bands are case insensitive (converted to lower case)
        dplyr::mutate(band = tolower(.data[["band"]])) |>
        # add path
        dplyr::mutate(path = paste(vector_dir,
                                   gpkg_files_filt, sep = "/")) |>
        # filter by the band
        dplyr::filter(.data[["band"]] == !!vector_band) |>
        # filter by the version
        dplyr::filter(.data[["version"]] == !!version) |>
        # select the relevant parts
        dplyr::select(
            "tile",
            "start_date",
            "end_date",
            "band",
            "path"
        ) |>
        # check the start date format
        dplyr::mutate(
            start_date = .timeline_format(.data[["start_date"]])
        ) |>
        # check the end date format
        dplyr::mutate(
            end_date = .timeline_format(.data[["end_date"]])
        ) |>
        # filter to remove duplicate combinations of file and band
        dplyr::distinct(
            .data[["tile"]],
            .data[["start_date"]],
            .data[["end_date"]],
            .data[["band"]],
            .keep_all = TRUE
        ) |>
        # order by dates
        dplyr::arrange(.data[["start_date"]])

    # post-condition
    .check_that(nrow(items) > 0)
    return(items)
}
#' @title Select items by bands
#' @keywords internal
#' @noRd
#' @param source       Data source (one of \code{"AWS"}, \code{"BDC"},
#' \code{"DEAFRICA"}, \code{"MPC"}, \code{"USGS"}).
#' @param collection   Image collection in data source (To find out
#'  the supported collections, use \code{\link{sits_list_collections}()}).
#' @param bands        Spectral bands and indices to be included
#'                     in the cube (optional).
#' @param items        Items retrieved by \code{local_cube_items_new}.
#' @return  Items selected for the chosen bands
.local_cube_items_bands_select <- function(source,
                                           collection,
                                           bands,
                                           items) {
    # set caller to show in errors
    .check_set_caller(".local_cube_items_bands")

    # convert the band names to SITS bands
    items <- dplyr::mutate(
        items,
        band = .source_bands_to_sits(
            source = !!source,
            collection = !!collection,
            bands = .data[["band"]]
        )
    )
    # filter bands
    if (.has(bands)) {
        # verify that the requested bands exist
        .check_chr_within(bands,
            within = unique(items[["band"]])
        )
        # select the requested bands
        items <- dplyr::filter(items, .data[["band"]] %in% !!bands)
    }
    return(items)
}
#' @title Select items by tiles
#' @keywords internal
#' @noRd
#' @param tiles        Tiles in data cube.
#' @param items        Items retrieved by \code{local_cube_items_new}.
#' @return  Items selected for the chosen tiles
.local_cube_items_tiles_select <- function(tiles,
                                           items) {
    # set caller to show in errors
    .check_set_caller(".local_cube_items_tiles_select")

    # filter tiles
    # verify that the requested tiles exist
    .check_chr_within(tiles,
        within = unique(items[["tile"]])
    )
    # select the requested tiles
    items <- dplyr::filter(items, .data[["tile"]] %in% !!tiles)
    return(items)
}

#' @title Build local cube file_info
#' @keywords internal
#' @noRd
#' @param tiles        Tiles in data cube.
#' @param multicores   Number of workers for parallel processing
#' @param progress     Show a progress bar?
#' @return  Items with file info information
.local_cube_file_info <- function(items,
                                  multicores,
                                  progress) {
    # set caller to show in errors
    .check_set_caller(".local_cube_file_info")
    # post-condition
    .check_that(nrow(items) > 0)
    # add feature id (fid)
    items <- dplyr::group_by(items, .data[["tile"]], .data[["date"]]) |>
        dplyr::mutate(fid = paste0(dplyr::cur_group_id())) |>
        dplyr::ungroup()
    # prepare parallel requests
    if (is.null(sits_env[["cluster"]])) {
        .parallel_start(workers = multicores)
        on.exit(.parallel_stop(), add = TRUE)
    }
    # do parallel requests
    results_lst <- .parallel_map(unique(items[["fid"]]), function(i) {
        # filter by feature
        item <- dplyr::filter(items, .data[["fid"]] == !!i)
        # open band rasters and get assets info
        assets_info <- purrr::map(item[["path"]], function(path) {
            tryCatch(
                {
                    asset <- .raster_open_rast(path)
                    res <- .raster_res(asset)
                    bbox <- .raster_bbox(asset)
                    size <- .raster_size(asset)
                    crs <- .raster_crs(asset)
                    tibble::as_tibble_row(c(res, bbox, size, list(crs = crs)))
                },
                error = function(e) {
                    path
                }
            )
        })
        # remove corrupted assets
        bad_assets <- purrr::map_lgl(assets_info, purrr::is_character)
        item <- item[!bad_assets, ]

        # bind items and assets info
        result <- list(
            item = dplyr::bind_cols(
                item, dplyr::bind_rows(assets_info[!bad_assets])
            ),
            error = unlist(assets_info[bad_assets])
        )
        return(result)
    }, progress = progress)

    items <- purrr::map(results_lst, `[[`, "item")
    errors <- unlist(purrr::map(results_lst, `[[`, "error"))
    if (length(errors) > 0) {
        warning(.conf("messages", ".local_cube_file_info_error"),
                toString(errors),
                call. = FALSE, immediate. = TRUE
        )
    }
    items <- dplyr::bind_rows(items) |>
        dplyr::arrange(.data[["date"]], .data[["fid"]], .data[["band"]])
    return(items)
}

#' @title Build local cube file_info for results cubes
#' @keywords internal
#' @noRd
#' @param tiles        Tiles in data cube.
#' @param multicores   Number of workers for parallel processing
#' @param progress     Show a progress bar?
#' @return  Items with file info information
.local_results_cube_file_info <- function(items, multicores, progress) {
    # set caller to show in errors
    .check_set_caller(".local_results_cube_file_info")

    # post-condition
    .check_that(nrow(items) > 0)

    # prepare parallel requests
    if (is.null(sits_env[["cluster"]])) {
        .parallel_start(workers = multicores)
        on.exit(.parallel_stop(), add = TRUE)
    }
    # do parallel requests
    results_lst <- .parallel_map(seq_len(nrow(items)), function(i) {
        item <- items[i, ]
        # open band rasters and get assets info
        assets_info <- purrr::map(item[["path"]], function(path) {
            tryCatch(
                {
                    asset <- .raster_open_rast(path)
                    res <- .raster_res(asset)
                    crs <- .raster_crs(asset)
                    bbox <- .raster_bbox(asset)
                    size <- .raster_size(asset)

                    # return a tibble row
                    tibble::as_tibble_row(c(res, bbox, size, list(crs = crs)))
                },
                error = function(e) {
                    path
                }
            )
        })

        # remove corrupted assets
        bad_assets <- purrr::map_lgl(assets_info, purrr::is_character)

        # bind items and assets info
        results <- list(
            item = dplyr::bind_cols(
                item, dplyr::bind_rows(assets_info[!bad_assets])
            ),
            error = unlist(assets_info[bad_assets])
        )

        return(results)
    }, progress = progress)

    items_lst <- purrr::map(results_lst, `[[`, "item")
    errors <- unlist(purrr::map(results_lst, `[[`, "error"))
    if (length(errors) > 0) {
        warning(.conf("messages", ".local_cube_file_info_error"),
                toString(errors),
                call. = FALSE, immediate. = TRUE
        )
    }
    items <- dplyr::bind_rows(items_lst)
    return(items)
}

#' @title Build data cube tibble
#' @keywords internal
#' @noRd
#' @param source       Data source (one of \code{"AWS"}, \code{"BDC"},
#' \code{"DEAFRICA"}, \code{"MPC"}, \code{"USGS"}).
#' @param collection   Image collection in data source (To find out
#'  the supported collections, use \code{\link{sits_list_collections}()}).
#' @param items        Items retrieved by \code{local_cube_items_new} and
#'                     filtered by bands and tile
#' @return  Data cube tibble
.local_cube_items_cube <- function(source,
                                   collection,
                                   items) {
    # pre-condition
    .check_local_items(items)
    # get crs from file_info
    crs <- unique(items[["crs"]])
    # get tile from file_info
    tile <- unique(items[["tile"]])
    # make a new file info for one tile
    file_info <- dplyr::select(
        items,
        dplyr::all_of(c(
            "fid",
            "band",
            "date",
            "nrows",
            "ncols",
            "xres",
            "yres",
            "xmin",
            "ymin",
            "xmax",
            "ymax",
            "crs",
            "path"
        ))
    )

    # create a tibble to store the metadata
    cube_tile <- .cube_create(
        source = source,
        collection = collection,
        satellite = .source_collection_satellite(source, collection),
        sensor = .source_collection_sensor(source, collection),
        tile = tile,
        xmin = max(file_info[["xmin"]]),
        xmax = min(file_info[["xmax"]]),
        ymin = max(file_info[["ymin"]]),
        ymax = min(file_info[["ymax"]]),
        crs = crs,
        file_info = file_info
    )
    return(cube_tile)
}

#' @title Build data cube tibble for results cube
#' @keywords internal
#' @noRd
#' @param source       Data source (one of \code{"AWS"}, \code{"BDC"},
#' \code{"DEAFRICA"}, \code{"MPC"}, \code{"USGS"}).
#' @param collection   Image collection in data source (To find out
#'  the supported collections, use \code{\link{sits_list_collections}()}).
#' @param raster_items        Items retrieved by \code{local_cube_items_new} and
#'                          filtered by bands and tile
#' @param labels       Labels associated to the classes (only for result cubes)
#' @return  Data cube tibble
.local_results_items_cube <- function(source,
                                      collection,
                                      raster_items,
                                      labels) {
    # pre-condition
    .check_local_items(raster_items)
    # get crs from file_info
    crs <- unique(raster_items[["crs"]])
    # get tile from file_info
    tile <- unique(raster_items[["tile"]])
    # make a new file info for one tile
    file_info <- dplyr::select(
        raster_items,
        dplyr::all_of(c(
            "band",
            "start_date",
            "end_date",
            "ncols",
            "nrows",
            "xres",
            "yres",
            "xmin",
            "xmax",
            "ymin",
            "ymax",
            "crs",
            "path"
        ))
    )
    # create a tibble to store the metadata
    cube_tile <- .cube_create(
        source = source,
        collection = collection,
        satellite = .source_collection_satellite(source, collection),
        sensor = .source_collection_sensor(source, collection),
        tile = tile,
        xmin = max(file_info[["xmin"]]),
        xmax = min(file_info[["xmax"]]),
        ymin = max(file_info[["ymin"]]),
        ymax = min(file_info[["ymax"]]),
        crs = crs,
        labels = labels,
        file_info = file_info
    )
    return(cube_tile)
}
.local_cube_include_vector_info <- function(cube, vector_items) {
    cube <- slider::slide_dfr(cube, function(tile) {
        item <- dplyr::filter(vector_items, .data[["tile"]] == !!tile[["tile"]])
        vector_info <- tibble::tibble(
            band = item[["band"]],
            start_date = item[["start_date"]],
            end_date = item[["end_date"]],
            xres = .xres(.fi(tile)),
            yres = .yres(.fi(tile)),
            xmin = .xmin(.fi(tile)),
            xmax = .xmax(.fi(tile)),
            ymin = .ymin(.fi(tile)),
            ymax = .ymax(.fi(tile)),
            path = item[["path"]]
        )
        tile[["labels"]] <- list(.label_gpkg_file(item[["path"]]))
        tile[["vector_info"]] <- list(vector_info)
        return(tile)
    })

    return(cube)
}
