#' @keywords internal
#' @noRd
.local_cube <- function(source,
                        collection,
                        data_dir,
                        parse_info,
                        version,
                        delim,
                        tiles,
                        bands,
                        labels,
                        start_date,
                        end_date,
                        multicores,
                        progress, ...) {

    # set caller to show in errors
    .check_set_caller(".local_cube")

    # is this a cube with results?
    if (!purrr::is_null(bands) &&
        all(bands %in% .conf("sits_results_bands"))) {
        results_cube <- TRUE
    } else {
        results_cube <- FALSE
    }
    # results cube should have only one band
    if (results_cube) {
        .check_that(
            length(bands) == 1,
            msg = "results cube should have only one band"
        )
    }
    # is parse info NULL? use the default
    if (purrr::is_null(parse_info)) {
        if (results_cube) {
            parse_info <- .conf("results_parse_info_def")
        } else {
            parse_info <- .conf("local_parse_info_def")
        }
    }
    # precondition - does the parse info have band and date?
    if (results_cube) {
        .check_chr_contains(
            parse_info,
            contains = .conf("results_parse_info_col"),
            msg = paste(
                "parse_info must include tile, start_date, end_date,",
                "and band."
            )
        )
    } else {
        .check_chr_contains(
            parse_info,
            contains = .conf("local_parse_info_col"),
            msg = "parse_info must include tile, date, and band."
        )
    }

    # bands in upper case for raw cubes, lower case for results cubes
    if (!purrr::is_null(bands)) {
        if (results_cube) {
            bands <- tolower(bands)
        } else {
            bands <- toupper(bands)
        }
    }

    # make query and retrieve items
    items <- .local_cube_items_new(
        data_dir = data_dir,
        parse_info = parse_info,
        version = version,
        delim = delim,
        start_date = start_date,
        end_date = end_date,
        bands = bands
    )

    # filter bands in items (only for raw image cube)
    if (!results_cube) {
        items <- .local_cube_items_bands_select(
            source = source,
            collection = collection,
            bands = bands,
            items = items
        )
    }
    # filter tiles
    if (!purrr::is_null(tiles)) {
        items <- .local_cube_items_tiles_select(
            tiles = tiles,
            items = items
        )
    }

    # build file_info for the items
    if (results_cube) {
        items <- .local_results_cube_file_info(
            items = items,
            multicores = multicores,
            progress = progress
        )
    } else {
        items <- .local_cube_file_info(
            items = items,
            multicores = multicores,
            progress = progress
        )
    }

    # get all tiles
    tiles <- unique(items[["tile"]])

    # make a cube for each tile (rows)
    cube <- purrr::map_dfr(tiles, function(tile) {
        # filter tile
        items_tile <- dplyr::filter(items, .data[["tile"]] == !!tile)
        # make a new cube tile
        if (results_cube) {
            if (purrr::is_null(labels)) {
                labels <- NA
            }

            tile_cube <- .local_results_items_cube(
                source = source,
                collection = collection,
                items = items_tile,
                labels = labels
            )
        } else {
            tile_cube <- .local_cube_items_cube(
                source = source,
                collection = collection,
                items = items_tile
            )
        }

        return(tile_cube)
    })

    if (results_cube) {
        result_class <- .conf("sits_results_s3_class")[[bands]]
        class(cube) <- c(result_class, "derived_cube", "raster_cube", class(cube))
    } else {
        class(cube) <- .cube_s3class(cube)
    }

    return(cube)
}

#' @keywords internal
#' @noRd
.local_cube_items_new <- function(data_dir,
                                  parse_info,
                                  version,
                                  delim,
                                  start_date,
                                  end_date,
                                  bands) {

    # set caller to show in errors
    .check_set_caller(".local_cube_items_new")

    # is this a cube with results?
    if (!purrr::is_null(bands) &&
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
    # post-condition
    .check_chr(img_files,
        allow_empty = FALSE, len_min = 1,
        msg = "no file found in provided directory"
    )

    # remove the extension
    img_files_noext <- tools::file_path_sans_ext(img_files)

    # split the file names
    img_files_lst <- strsplit(img_files_noext, split = delim, fixed = TRUE)

    are_img_files_ok <- purrr::map_lgl(img_files_lst, function(img_file) {
        if (length(img_file) == length(parse_info)) {
            return(TRUE)
        }
        return(FALSE)
    })

    img_files_ok <- img_files_lst[are_img_files_ok]

    # post condition
    .check_that(
        length(img_files_ok) > 0,
        local_msg = "no file matched fields of parse_info",
        msg = "invalid file names or 'parse_info' parameter"
    )

    # filtered only valid files
    img_files_filt <- img_files[are_img_files_ok]

    # bind rows
    img_files_mx <- do.call(rbind, img_files_ok)

    # joint the list into a tibble and convert bands name to upper case
    items <- suppressMessages(
        tibble::as_tibble(img_files_mx,
            .name_repair = "universal"
        )
    )
    # read the image files into a tibble with added parse info
    colnames(img_files_mx) <- parse_info
    # joint the list into a tibble and convert bands name to upper case
    items <- suppressMessages(
        tibble::as_tibble(img_files_mx,
            .name_repair = "universal"
        )
    )
    # get the information on the required bands, dates and path
    if (results_cube) {
        # get only the first band
        band <- bands[[1]]
        # get the information on the required band, dates and path
        items <- items %>%
            # bands are case insensitive (converted to lower case)
            dplyr::mutate(band = tolower(.data[["band"]])) %>%
            # add path
            dplyr::mutate(path = paste(data_dir, img_files_filt, sep = "/")) %>%
            # filter by the band
            dplyr::filter(.data[["band"]] == !!band) %>%
            # filter by the version
            dplyr::filter(.data[["version"]] == !!version) %>%
            # select the relevant parts
            dplyr::select(
                "tile",
                "start_date",
                "end_date",
                "band",
                "path"
            ) %>%
            # check the start date format
            dplyr::mutate(
                start_date = .timeline_format(.data[["start_date"]])
            ) %>%
            # check the end date format
            dplyr::mutate(
                end_date = .timeline_format(.data[["end_date"]])
            ) %>%
            # filter to remove duplicate combinations of file and band
            dplyr::distinct(
                .data[["tile"]],
                .data[["start_date"]],
                .data[["end_date"]],
                .data[["band"]],
                .keep_all = TRUE
            ) %>%
            # order by dates
            dplyr::arrange(.data[["start_date"]])
    } else {
        # bands are case insensitive (converted to upper case)
        items <- items %>%
            dplyr::mutate(band = toupper(.data[["band"]])) %>%
            # add path
            dplyr::mutate(
                path = paste(!!data_dir, !!img_files_filt, sep = "/")) %>%
            # select the relevant parts
            dplyr::select(
                "tile",
                "date",
                "band",
                "path"
            ) %>%
            # check the date format
            dplyr::mutate(date = .timeline_format(.data[["date"]])) %>%
            # filter to remove duplicate combinations of file and band
            dplyr::distinct(
                .data[["tile"]],
                .data[["date"]],
                .data[["band"]],
                .keep_all = TRUE
            ) %>%
            # order by dates
            dplyr::arrange(.data[["date"]], .data[["band"]])

        # filter start and end dates
        if (!purrr::is_null(start_date)) {
            items <- dplyr::filter(items, .data[["date"]] >= start_date)
        }
        if (!purrr::is_null(end_date)) {
            items <- dplyr::filter(items, .data[["date"]] <= end_date)
        }
    }
    # post-condition
    .check_that(nrow(items) > 0,
        msg = "no files found in the interval"
    )
    return(items)
}

#' @keywords internal
#' @noRd
.local_cube_items_bands_select <- function(source,
                                           collection,
                                           bands,
                                           items) {

    # set caller to show in errors
    .check_set_caller(".local_cube_items_bands_select")

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
    if (!purrr::is_null(bands)) {
        # verify that the requested bands exist
        .check_chr_within(bands,
            within = unique(items[["band"]]),
            msg = "invalid 'bands' value"
        )
        # select the requested bands
        items <- dplyr::filter(items, .data[["band"]] %in% !!bands)
    }
    return(items)
}
#' @keywords internal
#' @noRd
.local_cube_items_tiles_select <- function(tiles,
                                           items) {

    # set caller to show in errors
    .check_set_caller(".local_cube_items_tiles_select")

    # filter tiles
    # verify that the requested tiles exist
    .check_chr_within(tiles,
                      within = unique(items[["tile"]]),
                      msg = "invalid 'tiles' value"
    )
    # select the requested bands
    items <- dplyr::filter(items, .data[["tile"]] %in% !!tiles)

    return(items)
}

#' @keywords internal
#' @noRd
.local_cube_file_info <- function(items,
                                  multicores,
                                  progress) {
    # set caller to show in errors
    .check_set_caller(".local_cube_file_info")
    # post-condition
    .check_that(nrow(items) > 0,
        msg = "invalid 'items' parameter"
    )
    # add feature id (fid)
    items <- dplyr::group_by(items, .data[["tile"]], .data[["date"]]) %>%
        dplyr::mutate(fid = paste0(dplyr::cur_group_id())) %>%
        dplyr::ungroup()
    # prepare parallel requests
    if (is.null(sits_env[["cluster"]])) {
        .sits_parallel_start(workers = multicores, log = FALSE)
        on.exit(.sits_parallel_stop(), add = TRUE)
    }
    # do parallel requests
    results_lst <- .sits_parallel_map(unique(items[["fid"]]), function(i) {
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
        warning("cannot open file(s): ",
                paste0("'", errors, "'", collapse = ", "),
                call. = FALSE, immediate. = TRUE)
    }

    items <- dplyr::bind_rows(items) %>%
        dplyr::arrange(.data[["date"]], .data[["fid"]], .data[["band"]])

    return(items)
}

#' @keywords internal
#' @noRd
.local_results_cube_file_info <- function(items, multicores, progress) {

    # set caller to show in errors
    .check_set_caller(".local_results_cube_file_info")

    # post-condition
    .check_that(nrow(items) > 0,
        msg = "invalid 'items' parameter"
    )

    # prepare parallel requests
    if (is.null(sits_env[["cluster"]])) {
        .sits_parallel_start(workers = multicores, log = FALSE)
        on.exit(.sits_parallel_stop(), add = TRUE)
    }
    # do parallel requests
    results_lst <- .sits_parallel_map(seq_len(nrow(items)), function(i) {

        item <- items[i, ]
        # open band rasters and get assets info
        assets_info <- purrr::map(item[["path"]], function(path) {
            tryCatch(
                {
                    asset <- .raster_open_rast(path)
                    res  <- .raster_res(asset)
                    crs  <- .raster_crs(asset)
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
        warning("cannot open file(s):",
                paste0("'", errors, "'", collapse = ", "),
                call. = FALSE, immediate. = TRUE)
    }

    items <- dplyr::bind_rows(items_lst)

    return(items)
}

#' @keywords internal
#' @noRd
.local_cube_items_cube <- function(source,
                                   collection,
                                   items) {
    # pre-condition
    .check_length(
        unique(items[["tile"]]),
        len_min = 1,
        msg = "invalid number of tiles"
    )

    # get crs from file_info
    crs <- unique(items[["crs"]])

    # check crs
    .check_length(
        crs,
        len_min = 1,
        len_max = 1,
        msg = "invalid crs value"
    )
    # get tile from file_info
    tile <- unique(items[["tile"]])
    # check tile
    .check_length(
        tile,
        len_min = 1,
        len_max = 1,
        msg = "invalid tile value"
    )
    # make a new file info for one tile
    file_info <- dplyr::select(
        items,
        dplyr::all_of(c("fid",
          "band",
          "date",
          "xmin",
          "ymin",
          "xmax",
          "ymax",
          "xres",
          "yres",
          "nrows",
          "ncols",
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

#' @keywords internal
#' @noRd
.local_results_items_cube <- function(source,
                                      collection,
                                      items,
                                      labels) {
    # pre-condition
    .check_length(
        unique(items[["tile"]]),
        len_min = 1,
        msg = "invalid number of tiles"
    )
    # get crs from file_info
    crs <- unique(items[["crs"]])
    # check crs
    .check_length(
        crs,
        len_min = 1,
        len_max = 1,
        msg = "invalid crs value"
    )
    # get tile from file_info
    tile <- unique(items[["tile"]])
    # check tile
    .check_length(
        tile,
        len_min = 1,
        len_max = 1,
        msg = "invalid tile value"
    )
    # make a new file info for one tile
    file_info <- dplyr::select(
        items,
        dplyr::all_of(c("band",
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
