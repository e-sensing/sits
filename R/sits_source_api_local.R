#' @keywords internal
.local_cube <- function(source,
                        collection,
                        data_dir,
                        parse_info,
                        delim,
                        bands,
                        start_date,
                        end_date, ...) {

    # set caller to show in errors
    .check_set_caller(".local_cube")

    # make query and retrieve items
    items <- .local_cube_items_new(data_dir = data_dir,
                                   parse_info = parse_info,
                                   delim = delim,
                                   start_date = start_date,
                                   end_date = end_date)

    # filter bands in items
    items <- .local_cube_items_bands_select(source = source,
                                            collection = collection,
                                            bands = bands,
                                            items = items)

    # make a cube for each tile (rows)
    cube <- purrr::map_dfr(unique(items[["tile"]]), function(t) {

        # filter tile
        items_tile <- dplyr::filter(items, tile == t)

        # make a new file info for one tile
        file_info <- .local_cube_items_file_info(source = source,
                                                 items = items_tile,
                                                 collection = collection)

        # make a new cube tile
        tile_cube <- .local_cube_items_cube(source = source,
                                            collection = collection,
                                            items = items_tile,
                                            file_info = file_info)

        return(tile_cube)
    })

    class(cube) <- .cube_s3class(cube)

    return(cube)
}

#' @keywords internal
.local_cube_items_new <- function(data_dir,
                                  parse_info,
                                  delim,
                                  start_date,
                                  end_date) {

    # set caller to show in errors
    .check_set_caller(".local_cube_items_new")

    # how many of those files are images?
    # retrieve the known file extensions
    file_ext <- .config_local_file_extensions()

    # list the files in the data directory
    img_files <- list.files(
        path = data_dir,
        pattern = paste0("\\.(", paste0(file_ext, collapse = "|"), ")$")
    )

    # post-condition
    .check_chr(img_files, allow_empty = FALSE, len_min = 1,
               msg = "no file found in provided directory")

    # remove the extension
    img_files_noext <- tools::file_path_sans_ext(img_files)

    # split the file names
    img_files_lst <- strsplit(img_files_noext, split = delim)

    # bind rows
    img_files_mx <- do.call(rbind, img_files_lst)

    # read the image files into a tibble with added parse info
    colnames(img_files_mx) <- parse_info

    # joint the list into a tibble and convert bands name to upper case
    items <- suppressMessages(
        tibble::as_tibble(img_files_mx,
                          .name_repair = "universal")
    )

    # bands are case insensitive (converted to upper case)
    items <- dplyr::mutate(items, band = toupper(band))

    # get the information on the required bands, dates and path
    items <-
        # add path
        dplyr::mutate(items, path = paste(data_dir, img_files, sep = "/")) %>%
        # select the relevant parts
        dplyr::select(tile, date, band, path) %>%
        # check the date format
        .sits_timeline_date_format() %>%
        # filter to remove duplicate combinations of file and band
        dplyr::distinct(tile, band, date, .keep_all = TRUE) %>%
        # order by dates
        dplyr::arrange(date, band)

    # filter start and end dates
    if (!purrr::is_null(start_date))
        items <- dplyr::filter(items, date >= start_date)

    if (!purrr::is_null(end_date))
        items <- dplyr::filter(items, date <= end_date)

    # post-condition
    .check_that(nrow(items) > 0,
                msg = "no files found in the interval")

    return(items)
}

#' @keywords internal
.local_cube_items_bands_select <- function(source,
                                           collection,
                                           bands,
                                           items) {

    # set caller to show in errors
    .check_set_caller(".local_cube_items_bands_select")

    # convert the band names to SITS bands
    items <- dplyr::mutate(
        items,
        band = .source_bands_to_sits(source = source,
                                     collection = collection,
                                     bands = band))

    # filter bands
    if (!purrr::is_null(bands)) {

        # verify that the requested bands exist
        .check_chr_within(bands, within = unique(items[["band"]]),
                          msg = "invalid 'bands' value")

        # select the requested bands
        items <- dplyr::filter(items, band %in% bands)
    }

    return(items)
}

#' @keywords internal
.local_cube_items_file_info <- function(source,
                                        items,
                                        collection) {

    # set caller to show in errors
    .check_set_caller(".local_cube_items_file_info")

    # post-condition
    .check_that(nrow(items) > 0,
                msg = "invalid 'items' parameter")

    # add feature id (fid)
    items <- dplyr::group_by(items, tile, date) %>%
        dplyr::mutate(fid = dplyr::cur_group_id()) %>%
        dplyr::ungroup()

    # set progress bar
    progress <- (nrow(items) >= .config_gdalcubes_min_files_for_parallel())

    # prepare parallel requests
    .sits_parallel_start(workers = 1, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)

    # do in case of 'tile' strategy
    if (.source_collection_metadata_search(source = source,
                                           collection = collection) == "tile") {

        # get first item
        item <- dplyr::filter(items, fid == 1)

        # open bands raster
        assets <- purrr::map(item[["path"]], .raster_open_rast)

        # get asset info
        asset_info <- purrr::map(assets, function(asset) {
            res <- .raster_res(asset)
            bbox <- .raster_bbox(asset)
            size <- .raster_size(asset)
            crs <- .raster_crs(asset)
            tibble::as_tibble_row(c(res, bbox, size, list(crs = crs)))
        }) %>% dplyr::bind_rows()
    }

    # do parallel requests
    items <- .sits_parallel_map(
        unique(items[["fid"]]),
        function(i) {

            # filter by feature
            item <- dplyr::filter(items, fid == i)

            # do in case of 'tile' strategy
            if (.source_collection_metadata_search(source = source,
                                                   collection = collection) ==
                "feature") {

                # open band rasters
                assets <- purrr::map(item[["path"]], .raster_open_rast)

                # get asset info
                asset_info <- purrr::map(assets, function(asset) {
                    res <- .raster_res(asset)
                    bbox <- .raster_bbox(asset)
                    size <- .raster_size(asset)
                    crs <- .raster_crs(asset)
                    tibble::as_tibble_row(c(res, bbox, size, list(crs = crs)))
                }) %>% dplyr::bind_rows()
            }

            dplyr::bind_cols(item, asset_info) %>%
                dplyr::select(fid, band, date, xmin, ymin, xmax, ymax,
                              xres, yres, nrows, ncols, path, crs)

        },
        progress = progress
    ) %>% dplyr::bind_rows()

    return(items)
}

#' @keywords internal
.local_cube_items_cube <- function(source,
                                   collection,
                                   items,
                                   file_info) {

    # pre-condition
    .check_length(unique(items[["tile"]]), len_min = 1,
                  msg = "invalid number of tiles")

    # get crs from file_info
    crs <- unique(file_info[["crs"]])

    # discard crs from file_info
    file_info[["crs"]] <- NULL

    # check crs
    .check_length(crs, len_min = 1, len_max = 1,
                  msg = "invalid crs value")

    # get tile from file_info
    tile <- unique(items[["tile"]])

    # discard tile from file_info
    file_info[["tile"]] <- NULL

    # check tile
    .check_length(tile, len_min = 1, len_max = 1,
                  msg = "invalid tile value")

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
