#' @keywords internal
.local_cube <- function(source,
                        collection,
                        data_dir,
                        parse_info,
                        delim,
                        bands,
                        start_date,
                        end_date,
                        multicores,
                        progress, ...) {

    # set caller to show in errors
    .check_set_caller(".local_cube")

    # check documentation mode
    progress <- .check_documentation(progress)

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

    # retrieve all information of file_info
    items <- .local_cube_items_file_info(source = source,
                                         items = items,
                                         collection = collection,
                                         multicores = multicores,
                                         progress = progress)

    # get all tiles
    tiles <- unique(items[["tile"]])

    # make a cube for each tile (rows)
    cube <- purrr::map_dfr(tiles, function(tile) {

        # filter tile
        items_tile <- dplyr::filter(items, .data[["tile"]] == !!tile)

        # make a new cube tile
        tile_cube <- .local_cube_items_cube(source = source,
                                            collection = collection,
                                            items = items_tile)

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
        dplyr::mutate(date = .sits_timeline_date_format(date)) %>%
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
        band = .source_bands_to_sits(
            source = source,
            collection = collection,
            bands = band
        )
    )

    # filter bands
    if (!purrr::is_null(bands)) {

        # verify that the requested bands exist
        .check_chr_within(bands, within = unique(items[["band"]]),
                          msg = "invalid 'bands' value")

        # select the requested bands
        items <- dplyr::filter(items, band %in% !!bands)
    }

    return(items)
}

#' @keywords internal
.local_cube_items_file_info <- function(source,
                                        items,
                                        collection,
                                        multicores,
                                        progress) {

    # set caller to show in errors
    .check_set_caller(".local_cube_items_file_info")

    # post-condition
    .check_that(nrow(items) > 0,
                msg = "invalid 'items' parameter")

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
    items <- .sits_parallel_map(unique(items[["fid"]]), function(i) {

        # filter by feature
        item <- dplyr::filter(items, .data[["fid"]] == !!i)

        # open band rasters and get assets info
        assets_info <- purrr::map(item[["path"]], function(path) {
            tryCatch({
                asset <- .raster_open_rast(path)
                res <- .raster_res(asset)
                bbox <- .raster_bbox(asset)
                size <- .raster_size(asset)
                crs <- .raster_crs(asset)

                tibble::as_tibble_row(c(res, bbox, size, list(crs = crs)))
            }, error = function(e) {
                NULL
            })
        })

        # remove corrupted assets
        bad_assets <- purrr::map_lgl(assets_info, purrr::is_null)
        item <- item[!bad_assets,]

        # bind items and assets info
        item <- dplyr::bind_cols(item, dplyr::bind_rows(assets_info))

        return(item)
    }, progress = progress)

    items <- dplyr::bind_rows(items) %>%
        dplyr::arrange(.data[["date"]], .data[["fid"]], .data[["band"]])

    return(items)
}

#' @keywords internal
.local_cube_items_cube <- function(source,
                                   collection,
                                   items) {

    # pre-condition
    .check_length(unique(items[["tile"]]), len_min = 1,
                  msg = "invalid number of tiles")

    # get crs from file_info
    crs <- unique(items[["crs"]])

    # check crs
    .check_length(crs, len_min = 1, len_max = 1,
                  msg = "invalid crs value")

    # get tile from file_info
    tile <- unique(items[["tile"]])

    # check tile
    .check_length(tile, len_min = 1, len_max = 1,
                  msg = "invalid tile value")

    # make a new file info for one tile
    file_info <- dplyr::select(
        items,
        dplyr::all_of(c("fid", "band", "date", "xmin",
                        "ymin", "xmax", "ymax", "xres",
                        "yres", "nrows", "ncols", "path")))

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
