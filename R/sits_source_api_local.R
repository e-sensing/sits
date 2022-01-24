#' @keywords internal
.local_cube <- function(source,
                        collection,
                        data_dir,
                        parse_info,
                        delim,
                        bands,
                        start_date,
                        end_date,
                        multicores, ...) {

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

    # make a new file info for one tile
    cube <- .local_cube_items_cube(source = source,
                                   items = items,
                                   collection = collection,
                                   multicores = multicores)

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
.local_cube_items_cube <- function(source,
                                   items,
                                   collection,
                                   multicores) {

    # set caller to show in errors
    .check_set_caller(".local_cube_items_cube")

    # post-condition
    .check_that(nrow(items) > 0,
                msg = "invalid 'items' parameter")

    # add feature id (fid)
    items <- dplyr::group_by(items, .data[["tile"]], .data[["date"]]) %>%
        dplyr::mutate(fid = paste0(dplyr::cur_group_id())) %>%
        dplyr::ungroup() %>%
        tidyr::nest(features = c(.data[["band"]],
                                 .data[["date"]],
                                 .data[["path"]]))

    progress <- TRUE
    # check if progress bar and multicores processing can be enabled
    if (nrow(items) < .config_get("local_min_files_for_parallel")) {
        progress <- FALSE
        multicores <- 2
    }

    # prepare parallel requests
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)

    # do parallel requests
    cube <- .sits_parallel_map(seq_len(nrow(items)), function(i) {

        # get tile name
        tile <- items[["tile"]][[i]]

        # get item
        item <- items[["features"]][[i]]

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

        assets_info <- dplyr::bind_cols(item, asset_info) %>%
            dplyr::select(dplyr::all_of(c("band", "date", "xmin",
                                          "ymin", "xmax", "ymax", "xres",
                                          "yres", "nrows", "ncols", "path",
                                          "crs")))
        tidyr::unnest(
            tibble::tibble(
                tile = tile,
                fid = item[["fid"]],
                date = item[["date"]],
                band = item[["band"]],
                asset_info = asset_info,
                path = item[["path"]],
            ), cols = c("band", "asset_info", "path"))
    },
    progress = progress
    ) %>% dplyr::bind_rows()

    # post-condition
    .check_num(nrow(cube), min = 1, msg = "number metadata rows is empty")

    # prepare cube
    cube <- cube %>%
        tidyr::nest(file_info = -dplyr::matches(c("tile", "crs"))) %>%
        slider::slide_dfr(function(tile) {

            # get file_info
            file_info <- tile[["file_info"]][[1]]

            # arrange file_info
            file_info <- dplyr::arrange(file_info, .data[["date"]],
                                        .data[["fid"]], .data[["band"]])

            # get tile bbox
            bbox <- .source_tile_get_bbox(source = source,
                                          file_info = file_info,
                                          collection = collection)

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
                file_info  = file_info)

            return(tile)
        })

    return(cube)
}
