#' @title Images arrangement in sits cube
#' @name .gc_arrange_images
#'
#' @keywords internal
#'
#' @param cube       A sits data cube
#'
#' @param agg_method A \code{character} with method that will be applied by
#'  \code{gdalcubes} for aggregation. Options: \code{min}, \code{max},
#'  \code{mean}, \code{median} and \code{first}. Default is \code{median}.
#'
#' @param duration   A \code{Duration} object from lubridate package.
#'
#' @param ...        Additional parameters.
#'
#' @return  A sits cube with the images arranged according to some criteria.
.gc_arrange_images <- function(cube, agg_method, duration, ...) {

    class(agg_method) <- agg_method

    UseMethod(".gc_arrange_images", agg_method)
}

#' @keywords internal
#' @export
.gc_arrange_images.default <- function(cube, agg_method, duration, ...) {

    return(cube)
}

#' @keywords internal
#' @export
.gc_arrange_images.first <- function(cube, agg_method, duration, ...) {

    .sits_fast_apply(data = cube, col = "file_info", fn = function(x) {

        tl_length <- max(2,
                         lubridate::interval(start = min(x[["date"]]),
                                             end = max(x[["date"]]))  / duration
        )

        dplyr::group_by(x, date_interval = cut(.data[["date"]], tl_length),
                        .add = TRUE) %>%
            dplyr::arrange(.data[["cloud_cover"]], .by_group = TRUE) %>%
            dplyr::ungroup() %>%
            dplyr::select(-.data[["date_interval"]])
    })
}

#' @title Save the images based on an aggregation method.
#'
#' @name .reg_new_cube
#'
#' @keywords internal
#'
#' @param tile          A data cube tile
#'
#' @param res           ...
#'
#' @param resampling    ...
#'
#' @param roi    ...
#'
#' @param output_dir    Directory where the aggregated images will be written.
#'
#' @param multithreads  A \code{numeric} with the number of cores will be used in
#'  the regularize. By default is used 1 core.
#'
#' @param ...         ...
#'
#' @return  A data cube tile with information used in its creation.
.reg_new_cube <- function(tile,
                          res,
                          start_date,
                          resampling,
                          roi,
                          output_dir,
                          multithreads, ...) {

    # set caller to show in errors
    .check_set_caller(".gc_new_cube")

    t_file_info <- .file_info(tile)
    t_band <- .cube_bands(tile, add_cloud = FALSE)

    memsize <- 4

    if (purrr::is_null(roi))
        sub_image <- .sits_raster_sub_image_default(tile)
    else
        sub_image <- .sits_raster_sub_image(tile = tile, roi = roi)

    blocks <- .sits_raster_blocks_apply(
        tile = tile,
        sub_image = sub_image,
        memsize = memsize,
        multicores = multithreads
    )

    c_path <- .file_info_paths(tile, bands = .source_cloud())
    b_path <- .file_info_paths(tile, bands = .cube_bands(tile, FALSE))

    interp_values <- .source_cloud_interp_values(
        source = .cube_source(cube = tile),
        collection = .cube_collection(cube = tile)
    )

    # TODO: add resampling method for cloud in config_internals

    # calc block
    .sits_parallel_start(4, log = FALSE)
    b_reg_path <- .sits_parallel_map(blocks, function(b) {

        b_filename_block <- paste0(
            tools::file_path_sans_ext(b_path),
            "_block_", b[["first_row"]], "_", b[["nrows"]], ".tif"
        )

        c_filename_block <- paste0(
            tools::file_path_sans_ext(c_path),
            "_block_", b[["first_row"]], "_", b[["nrows"]], ".tif"
        )

        # TODO: adapt this if for this function
        if (file.exists(b_filename_block)) {
            # try to open the file
            r_obj <-
                tryCatch({
                    .raster_open_rast(b_filename_block)
                }, error = function(e) {
                    return(NULL)
                })
            # if file can be opened, check if the result is correct
            # this file will not be processed again
            if (!purrr::is_null(r_obj))
                if (.raster_nrows(r_obj) == b[["nrows"]]) {
                    # log
                    .sits_debug_log(output_dir = output_dir,
                                    event      = "skipping block",
                                    key        = "block file",
                                    value      = b_filename_block)
                    return(b_filename_block)
                }
        }


        currently_row <- b[["first_row"]]
        row_after <- b[["nrows"]] + b[["first_row"]]

        c_chunk_rast <- .reg_get_chunk(c_path, b)
        b_chunk_rast <- .reg_get_chunk(b_path, b)

        b_filename <- paste0(
            output_dir, "/",
            paste("cube", .cube_tiles(tile), unique(t_file_info[["date"]]),
                  t_band, currently_row, row_after, sep = "_"),
            ".tif"
        )

        c_filename <- paste0(
            output_dir, "/",
            paste("cube", .cube_tiles(tile), unique(t_file_info[["date"]]),
                  "CLOUD", currently_row, row_after, sep = "_"),
            ".tif"
        )

        # resample chunks
        c_chunk_res <- .reg_apply_resample(
            rast = c_chunk_rast,
            tile = tile,
            resolution = res,
            resampling = resampling
        )

        b_chunk_res <- .reg_apply_resample(
            rast = b_chunk_rast,
            tile = tile,
            resolution = res,
            resampling = resampling
        )

        # mask band
        b_mask <- .reg_apply_mask(
            b_rast = b_chunk_res,
            mask_rast = c_chunk_res,
            interp_values = interp_values
        )

        # join chunks
        b_merged <- .reg_merge_images(
            tile = tile,
            b_rast = b_mask,
            output_dir = output_dir,
            start_date = start_date,
            block = b
        )

        b_merged
    }, progress = FALSE)


    # TODO: adjust this function
    r_filename <- paste0(
        output_dir, "/",
        paste("cube", .cube_tiles(tile), start_date, t_band, sep = "_"),
        ".", "tif"
    )

    .raster_merge(
        in_files = unlist(b_reg_path),
        out_file = r_filename,
        format = "GTiff",
        gdal_datatype = "Int16",
        gdal_options = .config_gtiff_default_options(),
        overwrite = TRUE
    )

    b_reg_rast <- terra::rast(r_filename)

    # set file info values
    file_info <- tibble::tibble(
        fid = paste("cube", .cube_tiles(tile), start_date, sep = "_"),
        date = start_date,
        band = t_band,
        xres  = terra::xres(b_reg_rast),
        yres  = terra::yres(b_reg_rast),
        xmin  = terra::xmin(b_reg_rast),
        xmax  = terra::xmax(b_reg_rast),
        ymin  = terra::ymin(b_reg_rast),
        ymax  = terra::ymax(b_reg_rast),
        nrows = terra::nrow(b_reg_rast),
        ncols = terra::ncol(b_reg_rast),
        path = r_filename
    )

    tile[["file_info"]][[1]] <- file_info

    return(tile)
}

.reg_get_chunk <- function(path, b) {

    r <- terra::rast(path)

    r_ext <- terra::rast(
        resolution = c(terra::xres(r), terra::yres(r)),
        xmin = terra::xmin(r),
        xmax = terra::xmax(r),
        ymin = terra::ymin(r),
        ymax = terra::ymax(r),
        crs = terra::crs(r)
    )

    bbox_ext <-  c(
        xmin = terra::xFromCol(r, b[["first_col"]]),
        xmax = terra::xFromCol(r, (1 + b[["ncols"]]) - 1),
        ymin = terra::yFromRow(r, (b[["nrows"]] + b[["first_row"]]) - 1),
        ymax = terra::yFromRow(r, b[["first_row"]])
    )

    terra::ext(r_ext) <- bbox_ext

    r_croped <- terra::crop(x = r, y = r_ext)

    return(r_croped)
}

.reg_apply_resample <- function(rast, tile, resolution, resampling) {


    t_bbox <- terra::ext(rast)

    # output template
    new_rast <- terra::rast(
        resolution = resolution,
        xmin  = terra::xmin(t_bbox),
        xmax  = terra::xmax(t_bbox),
        ymin  = terra::ymin(t_bbox),
        ymax  = terra::ymax(t_bbox),
        nrows = terra::nrow(rast),
        ncols = terra::ncol(rast),
        crs   = terra::crs(rast)
    )

    img_out <- terra::resample(
        x = rast,
        y = new_rast,
        method = resampling,
        datatype = "INT2S"
    )

    return(img_out)
}

.reg_apply_mask <- function(b_rast, mask_rast, interp_values) {

    img_r <- terra::mask(
        x = b_rast,
        mask = mask_rast,
        maskvalues = interp_values,
        datatype = "INT2S",
        gdal = .config_gtiff_default_options()
    )

    return(img_r)
}

.reg_merge_images <- function(tile, b_rast, output_dir, start_date, block) {

    # TODO: remove this function and implement in .reg_new_cube body

    t_file_info <- .file_info(tile)
    t_band <- .cube_bands(tile, add_cloud = FALSE)

    currently_row <- block[["first_row"]]
    row_after <- block[["nrows"]] + block[["first_row"]]


    file_ext <- tools::file_ext(
        gsub(".*/([^?]*)\\??.*$", "\\1", t_file_info[["path"]][[1]])
    )

    r_filename <- paste0(
        output_dir, "/",
        paste("cube", .cube_tiles(tile), start_date,
              t_band, currently_row, row_after, sep = "_"),
        ".", file_ext
    )

    t_datatype <- .source_collection_gdalcubes_type(
        .cube_source(cube = tile),
        collection = .cube_collection(cube = tile)
    )

    #t_rast_list <- purrr::map(t_file_info[["path"]], terra::rast)

    t_rast_list <- purrr::map(seq_len(terra::nlyr(b_rast)), function(i) {
        b_rast[[i]]
    })
    terra::merge(
        x =  terra::src(t_rast_list),
        filename = r_filename,
        wopt = list(datatype = t_datatype,
                    gdal = .config_gtiff_default_options()),
        overwrite = TRUE
    )

    r_filename
}

#' @title Create an object image_mask with information about mask band
#' @name .gc_cloud_mask
#' @keywords internal
#'
#' @param tile Data cube tile from where data is to be retrieved.
#'
#' @return A \code{object} 'image_mask' from gdalcubes containing information
#'  about the mask band.
.gc_cloud_mask <- function(tile) {

    bands <- .cube_bands(tile)
    cloud_band <- .source_cloud()

    # checks if the cube has a cloud band
    .check_chr_within(
        x = cloud_band,
        within = unique(bands),
        discriminator = "any_of",
        msg = paste("It was not possible to use the cloud",
                    "mask, please include the cloud band in your cube")
    )

    # create a image mask object
    mask_values <- gdalcubes::image_mask(
        cloud_band,

    )

    # is this a bit mask cloud?
    if (.source_cloud_bit_mask(
        source = .cube_source(cube = tile),
        collection = .cube_collection(cube = tile)))

        mask_values <- list(
            band = cloud_band,
            min = 1,
            max = 2^16,
            bits = mask_values$values,
            values = NULL,
            invert = FALSE
        )

    class(mask_values) <- "image_mask"

    return(mask_values)
}

#' @title Get the timeline of intersection in all tiles
#' @name .gc_get_valid_timeline
#'
#' @keywords internal
#'
#' @param cube       Data cube from where data is to be retrieved.
#' @param period     A \code{character} with ISO8601 time period for regular
#'  data cubes produced by \code{gdalcubes}, with number and unit, e.g., "P16D"
#'  for 16 days. Use "D", "M" and "Y" for days, month and year.
#'
#' @return a \code{vector} with all timeline values.
.gc_get_valid_timeline <- function(cube, period) {

    # pre-condition
    .check_chr(period, allow_empty = FALSE,
               len_min = 1, len_max = 1,
               msg = "invalid 'period' parameter")

    # start date - maximum of all minimums
    max_min_date <- do.call(
        what = max,
        args = purrr::map(cube[["file_info"]], function(file_info){
            return(min(file_info[["date"]]))
        })
    )

    # end date - minimum of all maximums
    min_max_date <- do.call(
        what = min,
        args = purrr::map(cube[["file_info"]], function(file_info){
            return(max(file_info[["date"]]))
        }))

    # check if all timeline of tiles intersects
    .check_that(
        x = max_min_date <= min_max_date,
        msg = "the timeline of the cube tiles do not intersect."
    )

    if (substr(period, 3, 3) == "M") {
        max_min_date <- lubridate::date(paste(
            lubridate::year(max_min_date),
            lubridate::month(max_min_date),
            "01", sep = "-"))
    } else if (substr(period, 3, 3) == "Y") {
        max_min_date <- lubridate::date(paste(
            lubridate::year(max_min_date),
            "01", "01", sep = "-"))
    }

    # generate timeline
    date <- lubridate::ymd(max_min_date)
    min_max_date <- lubridate::ymd(min_max_date)
    tl <- date
    while (TRUE) {
        date <- lubridate::ymd(date) %m+% lubridate::period(period)
        if (date > min_max_date) break
        tl <- c(tl, date)
    }

    # timeline cube
    tiles_tl <- suppressWarnings(sits_timeline(cube))

    if (!is.list(tiles_tl))
        tiles_tl <- list(tiles_tl)

    return(tl)
}
