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
#' @param tile         A unique tile from \code{sits_cube} object
#'
#' @param res          A \code{numeric} with spatial resolution of the image
#'  that will be aggregated.
#'
#' @param period       A \code{character} vector with two position, first one is
#' the start date and second one is the end date.
#'
#' @param resampling   A \code{character} with method to be used  for resampling in mosaic operation.
#'  Options: \code{near}, \code{bilinear}, \code{bicubic}, \code{cubicspline},
#'  and \code{lanczos}.
#'  Default is bilinear.
#'
#' @param roi          A named \code{numeric} vector with a region of interest.
#'
#' @param output_dir   A \code{character} with a valid directory where the
#'  regularized images will be written.
#'
#' @param agg_method   A \code{character} with method that will be applied by
#'  \code{gdalcubes} for aggregation. Options: \code{median} and
#'  \code{least_cc_first}. The default aggregation method is
#'  \code{least_cc_first}. See more above.
#'
#' @param multithreads A \code{numeric} value that specifies the number of
#'  threads used in the regularization process.
#'  By default 2 threads are used.
#'
#' @param memsize A \code{numeric} with memory available for regularization
#'  (in GB).
#'
#' @return  A data cube tile with information used in its creation.
.reg_new_cube <- function(tile,
                          res,
                          period,
                          resampling,
                          roi,
                          output_dir,
                          multithreads,
                          memsize) {

    # set caller to show in errors
    .check_set_caller(".reg_new_cube")

    t_band <- .cube_bands(tile, add_cloud = FALSE)

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

    blocks_dates <- purrr::cross2(blocks, sits_timeline(tile))

    # get the interp values
    interp_values <- .source_cloud_interp_values(
        source = .cube_source(cube = tile),
        collection = .cube_collection(cube = tile)
    )

    reg_datatype <- .config_get("raster_cube_data_type")

    # prepare parallel requests
    # if (is.null(sits_env[["cluster"]])) {
    #     .sits_parallel_start(workers = multithreads, log = FALSE)
    #     on.exit(.sits_parallel_stop(), add = TRUE)
    # }

    # for each block
    #b_reg_path <- .sits_parallel_map(blocks, function(b) {
    b_reg_path <- purrr::map_dfr(blocks_dates, function(block_date) {

        block <- block_date[[1]]
        date <- block_date[[2]]

        c_path <- .file_info_paths(
            cube = tile,
            bands = .source_cloud(),
            dates = date
        )

        b_path <- .file_info_paths(
            cube = tile,
            bands = .cube_bands(tile, FALSE),
            dates = date
        )

        band_filename_block <- .reg_create_filaname(
            tile = tile,
            date = date,
            output_dir = output_dir,
            block = block
        )

        if (file.exists(band_filename_block)) {

            # try to open the band files
            r_obj <- tryCatch({
                .raster_open_rast(band_filename_block)
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
                                    value      = band_filename_block)
                    return(band_filename_block)
                }
        }

        # cloud preprocess
        c_chunk_res <- .reg_preprocess_rast(
            tile = tile,
            band_paths = c_path,
            resolution = res,
            resampling = .config_get("cat_resampling_methods"),
            block = block
        )

        # band preprocess
        b_chunk_res <- .reg_preprocess_rast(
            tile = tile,
            band_paths = b_path,
            resolution = res,
            resampling = resampling,
            block = block
        )

        # mask band
        b_mask <- .reg_apply_mask_rast(
            rast = b_chunk_res,
            mask_rast = c_chunk_res,
            interp_values = interp_values,
            filename = band_filename_block,
            datatype = reg_datatype,
        )

        tibble::tibble(
            date = date,
            path = band_filename_block,
            block = block[["first_row"]]
        )
    #}, progress = FALSE)
    })

    b_merge_filename <- .reg_create_filaname(
        tile = tile,
        date = period[[1]],
        output_dir = output_dir
    )

    blocks <- unique(b_reg_path[["block"]])
    purrr::map(blocks, function(block) {

        # b_merge_filename_block <- .reg_create_filaname(
        #     tile = tile,
        #     date = period[[1]],
        #     output_dir = output_dir,
        #     block = block
        # )
        b_merge_filename_block <- tempfile(pattern = block)

        block_paths <- dplyr::filter(b_reg_path, .data[["block"]] == !!block)

        .reg_merge_chunks(
            tbl_rast = block_paths[["rast"]],
            filename = b_merge_filename_block,
            datatype = reg_datatype
        )

        b_merge_filename_block
    })

    # join chunks
    .reg_merge_chunks(
        tbl_rast = b_reg_path,
        filename = b_merge_filename,
        datatype = reg_datatype
    )

    r_filename <- paste0(
        output_dir, "/",
        paste("cube", .cube_tiles(tile), period[[1]], t_band, sep = "_"),
        ".", "tif"
    )

    .raster_merge(
        in_files = unlist(b_reg_path),
        out_file = r_filename,
        format = "GTiff",
        gdal_datatype = .raster_gdal_datatype(reg_datatype),
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

#' @title Preprocessing steps of sits regularize
#'
#' @name .reg_preprocess_rast
#'
#' @keywords internal
#'
#' @param tile         A unique tile from \code{sits_cube} object
#'
#' @param band_paths   A \code{character} with paths for a unique band
#'
#' @param resolution   A \code{numeric} with spatial resolution of the image
#'  that will be aggregated.
#'
#' @param resampling   A \code{character} with method to be used  for resampling
#'  in mosaic operation. Options: \code{near}, \code{bilinear}, \code{bicubic},
#'  \code{cubicspline}, and \code{lanczos}. Default is bilinear.
#'
#' @param block      A \code{numeric} vector with information about a block
#'
#' @return A \code{SpatRast} object resampled
.reg_preprocess_rast <- function(tile, band_paths, resolution, resampling, block) {

    rast <- terra::rast(band_paths)

    chunk_rast <- .reg_get_chunk_rast(rast = rast, block = block)

    res_rast <- .reg_resample_rast(
        rast = chunk_rast,
        resolution = resolution,
        resampling = resampling
    )

    return(res_rast)
}

#' @title Create the merge image filename
#'
#' @name .reg_create_filaname
#'
#' @keywords internal
#'
#' @param tile         A unique tile from \code{sits_cube} object
#'
#' @param period       A \code{character} vector with two position, first one is
#' the start date and second one is the end date.
#'
#' @param output_dir   A \code{character} with a valid directory where the
#'  regularized images will be written.
#'
#' @param block      A \code{numeric} vector with information about a block
#'
#' @return A \code{character} with the file name of resampled image.
.reg_create_filaname <- function(tile, date, output_dir, block = NULL) {

    t_band <- .cube_bands(tile, add_cloud = FALSE)

    file_ext <- unique(
        tools::file_ext(
            x = gsub(".*/([^?]*)\\??.*$", "\\1", .file_info_paths(tile))
        )
    )

    .check_length(
        x = file_ext,
        len_min = 1,
        len_max = 1,
        msg = "invalid files extensions."
    )

    b_filename <- paste("cube", .cube_tiles(tile), date, t_band, sep = "_")
    if (!is.null(block)) {

        currently_row <- block[["first_row"]]
        next_rows <- (block[["nrows"]] + block[["first_row"]]) - 1

        b_filename <- paste(b_filename, currently_row, next_rows, sep = "_")
    }

    b_path <- paste0(output_dir, "/", b_filename, ".", file_ext)

    return(b_path)
}

#' @title Get chunks from rasters
#'
#' @name .reg_get_chunk_rast
#'
#' @keywords internal
#'
#' @param rast  A \code{SpatRast} object
#'
#' @param block A \code{numeric} vector with information about a block
#'
#' @return A \code{SpatRast} object cropped.
.reg_get_chunk_rast <- function(rast, block) {

    r_ext <- terra::rast(
        resolution = c(terra::xres(rast), terra::yres(rast)),
        xmin = terra::xmin(rast),
        xmax = terra::xmax(rast),
        ymin = terra::ymin(rast),
        ymax = terra::ymax(rast),
        crs = terra::crs(rast)
    )

    bbox_ext <-  c(
        xmin = terra::xFromCol(rast, block[["first_col"]]),
        xmax = terra::xFromCol(rast, (1 + block[["ncols"]]) - 1),
        ymin = terra::yFromRow(rast, (block[["nrows"]] + block[["first_row"]]) - 1),
        ymax = terra::yFromRow(rast, block[["first_row"]])
    )

    terra::ext(r_ext) <- bbox_ext

    return(
        terra::crop(x = rast, y = r_ext)
    )
}

#' @title Resample raster to a resolution
#'
#' @name .reg_resample_rast
#'
#' @keywords internal
#'
#' @param rast       A \code{SpatRast} object
#'
#' @param resolution A \code{numeric} with spatial resolution of the image
#'  that will be aggregated.
#'
#' @param resampling A \code{character} with method to be used  for resampling
#'  in mosaic operation. Options: \code{near}, \code{bilinear}, \code{bicubic},
#'  \code{cubicspline}, and \code{lanczos}. Default is bilinear.
#'
#' @param ...        additional paramters for terra resample methods.
#'
#' @return A \code{SpatRast} object resample
.reg_resample_rast <- function(rast, resolution, resampling, ...) {

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
        ...
    )

    return(img_out)
}

#' @title Apply a cloud mask in a raster
#'
#' @name .reg_apply_mask_rast
#'
#' @keywords internal
#'
#' @param rast       A \code{SpatRast} object
#'
#' @param mask_rast  A \code{numeric}
#'
#' @param interp_values A \code{numeric} with
#'
#' @param ...        additional paramters for terra mask methods.
#'
#' @return A \code{SpatRast} object masked.
.reg_apply_mask_rast <- function(rast, mask_rast, interp_values, ...) {

    img_r <- terra::mask(
        x = rast,
        mask = mask_rast,
        maskvalues = interp_values,
        gdal = .config_gtiff_default_options(),
        ...
    )

    return(img_r)
}

#' @title Merge raster chunks
#' @name .reg_merge_chunks
#'
#' @keywords internal
#'
#' @param rast     A \code{SpatRast} object
#'
#' @param filename A \code{character} with filename of the rast.
#'
#' @param datatype A \code{character} with terra datatype.
#'
#' @param ...      additional paramters for terra merge methods.
#'
#' @return An invisible null
.reg_merge_chunks <- function(file_paths, filename, datatype, ...) {

    t_rast_list <- purrr::map(file_paths, function(file_path) {
        terra::rast(file_path)
    })

    terra::merge(
        x =  terra::src(t_rast_list),
        filename = filename,
        datatype = datatype,
        gdal = .config_gtiff_default_options(),
        overwrite = TRUE,
        ...
    )

    return(invisible(NULL))
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
