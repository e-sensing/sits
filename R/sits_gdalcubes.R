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
#' @param tile_period_band A unique tile from \code{sits_cube} object
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
#' @param blocks ...
#'
#' @param date_period ...
#'
#' @param multithreads ...
#'
#' @return  A data cube tile with information used in its creation.
.reg_new_cube <- function(tile_period_band,
                          res,
                          blocks,
                          date_period,
                          resampling,
                          roi,
                          output_dir,
                          multithreads) {

    # set caller to show in errors
    .check_set_caller(".reg_new_cube")

    fi <- .file_info(tile_period_band)

    tile_band <- .cube_bands(tile_period_band, add_cloud = FALSE)

    # get the interp values
    interp_values <- .source_cloud_interp_values(
        source = .cube_source(cube = tile_period_band),
        collection = .cube_collection(cube = tile_period_band)
    )

    reg_datatype <- .config_get("raster_cube_data_type")

    # for each block
    blocks_reg_path <- purrr::map_chr(blocks, function(block) {

        multithreads <- min(multithreads, length(unique(fi[["fid"]])))

        .sits_parallel_start(multithreads, log = FALSE)
        on.exit(.sits_parallel_stop())

        #block_band_dates <- slider::slide_chr(unique(fi[["fid"]]), function(fid) {
        block_band_dates <- .sits_parallel_map(unique(fi[["fid"]]), function(fid) {
            tile_fid <- tile_period_band
            tile_fid[["file_info"]][[1]]  <- .file_info(tile_fid, fid = fid)

            fi_fid <- .file_info(tile_fid)

            band_date_block <- .reg_create_filename(
                tile = tile_fid,
                date_period = unique(fi_fid[["date"]]),
                output_dir = output_dir,
                band = tile_band,
                block = block,
                posfix = "reg"
            )

            cloud_date_block <- .reg_create_filename(
                tile = tile_fid,
                date_period = unique(fi_fid[["date"]]),
                output_dir = output_dir,
                band = .source_cloud(),
                block = block,
                posfix = "reg"
            )

            c_path <- .file_info_paths(
                cube = tile_fid,
                bands = .source_cloud()
            )

            b_path <- .file_info_paths(
                cube = tile_fid,
                bands = tile_band
            )

            # cloud preprocess
            c_chunk_res <- .reg_preprocess_rast(
                tile = tile_period_band,
                band_paths = c_paths,
                resolution = res,
                resampling = .config_get("cloud_resampling_methods"),
                block = block,
                datatype = reg_datatype,
                filename = cloud_date_block
            )

            # band preprocess
            b_chunk_res <- .reg_preprocess_rast(
                tile = tile_period_band,
                band_paths = b_paths,
                resolution = res,
                resampling = resampling,
                block = block,
                datatype = reg_datatype,
                filename = band_date_block
            )

            # mask band
            b_mask <- .reg_apply_mask_rast(
                rast = b_chunk_res,
                mask_rast = c_chunk_res,
                interp_values = interp_values,
                filename = band_date_block,
                datatype = reg_datatype,
            )

            unlink(cloud_date_block)
            gc()

            return(band_date_block)
        }, progress = TRUE)

        band_date_block <- .reg_create_filename(
            tile = tile_period_band,
            date_period = date_period,
            output_dir = output_dir,
            band = tile_band,
            block = block
        )

        .reg_aggregate_chunk(
            rast_files = unlist(block_band_dates),
            filename = band_date_block,
            datatype = reg_datatype
        )


        unlink(unlist(block_band_dates))
        gc()

        return(band_date_block)
    })

    output_filename <- paste0(
        output_dir, "/",
        paste("cube", .cube_tiles(tile_period_band), date_period, tile_band, sep = "_"),
        ".", "tif"
    )

    .raster_merge(
        in_files = blocks_reg_path,
        out_file = output_filename,
        format = "GTiff",
        gdal_datatype = .raster_gdal_datatype(reg_datatype),
        gdal_options = .config_gtiff_default_options(),
        overwrite = TRUE
    )

    return(output_filename)
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
.reg_preprocess_rast <- function(tile,
                                 band_paths,
                                 resolution,
                                 resampling,
                                 block,
                                 datatype,
                                 filename) {

    rast <- terra::rast(band_paths)

    chunk_rast <- .reg_get_chunk_rast(
        rast = rast,
        tile = tile,
        block = block,
        datatype = datatype,
        filename = filename
    )

    res_rast <- .reg_resample_rast(
        rast = chunk_rast,
        resolution = resolution,
        resampling = resampling,
        datatype = datatype,
        filename = filename
    )

    return(res_rast)
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
.reg_preprocess_new <- function(tile,
                                band_path,
                                cloud_path,
                                resolution,
                                resampling,
                                block,
                                datatype,
                                filename,
                                cloud_interp,
                                missing_value) {

    band_values <- .raster_read_stack(files = band_path,
                                      block = block)

    cloud_values <- .raster_read_stack(files = cloud_path,
                                       block = block)

    band_chunk_rast <- .reg_get_chunk_rast(
        rast = band_rast,
        tile = tile,
        block = block,
        datatype = datatype,
        filename = filename
    )


    reg_resample(
        band = band_values,
        cloud = cloud_values,
        ratio_band_out = ,
        ratio_cloud_out = ,
        nrows_out = ,
        ncols_out = ,
        cloud_interp = cloud_interp,
        missing_value = missing_value
    )



    return(res_rast)
}

#' @title Create the merge image filename
#'
#' @name .reg_create_filename
#'
#' @keywords internal
#'
#' @param tile         A unique tile from \code{sits_cube} object
#'
#' @param date_period  A \code{character} vector with two position, first one is
#' the start date and second one is the end date.
#'
#' @param output_dir   A \code{character} with a valid directory where the
#'  regularized images will be written.
#'
#' @param block      A \code{numeric} vector with information about a block
#'
#' @param tile ...
#'
#' @param band       ....
#'
#' @return A \code{character} with the file name of resampled image.
.reg_create_filename <- function(tile, date_period, output_dir, band, block = NULL, posfix = NULL) {

    file_ext <- unique(
        tools::file_ext(
            x = gsub(".*/([^?]*)\\??.*$", "\\1",
                     .file_info_paths(tile, bands = band))
        )
    )

    .check_length(
        x = file_ext,
        len_min = 1,
        len_max = 1,
        msg = "invalid files extensions."
    )

    b_filename <- paste("cube", .cube_tiles(tile), date_period, band, sep = "_")

    if (!is.null(block)) {

        currently_row <- block[["first_row"]]
        next_rows <- (block[["nrows"]] + block[["first_row"]]) - 1

        b_filename <- paste(b_filename, currently_row, next_rows, sep = "_")
    }

    if (!is.null(posfix))
        b_filename <- paste(b_filename, posfix, sep = "_")

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
.reg_get_chunk_rast <- function(rast, tile, block, filename, datatype) {

    sub_image <- .sits_raster_sub_image_from_block(block = block, tile = tile)

    sub_image_ext <- terra::ext(
        c(xmin = sub_image[["xmin"]],
          xmax = sub_image[["xmax"]],
          ymin = sub_image[["ymin"]],
          ymax = sub_image[["ymax"]])
    )

    cropped_rast <- terra::crop(
        x = rast,
        y = sub_image_ext,
        NAflag = .config_get("raster_cube_missing_value"),
        #filename = filename,
        datatype = datatype,
        #overwrite = TRUE
    )

    return(cropped_rast)
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
.reg_resample_rast <- function(rast,
                               resolution,
                               resampling,
                               datatype,
                               filename, ...) {

    t_bbox <- terra::ext(rast)

    # output template
    new_rast <- terra::rast(
        resolution = c(x = resolution, y = resolution),
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
        NAflag = .config_get("raster_cube_missing_value"),
        datatype = datatype,
        filename = filename,
        overwrite = TRUE, ...
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
        NAflag = .config_get("raster_cube_missing_value"),
        overwrite = TRUE, ...
    )

    return(img_r)
}

#' @title Aggregate raster chunk
#' @name .reg_aggregate_chunk
#'
#' @keywords internal
#'
#' @param rast_files     A \code{SpatRast} object
#'
#' @param filename A \code{character} with filename of the rast.
#'
#' @param datatype A \code{character} with terra datatype.
#'
#' @param ...      additional paramters for terra merge methods.
#'
#' @return An invisible null
.reg_aggregate_chunk <- function(rast_files, filename, datatype, ...) {


    rast_list <- purrr::map(rast_files, terra::rast)

    terra::mosaic(
        x =  terra::sprc(rast_list),
        fun = "first",
        filename = filename,
        datatype = datatype,
        gdal = .config_gtiff_default_options(),
        overwrite = TRUE,
        NAflag = .config_get("raster_cube_missing_value"), ...
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
