#' @title Build a regular data cube from an irregular one
#'
#' @name sits_regularize
#'
#' @description Creates cubes with regular time intervals
#'  using the gdalcubes package. Cubes can be composed using "median" or
#'  "least_cc_first" functions. Users need to provide an time
#'  interval which is used by the composition function.
#'
#' @references APPEL, Marius; PEBESMA, Edzer. On-demand processing of data cubes
#'  from satellite image collections with the gdalcubes library. Data, v. 4,
#'  n. 3, p. 92, 2019. DOI: 10.3390/data4030092.
#'
#' @examples{
#' \dontrun{
#'
#' # --- Access to the AWS STAC
#'
#' # define an AWS data cube
#'   s2_cube <- sits_cube(source = "AWS",
#'                       collection = "sentinel-s2-l2a-cogs",
#'                       bands = c("B08", "SCL"),
#'                       tiles = c("20LKP"),
#'                       start_date = as.Date("2018-07-18"),
#'                       end_date = as.Date("2018-08-18")
#'   )
#'
#' # create a directory to store the resulting images
#' dir.create(paste0(tempdir(),"/images/"))
#'
#' # Build a data cube of equal intervals using the "gdalcubes" package
#' gc_cube <- sits_regularize(cube       = s2_cube,
#'                            output_dir = paste0(tempdir(),"/images/"),
#'                            period     = "P1M",
#'                            res        = 320)
#' }
#' }
#'
#' @param cube         A \code{sits_cube} object whose spacing of observation
#'  times is not constant and will be regularized by the \code{gdalcubes}
#'  package.
#'
#' @param period       A \code{character} with ISO8601 time period for regular
#'  data cubes produced by \code{gdalcubes}, with number and unit, e.g., "P16D"
#'  for 16 days. Use "D", "M" and "Y" for days, month and year.
#'
#' @param res          A \code{numeric} with spatial resolution of the image
#'  that will be aggregated.
#'
#' @param output_dir   A \code{character} with a valid directory where the
#'  regularized images will be written by \code{gdalcubes}.
#'
#' @param multicores   A \code{numeric} with the number of cores used for
#'  regularization. This parameter specifies how many bands from different tiles
#'  should be processed in parallel. By default, 1 core is used.
#'
#' @param multithreads A \code{numeric} value that specifies the number of
#'  threads used in the gdalcubes package. This parameter determines how many
#'  chunks are executed in parallel. The gdalcubes package divides data cubes
#'  into smaller chunks, where the generated chunk creates a 3-dimensional array
#'  of band, latitude, and longitude information. By default 2 threads are used.
#'
#' @param memsize A \code{numeric} with memory available for regularization
#'  (in GB).
#'
#' @param progress     A \code{logical} value. Show progress bar?
#'
#' @note
#'    If malformed images with the same required tiles and bands are found in
#'    the current directory, these images are deleted and recreated.
#'
#' @note
#'    The "roi" parameter defines a region of interest. It can be
#'    an sf_object, a shapefile, or a bounding box vector with
#'    named XY values ("xmin", "xmax", "ymin", "ymax") or
#'    named lat/long values ("lat_min", "lat_max", "long_min", "long_max")
#'
#' @note
#'    The "least_cc_first" aggregation method sorts the images based on cloud
#'    cover, where images with the fewest clouds at the top of the stack. Once
#'    the stack of images is sorted, the method uses the first valid value to
#'    create the temporal aggregation.
#'
#' @note
#'    If the supplied data cube contains cloud band, the values indicated as
#'    clouds or cloud shadow will be removed.
#'
#' @return A \code{sits_cube} object with aggregated images.
#'
#' @export
sits_regularize <- function(cube,
                            period,
                            res,
                            output_dir,
                            multicores = 1,
                            multithreads = 2,
                            memsize = 4,
                            progress = TRUE) {

    # set caller to show in errors
    .check_set_caller("sits_regularize")

    # check documentation mode
    progress <- .check_documentation(progress)

    # precondition - test if provided object is a raster cube
    .check_that(
        x = inherits(cube, "raster_cube"),
        msg = paste("provided cube is invalid,",
                    "please provide a 'raster_cube' object.",
                    "see '?sits_cube' for more information.")
    )

    # precondition - check output dir fix
    output_dir <- normalizePath(output_dir)

    # verifies the path to save the images
    .check_that(
        x = dir.exists(output_dir),
        msg = "invalid 'output_dir' parameter."
    )

    # precondition - is the period valid?
    .check_na(lubridate::duration(period),
              msg = "invalid period specified")

    # TODO: check resolution as a multiple of input cube
    # precondition - is the resolution valid?
    .check_num(x = res,
               allow_zero = FALSE,
               min = 0,
               len_min = 1,
               len_max = 1,
               msg = "a valid resolution needs to be provided"
    )

    # check if output resolution is multiple of all input bands
    is_valid_res <- purrr::map_lgl(.cube_bands(cube = cube), function(band) {
        all(slider::slide_lgl(cube, function(tile) {
            res_tile <- .cube_resolution(cube = tile, bands = band)
            .is_int(max(res_tile[["yres"]], res) / min(res_tile[["yres"]], res),
                    tolerance = 0.01)
        }))
    })
    .check_that(
        all(is_valid_res),
        local_msg = paste0("provided resolution should be a multiple of",
                           "all input bands resolution"),
        msg = "invalid 'res' parameter"
    )

    # precondition - is there a cloud band?
    .check_chr_within(
        x = .source_cloud(),
        within = sits_bands(cube),
        discriminator = "all_of",
        msg = "cloud band should be in cube"
    )

    # precondition - is the multithreads valid?
    .check_num(
        x = multithreads,
        min = 1,
        len_min = 1,
        len_max = 1,
        msg = "invalid 'multithreads' parameter."
    )

    # precondition - is the multicores valid?
    .check_num(
        x = multicores,
        min = 1,
        len_min = 1,
        len_max = 1,
        msg = "invalid 'multicores' parameter."
    )

    # timeline of intersection
    reg_timeline <- .reg_timeline(cube, period = period)

    # start process
    .sits_parallel_start(multicores, log = FALSE)
    on.exit(.sits_parallel_stop())

    # TODO: check if files are corrupt?

    # does a local cube exist
    local_cube <- tryCatch({
        sits_cube(
            source = .cube_source(cube),
            collection = .cube_collection(cube),
            data_dir = output_dir,
            parse_info = .config_get("reg_file_parse_info"),
            multicores = multicores,
            progress = TRUE
        )
    },
    error = function(e) {
        return(NULL)
    })

    # find the tiles that have not been processed yet
    jobs <- .reg_missing_files(
        cube = cube,
        local_cube = local_cube,
        reg_timeline = reg_timeline
    )

    # start process
    .sits_parallel_start(multicores, log = FALSE)
    on.exit(.sits_parallel_stop())

    # recovery mode
    finished <- length(jobs) == 0

    while (!finished) {

        # process bands and tiles in parallel
        image_lst <- .sits_parallel_map(jobs, function(job) {

            tile_name <- job[[1]]
            band <- job[[2]]
            date <- job[[3]]

            tile <- dplyr::filter(cube, tile == !!tile_name)

            # post-condition
            .check_that(
                nrow(tile) == 1,
                local_msg = paste0("no tile '", tile_name, "' found"),
                msg = "invalid tile"
            )

            # filter band and cloud
            tile_band <- sits_select(
                data = tile,
                bands = c(band, .source_cloud())
            )

            # filter date interval
            end_date <- date %m+% lubridate::period(period)
            tile_band_interval <- tile_band

            tile_band_interval[["file_info"]][[1]] <-
                dplyr::filter(.file_info(tile_band_interval),
                              .data[["date"]] >= !!date,
                              .data[["date"]] < !!end_date)

            # least_cc_first requires images ordered based on cloud cover
            tile_band_interval[["file_info"]][[1]] <-
                dplyr::arrange(.file_info(tile_band_interval),
                               .data[["cloud_cover"]])

            # get output size
            out_size <- .reg_get_output_size(
                tile = tile_band_interval,
                out_res = res
            )

            # # get band_in_out ratio
            # ratio_band_in_out <- .reg_get_ratio_in_out(
            #     tile_band_interval,
            #     band = band,
            #     out_size = out_size
            # )
            ratio_band_in_out <- 1

            # get cloud_in_out ratio
            ratio_cloud_in_out <- .reg_get_ratio_in_out(
                tile_band_interval,
                band = .source_cloud(),
                out_size = out_size
            )

            # compute blocks to be processed
            blocks_list <- .reg_blocks_per_core(
                n_images_interval = nrow(.file_info(tile_band_interval)),
                nrows_out = out_size[["nrows"]],
                ncols_out = out_size[["ncols"]],
                ratio_in_out = ratio_band_in_out,
                ratio_cloud_out = ratio_cloud_in_out,
                memsize = memsize,
                multicores = multicores
            )

            # create of the composite cubes
            composite_file <- .reg_composite_image(
                tile_band_period = tile_band_interval,
                out_size = out_size,
                blocks = blocks_list,
                ratio_band_out = ratio_band_in_out,
                ratio_cloud_out = ratio_cloud_in_out,
                date = date,
                output_dir = output_dir
            )

            return(composite_file)

        }, progress = progress)

        # get a vector of produced images paths
        image_paths <- unlist(image_lst)

        # TODO: detect malformed files?

        # create local cube from files in output directory
        local_cube <- tryCatch({
            sits_cube(
                source = .cube_source(cube),
                collection = .cube_collection(cube),
                data_dir = output_dir,
                parse_info = .config_get("reg_file_parse_info"),
                multicores = multicores,
                progress = progress
            )
        },
        error = function(e){
            return(NULL)
        })

        # find if there are missing tiles
        jobs <- .reg_missing_files(
            cube = cube,
            local_cube = local_cube,
            reg_timeline = reg_timeline
        )

        # have we finished?
        finished <- length(jobs) == 0

        # inform the user
        if (!finished) {

            # convert list of missing tiles and bands to a list of vectors
            tiles_bands <- purrr::transpose(jobs)
            tiles_bands <- purrr::map(tiles_bands, unlist)

            # get missing tiles
            bad_tiles <- unique(tiles_bands[[1]])

            # get missing bands per missing tile
            msg <- paste(
                bad_tiles,
                purrr::map_chr(bad_tiles, function(tile) {
                    paste0("(",
                           paste0(unique(
                               tiles_bands[[2]][tiles_bands[[1]] == tile]),
                               collapse = ", "),
                           ")")
                }),
                collapse = ", ")

            # show message
            message(paste("Tiles", msg, "are missing or malformed",
                          "and will be reprocessed."))

            # remove cache
            .sits_parallel_stop()
            .sits_parallel_start(multicores, log = FALSE)
        }
    }

    return(local_cube)
}

#' @title Finds the missing files in a regularized cube
#'
#' @name .reg_missing_files
#' @keywords internal
#'
#' @param cube         original cube to be regularized
#' @param local_cube   regularized cube (may have missing files)
#' @param reg_timeline timeline used to regularize cube
#'
#' @return              tiles that are missing from the regularized cube
.reg_missing_files <- function(cube, local_cube, reg_timeline) {

    # get all tiles from cube
    tiles <- .cube_tiles(cube)

    # get all bands from cube
    bands <- .cube_bands(cube, add_cloud = FALSE)

    # do a cross product on tiles and bands
    tiles_bands_times <- unlist(slider::slide(cube, function(tile) {
        bands <- .cube_bands(tile, add_cloud = FALSE)
        purrr::cross3(.cube_tiles(tile), bands, reg_timeline)
    }), recursive = FALSE)

    # if regularized cube does not exist, return all tiles from original cube
    if (is.null(local_cube)) {
        return(tiles_bands_times)
    }

    # do a cross product on tiles and bands
    gc_tiles_bands_times <- unlist(slider::slide(local_cube, function(tile) {
        bands <- .cube_bands(tile, add_cloud = FALSE)
        purrr::cross3(.cube_tiles(tile), bands, reg_timeline)
    }), recursive = FALSE)

    # first, include tiles and bands that have not been processed
    miss_tiles_bands_times <-
        tiles_bands_times[!tiles_bands_times %in% gc_tiles_bands_times]

    # second, include tiles and bands that have been processed
    proc_tiles_bands_times <-
        tiles_bands_times[tiles_bands_times %in% gc_tiles_bands_times]

    # do all tiles and bands in local_cube have the same timeline as
    # the original cube?
    bad_timeline <- purrr::pmap_lgl(
        purrr::transpose(proc_tiles_bands_times),
        function(tile, band, date) {
            tile <- local_cube[local_cube[["tile"]] == tile, ]
            tile <- sits_select(tile, bands = band)
            return(!date %in% sits_timeline(tile))
        })

    # update malformed processed tiles and bands
    proc_tiles_bands_times <- proc_tiles_bands_times[bad_timeline]

    # return all tiles from the original cube
    # that have not been processed or regularized correctly
    miss_tiles_bands_times <-
        unique(c(miss_tiles_bands_times, proc_tiles_bands_times))

    return(miss_tiles_bands_times)
}

.reg_diagnostic <- function(data_dir, file_paths = NULL) {

    # check only if ...
    if (!is.null(file_paths) && length(file_paths) == 0)
        return(character(0))

    # get file_paths parameter as default path list
    paths <- file_paths

    # otherwise search in data_dir
    if (is.null(file_paths)) {

        # how many of those files are images?
        # retrieve the known file extensions
        file_ext <- .config_local_file_extensions()

        # list the files in the data directory
        paths <- list.files(
            path = data_dir,
            pattern = paste0("\\.(", paste0(file_ext, collapse = "|"), ")$"),
            full.names = TRUE
        )
    }

    # check documentation mode
    progress <- TRUE
    progress <- .check_documentation(progress)

    # open and read files
    bad_paths <- .sits_parallel_map(paths, function(path) {
        val <- tryCatch({
            img <- terra::rast(path)
            sum(is.na(terra::values(img)))
            FALSE
        }, error = function(e) TRUE)
        val
    }, progress = progress)

    bad_paths <- paths[unlist(bad_paths)]
    existing_files <- file.exists(bad_paths)

    return(bad_paths[existing_files])
}

#' @title Generate a composite image based on tile interval.
#'
#' @name .reg_composite_image
#'
#' @keywords internal
#'
#' @param tile_period_band A unique tile from \code{sits_cube} object
#' @param output_size  A \code{numeric} vector with nrows and ncols of
#'  image to be generated
#' @param blocks       A \code{list} with: blocks of output band,
#' blocks of input band, and blocks of cloud band
#' @param date         A \code{Date} of reference to generate the image
#' composite
#' @param output_dir   A \code{character} with a valid directory where the
#'  regularized images will be written.
#'
#' @return  A data cube tile with information used in its creation.
.reg_composite_image <- function(tile_band_period,
                                 out_size,
                                 blocks,
                                 ratio_band_out,
                                 ratio_cloud_out,
                                 date,
                                 output_dir) {

    # set caller to show in errors
    .check_set_caller(".reg_composite_image")

    fi <- .file_info(tile_band_period)

    # get the band to be processed
    band <- .cube_bands(tile_band_period, add_cloud = FALSE)


    # output file name
    output_filename <- .reg_filename(
        tile = tile_band_period,
        band = band,
        date = date,
        output_dir = output_dir
    )

    if (file.exists(output_filename))
        return(output_filename)

    # get output datatype
    reg_datatype <- .config_get("raster_cube_data_type")

    # open parallel process to read all interval dates
    reg_masked_blocks_lst <- purrr::map(
        .file_info_fids(tile_band_period),
        function(fid) {

            # for each block
            blocks_reg_path <- purrr::pmap_chr(
                blocks,
                function(block_out, block_in, block_cloud) {

                    tile_fid <- tile_band_period
                    tile_fid[["file_info"]][[1]] <-
                        .file_info(tile_fid, fid = fid)

                    fi_fid <- .file_info(tile_fid)

                    # name of output block
                    output_file_block <- .reg_filename(
                        tile = tile_fid,
                        band = band,
                        date = unique(fi_fid[["date"]]),
                        output_dir = output_dir,
                        block = block_out
                    )

                    # cloud preprocess
                    .reg_preprocess_block(
                        tile_fid = tile_fid,
                        band = band,
                        out_size = out_size,
                        band_block = block_in,
                        cloud_block = block_cloud,
                        output_block = block_out,
                        ratio_band_out = ratio_band_out,
                        ratio_cloud_out = ratio_cloud_out,
                        data_type = reg_datatype,
                        output_file = output_file_block
                    )

                    return(output_file_block)
                })

            return(blocks_reg_path)
        })

    # aggregate first method
    agg_block_paths <- purrr::map_chr(
        seq_along(blocks[["block_out"]]),
        function(i) {

            # get i-th block for all dates in interval
            block_files <- purrr::map_chr(reg_masked_blocks_lst, `[[`, i)

            # read values for all corresponding blocks in interval
            mtx <- .raster_read_stack(block_files)

            # make a local template
            r_obj <- .raster_rast(.raster_open_rast(block_files[[1]]))

            # do merge using first available pixel
            r_obj <- .raster_set_values(
                r_obj  = r_obj,
                values = reg_agg_first(mtx)
            )

            # get band missing value
            missing_value <- .cube_band_missing_value(
                cube = tile_band_period,
                band = band
            )

            # name of output block
            output_file_block <- .reg_filename(
                tile       = tile_band_period,
                band       = band,
                date       = date,
                output_dir = output_dir,
                block      = blocks[["block_out"]][[i]]
            )

            # write merged regularized masked block
            .raster_write_rast(
                r_obj         = r_obj,
                file          = output_file_block,
                format        = "GTiff",
                data_type     = reg_datatype,
                gdal_options  = .config_gtiff_default_options(),
                overwrite     = TRUE,
                missing_value = missing_value
            )

            return(output_file_block)
    })

    # # get tile bbox
    # bbox <- .cube_tile_bbox(tile_band_period)
    #
    # # create output raster
    # r_obj <- .raster_new_rast(
    #     nrows = out_size[["nrows"]],
    #     ncols = out_size[["ncols"]],
    #     xmin = bbox[["xmin"]],
    #     xmax = bbox[["xmax"]],
    #     ymin = bbox[["ymin"]],
    #     ymax = bbox[["ymax"]],
    #     nlayers = 1,
    #     crs = .cube_crs(tile_band_period)
    # )
    #
    # # set values
    # values <- reg_agg_first(do.call(rbind, agg_block_lst))
    # r_obj <- .raster_set_values(r_obj = r_obj, values = values)
    #
    # # get band missing value
    # missing_value <- .cube_band_missing_value(
    #     cube = tile_band_period,
    #     band = band
    # )
    #
    # # write raster
    # .raster_write_rast(
    #     r_obj = r_obj,
    #     file = output_filename,
    #     format = "GTiff",
    #     data_type = reg_datatype,
    #     gdal_options = .config_gtiff_default_options(),
    #     overwrite = FALSE,
    #     missing_value = missing_value
    # )

    # merge file info
    .raster_merge(
        in_files = agg_block_paths,
        out_file = output_filename,
        format = "GTiff",
        gdal_datatype = .raster_gdal_datatype(reg_datatype),
        gdal_options = .config_gtiff_default_options(),
        overwrite = TRUE
    )

    # remove blocks
    unlink(unlist(reg_masked_blocks_lst))
    gc()

    return(output_filename)
}

#' @title Preprocessing steps of sits regularize
#'
#' @name .reg_preprocess_block
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
.reg_preprocess_block <- function(tile_fid,
                                  band,
                                  out_size,
                                  band_block,
                                  cloud_block,
                                  output_block,
                                  ratio_band_out,
                                  ratio_cloud_out,
                                  data_type,
                                  output_file) {

    # get input band path
    band_path <- .file_info_paths(
        cube = tile_fid,
        bands = band
    )

    # check if cloud_path has length one
    .check_chr(band_path,
               allow_empty = FALSE, len_min = 1, len_max = 1,
               msg = "invalid band path value")

    # get input cloud path
    cloud_path <- .file_info_paths(
        cube = tile_fid,
        bands = .source_cloud()
    )

    # check if cloud_path has length one
    .check_chr(cloud_path,
               allow_empty = FALSE, len_min = 1, len_max = 1,
               msg = "invalid cloud path value")

    # read input band and change its dimension to input block
    band_values <- matrix(as.integer(
        .raster_read_stack(files = band_path,
                           block = band_block,
                           out_size = output_block)),
        nrow = output_block[["nrows"]],
        ncol = output_block[["ncols"]],
        byrow = TRUE
    )

    # read input band and change its dimension to cloud block
    cloud_values <- matrix(as.integer(
        .raster_read_stack(files = cloud_path,
                           block = cloud_block)),
        nrow = cloud_block[["nrows"]],
        ncol = cloud_block[["ncols"]],
        byrow = TRUE
    )

    # get the interpolation values (cloud values)
    cloud_interp <- .source_cloud_interp_values(
        source = .cube_source(cube = tile_fid),
        collection = .cube_collection(cube = tile_fid)
    )

    reg_masked_mtx <- reg_resample(
        band = band_values,
        cloud = cloud_values,
        ratio_band_out = 1,
        ratio_cloud_out = ratio_cloud_out,
        nrows_out = output_block[["nrows"]],
        ncols_out = output_block[["ncols"]],
        cloud_interp = cloud_interp
    )

    # compute bound box of output block
    bbox <- .cube_tile_bbox(tile_fid)

    # create output block raster
    r_obj <- .raster_crop(
        r_obj = .raster_new_rast(
            nrows = out_size[["nrows"]],
            ncols = out_size[["ncols"]],
            xmin = bbox[["xmin"]],
            xmax = bbox[["xmax"]],
            ymin = bbox[["ymin"]],
            ymax = bbox[["ymax"]],
            nlayers = 1,
            crs = .cube_crs(tile_fid)
        ),
        block = output_block
    )

    # set values
    r_obj <- .raster_set_values(r_obj = r_obj, values = reg_masked_mtx)

    # get band missing value
    missing_value <- .cube_band_missing_value(tile_fid, band = band)

    # write to disk
    .raster_write_rast(
        r_obj        = r_obj,
        file         = output_file,
        format       = "GTiff",
        data_type    = data_type,
        gdal_options = .config_gtiff_default_options(),
        overwrite    = TRUE,
        missing_value = missing_value
    )

    return(output_file)
}
