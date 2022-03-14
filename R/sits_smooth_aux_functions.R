#' @title Create a filename to store an output block from smoothing operation
#' @name .smth_block_filename
#' @keywords internal
#'
#' @param tile         Output tile to be written.
#' @param output_dir   Directory where block will be written.
#' @param block        Block designation.
#'
#' @return  returns a filename witn
#'
.smth_filename <- function(tile,
                           output_dir,
                           block) {

    band <- .file_info_bands(tile)

    start_date <- .file_info_start_date(tile)

    end_date <- .file_info_end_date(tile)

    b_filename <- paste("cube",
                        .cube_tiles(tile),
                        band,
                        start_date,
                        end_date, sep = "_")

    b_filename <- paste(b_filename,
                        "block",
                        block[["first_row"]],
                        block[["nrows"]] + block[["first_row"]] - 1,
                        sep = "_")

    b_path <- paste0(output_dir, "/", b_filename, ".tif")

    return(b_path)
}
#' @title Estimate the number of blocks to run .sits_split_cluster
#' @name .smth_estimate_block_size
#' @keywords internal
#'
#' @param cube         input data cube
#' @param multicores   number of processes to split up the data
#' @param memsize      maximum overall memory size (in GB)
#'
#' @return  returns a list with following information:
#'             - multicores theoretical upper bound;
#'             - block x_size (horizontal) and y_size (vertical)
#'
.smth_estimate_block_size <- function(cube, multicores, memsize) {

    # set caller to show in errors
    .check_set_caller(".smth_estimate_block_size")

    # precondition 1 - check if cube has probability data
    .check_that(
        x = inherits(cube, "probs_cube"),
        msg = "input is not probability cube"
    )
    size <- .cube_size(cube)
    n_layers <- length(cube$labels[[1]])
    bloat_mem <- .config_processing_bloat()
    n_bytes <- 8

    # total memory needed to do all work in GB
    needed_memory <-  1E-09 * size[["ncols"]] * size[["nrows"]] * n_layers * bloat_mem * n_bytes

    # minimum block size
    min_block_x_size <- size["ncols"] # for now, only vertical blocking
    min_block_y_size <- 1

    # compute factors
    memory_factor <- needed_memory / memsize
    blocking_factor <- size[["ncols"]] / min_block_x_size * size[["nrows"]] / min_block_y_size

    # stop if blocking factor is less than memory factor!
    # reason: the provided memory is not enough to process the data by
    # breaking it into small chunks
    .check_that(
        x = memory_factor <= blocking_factor,
        msg = "provided memory not enough to run the job"
    )

    # update multicores to the maximum possible processes given the available
    # memory and blocking factor
    multicores <- min(floor(blocking_factor / memory_factor), multicores)

    # compute blocking allocation that maximizes the
    # block / (memory * multicores) ratio, i.e. maximize parallel processes
    # and returns the following information:
    # - multicores theoretical upper bound;
    # - block x_size (horizontal) and y_size (vertical)
    blocks <- list(
        # theoretical max_multicores = floor(blocking_factor / memory_factor),
        block_x_size = floor(min_block_x_size),
        block_y_size = min(floor(blocking_factor / memory_factor / multicores),
                           size[["nrows"]])
    )

    return(blocks)
}
# function to compute blocks grid
.smth_compute_blocks <- function(xsize,
                                 ysize,
                                 block_y_size,
                                 overlapping_y_size) {

    r1 <- seq(1, ysize - 1, by = block_y_size)
    r2 <- c(r1[-1] - 1, ysize)
    nr1 <- r2 - r1 + 1
    ovr_r1 <- c(1, c(r1[-1] - overlapping_y_size))
    ovr_r2 <- c(r2[-length(r2)] + overlapping_y_size, ysize)
    ovr_nr1 <- ovr_r2 - ovr_r1 + 1

    # define each block as a list element
    blocks <- mapply(
        list,
        first_row      = ovr_r1,
        nrows          = ovr_nr1,
        first_col      = 1,
        ncols          = xsize,
        crop_first_row = r1 - ovr_r1 + 1,
        crop_nrows     = nr1,
        crop_first_col = 1,
        crop_ncols     = xsize,
        SIMPLIFY       = FALSE
    )

    return(blocks)
}

# function that run on workers...
.smth_worker_fun <- function(block, in_file, func, args) {

    # open brick
    b <- .raster_open_rast(in_file)

    # create extent
    blk_overlap <- list(first_row = block$r1,
                        nrows = block$r2 - block$r1 + 1,
                        first_col = 1,
                        ncols = .raster_ncols(b))

    # crop adding overlaps
    chunk <- .raster_crop(r_obj = b, block = blk_overlap)

    # process it
    raster_out <- do.call(func, args = c(list(chunk = chunk), args))

    # create extent
    blk_no_overlap <- list(first_row = block$o1,
                           nrows = block$o2 - block$o1 + 1,
                           first_col = 1,
                           ncols = .raster_ncols(raster_out))

    # crop removing overlaps
    raster_out <- .raster_crop(raster_out, block = blk_no_overlap)

    # export to temp file
    filename <- tempfile(tmpdir = dirname(.file_info(cube_out)$path),
                         fileext = ".tif")

    # save chunk
    .raster_write_rast(
        r_obj = raster_out,
        file = filename,
        format = "GTiff",
        data_type = .raster_data_type(.config_get("probs_cube_data_type")),
        gdal_options = .config_gtiff_default_options(),
        overwrite = TRUE
    )

    return(filename)
}

#' @title Parallel processing of classified images
#' @name .smth_map_layer
#' @keywords internal
#'
#' @description Process chunks of raster bricks individually in parallel.
#'
#' @param cube              Probability data cube tile
#' @param cube_out          Output probability data cube
#' @param overlapping_rows  number of overlapping rows of each chunk.
#' @param func              a function that receives RasterBrick and
#'                          returns any Raster*.
#' @param args              additional arguments to pass to \code{fun} function.
#' @param multicores        Number of process to run the Bayesian smoothing in
#'                          snow subprocess.
#' @param memsize           Maximum overall memory (in GB) to run the Bayesian
#'                          smoothing.
#' @param ...               optional arguments to merge final raster
#'
#' @return  Output data cube
#'
.smth_map_layer <- function(cube,
                            cube_out,
                            overlapping_y_size = 0,
                            func,
                            func_args = NULL,
                            multicores = 1,
                            memsize = 1,
                            gdal_datatype,
                            gdal_options, ...) {

    # set caller to show in errors
    .check_set_caller(".sits_smooth_map_layer")

    # pre-condition - one tile at a time
    .check_num(nrow(cube), min = 1, max = 1, is_integer = TRUE,
               msg = "process one tile only")

    # precondition - check if cube has probability data
    .check_that(
        x = inherits(cube, "probs_cube"),
        msg = "input is not probability cube"
    )

    # precondition - overlapping rows must be non negative
    .check_num(
        x = overlapping_y_size,
        min = 0,
        msg = "overlaping rows must be non negative"
    )

    # open probability file
    in_file <- .file_info_path(cube)

    # retrieve the file to be written
    out_file <- .file_info_path(cube_out)

    # compute how many tiles to be computed
    block_size <- .smth_estimate_block_size(cube = cube,
                                            multicores = multicores,
                                            memsize = memsize)

    # for now, only vertical blocks are allowed, i.e. 'x_blocks' is 1
    blocks <- .sits_compute_blocks(
        img_y_size = .cube_size(cube)["nrows"],
        block_y_size = block_size[["block_y_size"]],
        overlapping_y_size = overlapping_y_size)

    # process blocks in parallel
    block_files_lst <- .sits_parallel_map(blocks, function(block, in_file, func, args) {

        # open brick
        b <- .raster_open_rast(in_file)

        # create extent
        blk_overlap <- list(first_row = block$r1,
                            nrows = block$r2 - block$r1 + 1,
                            first_col = 1,
                            ncols = .raster_ncols(b))

        # crop adding overlaps
        chunk <- .raster_crop(r_obj = b, block = blk_overlap)

        # process it
        raster_out <- do.call(func, args = c(list(chunk = chunk), args))

        # create extent
        blk_no_overlap <- list(first_row = block$o1,
                               nrows = block$o2 - block$o1 + 1,
                               first_col = 1,
                               ncols = .raster_ncols(raster_out))

        # crop removing overlaps
        raster_out <- .raster_crop(raster_out, block = blk_no_overlap)

        # export to temp file
        block_file <- tempfile(tmpdir = dirname(.file_info(cube_out)$path),
                               fileext = ".tif")

        # save chunk
        .raster_write_rast(
            r_obj = raster_out,
            file = block_file,
            format = "GTiff",
            data_type = .raster_data_type(.config_get("probs_cube_data_type")),
            gdal_options = .config_gtiff_default_options(),
            overwrite = TRUE
        )

        return(block_file)
    })

    block_files <- unlist(block_files_lst)

    return(invisible(cube_out))
}


#' #' @title Parallel processing of classified images
#' #' @name .sits_smooth_map_layer
#' #' @keywords internal
#' #'
#' #' @description Process chunks of raster bricks individually in parallel.
#' #'
#' #' @param cube              Probability data cube tile
#' #' @param cube_out          Output probability data cube
#' #' @param overlapping_rows  number of overlapping rows of each chunk.
#' #' @param func              a function that receives RasterBrick and
#' #'                          returns any Raster*.
#' #' @param args              additional arguments to pass to \code{fun} function.
#' #' @param multicores        Number of process to run the Bayesian smoothing in
#' #'                          snow subprocess.
#' #' @param memsize           Maximum overall memory (in GB) to run the Bayesian
#' #'                          smoothing.
#' #' @param ...               optional arguments to merge final raster
#' #'
#' #' @return  Output data cube
#' #'
#' .sits_smooth_map_layer <- function(cube,
#'                                    cube_out,
#'                                    overlapping_y_size = 0,
#'                                    func,
#'                                    func_args = NULL,
#'                                    multicores = 1,
#'                                    memsize = 1,
#'                                    gdal_datatype,
#'                                    gdal_options, ...) {
#'
#'     # set caller to show in errors
#'     .check_set_caller(".sits_smooth_map_layer")
#'
#'     # pre-condition - one tile at a time
#'     .check_num(nrow(cube),
#'                min = 1, max = 1, is_integer = TRUE,
#'                msg = "process one tile only"
#'     )
#'
#'     # precondition - check if cube has probability data
#'     .check_that(
#'         x = inherits(cube, "probs_cube"),
#'         msg = "input is not probability cube"
#'     )
#'
#'     # precondition - overlapping rows must be non negative
#'     .check_num(
#'         x = overlapping_y_size,
#'         min = 0,
#'         msg = "overlaping rows must be non negative"
#'     )
#'
#'     # function to compute blocks grid
#'     .sits_compute_blocks <- function(img_y_size,
#'                                      block_y_size,
#'                                      overlapping_y_size) {
#'         r1 <- ceiling(seq(1, img_y_size - 1, by = block_y_size))
#'         r2 <- c(r1[-1] - 1, img_y_size)
#'         ovr_r1 <- c(1, c(r1[-1] - overlapping_y_size))
#'         ovr_r2 <- c(r2[-length(r2)] + overlapping_y_size, img_y_size)
#'
#'         # define each block as a list element
#'         mapply(list,
#'                r1 = ovr_r1,
#'                r2 = ovr_r2,
#'                o1 = r1 - ovr_r1 + 1,
#'                o2 = r2 - ovr_r1 + 1,
#'                SIMPLIFY = FALSE
#'         )
#'     }
#'
#'     # function that run on workers...
#'     .sits_cluster_worker_fun <- function(block, in_file, func, args) {
#'
#'         # open brick
#'         b <- .raster_open_rast(in_file)
#'
#'         # create extent
#'         blk_overlap <- list(
#'             first_row = block$r1,
#'             nrows = block$r2 - block$r1 + 1,
#'             first_col = 1,
#'             ncols = .raster_ncols(b)
#'         )
#'
#'         # crop adding overlaps
#'         chunk <- .raster_crop(r_obj = b, block = blk_overlap)
#'
#'         # process it
#'         raster_out <- do.call(func, args = c(list(chunk = chunk), args))
#'
#'         # create extent
#'         blk_no_overlap <- list(
#'             first_row = block$o1,
#'             nrows = block$o2 - block$o1 + 1,
#'             first_col = 1,
#'             ncols = .raster_ncols(raster_out)
#'         )
#'
#'         # crop removing overlaps
#'         raster_out <- .raster_crop(raster_out, block = blk_no_overlap)
#'
#'         # export to temp file
#'         filename <- tempfile(
#'             tmpdir = dirname(.file_info(cube)$path),
#'             fileext = ".tif"
#'         )
#'
#'         # save chunk
#'         .raster_write_rast(
#'             r_obj = raster_out,
#'             file = filename,
#'             format = "GTiff",
#'             data_type = .raster_data_type(.config_get("probs_cube_data_type")),
#'             gdal_options = .config_gtiff_default_options(),
#'             overwrite = TRUE
#'         )
#'
#'         return(filename)
#'     }
#'
#'     # function to call workers clusters and merge its results
#'     .sits_call_workers_cluster <- function(in_file, out_file, blocks, cl) {
#'
#'         # if file exists skip it (resume feature)
#'         if (file.exists(out_file)) {
#'             return(out_file)
#'         }
#'
#'         # apply function to blocks
#'         if (purrr::is_null(cl)) {
#'
#'             # do serial
#'             tmp_blocks <- lapply(
#'                 X = blocks,
#'                 FUN = .sits_cluster_worker_fun,
#'                 in_file = in_file,
#'                 func = func,
#'                 args = func_args
#'             )
#'         } else {
#'
#'             # do parallel
#'             tmp_blocks <- parallel::clusterApplyLB(
#'                 cl = cl,
#'                 x = blocks,
#'                 fun = .sits_cluster_worker_fun,
#'                 in_file = in_file,
#'                 func = func,
#'                 args = func_args
#'             )
#'         }
#'
#'         tmp_blocks <- unlist(tmp_blocks)
#'
#'         # on exit, remove temp files
#'         on.exit(unlink(tmp_blocks))
#'
#'         # merge to save final result
#'
#'         suppressWarnings(
#'             .raster_merge(
#'                 in_files = tmp_blocks,
#'                 out_file = out_file,
#'                 format = "GTiff",
#'                 gdal_datatype = gdal_datatype,
#'                 gdal_options = gdal_options,
#'                 overwrite = TRUE
#'             )
#'         )
#'
#'         return(tmp_blocks)
#'     }
#'
#'     # open probability file
#'     in_file <- .file_info_path(cube)
#'
#'     # retrieve the file to be written
#'     out_file <- .file_info_path(cube_out)
#'
#'     # compute how many tiles to be computed
#'     block_size <- .smth_estimate_block_size(
#'         cube = cube,
#'         multicores = multicores,
#'         memsize = memsize
#'     )
#'
#'     # for now, only vertical blocks are allowed, i.e. 'x_blocks' is 1
#'     blocks <- .sits_compute_blocks(
#'         img_y_size = .cube_size(cube)["nrows"],
#'         block_y_size = block_size[["block_y_size"]],
#'         overlapping_y_size = overlapping_y_size
#'     )
#'
#'     out_blocks <- .sits_call_workers_cluster(
#'         in_file = in_file,
#'         out_file = out_file,
#'         blocks = blocks,
#'         cl = cl
#'     )
#'
#'     return(invisible(out_blocks))
#' }
