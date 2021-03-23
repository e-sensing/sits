.sits_proc_cube_block <- function(cube_tile, block, resolution,
                                  sits_proc_fn, sits_proc_args_fn) {

    print(resolution)

    # precondition 1: process only a tile
    assertthat::assert_that(
        nrow(cube_tile) == 1,
        msg = ".sits_proc_cube_block: accepts only one tile at a time"
    )

    # load default multicores for access data
    multicores <- .sits_config_access_maxcores(.sits_cube_source(cube_tile))

    # make snow cluster to access raster files
    cl <- NULL
    if (multicores > 1) {

        # start clusters
        cl <- parallel::makeCluster(multicores)

        # make sure library paths is the same as actual environment
        lib_paths <- .libPaths()
        parallel::clusterExport(cl, "lib_paths", envir = environment())
        parallel::clusterEvalQ(cl, .libPaths(lib_paths))

        # stop all workers when finish
        on.exit(parallel::stopCluster(cl))
    }

    # get raster information to fill cube fields

    # define the function that retrieves, crop, and process a
    # block for a layer_path
    # parameter img_file_info contains all bands of one date
    # the function process only a block of all that bands
    # this function returns the processed blocks' file_info
    .sits_proc_img_block <- function(img_file_info, block) {

        # open all bands at a time (this allows arithmetic operations)
        path <- purrr::pmap_chr(img_file_info, function(path, band, res, ...) {

            # support for individual layers only
            # if the path points to a brick only the first band
            # will be retrieved
            # the image chunk is saved as a temporary file
            filename <- gdalUtilities::gdal_translate(
                src_dataset = path,
                dst_dataset = tempfile(fileext = ".tif"),
                b = 1,
                tr = c(resolution, resolution),
                r = .sits_config_resampling(cube_tile$sensor, band),
                srcwin = c(block$c1,
                           block$r1,
                           block$c2 - block$c1 + 1,
                           block$r2 - block$r1 + 1),
                co = "COMPRESS=LZW"
            )

            return(filename)
        })

        # prepare result file_info
        res_file_info <- dplyr::tibble(date = img_file_info$date,
                                       band = img_file_info$band,
                                       res = resolution,
                                       path = path)

        return(res_file_info)
    }

    # group file_info by date before process
    file_info_by_date <- unname(c(by(cube_tile$file_info[[1]],
                                     cube_tile$file_info[[1]]$date,
                                     FUN = list)))

    # retrieve, process, and return block chunks
    if (purrr::is_null(cl)) {

        # do serial
        chunk_file_info_by_date <- lapply(
            X = file_info_by_date,
            FUN = .sits_proc_img_block,
            block = block)
    } else {

        # do parallel
        chunk_file_info_by_date <- parallel::clusterApplyLB(
            cl = cl,
            x = file_info_by_date,
            fun = .sits_proc_img_block,
            block = block)
    }

    # bind all dates together
    chunk_file_info <- dplyr::bind_rows(chunk_file_info_by_date)

    # create a cube with temp chunks
    cube_chunk <- cube_tile
    cube_chunk$file_info[[1]] <- chunk_file_info
    cube_chunk <- .sits_cube_update_raster_params(cube_chunk)

    # delete chunk files on exit
    on.exit(unlink(chunk_file_info$path))

    # apply sits_proc_fn function
    cube_res <- do.call(what = sits_proc_fn, args = c(list(cube_chunk),
                                                      sits_proc_args_fn))

    return(cube_res)
}

#' @title parallel raster brick
#' @name .sits_map_stack_cluster
#' @keywords internal
#'
#' @description Process chunks of raster brick individually in parallel.
#'
#' @param cube_in           raster data cube
#' @param cube_out          output probability data cube
#' @param layer_fn          a function that receives RasterLayer and
#'                          returns a RasterLayer.
#' @param args              additional arguments to pass to \code{fun} function.
#' @param multicores        Number of process to run the function in
#'                          snow subprocess.
#' @param memsize           Maximum overall memory (in GB) to run the function.
#' @param ...               optional arguments to merge final raster
#'                          (see \link[raster]{writeRaster} function)
#'
#' @return  RasterBrick object
#'
.sits_map_stack_cluster <- function(cube_in,
                                    cube_out,
                                    resolution,
                                    sits_proc_fn,
                                    multicores,
                                    memsize, ...) {

    # #######################################################
    # # read the blocks and compute the probabilities
    # probs_blocks <- purrr::map(c(1:block_info$n), function(b) {
    #     # define the extent for each block
    #     extent <- c(
    #         block_info$row[b], block_info$nrows[b],
    #         block_info$col, block_info$ncols
    #     )
    #     names(extent) <- (c("row", "nrows", "col", "ncols"))
    #
    #     # read the data
    #     distances <- .sits_raster_data_read(
    #         cube = tile,
    #         samples = samples,
    #         extent = extent,
    #         stats = stats,
    #         filter_fn = filter_fn,
    #         impute_fn = impute_fn,
    #         interp_fn = interp_fn,
    #         compose_fn = compose_fn,
    #         multicores = multicores
    #     )
    #
    #     # get the attribute names
    #     attr_names <- names(.sits_distances(samples[1, ]))
    #     assertthat::assert_that(
    #         length(attr_names) > 0,
    #         msg = "sits_classify: training data not available"
    #     )
    #     # set column names for DT
    #     colnames(distances) <- attr_names
    #     # predict the classification values
    #     prediction <- .sits_classify_interval(
    #         data = distances,
    #         ml_model = ml_model,
    #         multicores = multicores
    #     )
    #     # convert probabilities matrix to INT2U
    #     scale_factor_save <- round(1 / .sits_config_probs_scale_factor())
    #     prediction <- round(scale_factor_save * prediction, digits = 0)
    #
    #     # estimate processing time
    #     .sits_est_class_time(
    #         start_time = start_time,
    #         n_blocks = block_info$n,
    #         block = b
    #     )
    #
    #     return(prediction)
    # })
    # ##################################################

    # precondition 1 - check if cube has raster data
    assertthat::assert_that(
        inherits(cube_in, "raster_cube"),
        msg = ".sits_split_cluster: input is not raster cube"
    )

    # make snow cluster
    cl <- NULL
    if (multicores > 1) {

        # start clusters
        cl <- parallel::makeCluster(multicores)

        # make sure library paths is the same as actual environment
        lib_paths <- .libPaths()
        parallel::clusterExport(cl, "lib_paths", envir = environment())
        parallel::clusterEvalQ(cl, .libPaths(lib_paths))

        # stop all workers when finish
        on.exit(parallel::stopCluster(cl))
    }

    # function to compute blocks grid
    .sits_compute_blocks <- function(img_x_size,
                                     img_y_size,
                                     block_x_size,
                                     block_y_size) {

        c1 <- ceiling(seq(1, img_x_size - 1, by = block_x_size))
        c2 <- c(c1[-1] - 1, img_x_size)
        r1 <- ceiling(seq(1, img_y_size - 1, by = block_y_size))
        r2 <- c(r1[-1] - 1, img_y_size)

        # define each block as a list element with names r1, r2, c1, c2
        do.call(mapply,
                args = c(list(FUN = list, SIMPLIFY = FALSE),
                         merge(dplyr::tibble(r1 = r1,
                                             r2 = r2),
                               dplyr::tibble(c1 = c1,
                                             c2 = c2)))
        )
    }


    # function that run on workers...
    .sits_cluster_worker_fun <- function(block) {

        # process a cube block
        .sits_proc_cube_block(
            cube_tile = cube_in,
            block = block,
            resolution = resolution,
            sits_proc_fn = sits_proc_fn
        )
    }

    return(invisible(cube_out))
}
