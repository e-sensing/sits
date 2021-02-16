#' @title Compute the 2-D Gaussian kernel
#' @name .sits_gauss_kernel
#' @keywords internal
#'
#' @param window_size   Size of the neighbourhood.
#' @param sigma         Standard deviation of the spatial Gaussian kernel
#'
#' @return  returns a squared matrix filled with Gaussian function
#'
.sits_gauss_kernel <- function(window_size, sigma = 1) {

    stopifnot(window_size %% 2 != 0)

    w_center <- ceiling(window_size / 2)
    w_seq <- seq_len(window_size)
    x <- stats::dnorm(
        (abs(rep(w_seq, each = window_size) - w_center) ^ 2 +
             abs(rep(w_seq, window_size) - w_center) ^ 2) ^ (1 / 2),
        sd = sigma) / stats::dnorm(0)
    matrix(x / sum(x), nrow = window_size, byrow = T)
}

#' @title Estimate the number of blocks to run .sits_split_cluster
#' @name .sits_probs_blocks_size_estimate
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
.sits_probs_blocks_size_estimate <- function(cube, multicores, memsize) {

    # precondition 1 - check if cube has probability data
    assertthat::assert_that(inherits(cube, "probs_cube"),
                            msg = "sits_smooth: input is not probability cube"
    )

    x_size <- cube$ncols
    y_size <- cube$nrows
    n_layers <- length(cube$labels[[1]])
    bloat_mem <- .sits_config_memory_bloat()
    n_bytes <- 8

    # total memory needed to do all work in GB
    needed_memory <- x_size * y_size * n_layers * bloat_mem * n_bytes * 1E-09

    # minimum block size
    min_block_x_size <- x_size # for now, only allowing vertical blocking
    min_block_y_size <- 1

    # compute factors
    memory_factor <- needed_memory / memsize
    blocking_factor <- x_size / min_block_x_size * y_size / min_block_y_size

    # stop if blocking factor is less than memory factor!
    # reason: the provided memory is not enough to process the data by
    # breaking it into small chunks
    assertthat::assert_that(memory_factor <= blocking_factor,
        msg = "sits_smooth: provided memory not enough to run the job"
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
                           y_size)
    )

    return(blocks)
}
#' @title parallel raster brick
#' @name .sits_map_layer_cluster
#' @keywords internal
#'
#' @description Process chunks of raster brick individually in parallel.
#'
#' @param cube              Probability data cube
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
#'                          (see \link[raster]{writeRaster} function)
#'
#' @return  RasterBrick object
#'
.sits_map_layer_cluster <- function(cube, cube_out, overlapping_y_size = 0,
                                    func, func_args = NULL, multicores = 1,
                                    memsize = 1, ...) {

    # precondition 1 - check if cube has probability data
    assertthat::assert_that(inherits(cube, "probs_cube"),
            msg = ".sits_split_cluster: input is not probability cube"
    )

    # precondition 2 - overlapping rows must be non negative
    assertthat::assert_that(overlapping_y_size >= 0,
            msg = ".sits_split_cluster: overlaping rows must be non negative"
    )

    # function to compute blocks grid
    .sits_compute_blocks <- function(img_y_size,
                                     block_y_size,
                                     overlapping_y_size) {

        r1 <- ceiling(seq(1, img_y_size - 1, by = block_y_size))
        r2 <- c(r1[-1] - 1, img_y_size)
        ovr_r1 <- c(1, c(r1[-1] - overlapping_y_size))
        ovr_r2 <- c(r2[-length(r2)] + overlapping_y_size, img_y_size)

        # define each block as a list element
        mapply(list,
               r1 = ovr_r1,
               r2 = ovr_r2,
               o1 = r1 - ovr_r1 + 1,
               o2 = r2 - ovr_r1 + 1,
               SIMPLIFY = FALSE)
    }

    # function that run on workers...
    .sits_cluster_worker_fun <- function(block, in_file, func, args) {

        # open brick
        b <- suppressWarnings(raster::brick(in_file))

        # crop adding overlaps
        chunk <- suppressWarnings(raster::crop(
            b, raster::extent(b,
                              r1 = block$r1,
                              r2 = block$r2,
                              c1 = 1,
                              c2 = ncol(b))
        ))

        # process it
        res <- do.call(func, args = c(list(chunk = chunk), args))
        stopifnot(inherits(res, c("RasterLayer", "RasterStack", "RasterBrick")))

        # crop removing overlaps
        res <- suppressWarnings(raster::crop(
            res, raster::extent(res,
                                r1 = block$o1,
                                r2 = block$o2,
                                c1 = 1,
                                c2 = ncol(res))
        ))

        # export to temp file
        filename <- tempfile(fileext = ".tif")
        suppressWarnings(
            raster::writeRaster(res, filename = filename, overwrite = TRUE,
                                datatype = "FLT8S")
        )

        filename
    }

    # function to call workers clusters and merge its results
    .sits_call_workers_cluster <- function(in_file, out_file, blocks, cl) {

        # apply function to blocks
        if (purrr::is_null(cl)) {

            # do serial
            tmp_blocks <- lapply(
                X = blocks,
                FUN = .sits_cluster_worker_fun,
                in_file = in_file,
                func = func,
                args = func_args)
        } else {

            # do parallel
            tmp_blocks <- parallel::clusterApplyLB(
                cl = cl,
                x = blocks,
                fun = .sits_cluster_worker_fun,
                in_file = in_file,
                func = func,
                args = func_args)
        }

        # on exit, remove temp files
        on.exit(unlink(tmp_blocks))

        # merge to save final result with '...' parameters
        # if there is only one block...
        if (length(tmp_blocks) == 1)
            return(suppressWarnings(
                raster::writeRaster(raster::brick(tmp_blocks),
                                    overwrite = TRUE,
                                    filename = out_file, ...)
            ))
        # ... else call raster::merge.
        suppressWarnings(
            do.call(raster::merge,
                    c(lapply(tmp_blocks, raster::brick),
                      list(overwrite = TRUE, filename = out_file),
                      list(...)))
        )
    }

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

    # traverse all tiles
    slider::slide2(cube, cube_out, function(cube_row, out_file_row) {

        # open probability file
        in_files <- .sits_cube_files(cube_row)

        # retrieve the files to be read and written
        out_files <- .sits_cube_files(out_file_row)

        # compute how many tiles to be computed
        block_size <- .sits_probs_blocks_size_estimate(cube = cube_row,
                                                       multicores = multicores,
                                                       memsize = memsize)

        # for now, only vertical blocks are allowed, i.e. 'x_blocks' is 1
        blocks <- .sits_compute_blocks(
            img_y_size = cube_row$nrows,
            block_y_size = block_size[["block_y_size"]],
            overlapping_y_size = overlapping_y_size)

        # traverse all years
        purrr::map2(in_files, out_files, .sits_call_workers_cluster,
                    blocks = blocks, cl = cl)

    })

    return(invisible(cube_out))
}


