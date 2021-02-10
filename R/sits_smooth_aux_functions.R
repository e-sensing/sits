#' @title Estimate the number of blocks to run .sits_split_cluster
#' @name .sits_split_probs_blocks_estimate
#' @keywords internal
#'
#' @param cube         input data cube
#' @param multicores   number of processes to split up the data
#' @param memory       maximum overall memory size (in GB)
#'
#' @return  returns a list with following information:
#'             - multicores theoretical upper bound;
#'             - how many blocks? horizontal (x) and vertical (y)
#'
.sits_split_probs_blocks_estimate <- function(cube, multicores, memory) {

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
    # horizontal
    min_block_x_size <- x_size # for now, only allowing vertical blocking
    # vertical
    min_block_y_size <- 1

    # compute factors
    memory_factor <- needed_memory / memory
    blocking_factor <- x_size / min_block_x_size * y_size / min_block_y_size

    # stop if blocking factor is less than memory factor
    # the provided memory is not enough
    assertthat::assert_that(memory_factor <= blocking_factor,
        msg = "sits_smooth: provided memory not enough to run the job"
    )

    # this function computes horizontal and vertical best block allocation
    .best_blocking <- function(block_memproc_ratio, x_blocking_factor,
                               y_blocking_factor) {

        x_blocking_factor = max(floor(x_blocking_factor), 1)
        y_blocking_factor = max(floor(y_blocking_factor), 1)

        m <- 1
        res <- list(x_min_blocks = 1, y_min_blocks = 1)
        for (j in seq_len(x_blocking_factor)) {
            # try to maximize vertical block size (second order maximization)...
            i <- min(floor(block_memproc_ratio / j), y_blocking_factor)
            x <- i * j
            # ...but maximize first 'block_memproc_ratio' resulting ratio
            if (x > m) {
                m <- x
                res[["x_min_blocks"]] <- j
                res[["y_min_blocks"]] <- i
            }
        }
        # number of minimum blocks in x and y axis
        res
    }

    # maximum processes to run given available memory and blocking factor
    multicores <- min(floor(blocking_factor / memory_factor), multicores)

    # compute blocking allocation that maximizes the
    #  block / (memory * multicores) ratio, i.e. maximize parallel processes
    blocking <- .best_blocking(block_memproc_ratio = blocking_factor /
                                   memory_factor / multicores,
                               x_blocking_factor = x_size / min_block_x_size,
                               y_blocking_factor = y_size / min_block_y_size)

    # returns the following information:
    # - multicores theoretical upper bound;
    # - how many blocks? horizontal (x) and vertical (y)
    blocks <- list(
        max_multicores = floor(blocking_factor / memory_factor),
        x_blocks = ceiling(x_size / (blocking[["x_min_blocks"]] *
                                         min_block_x_size)),
        y_blocks = ceiling(y_size / (blocking[["y_min_blocks"]] *
                                         min_block_y_size))
    )

    return(blocks)
}
#' @title parallel raster brick
#' @name .sits_split_cluster
#' @keywords internal
#'
#' @description Process chunks of raster brick in parallel.
#'
#' @param file               a path to raster (brick) file.
#' @param n_tiles            number of vertical chunks to break the raster.
#'                           For now, only vertical blocking is supported.
#' @param overlapping_rows   number of overlapping rows of each chunk.
#' @param fun                a function that receives RasterBrick and
#'                           returns any Raster*.
#' @param args               additional arguments to pass to \code{fun} function.
#' @param cl                 snow cluster object
#' @param ...                optional arguments to merge final raster
#'                           (see \link[raster]{writeRaster} function)
#'
#' @return  RasterBrick object
#'
.sits_split_cluster <- function(file, n_tiles, overlapping_rows, fun, args = NULL,
                                cl = NULL, ...) {

    # precondition 1 - number of vertical blocks (n_tiles) must be positive
    assertthat::assert_that(n_tiles > 0,
            msg = "sits_smooth: number of blocks must be positive"
    )

    # precondition 2 - overlaping rows must be non negative
    assertthat::assert_that(overlapping_rows >= 0,
            msg = "sits_smooth: overlaping rows must be non negative"
    )

    # open brick
    b <- suppressWarnings(raster::brick(file))

    # define blocks
    blocks <- ceiling(seq(1, nrow(b) + 1, length.out = n_tiles + 1))
    blocks <- mapply(list,
                     r1 = ifelse(blocks - overlapping_rows <= 0, 1,
                                 blocks - overlapping_rows)[seq_len(n_tiles)],
                     r2 = ifelse(blocks + overlapping_rows - 1 > nrow(b), nrow(b),
                                 blocks + overlapping_rows - 1)[-1:0], SIMPLIFY = FALSE,
                     orig1 = ifelse(blocks - overlapping_rows <= 0, 1,
                                    overlapping_rows + 1)[seq_len(n_tiles)],
                     orig2 = ifelse(blocks - overlapping_rows <= 0,
                                    blocks[-1:0] - blocks[seq_len(n_tiles)],
                                    blocks[-1:0] - blocks[seq_len(n_tiles)]
                                    + overlapping_rows + 1)[-1:0])

    # run on workers...
    .sits_cluster_block_fun <- function(block, file, fun, ...) {

        # open brick
        b <- suppressWarnings(raster::brick(file))

        # crop adding overlaps
        file <- raster::crop(b, raster::extent(b,
                                            r1 = block$r1,
                                            r2 = block$r2,
                                            c1 = 1,
                                            c2 = ncol(b)))

        # process it
        res <- fun(file, ...)
        stopifnot(inherits(res, c("RasterLayer", "RasterStack", "RasterBrick")))

        # crop removing overlaps
        res <- raster::crop(res, raster::extent(res,
                                                r1 = block$orig1,
                                                r2 = block$orig2,
                                                c1 = 1,
                                                c2 = ncol(res)))

        # export to temp file
        filename <- tempfile(fileext = ".tif")
        raster::writeRaster(res, filename = filename, overwrite = TRUE)

        filename
    }

    .apply_cluster <- function() {

        if (purrr::is_null(cl)) {

            # do serial
            res <- lapply(X = blocks,
                          FUN = .sits_cluster_block_fun,
                          file = file,
                          fun = fun, ...)
        } else {

            # do parallel
            res <- parallel::clusterApplyLB(cl = cl,
                                            x = blocks,
                                            fun = .sits_cluster_block_fun,
                                            file = file,
                                            fun = fun, ...)
        }

        return(res)
    }

    # apply function to blocks
    tmp_tiles <- .apply_cluster()

    # on exit, remove temp files
    on.exit(unlink(tmp_tiles))

    # merge to save final result with '...' parameters
    suppressWarnings(
        do.call(raster::merge, c(list(overwrite = TRUE),
                                 lapply(tmp_tiles, raster::brick), list(...)))
    )
}


