#' @title parallel raster brick
#' @name .sits_split_cluster
#' @keywords internal
#'
#' @description Process chunks of raster brick in parallel.
#'
#' @param file             a path to raster file.
#' @param n_tiles          number of horizontal chunks to break the raster.
#' @param pad_rows         number of overlaping rows of each chunk.
#' @param fun              a function that receives RasterBrick and
#'                         returns any Raster*.
#' @param args             additional arguments to pass to \code{fun} function.
#' @param cl               snow cluster object
#' @param ...              optional arguments to merge final raster
#'                         (see \link[raster]{writeRaster} function)
#'
#' @return  RasterBrick object
#'
.sits_split_cluster <- function(file, n_tiles, pad_rows, fun, args = NULL,
                                cl = NULL, ...) {

    stopifnot(file.exists(file))
    stopifnot(n_tiles > 0)
    stopifnot(pad_rows >= 0)

    # open brick
    b <- raster::brick(file)

    # define blocks
    blocks <- ceiling(seq(1, nrow(b) + 1, length.out = n_tiles + 1))
    blocks <- mapply(list,
                     r1 = ifelse(blocks - pad_rows <= 0, 1,
                                 blocks - pad_rows)[seq_len(n_tiles)],
                     r2 = ifelse(blocks + pad_rows - 1 > nrow(b), nrow(b),
                                 blocks + pad_rows - 1)[-1:0], SIMPLIFY = FALSE,
                     orig1 = ifelse(blocks - pad_rows <= 0, 1,
                                    pad_rows + 1)[seq_len(n_tiles)],
                     orig2 = ifelse(blocks - pad_rows <= 0,
                                    blocks[-1:0] - blocks[seq_len(n_tiles)],
                                    blocks[-1:0] - blocks[seq_len(n_tiles)]
                                    + pad_rows + 1)[-1:0])

    # start progress bar
    pb <- txtProgressBar(max = length(blocks) + 1, style = 3)

    .arg_fun <- function(i) {

        setTxtProgressBar(pb, i)
        c(list(block = blocks[[i]]), x = file, fun = fun, args)
    }

    .sits_cluster_block_fun <- function(block, x, fun, ...) {

        # open brick
        b <- raster::brick(x)

        # crop adding overlaps
        x <- raster::crop(b, raster::extent(b,
                                            r1 = block$r1,
                                            r2 = block$r2,
                                            c1 = 1,
                                            c2 = ncol(b)))

        # process it
        res <- fun(x, ...)
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
            return(lapply(seq_along(blocks), function(i) {
                do.call(.sits_cluster_block_fun, args = .arg_fun(i))
            }))
        }

        snow::dynamicClusterApply(cl = cl,
                                  fun = .sits_cluster_block_fun,
                                  n = length(blocks),
                                  argfun = .arg_fun)
    }

    # apply function to blocks
    tmp_tiles <- .apply_cluster()

    # stop progress bar
    setTxtProgressBar(pb, length(blocks) + 1)
    close(pb)

    # on exit, remove temp files
    on.exit(unlink(tmp_tiles))

    # merge to save final result with '...' parameters
    message("Merging files...", appendLF = TRUE)
    do.call(raster::merge, c(lapply(tmp_tiles, raster::brick), list(...)))
}


