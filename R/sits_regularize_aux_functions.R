sits_regularize_raster_blocks <- function(n_tiles,
                                          max_images_interval,
                                          nrows_out,
                                          ncols_out,
                                          ratio_in_out,
                                          ratio_cloud_out,
                                          memsize,
                                          multicores) {


    # size of the output (using RasterIO)
    output_size <- 4 * nrows_out * ncols_out
    # size of the input
    band_in_size <- output_size * ratio_in_out
    cloud_in_size <- output_size * ratio_cloud_out

    proc_bloat <- .config_processing_bloat()
    # total memory required
    total_mem_required <- (max_images_interval * (band_in_size + cloud_in_size) +
        output_size) * n_tiles * proc_bloat * 1e-09
    # total memory required per core
    total_mem_per_core <- total_mem_required/multicores
    # memory available per core
    avail_mem_per_core <- memsize/multicores
    # number of blocks
    num_blocks_per_core <- ceiling(total_mem_per_core/avail_mem_per_core)

    blocks_out <- .sits_regularize_block_list(nblocks,
                                              nrows_out,
                                              ncols_out)

    blocks_in <- .sits_regularize_block_list(nblocks,
                                             ceiling(nrows_out * ratio_in_out),
                                             ceiling(ncols_out * ratio_in_out))
    blocks_cloud <- .sits_regularize_block_list(nblocks,
                                                ceiling(nrows_out * ratio_cloud_out),
                                                ceiling(ncols_out * ratio_cloud_out))

    blocks.lst <- list(
        "blocks_out" = blocks_out,
        "blocks_in" = blocks_in,
        "blocks_cloud" = blocks_cloud
    )
    return (blocks.lst)

}

.sits_regularize_block_list <- function(nblocks,
                                        nrows,
                                        ncols) {

    # set caller to show in errors
    .check_set_caller(".sits_regularize_block_list")

    # number of rows per block
    block_rows <- ceiling(nrows/nblocks)

    first_row <- 1
    last_row <- nrows

    # initial row of each block
    row_vec <- seq.int(
        from = first_row,
        to = last_row,
        by = block_rows
    )

    # number of rows in each block
    n_rows <- length(row_vec)
    nrows_vec <- rep.int(block_rows, n_rows)

    # check that total number of rows is the same as the sum of all blocks
    # correct the last block for overflow
    if (sum(nrows_vec) != nrows) {
        nrows_vec[length(nrows_vec)] <-
           nrows - sum(nrows_vec[1:(length(nrows_vec) - 1)])
    }

    # elements of the block list
    # row        starting row in each block
    # nrows      number of rows in each block
    # col        first col
    # ncols      number of cols in each block
    blocks <- purrr::map2(row_vec, nrows_vec, function(rv, nr){
        block <- c("first_row"   = rv,
                   "nrows"       = nr,
                   "first_col"   = 1,
                   "ncols"       = ncols
        )

        return(block)
    })
    return(blocks)
}
