sits_regularize_get_ratio <- function(tile, band, res_out) {

    tile_size <- sits_regularize_get_size(tile, res_out = res_out)

    ratio_band <- unique(c(
        .file_info_nrows(tile, bands = band) / tile_size[["nrows"]],
        .file_info_ncols(tile, bands = band) / tile_size[["ncols"]]
    ))

    .check_length(
        ratio_band,
        len_min = 1,
        len_max = 1,
        msg = "invalid nrows and ncols from cube."
    )

    return(ratio_band)
}

sits_regularize_get_size <- function(tile, res_out) {

    tile_bbox <- .cube_tile_bbox(tile)

    tile_rast <- terra::rast(
        xmin = tile_bbox[["xmin"]],
        xmax = tile_bbox[["xmax"]],
        ymin = tile_bbox[["ymin"]],
        ymax = tile_bbox[["ymax"]],
        resolution = c(x = res_out, y = res_out),
        crs = .cube_crs(tile)
    )

    tile_size <- c(nrows = .raster_nrows(tile_rast),
                   ncols = .raster_ncols(tile_rast))

    return(tile_size)
}

sits_regularize_raster_blocks <- function(n_tiles,
                                          max_images_interval,
                                          nrows_out,
                                          ncols_out,
                                          ratio_in_out,
                                          ratio_cloud_out,
                                          memsize,
                                          multicores) {


    # size of the output (using RasterIO)
    nbytes <- 4
    output_size <- nbytes * nrows_out * ncols_out

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

    blocks_out <- .sits_regularize_block_list(num_blocks_per_core,
                                              nrows_out,
                                              ncols_out)

    blocks_in <- .sits_regularize_block_list(num_blocks_per_core,
                                             ceiling(nrows_out * ratio_in_out),
                                             ceiling(ncols_out * ratio_in_out))

    blocks_cloud <- .sits_regularize_block_list(num_blocks_per_core,
                                                ceiling(nrows_out * ratio_cloud_out),
                                                ceiling(ncols_out * ratio_cloud_out))

    blocks.lst <- list(
        "blocks_out" = blocks_out,
        "blocks_in" = blocks_in,
        "blocks_cloud" = blocks_cloud
    )

    return(blocks.lst)
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

#' @title Compute the timeline of the regularized cube
#' @name .reg_timeline
#'
#' @keywords internal
#'
#' @param cube       Data cube from where data is to be retrieved.
#' @param period     A \code{character} with ISO8601 time period for regular
#'  data cubes produced by \code{gdalcubes}, with number and unit, e.g., "P16D"
#'  for 16 days. Use "D", "M" and "Y" for days, month and year.
#'
#' @return a \code{vector} with all timeline values.
.reg_timeline <- function(cube, period) {

    .check_set_caller(".reg_timeline")

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
