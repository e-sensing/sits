#' @title Define a reasonable block size to process a RasterBrick
#' @name .sits_raster_blocks
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Defines the size of the block of a Raster Brick to be read.
#' The total pixels of a RasterBrick  the size of the timeline
#' with the number of rows and columns of the Brick. For example, a Raster Brick
#' with 500 rows and 500 columns and 400 time instances will have a total pixel size
#' of 800 Mb if pixels are 64-bit.
#'
#' @param  cube            input data cube.
#' @param  ml_model        machine learning model.
#' @param  sf_region       spatial region of interest (sf_object)
#' @param  interval        classification interval.
#' @param  memsize         memory available for classification (in GB).
#' @param  multicores      number of threads to process the time series.
#' @return                 list with three attributes: n (number of blocks),
#'                         rows (list of rows to begin),
#'                         nrows (number of rows to read at each iteration).
#'
.sits_raster_blocks <- function(cube, ml_model, sf_region, interval, memsize, multicores){
    # number of bands
    nbands <-  length(.sits_cube_bands(cube))
    # timeline
    timeline <- sits_timeline(cube[1,])
    # define the sub_image (which may be the same size as the original)
    sub_image <- .sits_raster_sub_image(cube, sf_region)


    nblocks <- .sits_raster_blocks_estimate(ml_model   = ml_model,
                                            nbands     = nbands,
                                            nrows      = nrows,
                                            ncols      = ncols,
                                            timeline   = timeline,
                                            interval   = interval,
                                            memsize    = memsize,
                                            multicores = multicores)

    block.lst <- .sits_raster_block_list(nblocks = nblocks,
                                         nrows = nrows,
                                         ncols = ncols)

    return(block.lst)
}
.sits_raster_sub_image <- function(cube, region) {
    sub_image = vector(length = 4)
    names(sub_image) <- c("first_row", "first_col", "nrows", "ncols")

    if (purrr::is_null(region)){
        sub_image["first_row"] <- 1
        sub_image["first_col"] <- 1
        sub_image["nrows"]  <- cube[1,]$nrows
        sub_image["ncols"]  <- cube[1,]$ncols
    }
    else {
        # if the sf object is not in image coordinates, convert it
        sf_region_im <- suppressWarnings(sf::st_transform(sf_region, crs = cube[1,]$crs))
        bbox <- sf::st_bbox(sf_region_im)
    }

}
#' @title Calculate a list of blocks to be read from disk to memory
#' @name .sits_raster_block_list
#' @keywords internal
#'
#' @param nblocks number of blocks to read from each image
#' @param nrows   number of rows in the image
#' @param ncols   number of cols in the image
#' @return        a list with n (number of blocks), row (vector of starting rows),
#'                nrow (vector with number of rows for each block) and
#'                size (vector with size of each block)
#'
.sits_raster_block_list <- function (nblocks, nrows, ncols){
    # number of rows per block
    block_rows <- ceiling(nrows/nblocks)

    # initial row of each block
    row.vec <- seq.int(from = 1, to = nrows, by = block_rows)
    # number of rows in each block
    nrows.vec <- rep.int(block_rows, length(row.vec))
    # check that total number of rows is the same as the sum of all blocks
    # correct the last block for overflow
    if (sum(nrows.vec) != nrows )
        nrows.vec[length(nrows.vec)] <- nrows - sum(nrows.vec[1:(length(nrows.vec) - 1)])

    # find out the size of the block in pixels
    size.vec <- nrows.vec * ncols

    # elements of the block list
    # n          number of blocks
    # row        starting row from the RasterBrick
    # nrow       Number of rows in the block extracted from the RasterBrick
    # size       size of each block in pixels

    block.lst <- list(n = length(row.vec), row = row.vec, nrows = nrows.vec, size = size.vec)

    return(block.lst)
}


#' @title Estimate the number of blocks
#' @name .sits_raster_blocks_estimate
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Defines the number of blocks of a Raster Brick
#'              to be read into memory.
#'
#' @param  ml_model        Machine learning model.
#' @param  nbands          Number of bands.
#' @param  nrows           Number of rows per brick.
#' @param  ncols           Number of cols per brick.
#' @param  timeline        Timeline of the brick.
#' @param  interval        Classification interval.
#' @param  memsize         Memory available for classification (in GB).
#' @param  multicores      Number of threads to process the time series.
#' @return Number of blocks to be read.
.sits_raster_blocks_estimate <- function(ml_model,
                                         nbands,
                                         nrows,
                                         ncols,
                                         timeline,
                                         interval,
                                         memsize,
                                         multicores) {
    # total number of instances
    ninstances <- length(timeline)
    # number of instances per classification interval
    # calculate the interval between each scene and the start date
    data_duration <- lubridate::as.duration(lubridate::as_date(timeline)
                                          - lubridate::as_date(timeline[1]))
    # are there images outside the classification interval?
    interval_dates <- data_duration > lubridate::as.duration(interval)
    if (any(interval_dates))
        ninterval <- which(interval_dates)[1] - 1
    else
        ninterval <- ninstances
    # number of bytes per pixel
    nbytes <-  8
    # estimated memory bloat
    bloat <- as.numeric(.sits_config_memory_bloat())
    # estimated processing bloat
    proc_bloat <- as.numeric(.sits_config_processing_bloat())
    if (proc_bloat == 0) proc_bloat <- multicores

    # single instance size
    single_data_size <- as.numeric(nrows)*as.numeric(ncols)*as.numeric(nbytes)
    # total size including all bands
    nbands_data_size <- single_data_size*as.numeric(nbands)

    # estimated full size of the data
    full_size <- as.numeric(ninstances)*nbands_data_size

    # estimated size of memory required for scaling and normalization
    mem_required_scaling <- (full_size + as.numeric(.sits_mem_used()))*bloat

    .sits_log_debug(paste0("max memory required for scaling (GB) - ",
                           round(mem_required_scaling/1e+09, digits = 3)))

    # number of labels
    nlabels <- length(sits_labels(environment(ml_model)$data)$label)
    # estimated size of the data for classification
    input_class_data_size <- as.numeric(ninterval)*nbands_data_size
    output_class_data_size <- as.numeric(nlabels)*single_data_size
    class_data_size <- input_class_data_size + output_class_data_size

    # memory required for processing depends on the model
    if ("keras_model" %in% class(ml_model) || "rfor_model" %in% class(ml_model))
    {
        .sits_log_debug(paste0("keras and ranger run on multiple threads"))
        mem_required_processing <- (class_data_size +
                                    as.numeric(.sits_mem_used()))*proc_bloat
    }
    else {
        # test two different cases
        if (ninstances == ninterval) # one interval only
            mem_required_processing <- as.numeric(multicores) *
                (class_data_size + as.numeric(.sits_mem_used()))
        else
            mem_required_processing <- as.numeric(multicores) *
                (.sits_mem_used() + class_data_size + full_size)
    }
    .sits_log_debug(paste0("max memory required for processing (GB) - ",
                           round(mem_required_processing/1e+09, digits = 3)))

    # number of passes to read the full data sets
    nblocks <- max(ceiling(mem_required_scaling/(memsize*1e+09)),
                   ceiling(mem_required_processing/(memsize*1e+09)))

    return(nblocks)
}

#' @title Filter the time series values in the case of a matrix
#' @name .sits_raster_filter_data
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function filters a matrix.
#'
#' @param  values.mx      Matrix of values.
#' @param  filter         Filter function to apply to matrix.
#' @param  multicores     Number of cores.
#' @return Scaled integer matrix.
.sits_raster_filter_data <- function(values.mx, filter, multicores) {
    # scale the data set
    # auxiliary function to scale a block of data
    filter_matrix_block <- function(chunk) {
        filtered_block.mx <- filter(chunk)
    }
    # use multicores to speed up filtering
    if (multicores > 1) {
        chunk.lst <- .sits_raster_split_data(values.mx, multicores)
        rows.lst  <- parallel::mclapply(chunk.lst, filter_matrix_block,
                                        mc.cores = multicores)
        values.mx <- do.call(rbind, rows.lst)
        rm(chunk.lst)
        rm(rows.lst)
        gc()
    }
    else
        values.mx <- filter(values.mx)

    return(values.mx)
}


#' @title Preprocess a set of values retrived from a raster brick
#' @name  .sits_raster_preprocess_data
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  values.mx        Matrix of values retrieved from a brick.
#' @param  band             Band to be processed.
#' @param  missing_value    Missing value for the band.
#' @param  minimum_value    Minimum values for the band.
#' @param  scale_factor     Scale factor for each band (only for raster data).
#' @param  stats            Normalization parameters.
#' @param  filter           Smoothing filter to be applied.
#' @param  multicores       Number of cores to process the time series.
#' @return Matrix with pre-processed values.
.sits_raster_preprocess_data <- function(values.mx,
                                         band,
                                         missing_value,
                                         minimum_value,
                                         scale_factor,
                                         stats,
                                         filter,
                                         multicores) {
    # correct minimum value
    values.mx[is.na(values.mx)] <- minimum_value
    values.mx[values.mx <= minimum_value] <- minimum_value

    # scale the data set
    values.mx <- .sits_raster_scale_data(values.mx, scale_factor, multicores)

    # filter the data
    if (!(purrr::is_null(filter))) {
        values.mx <- .sits_raster_filter_data(values.mx, filter, multicores)
    }

    # normalize the data
    if (!purrr::is_null(stats)) {
        values.mx <- .sits_normalize_matrix(values.mx, stats, band, multicores)
    }

    return(values.mx)
}

#' @title Read a block of values retrived from a set of raster bricks
#' @name  .sits_raster_read_data
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  cube            Input data cube.
#' @param  samples         Tibble with samples.
#' @param  ml_model        Machine learning model.
#' @param  first_row       First row to start reading.
#' @param  n_rows_block    Number of rows in the block.
#' @param  stats           Normalization parameters.
#' @param  filter          Smoothing filter to be applied.
#' @param  multicores      Number of cores to process the time series.
#' @return A data.table with values for classification.
.sits_raster_read_data <- function(cube, samples, ml_model,
                                   first_row, n_rows_block,
                                   stats, filter, multicores) {
    # get the bands in the same order as the samples
    bands <- sits_bands(samples)
    n_bands <- length(bands)
    # get the missing values, minimum values and scale factors
    missing_values <- unlist(cube$missing_values)
    minimum_values <- unlist(cube$minimum_values)
    scale_factors  <- unlist(cube$scale_factors)

    # get the file info
    file_info <- cube$file_info[[1]]

    # index to go through the bands vector
    b <- 0

    # read the values from the raster bricks ordered by bands
    values.lst <- bands %>%
        purrr::map(function(band) {
            r_obj <- .sits_cube_robj_band(cube, band)
            # getValues function returns a matrix
            # the rows of the matrix are the pixels
            # the cols of the matrix are the layers
            values.mx    <- suppressWarnings(raster::getValues(r_obj,
                                                               first_row,
                                                               n_rows_block))
            rm(r_obj)

            # proprocess the input data
            b <<- b + 1
            band <- bands[b]
            values.mx <- .sits_raster_preprocess_data(values.mx, band,
                                                      missing_values[band],
                                                      minimum_values[band],
                                                      scale_factors[band],
                                                      stats, filter,
                                                      multicores)

            # save information about memory use for debugging later
            .sits_log_debug(paste0("Memory used after readGDAL - ",
                                   .sits_mem_used(), " GB"))
            .sits_log_debug(paste0("Read band ", b, " from rows ",
                                   first_row, "to ",
                                   (first_row + n_rows_block - 1)))

            return(values.mx)
        })
    # create a data.table joining the values
    data_DT <- data.table::as.data.table(do.call(cbind,values.lst))

    # memory cleanup
    rm(values.lst)
    gc()

    # create two additional columns for prediction
    size <- n_rows_block*cube[1,]$ncols
    two_cols_DT <- data.table::data.table("original_row" = rep(1,size),
                                          "reference"    = rep("NoClass", size))

    # join the two columns with the data values
    data_DT <- data.table::as.data.table(cbind(two_cols_DT, data_DT))

    # memory debug
    .sits_log_debug(paste0("Memory used after reading block - ",
                           .sits_mem_used(), " GB"))

    return(data_DT)
}

#' @title Scale the time series values in the case of a matrix
#' @name .sits_raster_scale_data
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Normalizes one band of the values read from a raster brick.
#'
#' @param  values.mx      Matrix of values.
#' @param  scale_factor   Scaling factor.
#' @param  multicores     Number of cores.
#' @return A scaled matrix.
.sits_raster_scale_data <- function(values.mx, scale_factor, multicores) {
    # scale the data set
    # auxiliary function to scale a block of data
    scale_block <- function(chunk, scale_factor) {
        scaled_block.mx <- scale_data(chunk, scale_factor)
    }
    # use multicores to speed up scaling
    if (multicores > 1) {
        chunk.lst <- .sits_raster_split_data(values.mx, multicores)
        rows.lst  <- parallel::mclapply(chunk.lst, scale_block,
                                        scale_factor, mc.cores = multicores)
        values.mx <- do.call(rbind, rows.lst)
        rm(chunk.lst)
        rm(rows.lst)
        gc()
    }
    else
        values.mx <- scale_data(values.mx, scale_factor)

    return(values.mx)
}

#' @title Scale the time series values in the case of a matrix
#' @name .sits_raster_scale_matrix_integer
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function transforms a numerical matrix into an integer one.
#'
#' @param  values.mx      Matrix of values.
#' @param  scale_factor   Scaling factor.
#' @param  multicores     Number of cores.
#' @return Scaled integer matrix.
.sits_raster_scale_matrix_integer <- function(values.mx,
                                              scale_factor,
                                              multicores) {
    # scale the data set
    # auxiliary function to scale a block of data
    scale_matrix_block <- function(chunk, scale_factor) {
        scaled_block.mx <- scale_matrix_integer(chunk, scale_factor)
    }
    # use multicores to speed up scaling
    if (multicores > 1) {
        chunk.lst <- .sits_raster_split_data(values.mx, multicores)
        rows.lst  <- parallel::mclapply(chunk.lst, scale_matrix_block,
                                        scale_factor, mc.cores = multicores)
        int_values.mx <- do.call(rbind, rows.lst)
        rm(chunk.lst)
        rm(rows.lst)
        gc()
    }
    else
        int_values.mx <- scale_matrix_integer(values.mx, scale_factor)

    return(int_values.mx)
}

#' @title Split a data.table or a matrix for multicore processing
#' @name .sits_raster_split_data
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function splits a data.table into a
#'              list of chunks for multicore processing.
#'
#' @param data             Data (data.table or matrix).
#' @param ncores           Number of cores for processing.
#' @return                 List of pairs of positions (first row, last row)
#'                         to be assigned to each core.
#'
.sits_raster_split_data <- function(data, ncores) {
    # number of rows in the data
    nrows <- nrow(data)
    # find the number of rows per core
    step <- ceiling(nrows/ncores)

    # create a vector with the initial rows per block
    blocks <- seq(from = 1, to = nrows, by = step)

    # fill the list with the initial and final row per block
    block.lst <- purrr::map2(blocks, 1:ncores, function(blk, i) {
        start <- blk
        end   <- start + step - 1
        if (i == ncores )
            end <- nrows
        return(data[start:end,])
    })
    return(block.lst)
}






