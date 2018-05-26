#' @title Preprocess a set of values retrived from a raster brick
#' @name  .sits_preprocess_data
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  values.mx        matrix of values retrieved from a brick
#' @param  band             band to be processed
#' @param  missing_value    missing value for the band
#' @param  minimum_value    minimum values for the band
#' @param  scale_factor     scale factor for each band (only for raster data)
#' @param  stats            normalization parameters
#' @param  filter           smoothing filter to be applied
#' @param  multicores       number of cores to process the time series
#' @return values.mx        matrix with pre-processed values
.sits_preprocess_data <- function(values.mx, band, missing_value, minimum_value, scale_factor,
                                  stats, filter, multicores){

    # correct minimum value
    values.mx[is.na(values.mx)] <- minimum_value
    values.mx[values.mx <= minimum_value] <- minimum_value

    # scale the data set
    values.mx <- .sits_scale_data(values.mx, scale_factor, multicores)

    # normalize the data
    if (!purrr::is_null(stats)) {
        values.mx <- .sits_normalize_matrix(values.mx, stats, band, multicores)
    }

    if (!(purrr::is_null(filter))) {
        values.mx <- filter(values.mx)
    }
    return(values.mx)
}
#' @title Read a block of values retrived from a set of raster bricks
#' @name  .sits_read_data
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  coverage        input raster coverage
#' @param  samples         tibble with samples
#' @param  ml_model        machine learning model
#' @param  first_row       first row to start reading
#' @param  n_rows_block    number of rows in the block
#' @param  stats           normalization parameters
#' @param  filter          smoothing filter to be applied
#' @param  multicores      number of cores to process the time series
#' @return dist_DT         data.table with values for classification
#'
.sits_read_data <- function(coverage, samples, ml_model, first_row, n_rows_block, stats, filter, multicores) {

    # get the bands in the same order as the samples
    bands <- sits_bands(samples)

    # get the missing values, minimum values and scale factors
    missing_values <- unlist(coverage$missing_values)
    minimum_values <- unlist(coverage$minimum_values)
    scale_factors  <- unlist(coverage$scale_factors)

    # get the raster bricks to be read
    bricks.lst <- coverage$files[[1]]

    ordered_bricks.lst <- vector(mode = "list", length = length(bands))

    for (i in 1:length(bands))
        ordered_bricks.lst[[i]] <- bricks.lst[[bands[i]]]

    names(ordered_bricks.lst) <- bands

    # index to go through the bands vector
    b <- 0

    # set the offset and region to be read by GDAL
    offset     <- c(first_row - 1, 0)
    region.dim <- c(n_rows_block, coverage[1,]$ncols)

    # read the values from the raster bricks ordered by bands
    values.lst <- ordered_bricks.lst %>%
        purrr::map(function(r_brick) {
            # the readGDAL function returns a matrix
            # the rows of the matrix are the pixels
            # the cols of the matrix are the layers
            values.mx    <- as.matrix(suppressWarnings(rgdal::readGDAL(r_brick, offset, region.dim, silent = TRUE))@data)

            # proprocess the input data
            b <<- b + 1
            band <- bands[b]
            values.mx <- .sits_preprocess_data(values.mx, band, missing_values[band], minimum_values[band], scale_factors[band],
                                               stats, filter, multicores)

            # save information about memory use for debugging later
            .sits_log_debug(paste0("Memory used after readGDAL - ", .sits_mem_used(), " GB"))
            .sits_log_debug(paste0("Read band ", b, " from rows ", first_row, "to ", (first_row + n_rows_block - 1)))

            return(values.mx)
        })
    # create a data.table joining the values
    data_DT <- data.table::as.data.table(do.call(cbind,values.lst))

    # memory cleanup
    rm(values.lst)
    gc()

    # create two additional columns for prediction
    size <- n_rows_block*coverage[1,]$ncols
    two_cols_DT <- data.table::data.table("original_row" = rep(1,size),
                                          "reference"    = rep("NoClass", size))

    # join the two columns with the data values
    data_DT <- data.table::as.data.table(cbind(two_cols_DT, data_DT))

    # memory debug
    .sits_log_debug(paste0("Memory used after reading block - ", .sits_mem_used(), " GB"))

    return(data_DT)
}

#' @title Scale the time series values in the case of a matrix
#' @name .sits_scale_data
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description this function normalizes one band of the values read from a raster brick
#'
#' @param  values.mx      matrix of values
#' @param  scale_factor   scaling factor
#' @param  multicores     number of cores
#' @return values.mx      scaled matrix
#'
.sits_scale_data <- function(values.mx, scale_factor, multicores) {

    # scale the data set
    # auxiliary function to scale a block of data
    scale_block <- function(chunk, scale_factor) {
        scaled_block.mx <- scale_data(chunk, scale_factor)
    }
    # use multicores to speed up scaling
    if (multicores > 1) {
        chunk.lst <- .sits_split_data(values.mx, multicores)
        rows.lst  <- parallel::mclapply(chunk.lst, scale_block, scale_factor, mc.cores = multicores)
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
#' @name .sits_scale_matrix_integer
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description this function transforms a numerical matrix into an integer one
#'
#' @param  values.mx      matrix of values
#' @param  scale_factor   scaling factor
#' @param  multicores     number of cores
#' @return values.mx      scaled integer matrix
#'
.sits_scale_matrix_integer <- function(values.mx, scale_factor, multicores) {

    # scale the data set
    # auxiliary function to scale a block of data
    scale_matrix_block <- function(chunk, scale_factor) {
        scaled_block.mx <- scale_matrix_integer(chunk, scale_factor)
    }
    # use multicores to speed up scaling
    if (multicores > 1) {
        chunk.lst <- .sits_split_data(values.mx, multicores)
        rows.lst  <- parallel::mclapply(chunk.lst, scale_matrix_block, scale_factor, mc.cores = multicores)
        values.mx <- do.call(rbind, rows.lst)
        rm(chunk.lst)
        rm(rows.lst)
        gc()
    }
    else
        values.mx <- scale_data(values.mx, scale_factor)

    return(values.mx)
}

#' @name .sits_write_raster_values
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description write the raster values to the outout files
#'
#' @param  output.lst         list with value layers and probability bricks
#' @param  prediction        prototype with values and predicted probabilities for each class (INT2U)
#' @param  labels            class labels
#' @param  int_labels        integer values corresponding to labels
#' @param  time              interval to be written to file
#' @param  first_row         initial row of the output layer to write block
#' @param  multicores        number of cores to process the time series
#' @return output.lst       updated list with value layers and probability bricks
.sits_write_raster_values <- function(output.lst,
                                      prediction,
                                      labels,
                                      int_labels,
                                      time,
                                      first_row,
                                      multicores) {


    # for each layer, write the predicted values
    layers.lst <- output.lst$layers
    layers.lst[[time]] <- raster::writeValues(layers.lst[[time]], as.integer(int_labels[prediction$values]), first_row)

    # convert probabilities matrix to INT2U
    scale_factor_save <- as.numeric(10000)
    prediction$probs     <- .sits_scale_matrix_integer(prediction$probs, scale_factor_save, multicores)

    # write the probabilities
    bricks.lst <- output.lst$bricks
    bricks.lst[[time]] <- raster::writeValues(bricks.lst[[time]], as.matrix(prediction$probs), first_row)

    # update the joint list of layers (values) and bricks (probs)
    output.lst <- list(layers = layers.lst, bricks = bricks.lst)

    return(output.lst)
}
