#' @title Preprocess a set of values retrived from a raster brick
#' @name  .sits_preprocess_data
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
#' @param  coverage        Input raster coverage.
#' @param  samples         Tibble with samples.
#' @param  ml_model        Machine learning model.
#' @param  first_row       First row to start reading.
#' @param  n_rows_block    Number of rows in the block.
#' @param  stats           Normalization parameters.
#' @param  filter          Smoothing filter to be applied.
#' @param  multicores      Number of cores to process the time series.
#' @return A data.table with values for classification.
.sits_read_data <- function(coverage, samples, ml_model, first_row, n_rows_block, stats, filter, multicores) {
    # get the bands in the same order as the samples
    bands <- sits_bands(samples)

    # get the missing values, minimum values and scale factors
    missing_values <- unlist(coverage$missing_values)
    minimum_values <- unlist(coverage$minimum_values)
    scale_factors  <- unlist(coverage$scale_factors)

    # get the raster bricks to be read
    ordered_bricks.lst <- coverage$r_objs[[1]][bands]

    # index to go through the bands vector
    b <- 0

    # read the values from the raster bricks ordered by bands
    values.lst <- ordered_bricks.lst %>%
        purrr::map(function(r_brick) {
            # the readGDAL function returns a matrix
            # the rows of the matrix are the pixels
            # the cols of the matrix are the layers
            values.mx    <- as.matrix(suppressWarnings(raster::getValues(r_brick, first_row, n_rows_block)))

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
#' @description This function normalizes one band of the values read from a raster brick.
#'
#' @param  values.mx      Matrix of values.
#' @param  scale_factor   Scaling factor.
#' @param  multicores     Number of cores.
#' @return A scaled matrix.
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
#' @description This function transforms a numerical matrix into an integer one.
#'
#' @param  values.mx      Matrix of values.
#' @param  scale_factor   Scaling factor.
#' @param  multicores     Number of cores.
#' @return Scaled integer matrix.
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
        int_values.mx <- do.call(rbind, rows.lst)
        rm(chunk.lst)
        rm(rows.lst)
        gc()
    }
    else
        int_values.mx <- scale_matrix_integer(values.mx, scale_factor)

    return(int_values.mx)
}

#' @title Write the values and probs into raster files
#' @name .sits_write_raster_values
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Write the raster values to the outout files.
#'
#' @param  output.lst        List with value layers and probability bricks.
#' @param  prediction_DT     A data.table with predicted probabilities for each class.
#' @param  labels            Class labels.
#' @param  int_labels        Integer values corresponding to labels.
#' @param  time              Interval to be written to file.
#' @param  first_row         Initial row of the output layer to write block.
#' @param  multicores        Number of cores to process the time series.
#' @return Updated list with value layers and probability bricks.
.sits_write_raster_values <- function(output.lst,
                                      prediction_DT,
                                      labels,
                                      int_labels,
                                      time,
                                      first_row,
                                      multicores) {
    # for each layer, write the predicted values
    # extract the values
    values <-  int_labels[max.col(prediction_DT)]

    layers.lst <- output.lst$layers
    layers.lst[[time]] <- raster::writeValues(layers.lst[[time]], values, first_row)

    # convert probabilities matrix to INT2U
    scale_factor_save <- 10000
    probs  <- .sits_scale_matrix_integer(as.matrix(prediction_DT), scale_factor_save, multicores)

    # write the probabilities
    bricks.lst <- output.lst$bricks
    bricks.lst[[time]] <- raster::writeValues(bricks.lst[[time]], probs, first_row)

    # update the joint list of layers (values) and bricks (probs)
    output.lst <- list(layers = layers.lst, bricks = bricks.lst)

    return(output.lst)
}
