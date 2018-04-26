#' @title Normalize the time series and produce a data.table with distances
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a SITS tibble with time series as input and produces a normalized data.table with
#' distances to be used to train a machine learning model. The statistics used to derive the
#' normalization is also returned for later use
#'
#' @param data.tb       A SITS tibble with the time series
#' @param normalize     (integer) 0 = no normalization, 1 = normalize per band, 2 = normalize per dimension
#' @param multicores    number of cores for parallel processing
#' @return (train_data_DT, stats.tb)   A list with the distances for traning and the statistics for normalization

.sits_distances_normalized <- function(data.tb, normalize, multicores) {
    if (normalize == 1) {
        stats.tb            <- .sits_normalization_param(data.tb, 1)
        train_data_DT       <- sits_distances(.sits_normalize(data.tb, stats.tb, multicores = multicores))
    }
    else if (normalize == 2) {
        stats.tb            <- .sits_normalization_param(data.tb, 2)
        train_data_DT       <- .sits_normalize(sits_distances(data.tb), stats.tb)
    }
    else {
        stats.tb <- NULL
        train_data_DT <- sits_distances(data.tb)
    }
    return(list("train_data" = train_data_DT, "stats" = stats.tb))
}


#' @title Normalize the time series data
#' @name .sits_normalize
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description this function normalizes the time series using the mean and
#' standard deviation of all the time series.
#'
#' @param data        a tibble in SITS format, a data.table with distances or a matrix of values
#' @param stats.tb    statistics for normalization
#' @param band        band to be normalized
#' @param  multicores number of threads to process the time series.
#' @return data.tb    a normalized data set
.sits_normalize <- function(data, stats.tb, band = NULL, multicores = 2) {
    if ("tbl" %in% class(data)) # data is a tibble
        data <- .sits_normalize_data(data, stats.tb, multicores = multicores)
    else if ("data.table" %in% class(data)) # data is a data.table
        data <- .sits_normalize_DT(data, stats.tb)
    else if ("matrix" %in% class(data)) # data is a matrix
        data <- .sits_normalize_matrix(data, stats.tb, band, multicores)

    return(data)
}

#' @title Normalize the time series in the given sits_tibble
#' @name .sits_normalize_data
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description this function normalizes the time series using the mean and
#' standard deviation of all the time series.
#'
#' @param data.tb     a tibble in SITS format
#' @param stats.tb    statistics for normalization
#' @param  multicores number of threads to process the time series.
#' @return data.tb    a normalized sits tibble
#'
.sits_normalize_data <- function(data.tb, stats.tb, multicores = 2){
    .sits_test_tibble(data.tb)

    # get the bands of the input data
    bands <- sits_bands(data.tb)
    # check that the bands in the input are include in the statistics already calculated
    ensurer::ensure_that(bands, all((.) %in% colnames(stats.tb[,-1])),
                         err_desc = "sits_normalize: bands in the data do not match bands in the model")

    # extract the values of the time series to a list of tibbles
    values.lst <- data.tb$time_series

    normalize_list <- function(chunk.lst) {
        norm_chunk.lst <- chunk.lst %>%
            purrr::map(function(ts) {
                norm.lst <- bands %>%
                    purrr::map(function(b){
                        med      <- as.numeric(stats.tb[1, b])
                        quant_2  <- as.numeric(stats.tb[2, b])
                        quant_98 <- as.numeric(stats.tb[3, b])
                        values <- tibble::as.tibble(normalize_data(as.matrix(ts[,b]), quant_2, quant_98))
                        return(values)
                    })
                ts.tb <- dplyr::bind_cols(norm.lst)
                ts.tb <- dplyr::bind_cols(list(ts[,1], ts.tb))
                colnames(ts.tb) <- colnames(ts)
                return(ts.tb)
            })
        return(norm_chunk.lst)
    }

    if (multicores > 1) {
        parts.lst <- split(values.lst, cut(1:length(values.lst), 2, labels = FALSE))
        norm.lst <- dplyr::combine(parallel::mclapply(parts.lst, normalize_list, mc.cores = multicores))
    }
    else
        norm.lst <- normalize_list(values.lst)

    data.tb$time_series <- norm.lst
    return(data.tb)
}
#' @title Normalize the time series in the given distance table
#' @name .sits_normalize_DT
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description this function normalizes all dimensions of the distance table
#'
#' @param  DT             data.table with distances
#' @param  stats.tb       statistics for normalization
#' @return DT             a normalized data.table
#'
.sits_normalize_DT <- function(DT, stats.tb){

    for (col in colnames(3:ncol(DT))) {
        quant_2  <- as.numeric(stats.tb[2, col])
        quant_98 <- as.numeric(stats.tb[3, col])
        DT[,c(col) := normalize_data(as.matrix(DT[,get(col)]), quant_2, quant_98)]
    }
    return(DT)
}
#' @title Normalize the time series values in the case of a matrix
#' @name .sits_normalize_matrix
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description this function normalizes one band of the values read from a raster brick
#'
#' @param  data.mx        matrix of values
#' @param  stats.tb       statistics for normalization
#' @param  band           band to be normalized
#' @param  multicores     number of threads to process the time series.
#' @return data.mx        a normalized matrix
#'
.sits_normalize_matrix <- function(data.mx, stats.tb, band, multicores) {
    # select the 2% and 98% quantiles
    quant_2   <- as.numeric(stats.tb[2, band])
    quant_98  <- as.numeric(stats.tb[3, band])

    # estimate the list for breaking a block
    chunk_size.lst <- .sits_split_block_size(1, nrow(data.mx), multicores)

    # auxiliary function to normalize a block of data
    normalize_block <- function(cs) {
        # normalize a block of data
        values_block.mx <- normalize_data(data.mx[cs[1]:cs[2],], quant_2, quant_98)
    }
    # parallel processing for normalization
    if (multicores > 1) {
        # apply parallel processing to the split data and join the result
        rows.lst  <- parallel::mclapply(chunk_size.lst, normalize_block, mc.cores = multicores)
        data <- do.call(rbind, rows.lst)
    }
    else
        data.mx <- normalize_data(data.mx, quant_2, quant_98)

    .sits_log_debug(paste0("Data has been normalized between ", quant_2 , " (2%) and ", quant_98, "(98%)"))

    return(data.mx)
}


#' @title Normalize the time series in the given sits_tibble
#' @name .sits_normalization_param
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description this function normalizes the time series using the mean and
#' standard deviation of all the time series.
#'
#' @param data.tb     a tibble in SITS format
#' @param normalize   (integer) 0 = no normalization, 1 = normalize per band, 2 = normalize per dimension
#' @return result.tb  a tibble with statistics
.sits_normalization_param <- function(data.tb, normalize = 1) {

    .sits_test_tibble(data.tb)

    if (normalize == 1) {
        DT <- data.table::data.table(dplyr::bind_rows(data.tb$time_series))
        DT[, Index := NULL]
    }
    else if (normalize == 2) {
        DT <- sits_distances(data.tb)
        DT[, c("original_row", "reference") := NULL]
    }
    else
        return(NULL)

    # compute statistics
    DT_med      <- DT[, lapply(.SD, stats::median, na.rm = TRUE)]
    DT_quant_2  <- DT[, lapply(.SD, function(x) stats::quantile(x, 0.02, na.rm = TRUE))]
    DT_quant_98 <- DT[, lapply(.SD, function(x) stats::quantile(x, 0.98, na.rm = TRUE))]

    stats.tb <- dplyr::bind_cols(stats = c("med", "quant_2", "quant_98"),
                                 dplyr::bind_rows(DT_med, DT_quant_2, DT_quant_98))

    return(stats.tb)
}
