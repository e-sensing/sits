#' @title Normalize the time series in the given sits_tibble
#' @name sits_normalize_data
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description this function normalizes the time series using the mean and
#' standard deviation of all the time series.
#'
#' @param data.tb     a tibble in SITS format
#' @param stats.tb    statistics for normalization
#' @param multicores  number of cores to process
#' @return data.tb    a normalized sits tibble
#' @export
sits_normalize_data <- function(data.tb, stats.tb, multicores = 1){
    .sits_test_tibble(data.tb)
    # find the number of cores
    if (purrr::is_null(multicores))
        multicores <- max(parallel::detectCores(logical = FALSE) - 1, 1)
    # avoid overhead on multicore processing
    if (nrow(data.tb) < sits.env$config$minimum_number_samples)
        multicores <- 1
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
#' @title Normalize the time series values in the case of a matrix
#' @name .sits_normalize_matrix
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description this function normalizes one band of the values read from a raster brick
#'
#' @param  data.mx        matrix of values
#' @param  stats.tb       statistics for normalization
#' @param  band           band to be normalized
#' @param  multicores     number of cores
#' @return data.mx        a normalized matrix
#'
.sits_normalize_matrix <- function(data.mx, stats.tb, band, multicores) {
    # select the 2% and 98% quantiles
    quant_2   <- as.numeric(stats.tb[2, band])
    quant_98  <- as.numeric(stats.tb[3, band])

    # auxiliary function to normalize a block of data
    normalize_block <- function(chunk, quant_2, quant_98) {
        # normalize a block of data
        values_block.mx <- normalize_data(chunk, quant_2, quant_98)
    }

    # parallel processing for normalization
    if (multicores > 1) {
        chunk.lst <- .sits_split_data(data.mx, multicores)
        rows.lst  <- parallel::mclapply(chunk.lst, normalize_block, quant_2, quant_98, mc.cores = multicores)
        data.mx <- do.call(rbind, rows.lst)
        rm(chunk.lst)
        rm(rows.lst)
        gc()
    }
    else
        data.mx <- normalize_data(data.mx, quant_2, quant_98)

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
#' @return result.tb  a tibble with statistics
.sits_normalization_param <- function(data.tb) {

    .sits_test_tibble(data.tb)

    DT <- data.table::data.table(dplyr::bind_rows(data.tb$time_series))
    DT[, Index := NULL]

    # compute statistics
    DT_med      <- DT[, lapply(.SD, stats::median, na.rm = TRUE)]
    DT_quant_2  <- DT[, lapply(.SD, function(x) stats::quantile(x, 0.02, na.rm = TRUE))]
    DT_quant_98 <- DT[, lapply(.SD, function(x) stats::quantile(x, 0.98, na.rm = TRUE))]

    stats.tb <- dplyr::bind_cols(stats = c("med", "quant_2", "quant_98"),
                                 dplyr::bind_rows(DT_med, DT_quant_2, DT_quant_98))

    return(stats.tb)
}


