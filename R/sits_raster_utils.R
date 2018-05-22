#' @title Check clasification parameters
#' @name .sits_check_classify_params
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Verify that required parameters are correct
#'
#' @param  file            vector of file names to store the output (one file per classified year)
#' @param  coverage        tibble with information about a set of space-time raster bricks
#' @param  ml_model        an R model trained by \code{\link[sits]{sits_train}}
#' @return OK              (logical) tests succeeded?
#'
.sits_check_classify_params <- function(file, coverage, ml_model){

    # ensure metadata tibble exists
    ensurer::ensure_that(coverage, NROW(.) > 0,
                         err_desc = "sits_classify_raster: need a valid metadata for coverage")

    # ensure that file name is provided
    ensurer::ensure_that(file, !purrr::is_null(.),
                         err_desc = "sits-classify-raster: please provide name of output file")

    # ensure the machine learning model has been built
    ensurer::ensure_that(ml_model,  !purrr::is_null(.), err_desc = "sits-classify: please provide a machine learning model already trained")

    return(invisible(TRUE))
}
#' @name .sits_check_results
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description verify that classification results have the right size
#'
#' @param  pred_class.vec  vector of predicted categorical values
#' @param  pred_probs.mx   matrix of predicted probabilities for each class
#' @param  DT              distance matrix
#' @return check           TRUE if check is OK
.sits_check_results <- function(pred_class.vec, pred_probs.mx, DT) {
    # check the result has the right dimension
    ensurer::ensure_that(pred_class.vec, length(.) == nrow(DT),
                         err_desc = "sits_classify_raster - number of classified pixels is different
                         from number of input pixels")

    ensurer::ensure_that(pred_probs.mx, nrow(.) == nrow(DT),
                         err_desc = "sits_classify_raster - number of rows of probability matrix is different
                         from number of input pixels")
    return(invisible(TRUE))
}
#' @title Estimate the processing time
#' @name .sits_estimate_processing_time
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description this function normalizes one band of the values read from a raster brick
#'
#' @param  start_time     initial processing time
#' @param  select.lst     list of time intervals
#' @param  bs             raster block parameters
#' @param  block          current block
#' @param  time           current interval
#' @return values.mx      scaled matrix

.sits_estimate_processing_time <- function(start_time, select.lst, bs, block, time) {
    # compute current time
    current_time <- lubridate::now()

    # compute elapsed time and estimates remaining time
    if (((block - 1) * length(select.lst) + time) < (bs$n * length(select.lst))) {
        message(sprintf("Elapsed time %s minute(s). Estimated total process time %s minute(s)...",
                        round(as.numeric((lubridate::time_length(current_time - start_time, unit = "minute"))), 1),
                        round(as.numeric((lubridate::time_length(current_time - start_time, unit = "minute")) / ((block - 1) *
                                                                                                                     length(select.lst) + time)) * (bs$n * length(select.lst)), 1)))
    } else {
        message(sprintf("Classification finished at %s. Total elapsed time: %s minute(s).",
                        current_time,
                        round(as.numeric((lubridate::time_length(current_time - start_time, unit = "minute"))), 1)))
    }
    return(invisible(TRUE))
}
