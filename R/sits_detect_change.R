#' @title Detect changes in time series
#' @name sits_detect_change
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description Given a set of time series or an image, this function compares
#' each time series with a set of change/no-change patterns, and indicates
#' places and dates where change has been detected.
#'
#' @param data         Set of time series.
#' @param cd_method    Change detection method (with parameters).
#' @param ...          Other relevant parameters.
#' @param filter_fn    Smoothing filter function to be applied to the data.
#' @param multicores   Number of threads to process the time series.
#' @param progress     Show progress bar?
#' @return             Set of time series where significant change has been
#'                     detected.
#' @export
sits_detect_change <- function(data,
                               cd_method,
                               ...,
                               filter_fn = NULL,
                               multicores = 2L,
                               progress = TRUE) {
    UseMethod("sits_detect_change", data)
}

#' @rdname sits_detect_change
#' @export
sits_detect_change.sits <- function(data,
                                    cd_method,
                                    ...,
                                    filter_fn = NULL,
                                    multicores = 2L,
                                    progress = TRUE) {
    # set caller for error messages
    .check_set_caller("sits_detect_change_sits")
    # Pre-conditions
    data <- .check_samples_ts(data)
    .check_is_sits_model(cd_method)
    .check_int_parameter(multicores, min = 1, max = 2048)
    .check_progress(progress)
    # Do detection
    detections <- .detect_change_ts(
        samples = data,
        cd_method = cd_method,
        filter_fn = filter_fn,
        multicores = multicores,
        progress = progress
    )
    return(detections)
}

#' @rdname sits_detect_change
#' @export
sits_detect_change.default <- function(data, cd_method, ...) {
    stop("Input should be a sits tibble or a data cube")
}
