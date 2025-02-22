#' @title Create detect change method.
#' @name sits_detect_change_method
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description Prepare detection change method. Currently, sits supports the
#' following methods: 'dtw' (see \code{\link[sits]{sits_dtw}})
#'
#' @param  samples          Time series with the training samples.
#' @param  dc_method        Detection change method.
#' @return                  Change detection method prepared
#'                          to be passed to
#'                          \code{\link[sits]{sits_detect_change}}
#' @noRd
sits_detect_change_method <- function(samples = NULL, dc_method = sits_dtw()) {
    # set caller to show in errors
    .check_set_caller("sits_detect_change_method")
    # is the train method a function?
    .check_that(inherits(dc_method, "function"),
                msg = .conf("messages", "sits_detect_change_method_model")
    )
    if (.has(samples))
        # check if samples are valid
        .check_samples_train(samples)
    # compute the training method by the given data
    result <- dc_method(samples)
    # return a valid detect change method
    return(result)
}
