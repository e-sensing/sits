#' @title Returns the information about labels of a data set (tibble or cube)
#'
#' @name sits_labels
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description  Finds labels in a sits tibble or data cube
#'
#' @param data      Valid sits tibble (time series or a cube)
#'
#' @return A string vector with the labels.
#'
#' @examples
#' # read a tibble with 400 samples of Cerrado and 346 samples of Pasture
#' data(cerrado_2classes)
#' # print the labels
#' sits_labels(cerrado_2classes)
#'
#' @export
#'
sits_labels <- function(data) {

    # get the meta-type (sits or cube)
    data <- .config_data_meta_type(data)

    UseMethod("sits_labels", data)
}

#' @export
#'
sits_labels.sits <- function(data) {

    return(sort(unique(data$label)))
}

#' @export
#'
sits_labels.cube <- function(data) {

    return(data$labels[[1]])
}

#' @export
#'
sits_labels.patterns <- function(data) {

    return(data$label)
}

#' @export
#'
sits_labels.sits_model <- function(data) {

    assertthat::assert_that(
        inherits(data, "function"),
        msg = "sits_labels: invalid sits model"
    )

    assertthat::assert_that(
        "data" %in% ls(environment(data)),
        msg = "sits_labels: no samples found in the sits model"
    )

    return(sits_labels.sits(environment(data)$data))

}
