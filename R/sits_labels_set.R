#' @title Change labels of a sits tibble
#'
#' @name `sits_labels<-`
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Given a sits tibble with a set of labels, renames the labels
#' to the specified in value.
#'
#' @param  data      A sits tibble.
#' @param  value     A character vector used to convert labels. Labels will
#'                   be renamed to the respective value positioned at the
#'                   labels order returned by \code{\link{sits_labels}}.
#'
#' @return           A sits tibble with modified labels.
#'
#' @examples
#' # Read a set of time series with information on deforestation
#' data("samples_modis_4bands")
#' # Print the labels
#' sits_labels(samples_modis_4bands)
#' # Create a conversion list.
#' # relabel the data
#' sits_labels(samples_modis_4bands) <- c("Natural", "Natural",
#'                                        "Anthropic", "Anthropic")
#' # show the new labels
#' sits_labels(samples_modis_4bands)
#'
#' @export
#'
`sits_labels<-` <- function(data, value) {

    # get the meta-type (sits or cube)
    data <- .config_data_meta_type(data)

    UseMethod("sits_labels<-", data)
}

#' @export
#'
`sits_labels<-.sits` <- function(data, value) {

    # does the input data exist?
    .sits_tibble_test(data)

    labels <- sits_labels(data)

    # check if value is an atomic vector
    .check_chr_type(x = value,
                    msg = "sits_labels: value must be a character vetor")

    # check if length is correct
    .check_length(
        x = labels,
        len_max = length(value),
        len_min = length(value),
        msg = "sits_labels: informed labels have a different expected length"
    )

    # check if there are no NA
    .check_that(
        x = all(!is.na(value)),
        msg = "sits_labels: invalid labels value"
    )

    # check if there are empty strings
    .check_that(
        x = any(trimws(value) != ""),
        msg = "sits_labels: invalid labels value"
    )

    names(value) <- labels

    data$label <- value[data$label]

    return(data)
}

#' @export
#'
`sits_labels<-.pattern` <- function(data, value) {

    return(`sits_labels<-.pattern`(data, value))
}

