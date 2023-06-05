#' @title Get labels associated to a data set
#' @name sits_labels
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Finds labels in a sits tibble or data cube
#'
#' @param data      Time series or a cube.
#' @return          The labels associated to a set of time series or to
#'                  a data cube.
#'
#' @examples
#' # read a tibble with 400 samples of Cerrado and 346 samples of Pasture
#' data(cerrado_2classes)
#' # print the labels
#' sits_labels(cerrado_2classes)
#' @export
#'
sits_labels <- function(data) {

    # get the meta-type (sits or cube)
    data <- .conf_data_meta_type(data)
    UseMethod("sits_labels", data)
}

#' @rdname sits_labels
#' @export
#'
sits_labels.sits <- function(data) {

    # pre-condition
    return(sort(unique(data$label)))
}

#' @rdname sits_labels
#' @export
#'
sits_labels.raster_cube <- function(data) {
    return(data$labels[[1]])
}
#' @rdname sits_labels
#' @export
#'
sits_labels.patterns <- function(data) {
    return(data$label)
}
#' @rdname sits_labels
#'
#' @export
sits_labels.sits_model <- function(data) {
    .check_is_sits_model(data)
    # Get labels from ml_model
    labels <- .ml_labels(data)
    return(labels)
}
#' @title Change the labels of a set of time series
#'
#' @name `sits_labels<-`
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Given a sits tibble with a set of labels, renames the labels
#' to the specified in value.
#'
#' @param  data      Data cube or time series.
#' @param  value     A character vector used to convert labels. Labels will
#'                   be renamed to the respective value positioned at the
#'                   labels order returned by \code{\link{sits_labels}}.
#'
#' @return           A sits tibble with modified labels.
#' @examples
#' # show original samples ("Cerrado" and "Pasture")
#' sits_labels(cerrado_2classes)
#' # rename label samples to "Savanna" and "Grasslands"
#' sits_labels(cerrado_2classes) <-  c("Savanna", "Grasslands")
#' # see the change
#' sits_labels(cerrado_2classes)
#'
#' @export
#'
`sits_labels<-` <- function(data, value) {

    # set caller to show in errors
    .check_set_caller("sits_labels")
    # get the meta-type (sits or cube)
    data <- .conf_data_meta_type(data)
    UseMethod("sits_labels<-", data)
}

#' @name `sits_labels<-`
#' @export
#' @return           A sits tibble with modified labels.
#'
`sits_labels<-.sits` <- function(data, value) {

    # does the input data exist?
    .check_samples(data)

    labels <- sits_labels(data)

    # check if value and labels match
    .check_chr_parameter(value,
                         len_max = length(labels),
                         len_min = length(labels)
    )
    # check if there are no NA
    .check_that(
        x = all(!is.na(value)),
        msg = "invalid values to replace labels"
    )

    # check if there are empty strings
    .check_that(
        x = any(trimws(value) != ""),
        msg = "invalid values to replace labels"
    )

    names(value) <- labels
    data$label <- value[data$label]
    return(data)
}
#' @name `sits_labels<-`
#' @export
#' @return    A probs or class_cube cube with modified labels.
#'
`sits_labels<-.probs_cube` <- function(data, value) {
    # precondition
    .check_chr(
        x = value,
        allow_empty = FALSE,
        len_min = length(sits_labels(data)),
        len_max = length(sits_labels(data)),
        msg = "number of new labels dos not match current labels"
    )
    data[["labels"]] <- list(value)
    return(data)
}
#' @export
#'
`sits_labels<-.class_cube` <- function(data, value) {
    return(`sits_labels<-.probs_cube`(data, value))
}

#' @name `sits_labels<-`
#' @export
#' @return           A probs cube with modified labels.
#'
`sits_labels<-.class_cube` <- function(data, value) {
    # precondition
    n_labels <- length(sits_labels(data))
    .check_chr(value,
               len_min = n_labels,
               msg = "not enough new labels to replace current ones"
    )
    rows <- slider::slide_dfr(data, function(row) {
        row$labels <- list(value)
        return(row)
    })
    return(rows)
}

#' @title Inform label distribution of a set of time series
#' @name sits_labels_summary
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Describes labels in a sits tibble
#'
#' @param data      Valid sits tibble
#'
#' @return A tibble with the frequency of each label.
#'
#' @examples
#' # read a tibble with 400 samples of Cerrado and 346 samples of Pasture
#' data(cerrado_2classes)
#' # print the labels
#' sits_labels_summary(cerrado_2classes)
#' @export
#'
sits_labels_summary <- function(data) {
    UseMethod("sits_labels_summary", data)
}

#' @rdname sits_labels_summary
#' @export
#'
sits_labels_summary.sits <- function(data) {

    warning("This function is deprecated. Please use summary()")

    # get frequency table
    data_labels <- table(data$label)

    # compose tibble containing labels, count and relative frequency columns
    result <- tibble::as_tibble(list(
        label = names(data_labels),
        count = as.integer(data_labels),
        prop = as.numeric(prop.table(data_labels))
    ))
    return(result)
}
