#' @title Replaces the names of the bands
#' @name `sits_bands<-`
#'
#' @param x         Valid sits tibble (time series or a cube)
#' @param value     Vector of bands
#' @examples
#' # Replace the name of bands for the samples for Mato Grosso
#' sits_bands(samples_modis_4bands) <- c("ndvi", "evi", "nir", "mir")
#' @export
#'
`sits_bands<-` <- function(x, value) {
    # get the meta-type (sits or cube)
    x <- .config_data_meta_type(x)

    UseMethod("sits_bands<-", x)
}
#' @export
#'
`sits_bands<-.sits` <- function(x, value) {

    # set caller to show in errors
    .check_set_caller("sits_bands")

    # get the data bands
    data_bands <- sits_bands(x)

    .check_chr(value, allow_empty = FALSE, len_min = length(data_bands),
               len_max = length(data_bands),
               msg = "Invalid bands values to be replaced")

    # create an row_id to use later in nest
    x[["..row_id"]] <- seq_len(nrow(x))

    # unnest bands
    x <- tidyr::unnest(x, cols = "time_series")

    # here, you could pass a function to process fast
    new_bands <- colnames(x)
    names(new_bands) <- new_bands

    new_bands[data_bands] <- toupper(value)
    colnames(x) <- unname(new_bands)

    # nest again
    x <- tidyr::nest(x,  time_series = c("Index", toupper(value)))

    # remove ..row_id
    x <- dplyr::select(x, -"..row_id")

    # set sits tibble class
    class(x) <- c("sits", class(x))

    return(x)
}

#' @export
#'
`sits_bands<-.cube` <- function(x, value) {

    # set caller to show in errors
    .check_set_caller("sits_bands")

    rows <- slider::slide(x, function(row) {
        old_bands <- row$bands[[1]]
        .check_that(
            x = length(old_bands) == length(value),
            msg = "replacement bands have wrong length")
        # rename bands
        names(value) <- row$bands[[1]]
        row$bands[[1]] <- unname(value)
        row$file_info[[1]]$bands <- unname(value[row$file_info[[1]]$band])
        return(row)
    })
    x <- dplyr::bind_rows(rows)
    return(x)
}
#' @export
#'
`sits_bands<-.patterns` <- function(x, value) {
    return(`sits_bands<-.sits`(x, value))
}
#' @export
#'
`sits_bands<-.predicted` <- function(x, value) {
    return(`sits_bands<-.sits`(x, value))
}
