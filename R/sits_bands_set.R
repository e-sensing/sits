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
    # get the time series
    ts <- sits_time_series(x)
    .check_that(
        x = ncol(ts) == length(value) + 1,
        msg = "sits_bands: invalid number of bands to be replaced")

    # change band names for all rows of the tibble
    rows <- slider::slide(x, function(row) {
        ts <- sits_time_series(row)
        names(ts) <- c("Index", value)
        row$time_series[[1]] <- ts
        return(row)
    })
    x <- dplyr::bind_rows(rows)

    return(x)
}
#' @export
#'
`sits_bands<-.cube` <- function(x, value) {
    rows <- slider::slide(x, function(row) {
        old_bands <- row$bands[[1]]
        .check_that(
            x = length(old_bands) == length(value),
            msg = "sits_bands: replacement bands have wrong length")
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
