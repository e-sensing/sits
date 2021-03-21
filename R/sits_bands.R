#' @title Informs and replaces the names of the bands
#' @name sits_bands
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description  Finds/replaces the names of the bands of
#'               a set of time series or of a data cube
#'
#' @param x         Valid sits tibble (time series or a cube)
#' @param value     Vector of bands
#' @return A string vector with the names of the bands.
#'
#' @examples {
#' # Retrieve the set of samples for Mato Grosso (provided by EMBRAPA)
#' # show the bands
#' sits_bands(samples_mt_6bands)
#' # Replace the name of bands for the samples for Mato Grosso
#' sits_bands(samples_mt_4bands) <- c("ndvi", "evi", "nir", "mir")
#' }
#' @export
sits_bands <- function(x) {
    # get the meta-type (sits or cube)
    x <- .sits_config_data_meta_type(x)

    UseMethod("sits_bands", x)
}
#' @rdname sits_bands
#' @export
sits_bands.sits <- function(x) {
    bands <- sits_time_series(x) %>%
        colnames() %>%
        .[2:length(.)]
    return(bands)
}
#' @rdname sits_bands
#' @export
sits_bands.cube <- function(x) {
    return(x[1, ]$bands[[1]])
}
#' @rdname sits_bands
#' @export
sits_bands.patterns <- function(x) {
    bands <- sits_bands.sits(x)
    return(bands)
}
#' @rdname sits_bands
#'
#' @export
`sits_bands<-` = function(x, value) {
    # get the meta-type (sits or cube)
    x <- .sits_config_data_meta_type(x)

    UseMethod("sits_bands<-", x)
}
#' @rdname sits_bands
#' @export
`sits_bands<-.sits` =  function(x, value) {
    # backward compatibility
    x <- .sits_tibble_rename(x)

    ts <- sits_time_series(x)
    assertthat::assert_that(ncol(ts) == length(value) + 1,
                            msg = "Invalid number of bands to be replaced")

    rows <- slider::slide(x, function(row){
        ts <- sits_time_series(row)
        names(ts) <- c("Index", value)
        row$time_series[[1]] <- ts
        return(row)
    })
    x <- dplyr::bind_rows(rows)

    return(x)
}
#' @rdname sits_bands
#' @export
`sits_bands<-.cube` = function(x, value) {
    rows <- slider::slide(x, function(row){
        old_bands <- row$bands[[1]]
        assertthat::assert_that(length(old_bands) == length(value),
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
#' @rdname sits_bands
#' @export
`sits_bands<-.patterns` =  function(x, value) {
    return(`sits_bands<-.sits`(x, value))
}
#' @rdname sits_bands
#' @export
`sits_bands<-.predicted` =  function(x, value) {
    return(`sits_bands<-.sits`(x, value))
}
