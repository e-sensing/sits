#' @title Filter bands on a data set (tibble or cube)
#'
#' @name sits_select
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param data         A sits tibble or data cube
#' @param bands        Character vector with the names of the bands
#'
#' @description Filter only the selected bands from a tibble or a data cube.
#'
#' @examples
#' # Retrieve a set of time series with 2 classes
#' data(cerrado_2classes)
#' # Print the original bands
#' sits_bands(cerrado_2classes)
#' # Select only the NDVI band
#' data <- sits_select(cerrado_2classes, bands = c("NDVI"))
#' # Print the labels of the resulting tibble
#' sits_bands(data)
#'
#' @return
#' For sits tibble, returns a sits tibble with the selected bands.
#' For data cube, a data cube with the selected bands.
#'
#' @export
#'
sits_select <- function(data, bands) {
    # get the meta-type (sits or cube)
    data <- .sits_config_data_meta_type(data)

    UseMethod("sits_select", data)
}

#' @export
#'
sits_select.sits <- function(data, bands) {

    # backward compatibility
    data <- .sits_tibble_rename(data)

    # bands names in SITS are uppercase
    bands <- toupper(bands)
    sits_bands(data) <- toupper(sits_bands(data))

    assertthat::assert_that(
        all(bands %in% sits_bands(data)),
        msg = paste("sits_select: missing bands:",
                    paste(bands[!bands %in% sits_bands(data)],
                          collapse = ", "))
    )

    # prepare result sits tibble
    result <- data

    # select the chosen bands for the time series
    result$time_series <- data$time_series %>%
        purrr::map(function(ts) ts[, c("Index", bands)])

    # return the result
    return(result)
}

#' @export
#'
sits_select.cube <- function(data, bands) {

    assertthat::assert_that(
        bands %in% sits_bands(data),
        msg = "sits_select: requested bands are not available in the data cube"
    )

    # assign the bands
    data$bands[[1]] <- bands

    # filter the file info
    db_info <- data$file_info[[1]]
    db_info <- dplyr::filter(db_info, band %in% bands)
    data$file_info[[1]] <- db_info

    return(data)
}

#' @export
#'
sits_select.patterns <- function(data, bands) {

    return(sits_select.sits(data, bands))
}
