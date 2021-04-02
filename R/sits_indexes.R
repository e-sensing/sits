#' @title Builds soil-adjusted vegetation index
#' @name sits_savi
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description  Adds new tasseled cap bands.
#' @param data       Valid sits tibble.
#' @return A sits tibble with the SAVI index.
#' @examples
#' # Retrieve data for time series with label samples in Mato Grosso in Brazil
#' data(samples_mt_6bands)
#' # Generate a new image with the tasseled cap
#' savi.tb <- sits_savi(samples_mt_6bands)
#' @export
sits_savi <- function(data) {

    bands <- sits_bands(data)
    bands_savi <- c("NIR", "RED")
    assertthat::assert_that(
        all(bands_savi %in% (bands)),
        msg = "sits_savi: not enough bands to compute"
    )

    data <- sits_mutate_bands(data,
                              SAVI = (1.5) * (NIR - RED) / (NIR + RED + 0.5)
    )

    return(data)
}

#' @title Builds normalized difference water index
#' @name sits_ndwi
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description  Adds new tasseled cap bands.
#' @param data       A valid sits tibble.
#' @return A sits tibble with the SAVI index.
#' @examples
#'
#' # Retrieve data for time series with label samples in Mato Grosso in Brazil
#' data(samples_mt_6bands)
#' # Generate a new image with the tasseled cap
#' ndwi.tb <- sits_ndwi(samples_mt_6bands)
#' @export
sits_ndwi <- function(data) {

    bands <- sits_bands(data)
    bands_ndwi <- c("NIR", "MIR")

    assertthat::assert_that(
        all(bands_ndwi %in% (bands)),
        msg = "sits_ndwi: not enough bands to compute"
    )

    data <- sits_mutate_bands(data, NDWI = (1.5) * (NIR - MIR) / (NIR + MIR))

    return(data)
}
