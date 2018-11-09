#' @title Builds tasseled cap bands
#' @name sits_tasseled_cap
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description  Adds new tasseled cap bands.
#' @param data.tb       Valid sits tibble.
#' @param satellite     Satellite name.
#' @return A sits tibble with same samples and the new bands.
#' @examples
#' \donttest{
#' # Retrieve data for time series with label samples in Mato Grosso in Brazil
#' data (samples_MT_9classes)
#' # Generate a new image with the tasseled cap
#' tc.tb <- sits_tasseled_cap(samples_MT_9classes, satellite = "MODIS")
#' }
#' @export
sits_tasseled_cap <- function(data.tb, satellite = "MODIS"){
    bands <- sits_bands(data.tb)
    bands_mod13q1 <- c("mir", "blue", "nir", "red")
    ensurer::ensure_that(bands, all(bands_mod13q1 %in% (.)),
                         err_desc = "sits_tasseled_cap: not enough bands to compute")

    b_coef <- .sits_get_tcap_brightness(satellite)
    data.tb <- sits_mutate(data.tb, tcb = b_coef["blue"]*blue + b_coef["red"]*red
                           + b_coef["nir"]*nir + b_coef["mir"]*mir)

    g_coef <- .sits_get_tcap_greenness(satellite)
    data.tb <- sits_mutate(data.tb, tcg = g_coef["blue"]*blue + g_coef["red"]*red
                           + g_coef["nir"]*nir + g_coef["mir"]*mir)

    w_coef <- .sits_get_tcap_wetness(satellite)
    data.tb <- sits_mutate(data.tb, tcw = w_coef["blue"]*blue + w_coef["red"]*red
                           + w_coef["nir"]*nir + w_coef["mir"]*mir)

    return(data.tb)
}

#' @title Builds soil-adjusted vegetation index
#' @name sits_savi
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description  Adds new tasseled cap bands.
#' @param data.tb       Valid sits tibble.
#' @return A sits tibble with the SAVI index.
#' @examples
#' \donttest{
#' # Retrieve data for time series with label samples in Mato Grosso in Brazil
#' data (samples_MT_9classes)
#' # Generate a new image with the tasseled cap
#' savi.tb <- sits_savi(samples_MT_9classes)
#' }
#' @export
sits_savi <- function(data.tb){
    bands <- sits_bands(data.tb)
    bands_savi <- c("nir", "red")
    ensurer::ensure_that(bands, all(bands_savi %in% (.)),
                         err_desc = "sits_savi: not enough bands to compute")

    data.tb <- sits_mutate(data.tb, savi = (1.5)*(nir - red)/(nir + red + 0.5))

    return(data.tb)
}

#' @title Builds normalized difference water index
#' @name sits_ndwi
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description  Adds new tasseled cap bands.
#' @param data.tb       A valid sits tibble.
#' @return A sits tibble with the SAVI index.
#' @examples
#' \donttest{
#' # Retrieve data for time series with label samples in Mato Grosso in Brazil
#' data (samples_MT_9classes)
#' # Generate a new image with the tasseled cap
#' ndwi.tb <- sits_ndwi(samples_MT_9classes)
#' }
#' @export
sits_ndwi <- function(data.tb){
    bands <- sits_bands(data.tb)
    bands_ndwi <- c("nir", "mir")
    ensurer::ensure_that(bands, all(bands_ndwi %in% (.)),
                         err_desc = "sits_ndwi: not enough bands to compute")

    data.tb <- sits_mutate(data.tb, ndwi = (1.5) * (nir - mir)/(nir + mir))

    return(data.tb)
}
