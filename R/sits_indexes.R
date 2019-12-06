#' @title Builds tasseled cap bands
#' @name sits_tasseled_cap
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description  Adds new tasseled cap bands.
#' @param data          Valid sits tibble.
#' @param sensor        sensor.
#' @return A sits tibble with same samples and the new bands.
#' @examples
#' \donttest{
#' # Retrieve data for time series with label samples in Mato Grosso in Brazil
#' data (samples_mt_6bands)
#' # Generate a new image with the tasseled cap
#' tc.tb <- sits_tasseled_cap(samples_mt_6bands, sensor = "MODIS")
#' }
#' @export
sits_tasseled_cap <- function(data, sensor = "MODIS"){
    # backward compatibility
    if ("coverage" %in% names(data))
        data <- .sits_tibble_rename(data)
    bands <- sits_bands(data)

    b_coef <- .sits_config_tcap_brightness(sensor)
    data   <- sits_mutate_bands(data, tcb = b_coef["blue"]*blue
                                          + b_coef["red"]*red
                                          + b_coef["nir"]*nir
                                          + b_coef["mir"]*mir)

    g_coef <- .sits_config_tcap_greenness(sensor)
    data   <- sits_mutate_bands(data, tcg = g_coef["blue"]*blue
                                          + g_coef["red"]*red
                                          + g_coef["nir"]*nir
                                          + g_coef["mir"]*mir)

    w_coef <- .sits_config_tcap_wetness(sensor)
    data   <- sits_mutate_bands(data, tcw = w_coef["blue"]*blue
                                          + w_coef["red"]*red
                                          + w_coef["nir"]*nir
                                          + w_coef["mir"]*mir)

    return(data)
}

#' @title Builds soil-adjusted vegetation index
#' @name sits_savi
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description  Adds new tasseled cap bands.
#' @param data       Valid sits tibble.
#' @return A sits tibble with the SAVI index.
#' @examples
#' \donttest{
#' # Retrieve data for time series with label samples in Mato Grosso in Brazil
#' data (samples_mt_6bands)
#' # Generate a new image with the tasseled cap
#' savi.tb <- sits_savi(samples_mt_6bands)
#' }
#' @export
sits_savi <- function(data){
    # backward compatibility
    if ("coverage" %in% names(data))
        data <- .sits_tibble_rename(data)
    bands <- sits_bands(data)
    bands_savi <- c("nir", "red")
    assertthat::assert_that(all(bands_savi %in% (bands)),
        msg = "sits_savi: not enough bands to compute")

    data <- sits_mutate_bands(data, savi = (1.5)*(nir - red)/(nir + red + 0.5))

    return(data)
}

#' @title Builds normalized difference water index
#' @name sits_ndwi
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description  Adds new tasseled cap bands.
#' @param data       A valid sits tibble.
#' @return A sits tibble with the SAVI index.
#' @examples
#' \donttest{
#' # Retrieve data for time series with label samples in Mato Grosso in Brazil
#' data(samples_mt_6bands)
#' # Generate a new image with the tasseled cap
#' ndwi.tb <- sits_ndwi(samples_mt_6bands)
#' }
#' @export
sits_ndwi <- function(data){
    # backward compatibility
    if ("coverage" %in% names(data))
        data <- .sits_tibble_rename(data)
    bands <- sits_bands(data)
    bands_ndwi <- c("nir", "mir")
    assertthat::assert_that(all(bands_ndwi %in% (bands)),
                         msg = "sits_ndwi: not enough bands to compute")

    data <- sits_mutate_bands(data, ndwi = (1.5) * (nir - mir)/(nir + mir))

    return(data)
}
