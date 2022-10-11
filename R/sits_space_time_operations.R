#' @title Get the time series for a row of a sits tibble
#' @name sits_time_series
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns the time series associated to a row of the a sits tibble
#'
#' @param data     A sits tibble with one or more time series.
#' @return A tibble in sits format with the time series.
#' @examples
#' sits_time_series(cerrado_2classes)
#' @export
sits_time_series <- function(data) {
    return(data$time_series[[1]])
}
#' @title Coordinate transformation (lat/long to X/Y)
#' @name .sits_proj_from_latlong
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Transform a latitude and longitude coordinate to XY coordinate
#'
#' @param longitude Longitude of the chosen location.
#' @param latitude  Latitude of the chosen location.
#' @param crs       Projection definition to be converted to.
#' @return          Tibble with X and Y coordinates.
.sits_proj_from_latlong <- function(longitude,
                                    latitude,
                                    crs) {
    t <- tibble::tibble(long = longitude, lat = latitude) %>%
        sf::st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
        sf::st_transform(crs = crs) %>%
        sf::st_coordinates() %>%
        tibble::as_tibble()

    colnames(t) <- c("X", "Y")
    return(t)
}

#' @title Coordinate transformation (X/Y to lat/long)
#' @name .sits_proj_to_latlong
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Transform a XY coordinate to a latitude and longitude
#'
#' @param x   X coordinate of the chosen location.
#' @param y   Y coordinate of the chosen location.
#' @param crs Projection definition to be converted from.
#' @return Matrix with latlong coordinates.
.sits_proj_to_latlong <- function(x, y, crs) {
    ll <- tibble::tibble(xc = x, yc = y) %>%
        sf::st_as_sf(coords = c("xc", "yc"), crs = crs) %>%
        sf::st_transform(crs = "EPSG:4326") %>%
        sf::st_coordinates()

    colnames(ll) <- c("longitude", "latitude")
    return(ll)
}

#' @title Checks if the crs provided is valid
#' @name .sits_proj_format_crs
#' @keywords internal
#' @noRd
#'
#' @param crs a \code{numeric} or \code{character} with CRS.
#'
#' @return a \code{character} with the formatted CRS.
.sits_proj_format_crs <- function(crs) {
    return(sf::st_crs(crs)[["input"]])
}
#' @title Transform samples to wgs84
#' @name .sits_transform_samples
#' @keywords internal
#' @noRd
#
#' @description Transforming samples points to wgs84 and replace the longitude
#' and latitude values.
#'
#' @param samples         Samples to be retrieved.
#' @param crs             A coordinate reference system of samples.
#'                        The provided crs could be a character
#'                        (e.g, "EPSG:4326" or "WGS84" or a proj4string), or a
#'                        a numeric with the EPSG code (e.g. 4326).
#'                        This parameter only works for 'csv' or data.frame'
#'                        samples. Default is 4326.
#'
#' @return A tibble with tranformed points.
.sits_transform_samples <- function(samples, crs) {

    .check_chr_within(
        x = .conf("df_sample_columns"),
        within = colnames(samples),
        msg = "data input is not valid"
    )

    samples <- suppressWarnings(
        sf::st_transform(
            x = sits_as_sf(data = samples, crs = crs),
            crs = 4326
        )
    )
    pts_repr <- tibble::as_tibble(sf::st_coordinates(samples))
    samples[, c("longitude", "latitude")] <- pts_repr[, c("X", "Y")]

    samples <- sf::st_drop_geometry(samples)

    return(samples)
}
