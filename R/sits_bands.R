#' @title Get the names of the bands
#'
#' @name sits_bands
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description  Finds the names of the bands of
#'               a set of time series or of a data cube
#'
#' @param x         Valid sits tibble (time series or a cube)
#'
#' @return          A vector with the names of the bands.
#' @examples
#' bands <- sits_bands(point_mt_6bands)
#' @export
#'
sits_bands <- function(x) {

    # Set caller to show in errors
    .check_set_caller("sits_bands")
    # Get the meta-type (sits or cube)
    x <- .conf_data_meta_type(x)
    UseMethod("sits_bands", x)
}

#' @rdname sits_bands
#' @export
#'
sits_bands.sits <- function(x) {
    return(setdiff(names(sits_time_series(x)), "Index"))
}
#' @rdname sits_bands
#' @export
#'
sits_bands.sits_cube <- function(x) {
    bands_lst <- slider::slide(x, function(tile) {
        bands_tile <- .tile_bands(tile)
        return(sort(bands_tile))
    })
    bands <- unique(bands_lst)
    .check_that(length(bands) == 1,
        local_msg = "tiles have different bands",
        msg = "cube is inconsistent"
    )
    return(unlist(bands))
}

#' @rdname sits_bands
#' @export
#'
sits_bands.patterns <- function(x) {
    return(sits_bands.sits(x))
}

#' @rdname sits_bands
#' @export
#'
sits_bands.sits_model <- function(x) {
    .check_is_sits_model(x)
    bands <- .ml_bands(x)
    return(bands)
}
#' @keywords internal
#' @noRd
.band_rename <- function(x, bands) {
    UseMethod(".band_rename", x)
}

#' @export
.band_rename.sits <- function(x, bands) {
    data_bands <- sits_bands(x)

    # pre-condition
    .check_chr(bands,
               allow_empty = FALSE, len_min = length(data_bands),
               len_max = length(data_bands),
               msg = "invalid 'bands' value"
    )

    .apply(x, col = "time_series", fn = function(x) {

        # create a conversor
        new_bands <- colnames(x)
        names(new_bands) <- new_bands

        # rename
        new_bands[data_bands] <- toupper(bands)
        colnames(x) <- unname(new_bands)

        return(x)
    })
}
#' @export
.band_rename.raster_cube <- function(x, bands) {
    data_bands <- sits_bands(x)
    # pre-condition
    .check_chr(bands,
               allow_empty = FALSE,
               len_min = length(data_bands),
               len_max = length(data_bands),
               msg = "invalid 'bands' value"
    )
    .apply(x, col = "file_info", fn = function(x) {
        x <- tidyr::pivot_wider(x,
                                names_from = "band",
                                values_from = "path"
        )

        # create a conversor
        new_bands <- colnames(x)
        names(new_bands) <- new_bands

        # rename
        new_bands[data_bands] <- toupper(bands)
        colnames(x) <- unname(new_bands)

        x <- tidyr::pivot_longer(x,
                                 cols = toupper(bands),
                                 names_to = "band",
                                 values_to = "path"
        )

        return(x)
    })
}
#---- Band API: ----

#' Band API
#'
#' We use term \code{band} to refer either to spectral bands as index generated
#' from images of actual sensor bands. To organize internal metadata and work
#' properly there are some restrictions in band names. These functions aims
#' to impose band name restrictions.
#'
#' @param band Band name.
#'
#' @examples
#' if (sits_run_examples()) {
#' .band_cloud() # 'CLOUD'
#' # eo bands name are uppercase
#' .band_eo("nDvI") # 'NDVI'
#' # derived bands name are lowercase
#' .band_derived("PrObS") # 'probs'
#' # bands name cannot have '_' (underscore)
#' .band_eo("NDVI_2") # 'NDVI-2'
#' }
#'
#' @seealso \link{band_accessors}
#' @keywords internal
#' @name band_api
#' @noRd
NULL

#' @describeIn band_api Returns the name of cloud band.
#' @noRd
.band_cloud <- function() {
    "CLOUD"
}

#' @describeIn band_api Returns a well formatted band name for \code{eo_cube}.
#' @noRd
.band_eo <- function(band) {
    gsub("_", "-", toupper(band))
}

#' @describeIn band_api Returns a well formatted band name for
#'   \code{derived_cube}.
#' @noRd
.band_derived <- function(band) {
    gsub("_", "-", tolower(band))
}

