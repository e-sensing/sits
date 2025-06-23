#' @title Rename bands (S3 Generic function)
#' @name .band_rename
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @param x sits object (time series or cube)
#' @param bands new bands for the object
#' @return updated sits object
.band_rename <- function(x, bands) {
    UseMethod(".band_rename", x)
}
#' @title Rename bands for sits tibble (S3 Generic function)
#' @name .band_rename.sits
#' @noRd
#' @param x sits object (time series)
#' @param bands new bands for the object
#' @return Updated sits object
#' @export
.band_rename.sits <- function(x, bands) {
    data_bands <- .samples_bands.sits(x)
    # pre-condition
    .check_chr(
        bands,
        allow_empty = FALSE, len_min = length(data_bands),
        len_max = length(data_bands)
    )
    .apply(x, col = "time_series", fn = function(x) {
        # create a conversor
        new_bands <- colnames(x)
        names(new_bands) <- new_bands

        # rename
        new_bands[data_bands] <- toupper(bands)
        colnames(x) <- unname(new_bands)
        x
    })
}
#' @title Rename bands for data cube (S3 Generic function)
#' @name .band_rename.raster_cube
#' @noRd
#' @param x sits object (cube)
#' @param bands new bands for the object
#' @return updated sits object
#' @export
.band_rename.raster_cube <- function(x, bands) {
    data_bands <- .cube_bands(x)
    # pre-condition
    .check_chr(
        bands,
        allow_empty = FALSE,
        len_min = length(data_bands),
        len_max = length(data_bands)
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

        tidyr::pivot_longer(
            x,
            cols = toupper(bands),
            names_to = "band",
            values_to = "path"
        )
    })
}
#' @title Return cloud band
#' @name .band_could
#' @noRd
#' @return name used in SITS for cloud band
.band_cloud <- function() {
    "CLOUD"
}
#' @title Convert band names in case of eo_cubes (non-processed)
#' @name .band_eo
#' @noRd
#' @param band band name (may be lower or upper case)
#' @return band name in upper case
.band_eo <- function(band) {
    gsub("_", "-", toupper(band), fixed = TRUE)
}
#' @title Convert band names for derived cubes
#' @name .band_derived
#' @noRd
#' @param band band name (may be lower or upper case)
#' @return band name in lower case
.band_derived <- function(band) {
    gsub("_", "-", tolower(band), fixed = TRUE)
}
#' @title Convert band names for time series
#' @name .band_samples
#' @noRd
#' @param band band name (may be lower or upper case)
#' @return band name in upper case
.band_samples <- function(band) {
    gsub("_", "-", toupper(band), fixed = TRUE)
}
#' @title Convert band names for data cube
#' @name .band_set_case
#' @description non-processed cubes (eo_cubes) use upper case
#'              processed cubes (results_cubes) use lower case
#' @noRd
#' @param bands band names (may be lower or upper case)
#' @return band names in case required by SITS
.band_set_case <- function(bands) {
    if (.has(bands)) {
        if (all(tolower(bands) %in% .conf("sits_results_bands"))) {
            bands <- tolower(bands)
        } else {
            bands <- toupper(bands)
        }
    }
    bands
}
#' @title Set reasonable bands for visualisation
#' @name .band_set_bw_rgb
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param cube      cube to choose band
#' @param band      B/W band for view
#' @param red       Red band for view
#' @param green     Green band for view
#' @param blue      Blue band for view
#' @return vector with bands
#' @keywords internal
#' @noRd
.band_set_bw_rgb <- function(cube, band, red, green, blue) {
    .check_set_caller(".band_set_bw_rgb")
    # check band is available
    if (.has(band)) {
        .check_that(band %in% .cube_bands(cube))
        return(band)
    } else if (.has(red) && .has(green) && .has(blue)) {
        # check bands are available
        bands <- c(red, green, blue)
        .check_that(all(bands %in% .cube_bands(cube)))
        return(bands)
    }
    bands <- .band_best_guess(cube)
    bands
}
#' @title Make a best guess on bands to be displayed
#' @name .band_best_guess
#' @description if user did not provide band names,
#' try some reasonable color composites.
#' A full list of color composites is available
#' in "./inst/extdata/config_colors.yaml"
#' @noRd
#' @param cube data cube
#' @return band names to be displayed
.band_best_guess <- function(cube) {
    # get all bands in the cube
    cube_bands <- .cube_bands(cube)
    # get source and collection for the cube
    cube_source <- .cube_source(cube)
    collection <- .cube_collection(cube)
    # find which are possible color composites for the cube
    comp_source <- sits_env[["composites"]][["sources"]][[cube_source]]
    composites <- comp_source[["collections"]][[collection]]
    # for each color composite (in order)
    # see if bands are available
    for (i in seq_along(composites)) {
        bands <- composites[[i]]
        if (all(bands %in% .cube_bands(cube))) {
            return(bands)
        }
    }
    # if composites fail, try NDVI
    if ("NDVI" %in% cube_bands) {
        "NDVI"
    } # return the first band if all fails
    else {
        cube_bands[[1L]]
    }
}
