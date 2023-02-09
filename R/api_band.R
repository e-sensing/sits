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

.band_cloud <- function() {
    "CLOUD"
}

.band_eo <- function(band) {
    gsub("_", "-", toupper(band))
}

.band_derived <- function(band) {
    gsub("_", "-", tolower(band))
}

.band_samples <- function(band) {
    gsub("_", "-", toupper(band))
}
