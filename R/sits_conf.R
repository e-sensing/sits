.config <- function() {

}

.config_show <- function() {

}

.config_get <- function(key, default = NULL) {

    tryCatch({
        value <- sits_env$config[[key]]
    },
    error = function(e) {
        if (is.null(default))
            stop(paste(".config_get:", paste0(key, collapse = "$"),
                       "not found in sits config."), call. = FALSE)
    })

    if (missing(default))
        assertthat::assert_that(
            !is.null(value),
            msg = paste(".config_get:", paste0(key, collapse = "$"),
                        "not found in sits config.")
        )

    return(value)
}

.config_aws_default_region <- function(source,
                                       collection) {

    .config_get(c("sources", source, "collections", collection,
                  "AWS", "AWS_DEFAULT_REGION", default = NULL))
}

.config_aws_endpoint <- function(source,
                                 collection) {

    .config_get(c("sources", source, "collections", collection,
                  "AWS", "AWS_S3_ENDPOINT", default = NULL))
}

.config_aws_request_payer <- function(source,
                                      collection) {

    .config_get(c("sources", source, "collections", collection,
                  "AWS", "AWS_REQUEST_PAYER"), default = NULL)
}

.config_bands <- function(source,
                          collection, ...,
                          fn_filter = NULL,
                          add_cloud = TRUE) {


    bands <- .config_get(c("sources", source, "collections",
                           collection, "bands"))

    if (!add_cloud)
        bands <- bands[names(bands) != "CLOUD"]

    if (!is.null(fn_filter)) {
        select <- vapply(bands, fn_filter, logical(1))
        return(names(bands)[select])
    }

    names(bands)
}

.config_bands_reap <- function(source,
                               collection,
                               key, ...,
                               bands = NULL,
                               fn_filter = NULL,
                               add_cloud = TRUE) {

    if (is.null(bands))
        bands <- .config_bands(source = source,
                               collection = collection,
                               fn_filter = fn_filter,
                               add_cloud = add_cloud)

    assertthat::assert_that(
        all(bands %in% .config_bands(source = source,
                                     collection = collection)),
        msg = ".config_bands_reap: invalid bands"
    )

    values <- lapply(.config_get(c("sources", source, "collections",
                                   collection, "bands"))[bands], `[[`, key)

    if (length(values) > 0 && is.atomic(values[[1]]))
        return(unlist(unname(values)))

    return(unname(values))
}

.config_bands_band_name <- function(source,
                                    collection, ...,
                                    bands = NULL,
                                    fn_filter = NULL,
                                    add_cloud = TRUE) {

    .config_bands_reap(source = source,
                       collection = collection,
                       key = "band_name",
                       bands = bands,
                       fn_filter = fn_filter,
                       add_cloud = add_cloud)
}


.config_cloud <- function() {

    return("CLOUD")
}

.config_cloud_bit_mask <- function(source,
                                   collection) {

    .config_get(c("sources", source, "collections", collection,
                  "bands", "CLOUD", "bit_mask"))
}

.config_cloud_values <- function(source,
                                 collection) {

    .config_get(c("sources", source, "collections", collection,
                  "bands", "CLOUD", "values"))
}

.config_cloud_interp_values <- function(source,
                                        collection) {

    .config_get(c("sources", source, "collections", collection,
                  "bands", "CLOUD", "interp_values"))
}

.config_collections <- function(source) {

    names(.config_get(c("sources", source, "collections")))
}

.config_gtiff_default_options <- function() {

    .config_get(c("GTiff_default_options"))
}

.config_local_file_extensions <- function() {

    .config_get(c("sources", "LOCAL", "file_extensions"))
}


.config_memory_bloat <- function() {

    .config_get(c("R_memory_bloat"))
}

.config_palettes <- function() {

    names(.config_get(c("palettes")))
}

.config_palette_colors <- function(labels, ...,
                                   palette = "default") {

    values <- .config_get(c("palettes", palette))[labels]
    names(values) <- labels

    if (any(is.na(values))) {

        random <- colors()
        random <- random[!random %in% values]
        values[is.na(values)] <- sample(random, sum(is.na(values)))
    }

    values
}

.config_processing_bloat <- function() {

    .config_get(c("R_processing_bloat"))
}

.config_rstac_limit <- function() {

    .config_get(c("rstac_pagination_limit"))
}

.config_raster_pkg <- function() {

    .config_get(c("R_raster_pkg"))
}

.config_sources <- function() {

    names(.config_get(c("sources")))
}

.config_source_url <- function(source) {

    .config_get(c("sources", source, "url"))
}

.config_source_service <- function(source) {

    .config_get(c("sources", source, "service"))
}

.config_source_s3class <- function(source) {

    .config_get(c("sources", source, "s3_class"))
}
