.config_show <- function() {

}

.config_get <- function(key) {

    value <- sits_env$config[[key]]

    assertthat::assert_that(
        !is.null(value),
        msg = paste(".config_get:", paste0(key, collapse = "$"),
                    "not found in sits config.")
    )

    return(value)
}


.config_memory_bloat <- function() {

    .config_get(c("R_memory_bloat"))
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

.config_gtiff_default_options <- function() {

    .config_get(c("gtiff_default_options"))
}


# src

.config_src_url <- function(source) {

    .config_get(c("sources", source, "url"))
}

.config_src_service <- function(source) {

    .config_get(c("sources", source, "service"))
}

.config_src_s3class <- function(source) {

    .config_get(c("sources", source, "s3_class"))
}

.config_src_cols <- function(source) {

    names(.config_get(c("sources", source, "collections")))
}

.config_src_col_bands <- function(source,
                                  collection) {

    names(.config_get(c("sources", source, "collections", collection, "bands")))
}

.config_src_col_bands_get <- function(source,
                                      collection,
                                      key,
                                      add_cloud = TRUE) {

    bands <- .config_src_col_bands(source = source,
                                   collection = collection)

    if (!add_cloud)
        bands <- bands[bands != "CLOUD"]

    values <- sapply(.config_get(c("sources", source, "collections",
                                   collection, "bands")), `[[`, key)

    return(unname(values[bands]))
}

.config_src_col_cloud_get <- function(source,
                                      collection,
                                      key) {

    .config_get(c("sources", source, "collections",
                  collection, "bands", "CLOUD", key))
}

# pre

.config_missing_value <- function(source, collection) {

    .config_src_col_bands_get(source = source, collection = collection,
                              key = "missing_value", add_cloud = FALSE)
}

.config_minimum_value <- function(source, collection) {

    .config_src_col_bands_get(source = source, collection = collection,
                              key = "minimum_value", add_cloud = FALSE)
}

.config_maximum_value <- function(source, collection) {

    .config_src_col_bands_get(source = source, collection = collection,
                              key = "maximum_value", add_cloud = FALSE)
}

.config_scale_value <- function(source, collection) {

    .config_src_col_bands_get(source = source, collection = collection,
                              key = "scale_value", add_cloud = FALSE)
}

.config_offset_value <- function(source, collection) {

    .config_src_col_bands_get(source = source, collection = collection,
                              key = "offset_value", add_cloud = FALSE)
}

.config_resampling <- function(source, collection) {

    .config_src_col_bands_get(source = source, collection = collection,
                              key = "resampling", add_cloud = TRUE)
}

.config_resolutions <- function(source, collection) {

    .config_src_col_bands_get(source = source, collection = collection,
                              key = "resolutions", add_cloud = TRUE)
}

.config_band_name <- function(source, collection) {

    .config_src_col_bands_get(source = source, collection = collection,
                              key = "band_name", add_cloud = TRUE)
}

.config_cloud_bit_mask <- function(source, collection) {

    .config_src_col_cloud_get(source = source, collection = collection,
                              key = "bit_mask")
}

.config_cloud_bit_mask <- function(source, collection) {

    .config_src_col_cloud_get(source = source, collection = collection,
                              key = "bit_mask")
}

.config_cloud_values <- function(source, collection) {

    .config_src_col_cloud_get(source = source, collection = collection,
                              key = "values")
}

.config_cloud_interp_values <- function(source, collection) {

    .config_src_col_cloud_get(source = source, collection = collection,
                              key = "interp_values")
}

.config_local_file_extensions <- function() {

    .config_get(c("sources", "LOCAL", "file_extensions"))
}

.config_aws_default_region <- function(source, collection) {

    .config_get(c("sources", source, "collections", collection,
                  "AWS", "AWS_DEFAULT_REGION"))
}

.config_aws_endpoint <- function(source, collection) {

    .config_get(c("sources", source, "collections", collection,
                  "AWS", "AWS_S3_ENDPOINT"))
}

.config_aws_request_payer <- function(source, collection) {

    .config_get(c("sources", source, "collections", collection,
                  "AWS", "AWS_REQUEST_PAYER"))
}

