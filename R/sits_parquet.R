#' @title Export a sits tibble to the Parquet format
#'
#' @name sits_to_parquet
#'
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @description Writes a set of training samples to a Parquet file, in a
#'   long layout: one row per sample and date. The time series are flattened
#'   into ordinary columns, so that the data can be read and queried by any
#'   Parquet client. Sample-level columns are repeated for each date and cost
#'   almost nothing, since they are dictionary encoded.
#'
#'   Two metadata blocks are written in the file footer. The \code{sits} key
#'   carries what is needed to rebuild the original tibble - the S3 class,
#'   the column order, the band order and the nested column names. The
#'   \code{geo} key follows the GeoParquet 1.1 specification, so that GDAL,
#'   QGIS and spatial databases read the file as a point layer.
#'
#'   A \code{geometry} column in WKB is written alongside \code{longitude}
#'   and \code{latitude}. It is derived from them and ignored when reading
#'   back.
#'
#' @param  data   Time series (tibble of class "sits").
#' @param  file   Full path of the exported file
#'                (valid file name with extension ".parquet").
#' @return        Called for side effects; returns \code{file} invisibly.
#'
#' @note Requires the \code{arrow} and \code{jsonlite} packages.
#'
#' @examples
#' if (sits_run_examples()) {
#'     parquet_file <- paste0(tempdir(), "/samples.parquet")
#'     sits_to_parquet(samples_modis_ndvi, file = parquet_file)
#'     samples <- sits_from_parquet(parquet_file)
#' }
#' @family data conversion
#' @seealso \code{\link[sits]{sits_from_parquet}}
#' @export
#'
sits_to_parquet <- function(data, file) {
    .check_set_caller("sits_to_parquet")
    UseMethod("sits_to_parquet", data)
}
#' @rdname sits_to_parquet
#' @export
sits_to_parquet.sits <- function(data, file) {
    .check_set_caller("sits_to_parquet_sits")
    .check_require_packages(c("arrow", "jsonlite"))
    .check_samples(data)
    .check_file(x = file, extensions = "parquet", file_exists = FALSE)
    # flatten the samples and build both metadata blocks
    flat <- .parquet_flatten(data)
    table <- arrow::arrow_table(flat[["table"]])
    blocks <- list(sits = flat[["sits"]])
    if (.has(flat[["geo"]])) {
        blocks[["geo"]] <- flat[["geo"]]
    }
    table$metadata <- c(table$metadata, blocks)
    arrow::write_parquet(table, file)
    invisible(file)
}
#' @rdname sits_to_parquet
#' @export
sits_to_parquet.tbl_df <- function(data, file) {
    if (all(.conf("sits_tibble_cols") %in% colnames(data))) {
        class(data) <- c("sits", class(data))
    } else {
        stop(.conf("messages", "sits_to_parquet_default"))
    }
    sits_to_parquet(data, file)
}
#' @rdname sits_to_parquet
#' @export
sits_to_parquet.default <- function(data, file) {
    data <- tibble::as_tibble(data)
    if (all(.conf("sits_tibble_cols") %in% colnames(data))) {
        class(data) <- c("sits", class(data))
        sits_to_parquet(data, file)
    } else {
        stop(.conf("messages", "sits_to_parquet_default"))
    }
}

#' @title Read a sits tibble from the Parquet format
#'
#' @name sits_from_parquet
#'
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @description Reads a file written by \code{\link[sits]{sits_to_parquet}}
#'   and rebuilds the original tibble, including its S3 class, its column
#'   order and its nested time series.
#'
#'   When the file carries no \code{sits} metadata block - because it came
#'   from somewhere else - the flat table is returned as read, with a
#'   warning. Nothing is guessed.
#'
#' @param  file   Full path of the file to read
#'                (valid file name with extension ".parquet").
#' @return        Time series (tibble of class "sits"), or a plain tibble
#'                when the file has no sits metadata.
#'
#' @note Requires the \code{arrow} and \code{jsonlite} packages.
#'
#' @examples
#' if (sits_run_examples()) {
#'     parquet_file <- paste0(tempdir(), "/samples.parquet")
#'     sits_to_parquet(samples_modis_ndvi, file = parquet_file)
#'     samples <- sits_from_parquet(parquet_file)
#' }
#' @family data conversion
#' @seealso \code{\link[sits]{sits_to_parquet}}
#' @export
#'
sits_from_parquet <- function(file) {
    .check_set_caller("sits_from_parquet")
    .check_require_packages(c("arrow", "jsonlite"))
    .check_file(x = file, extensions = "parquet")
    reader <- arrow::ParquetFileReader$create(file)
    block <- reader$GetSchema()$metadata[["sits"]]
    tbl <- arrow::read_parquet(file)
    if (!.has(block)) {
        warning(.conf("messages", "sits_from_parquet_no_metadata"),
            call. = FALSE
        )
        return(tbl)
    }
    .parquet_rebuild(tbl, jsonlite::fromJSON(block, simplifyVector = FALSE))
}
