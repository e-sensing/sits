#' @title Export a sits tibble to the Parquet format
#'
#' @name sits_to_parquet
#'
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @description Writes training samples to a Parquet file. The layout is
#'   long: one row per sample and date. Time series become plain columns.
#'
#'   The footer holds two metadata blocks. The \code{sits} key carries the S3
#'   class, the column order, the band order and the nested column names. The
#'   \code{geo} key follows GeoParquet 1.1, so GDAL and QGIS read the file as
#'   points.
#'
#'   A WKB \code{geometry} column is derived from the coordinates. It is
#'   ignored on read.
#'
#'   \code{predicted} and \code{base_data} stay nested. Their row count does
#'   not follow the number of dates.
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
#'   and rebuilds the tibble, with its S3 class, column order and nested
#'   series.
#'
#'   Files written by other programs have no \code{sits} block. The class is
#'   then inferred from the columns. The file must hold \code{longitude},
#'   \code{latitude}, \code{start_date}, \code{end_date} and \code{label}.
#'   Other files are refused. Inference resolves \code{sits},
#'   \code{sits_cluster}, \code{patterns}, \code{embeddings} and
#'   \code{som_clean_samples}. A column named \code{Index} is read as a time
#'   index.
#'
#'   Two rules are assumed: bands named \code{EMB*} mean embeddings, and a
#'   column named \code{cluster} means a clustered set.
#'
#'   Files from another layout version are read, not refused. A warning is
#'   raised.
#'
#'   The file may be an HTTP or HTTPS URL. The server must accept byte
#'   ranges. The footer is fetched first and the file is checked from it, so
#'   a file that is not a sits sample set is refused before any data is
#'   transferred. The data is downloaded next, to a temporary file. The size
#'   is reported before the download.
#'
#' @param  file     Full path or URL of the file to read
#'                  (valid file name with extension ".parquet").
#' @param  ...      Additional parameters to be passed to the request package
#'                  by the steps that read an URL, the footer and the data
#'                  alike.
#' @param  timeout  Seconds each request to an URL may take. Ignored for a
#'                  local file.
#' @return          Time series (tibble of class "sits").
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
sits_from_parquet <- function(file, ..., timeout = getOption("timeout")) {
    .check_set_caller("sits_from_parquet")
    .check_require_packages(c("arrow", "jsonlite"))
    source <- .parquet_source(file)
    source <- .parquet_check(source, timeout = timeout, ...)
    # the footer alone decides if the file is readable, before any row
    footer <- .parquet_footer(source, timeout = timeout, ...)
    on.exit(.parquet_close(source, footer), add = TRUE)
    reader <- arrow::ParquetFileReader$create(footer)
    block <- .parquet_read_block(reader)
    .parquet_check_block(reader, block)
    .parquet_notify(source, reader)
    tbl <- .parquet_read(source, timeout = timeout, ...)
    # no block: infer the class, then rebuild through the same path
    if (!.has(block)) {
        warning(.conf("messages", "sits_from_parquet_no_metadata"),
            call. = FALSE
        )
        block <- .parquet_infer(tbl)
        tbl <- block[["table"]]
    }
    .parquet_rebuild(tbl, block)
}
