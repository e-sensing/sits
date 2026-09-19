#' @title Export a sits tibble to the Parquet format
#'
#' @name sits_to_parquet
#'
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @description Writes a set of training samples to a Parquet file. The
#'   layout is long: one row per sample and date. The time series become
#'   ordinary columns, so any Parquet client can read and query the data.
#'
#'   Columns that describe the sample are repeated on every date. This costs
#'   almost nothing, because Parquet stores repeated values as a dictionary.
#'
#'   The file footer carries two metadata blocks. The \code{sits} key holds
#'   what is needed to rebuild the original tibble: the S3 class, the column
#'   order, the band order and the names of the nested columns. The
#'   \code{geo} key follows the GeoParquet 1.1 specification, so that GDAL,
#'   QGIS and spatial databases open the file as a layer of points.
#'
#'   A \code{geometry} column in WKB is written next to \code{longitude}
#'   and \code{latitude}. It is derived from them, and it is ignored when
#'   the file is read back.
#'
#'   Columns that hold another nested table, such as \code{predicted} and
#'   \code{base_data}, stay nested. Their number of rows does not follow the
#'   number of dates, so they cannot become plain columns.
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
#' @description Reads a file written by \code{\link[sits]{sits_to_parquet}}.
#'   Rebuilds the original tibble. Keeps its S3 class, its column order and
#'   its nested time series.
#'
#'   Some files have no \code{sits} metadata block. This happens when another
#'   program wrote them. In that case, the class is inferred from the columns.
#'
#'   The file must have the five columns that define a set of samples:
#'   \code{longitude}, \code{latitude}, \code{start_date}, \code{end_date}
#'   and \code{label}. Files without them are refused. They are not sample
#'   data.
#'
#'   Inference can resolve five classes: \code{sits}, \code{sits_cluster},
#'   \code{patterns}, \code{embeddings} and \code{som_clean_samples}.
#'
#'   A column named \code{Index} is always read as the index of a time
#'   series. The series is rebuilt from it.
#'
#'   Two rules are assumed, not proven. Bands whose names start with
#'   \code{EMB} are read as embeddings. A column named \code{cluster} is
#'   read as a clustered set. A file that has either one by chance gets that
#'   class. The data itself does not change.
#'
#'   Files written by another version of the layout are read, not refused.
#'   The fields that are understood are used, and a warning is raised.
#'
#' @param  file   Full path of the file to read
#'                (valid file name with extension ".parquet").
#' @return        Time series (tibble of class "sits").
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
        # no block: resolve the class from the columns, and rebuild through
        # the same path used for files that carry one
        warning(.conf("messages", "sits_from_parquet_no_metadata"),
            call. = FALSE
        )
        block <- .parquet_infer(tbl)
        return(.parquet_rebuild(block[["table"]], block))
    }
    block <- jsonlite::fromJSON(block, simplifyVector = FALSE)
    # forward compatibility, as GeoParquet has it: do not reject a file for
    # carrying a version we do not know, warn and read what we understand
    if (!identical(block[["sits_version"]], .parquet_version)) {
        warning(.conf("messages", "sits_from_parquet_version"), call. = FALSE)
    }
    .parquet_rebuild(tbl, block)
}
