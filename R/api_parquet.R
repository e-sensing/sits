#---- parquet ----

#' @title Version of the sits Parquet layout
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @keywords internal
.parquet_version <- "0.1"

#' @title Map between file identifier names and sits column names
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @keywords internal
#' @description Identifiers are `<entity>:id`, where the entity assigned the
#'   number. Keeps unrelated counters from sharing a name.
.parquet_id_map <- c(
    "csv:id"       = "id",
    "som:id"       = "id_sample",
    "neuron:id"    = "id_neuron",
    "cluster:id"   = "id_cluster",
    "polygon:id"   = "polygon_id",
    "partition:id" = "part_id"
)

#' @title Build WKB point geometries
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @keywords internal
#' @param longitude  Vector of longitudes
#' @param latitude   Vector of latitudes
#' @return List of raw vectors, one WKB point each
.parquet_wkb_point <- function(longitude, latitude) {
    purrr::map2(longitude, latitude, function(lon, lat) {
        # null geometry says there is no location, as patterns have none
        if (is.na(lon) || is.na(lat)) {
            return(NULL)
        }
        c(
            as.raw(1L),                                 # little endian
            as.raw(c(1L, 0L, 0L, 0L)),                  # wkbPoint
            writeBin(as.double(lon), raw(), size = 8L),
            writeBin(as.double(lat), raw(), size = 8L)
        )
    })
}

#' @title Which columns hold nested tables
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @keywords internal
#' @param data  Samples tibble
#' @return Character vector of column names
.parquet_nested_cols <- function(data) {
    names(data)[purrr::map_lgl(data, function(col) {
        is.list(col) && length(col) > 0L && is.data.frame(col[[1L]])
    })]
}

#' @title PROJJSON for WGS 84
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @keywords internal
#' @return Named list ready to be serialised as PROJJSON
.parquet_projjson_wgs84 <- function() {
    list(
        "$schema" = paste0(
            "https://proj.org/schemas/v0.5/projjson.schema.json"
        ),
        type = "GeographicCRS",
        name = "WGS 84",
        datum_ensemble = list(
            name = "World Geodetic System 1984 ensemble",
            members = list(
                list(name = "World Geodetic System 1984 (Transit)"),
                list(name = "World Geodetic System 1984 (G730)"),
                list(name = "World Geodetic System 1984 (G873)"),
                list(name = "World Geodetic System 1984 (G1150)"),
                list(name = "World Geodetic System 1984 (G1674)"),
                list(name = "World Geodetic System 1984 (G1762)"),
                list(name = "World Geodetic System 1984 (G2139)")
            ),
            ellipsoid = list(
                name = "WGS 84",
                semi_major_axis = 6378137.0,
                inverse_flattening = 298.257223563
            ),
            accuracy = "2.0",
            id = list(authority = "EPSG", code = 6326L)
        ),
        coordinate_system = list(
            subtype = "ellipsoidal",
            axis = list(
                list(
                    name = "Geodetic longitude", abbreviation = "Lon",
                    direction = "east", unit = "degree"
                ),
                list(
                    name = "Geodetic latitude", abbreviation = "Lat",
                    direction = "north", unit = "degree"
                )
            )
        ),
        id = list(authority = "EPSG", code = 4326L)
    )
}

#' @title Build the geo metadata block (GeoParquet 1.1)
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @keywords internal
#' @param longitude  Vector of longitudes
#' @param latitude   Vector of latitudes
#' @return JSON string
.parquet_geo_block <- function(longitude, latitude) {
    # patterns have no location, so there is nothing to declare
    if (all(is.na(longitude)) || all(is.na(latitude))) {
        return(NULL)
    }
    jsonlite::toJSON(
        list(
            version = "1.1.0",
            primary_column = "geometry",
            columns = list(
                geometry = list(
                    encoding = "WKB",
                    geometry_types = I("Point"),
                    crs = .parquet_projjson_wgs84(),
                    bbox = c(
                        min(longitude, na.rm = TRUE),
                        min(latitude, na.rm = TRUE),
                        max(longitude, na.rm = TRUE),
                        max(latitude, na.rm = TRUE)
                    )
                )
            )
        ),
        auto_unbox = TRUE, digits = NA
    )
}

#' @title Build the sits metadata block
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @keywords internal
#' @param data          Samples tibble
#' @param sample_cols   Sample-level column names, already renamed
#' @param series        List describing the series, or NULL
#' @param nested        List describing nested columns kept as list<struct>
#' @return JSON string
.parquet_block <- function(data, sample_cols, series, nested) {
    # I() keeps length-one vectors as JSON arrays, so a strict reader in
    # another language does not get a scalar where a list is declared
    empty_object <- stats::setNames(list(), character(0L))
    ids <- as.list(.parquet_id_map[names(.parquet_id_map) %in% sample_cols])
    block <- list(
        sits_version = .parquet_version,
        layout = "long",
        key = "sample:id",
        class = I(class(data)),
        column_order = I(colnames(data)),
        sample_columns = I(sample_cols),
        id_map = if (length(ids) > 0L) ids else empty_object,
        nested = if (length(nested) > 0L) nested else empty_object
    )
    if (.has(series)) {
        series[["bands"]] <- I(series[["bands"]])
        block[["series"]] <- series
    }
    jsonlite::toJSON(block, auto_unbox = TRUE, digits = NA, null = "null")
}

#' @title Flatten a samples tibble into the long layout
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @keywords internal
#' @param data  Samples tibble
#' @return List with the flat table and the two metadata blocks
.parquet_flatten <- function(data) {
    .check_set_caller(".parquet_flatten")
    nested <- .parquet_nested_cols(data)
    series_col <- intersect("time_series", nested)
    extra_cols <- setdiff(nested, series_col)
    # sample columns keep their order, without the nested ones
    sample_cols <- setdiff(colnames(data), nested)
    flat <- data[sample_cols]
    flat[["sample:id"]] <- seq_len(nrow(flat))
    # `predicted` has one row per interval, unrelated to the number of
    # dates. Flattening it would need a cartesian product, so it stays nested
    nested_kept <- list()
    for (col in extra_cols) {
        inner <- data[[col]]
        flat[[col]] <- inner
        nested_kept[[col]] <- I(colnames(inner[[1L]]))
    }
    # expand the series, keeping empty ones as a single NA row
    series <- NULL
    if (.has(series_col)) {
        inner <- data[[series_col]]
        bands <- setdiff(colnames(inner[[1L]]), "Index")
        empty <- tibble::tibble(
            Index = as.Date(NA), !!!stats::setNames(
                rep(list(NA_real_), length(bands)), bands
            )
        )
        inner <- purrr::map(inner, function(ts) {
            if (nrow(ts) == 0L) empty else ts
        })
        flat[[series_col]] <- inner
        flat <- tidyr::unnest(flat, cols = dplyr::all_of(series_col))
        series <- list(
            column = series_col, index = "Index", bands = bands
        )
    }
    # <entity>:id keeps unrelated counters from sharing a name
    old <- unname(.parquet_id_map)
    new <- names(.parquet_id_map)
    hit <- old %in% colnames(flat)
    if (any(hit)) {
        flat <- dplyr::rename(flat, !!!stats::setNames(old[hit], new[hit]))
        sample_cols[match(old[hit], sample_cols)] <- new[hit]
    }
    # derived from the coordinates, so it is ignored on read
    flat[["geometry"]] <- .parquet_wkb_point(
        flat[["longitude"]], flat[["latitude"]]
    )
    list(
        table = flat,
        sits = .parquet_block(
            data, c("sample:id", sample_cols), series, nested_kept
        ),
        geo = .parquet_geo_block(data[["longitude"]], data[["latitude"]])
    )
}

#' @title Columns that belong to the sample, not to the series
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @keywords internal
#' @description Used when a file has no sits block. With an index column,
#'   every other column is a band.
.parquet_sample_level <- c(
    "sample:id", "geometry",
    "longitude", "latitude", "start_date", "end_date", "label", "cube",
    "cluster", "folds", "train",
    "count", "prior_prob", "post_prob", "eval",
    names(.parquet_id_map), unname(.parquet_id_map)
)

#' @title Infer a metadata block from the columns alone
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @keywords internal
#' @description Builds the block that would have been written, so the
#'   rebuilding path is the same for files that carry one.
#' @param tbl  Flat table read from the file
#' @return Block, as a list
.parquet_infer <- function(tbl) {
    .check_set_caller(".parquet_infer")
    cols <- colnames(tbl)
    # without these columns it is not a set of samples
    .check_that(all(.conf("df_sample_columns") %in% cols))
    index <- if ("Index" %in% cols) "Index" else NULL
    nested_cols <- .parquet_nested_cols(tbl)
    bands <- if (.has(index)) {
        setdiff(cols, c(.parquet_sample_level, index, nested_cols))
    } else {
        character(0L)
    }
    sample_cols <- setdiff(cols, c(index, bands, nested_cols, "geometry"))
    # rows of one sample are written together, so a run of identical
    # sample-level values is one sample
    key <- "sample:id"
    if (!(key %in% cols)) {
        run <- do.call(paste, c(tbl[setdiff(sample_cols, key)], sep = "\r"))
        tbl[[key]] <- cumsum(c(TRUE, run[-1L] != run[-length(run)]))
        sample_cols <- c(key, sample_cols)
    }
    list(
        sits_version = .parquet_version,
        layout = "long",
        key = key,
        class = .parquet_infer_class(tbl, index, bands),
        column_order = setdiff(
            c(sample_cols, if (.has(index)) "time_series"), key
        ),
        sample_columns = sample_cols,
        id_map = list(),
        nested = list(),
        series = if (.has(index)) {
            list(column = "time_series", index = index, bands = bands)
        },
        table = tbl
    )
}

#' @title Resolve the S3 class from the columns and the series
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @keywords internal
#' @description Most specific first. Classes not carried by the data
#'   resolve to "sits".
#' @param tbl    Flat table
#' @param index  Name of the index column, or NULL
#' @param bands  Band column names
#' @return Character vector of classes
.parquet_infer_class <- function(tbl, index, bands) {
    base <- c("sits", "tbl_df", "tbl", "data.frame")
    som_cols <- c("count", "prior_prob", "post_prob", "eval")
    n_times <- if (.has(index)) {
        max(table(tbl[["sample:id"]]))
    } else {
        0L
    }
    # without a series these classes cannot apply
    if (.has(index)) {
        if (all(som_cols %in% colnames(tbl))) {
            return(c("som_clean_samples", base))
        }
        if ("cluster" %in% colnames(tbl)) {
            return(c("sits_cluster", base))
        }
        if (n_times > 1L && all(is.na(tbl[["longitude"]]))) {
            return(c("patterns", base))
        }
        if (n_times == 1L && length(bands) > 0L &&
            all(startsWith(bands, "EMB"))) {
            return(c("embeddings", base))
        }
    }
    base
}

#' @title Rebuild a samples tibble from the long layout
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @keywords internal
#' @param tbl    Flat table read from the file
#' @param block  Parsed sits metadata block
#' @return Samples tibble
.parquet_rebuild <- function(tbl, block) {
    .check_set_caller(".parquet_rebuild")
    key <- block[["key"]]
    .check_that(key %in% colnames(tbl))
    tbl[["geometry"]] <- NULL
    sample_cols <- setdiff(unlist(block[["sample_columns"]]), key)
    # order of first occurrence: the row order of a sits tibble matters
    ids <- unique(tbl[[key]])
    first <- match(ids, tbl[[key]])
    data <- tbl[first, sample_cols, drop = FALSE]
    # empty series come back with no rows, not one NA row
    series <- block[["series"]]
    if (.has(series)) {
        bands <- unlist(series[["bands"]])
        cols <- c(series[["index"]], bands)
        parts <- split(tbl[cols], factor(tbl[[key]], levels = ids))
        data[[series[["column"]]]] <- purrr::map(parts, function(ts) {
            if (nrow(ts) == 1L && is.na(ts[[1L]][[1L]])) ts[0L, ] else ts
        })
    }
    # nested columns keep the row count each sample had
    for (col in names(block[["nested"]])) {
        data[[col]] <- purrr::map(tbl[[col]][first], tibble::as_tibble)
    }
    # identifiers take their sits names again
    id_map <- block[["id_map"]]
    if (length(id_map) > 0L) {
        data <- dplyr::rename(
            data, !!!stats::setNames(names(id_map), unlist(id_map))
        )
    }
    data <- data[unlist(block[["column_order"]])]
    class(data) <- unlist(block[["class"]])
    data
}

#---- parquet source ----

#' @title Classify the origin of a Parquet file
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @keywords internal
#' @description The steps that differ between a local path and an HTTP URL
#'   dispatch on this class. The other steps do not.
#' @param file  Path or URL
#' @return The file, with class "parquet_local" or "parquet_http"
.parquet_source <- function(file) {
    .check_set_caller(".parquet_source")
    .check_chr_parameter(file, len_max = 1L)
    origin <- if (grepl("^https?://", file)) "parquet_http" else "parquet_local"
    .set_class(file, origin, class(file))
}

#' @title Check that the file can be read
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @keywords internal
#' @description A remote file must accept byte ranges, or the footer cannot
#'   be read apart from the data.
#' @param source  Object from .parquet_source()
#' @return The source, with the size of a remote file as an attribute
.parquet_check <- function(source, ...) {
    UseMethod(".parquet_check")
}
#' @export
.parquet_check.parquet_local <- function(source, ...) {
    .check_set_caller(".parquet_check_parquet_local")
    file <- unclass(source)
    .check_file(x = file, extensions = "parquet")
    source
}
#' @export
.parquet_check.parquet_http <- function(source, ...) {
    .check_set_caller(".parquet_check_parquet_http")
    .check_that(.file_ext(source) == "parquet")
    resp <- .head_request(unclass(source), ...)
    size <- as.numeric(.response_header(resp, "content-length"))
    .check_that(
        identical(.response_header(resp, "accept-ranges"), "bytes") &&
            .has(size) && !is.na(size)
    )
    attr(source, "size") <- size
    source
}

#' @title Local path whose footer arrow can open
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @keywords internal
#' @description For a URL, a stub with the footer alone. The stub is removed
#'   by .parquet_close().
#' @param source  Object from .parquet_source()
#' @return Path
.parquet_footer <- function(source, ...) {
    UseMethod(".parquet_footer")
}
#' @export
.parquet_footer.parquet_local <- function(source, ...) {
    unclass(source)
}
#' @export
.parquet_footer.parquet_http <- function(source, ...) {
    .parquet_remote_footer(unclass(source), attr(source, "size"), ...)
}

#' @title Remove what .parquet_footer() created
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @keywords internal
#' @param source  Object from .parquet_source()
#' @param footer  Path returned by .parquet_footer()
#' @return Called for side effects
.parquet_close <- function(source, footer) {
    UseMethod(".parquet_close")
}
#' @export
.parquet_close.parquet_local <- function(source, footer) {
    invisible(NULL)
}
#' @export
.parquet_close.parquet_http <- function(source, footer) {
    unlink(footer)
    invisible(NULL)
}

#' @title Tell the user what is about to be read
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @keywords internal
#' @description A remote file is downloaded whole by arrow, so the size is
#'   shown before that happens. A local file needs no notice.
#' @param source  Object from .parquet_source()
#' @param reader  arrow ParquetFileReader over the footer
#' @return Called for side effects
.parquet_notify <- function(source, reader) {
    UseMethod(".parquet_notify")
}
#' @export
.parquet_notify.parquet_local <- function(source, reader) {
    invisible(NULL)
}
#' @export
.parquet_notify.parquet_http <- function(source, reader) {
    if (.message_warnings()) {
        size <- structure(attr(source, "size"), class = "object_size")
        message(
            .conf("messages", "sits_from_parquet_size"),
            format(size, units = "auto", standard = "SI"),
            ", ", format(reader$num_rows, big.mark = ","), " rows"
        )
    }
    invisible(NULL)
}

#' @title Read the whole table
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @keywords internal
#' @description For a URL, arrow downloads the file, once.
#' @param source  Object from .parquet_source()
#' @return Flat table
.parquet_read <- function(source, ...) {
    UseMethod(".parquet_read")
}
#' @export
.parquet_read.parquet_local <- function(source, ...) {
    arrow::read_parquet(unclass(source))
}
#' @export
.parquet_read.parquet_http <- function(source, ..., timeout = NULL) {
    .check_set_caller(".parquet_read_parquet_http")
    # arrow downloads with download.file(), which reads the option, not an
    # argument, so the value is set for this read alone
    if (.has(timeout)) {
        old <- options(timeout = timeout)
        on.exit(options(old), add = TRUE)
    }
    tryCatch(
        arrow::read_parquet(unclass(source)),
        error = function(e) {
            .check_that(FALSE,
                msg = paste(
                    .conf("messages", ".parquet_read_parquet_http"),
                    getOption("timeout"), "seconds -", conditionMessage(e)
                )
            )
        }
    )
}

#' @title Fetch the last bytes of a remote file
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @keywords internal
#' @param url     URL
#' @param nbytes  How many bytes from the end
#' @param ...     Additional parameters to be passed to the request package
#' @return Raw vector
.parquet_remote_tail <- function(url, nbytes, ...) {
    .check_set_caller(".parquet_remote_tail")
    # a large count would reach the header in scientific notation
    nbytes <- format(nbytes, scientific = FALSE, trim = TRUE)
    resp <- .get_request(
        url, headers = list(Range = paste0("bytes=-", nbytes)), ...
    )
    # 200 means the server sent the whole file
    .check_that(.response_status(resp) == 206L)
    .response_body_raw(resp)
}

#' @title Write a stub file with the footer of a remote file
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @keywords internal
#' @description arrow reads the footer from the end, so magic + footer +
#'   trailer is enough to open the file and read its schema.
#' @param url   URL
#' @param size  Size of the remote file, in bytes
#' @param ...   Additional parameters to be passed to the request package
#' @return Path of the stub. The caller removes it.
.parquet_remote_footer <- function(url, size, ...) {
    .check_set_caller(".parquet_remote_footer")
    # 64 KiB is what arrow reads first for a footer
    tail <- .parquet_remote_tail(url, 65536L, ...)
    n <- length(tail)
    # "PARE" is an encrypted footer, which arrow cannot open without a key
    .check_that(n >= 8L && identical(rawToChar(tail[(n - 3L):n]), "PAR1"))
    len <- readBin(
        tail[(n - 7L):(n - 4L)], "integer", size = 4L, endian = "little"
    )
    # the footer cannot be larger than the file, so a length beyond it means
    # the bytes are not a footer. In double, because len + 8 overflows int
    len <- as.numeric(len)
    .check_that(!is.na(len) && len > 0.0 && len + 8.0 <= size)
    if (len + 8.0 > n) {
        tail <- .parquet_remote_tail(url, len + 8.0, ...)
        n <- length(tail)
    }
    .check_that(len + 8.0 <= n)
    stub <- tempfile(fileext = ".parquet")
    writeBin(c(charToRaw("PAR1"), tail[(n - len - 7L):n]), stub)
    stub
}

#' @title Read the sits block from the footer
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @keywords internal
#' @param reader  arrow ParquetFileReader
#' @return Parsed block, or NULL for a file written by another program
.parquet_read_block <- function(reader) {
    block <- reader$GetSchema()$metadata[["sits"]]
    if (!.has(block)) {
        return(NULL)
    }
    block <- jsonlite::fromJSON(block, simplifyVector = FALSE)
    # unknown version: warn and read what we understand
    if (!identical(block[["sits_version"]], .parquet_version)) {
        warning(.conf("messages", "sits_from_parquet_version"), call. = FALSE)
    }
    block
}

#' @title Check the columns against the footer, before any row is read
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @keywords internal
#' @param reader  arrow ParquetFileReader
#' @param block   Parsed block, or NULL
#' @return Called for side effects
.parquet_check_block <- function(reader, block) {
    .check_set_caller(".parquet_check_block")
    cols <- if (.has(block)) {
        c(
            block[["key"]], unlist(block[["sample_columns"]]),
            block[["series"]][["index"]], unlist(block[["series"]][["bands"]]),
            names(block[["nested"]])
        )
    } else {
        .conf("df_sample_columns")
    }
    .check_that(all(cols %in% reader$GetSchema()$names))
}
