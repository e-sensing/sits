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
#' @description Identifiers are written as `<entity>:id`, where the entity is
#'   the domain that assigned the number. Keeps counters that mean different
#'   things from sharing a name.
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
        # a null geometry is the GeoParquet way of saying there is no
        # location; patterns are the case
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
    # patterns carry no location, so there is nothing to declare
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
    # I() keeps length-one vectors as JSON arrays under auto_unbox, so that a
    # strict reader in another language does not get a scalar where the
    # format declares a list
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
    # sample-level columns keep their order, minus the nested ones
    sample_cols <- setdiff(colnames(data), nested)
    flat <- data[sample_cols]
    flat[["sample:id"]] <- seq_len(nrow(flat))
    # the remaining nested columns keep their own cardinality: `predicted`
    # holds one row per classified interval, which is unrelated to the number
    # of dates in the series. Flattening them would need a cartesian product,
    # so they stay nested as list<struct>, which Parquet stores natively
    nested_kept <- list()
    for (col in extra_cols) {
        inner <- data[[col]]
        flat[[col]] <- inner
        nested_kept[[col]] <- I(colnames(inner[[1L]]))
    }
    # unnest the series, keeping empty ones as a single NA row
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
    # rename identifier columns to the <entity>:id convention
    old <- unname(.parquet_id_map)
    new <- names(.parquet_id_map)
    hit <- old %in% colnames(flat)
    if (any(hit)) {
        flat <- dplyr::rename(flat, !!!stats::setNames(old[hit], new[hit]))
        sample_cols[match(old[hit], sample_cols)] <- new[hit]
    }
    # geometry, derived from the coordinates and ignored on read
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
#' @description Used when a file carries no sits metadata block. Everything
#'   else, once there is an index column, is taken to be a band.
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
#' @description Builds the same block that would have been written, so that
#'   the rebuilding path is the same one used for files that carry it.
#'   Resolves only the classes whose signature does not depend on the
#'   denormalisation convention of the writer.
#' @param tbl  Flat table read from the file
#' @return Block, as a list
.parquet_infer <- function(tbl) {
    .check_set_caller(".parquet_infer")
    cols <- colnames(tbl)
    # the gate: without these it is not a set of samples
    .check_that(all(.conf("df_sample_columns") %in% cols))
    index <- if ("Index" %in% cols) "Index" else NULL
    nested_cols <- .parquet_nested_cols(tbl)
    bands <- if (.has(index)) {
        setdiff(cols, c(.parquet_sample_level, index, nested_cols))
    } else {
        character(0L)
    }
    sample_cols <- setdiff(cols, c(index, bands, nested_cols, "geometry"))
    # a key, when the file does not carry one: rows of the same sample are
    # written consecutively, so a run of identical sample-level values is
    # one sample
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
#' @description Most specific first. Only classes whose signature is carried
#'   by the data itself are resolved; the ones that depend on the writer\'s
#'   denormalisation are not guessed, and fall through to "sits".
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
    # classes below always carry a series; without one they cannot apply
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
    # one row per sample, in order of first occurrence
    ids <- unique(tbl[[key]])
    first <- match(ids, tbl[[key]])
    data <- tbl[first, sample_cols, drop = FALSE]
    # rebuild the series
    series <- block[["series"]]
    if (.has(series)) {
        bands <- unlist(series[["bands"]])
        cols <- c(series[["index"]], bands)
        parts <- split(tbl[cols], factor(tbl[[key]], levels = ids))
        data[[series[["column"]]]] <- purrr::map(parts, function(ts) {
            if (nrow(ts) == 1L && is.na(ts[[1L]][[1L]])) ts[0L, ] else ts
        })
    }
    # the nested columns come back as list<struct>: one table per sample,
    # with whatever number of rows it had
    for (col in names(block[["nested"]])) {
        data[[col]] <- purrr::map(tbl[[col]][first], tibble::as_tibble)
    }
    # identifiers back to their sits names
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
