#---- time_series ----

.ts_cols <- c("sample_id", "label")

.is_ts <- function(x) {
    "Index" %in% names(x) && is.data.frame(x)
}

.has_ts <- function(x) {
    "time_series" %in% names(x) && .is_ts(x[["time_series"]][[1]])
}

.ts <- function(x) {
    # Check time_series column
    if (!.has_ts(x)) {
        stop("time_series not found")
    }
    # Add sample_id column
    x[["sample_id"]] <- seq_along(x[["time_series"]])
    # Extract time_series from column
    ts <- tidyr::unnest(
        data = x[c(.ts_cols, "time_series")],
        cols = "time_series"
    )
    # Return time series
    ts
}

`.ts<-` <- function(x, value) {
    if (!.is_ts(value)) {
        stop("invalid time series value")
    }
    # Pack time series
    value <- tidyr::nest(value, time_series = -dplyr::all_of(.ts_cols))
    x <- x[.ts_sample_id(value), ]
    x[["time_series"]] <- value[["time_series"]]
    # Return samples
    x
}

.ts_index <- function(ts) {
    .as_date(ts[["Index"]])
}

.ts_sample_id <- function(ts) {
    ts[["sample_id"]]
}

.ts_bands <- function(ts) {
    setdiff(colnames(ts), c(.ts_cols, "Index"))
}

.ts_select_bands <- function(ts, bands) {
    # Check missing bands
    miss_bands <- bands[!bands %in% .ts_bands(ts)]
    if (.has(miss_bands)) {
        stop("band(s) ", .collapse("'", miss_bands, "'"), " not found")
    }
    # Select the same bands as in the first sample
    ts <- ts[unique(c(.ts_cols, "Index", bands))]
    # Return time series
    ts
}

.ts_start_date <- function(ts) {
    # TODO: create a utility function instead. See .by() function
    .as_date(unlist(unname(tapply(
        as.character(.ts_index(ts)), .ts_sample_id(ts), min, simplify = FALSE
    ))))
}

.ts_min_date <- function(ts) {
    min(.ts_index(ts))
}

.ts_end_date <- function(ts) {
    .as_date(unlist(unname(tapply(
        as.character(.ts_index(ts)), .ts_sample_id(ts), max, simplify = FALSE
    ))))
}

.ts_max_date <- function(ts) {
    max(.ts_index(ts))
}

.ts_filter_interval <- function(ts, start_date, end_date) {
    if (!.has(start_date)) {
        start_date <- .ts_min_date(ts)
    }
    if (!.has(end_date)) {
        end_date <- .ts_max_date(ts)
    }
    # Filter the interval period
    ts <- ts[.ts_index(ts) >= start_date & .ts_index(ts) <= end_date, ]
    # Return time series
    ts
}

.ts_values <- function(ts, bands = NULL) {
    # Get the time series of samples
    bands <- .default(bands, .ts_bands(ts))
    # Check missing bands
    miss_bands <- bands[!bands %in% .ts_bands(ts)]
    if (.has(miss_bands)) {
        stop("band(s) ", .collapse("'", miss_bands, "'"), " not found")
    }
    ts[bands]
}
#' @title Extract a time series from raster
#' @name .ts_get_raster_data
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Retrieve a set of time series for a raster data cube.
#'
#' @param tile              Metadata describing a tile of a raster data cube.
#' @param points            tibble with points
#' @param bands             Bands to be retrieved.
#' @param xy                A matrix with longitude as X and latitude as Y.
#' @param cld_band          Cloud band (if available)
#' @return                  A sits tibble with the time series.
.ts_get_raster_data <- function(tile,
                                points,
                                bands,
                                xy,
                                cld_band = NULL) {


    # set caller to show in errors
    .check_set_caller(".raster_data_get_ts")

    timeline <- sits_timeline(tile)

    # retrieve values for the cloud band (if available)
    if (!purrr::is_null(cld_band)) {

        # retrieve values that indicate clouds
        cld_index <- .source_cloud_interp_values(
            source = .tile_source(tile),
            collection = .tile_collection(tile)
        )

        # get the values of the time series
        cld_values <- .tile_extract(
            tile = tile,
            band = cld_band,
            xy = xy
        )

        # get information about cloud bitmask
        if (.source_cloud_bit_mask(
            source = .tile_source(tile),
            collection = .tile_collection(tile)
        )) {
            cld_values <- as.matrix(cld_values)
            cld_rows <- nrow(cld_values)
            cld_values <- matrix(bitwAnd(cld_values, sum(2^cld_index)),
                                 nrow = cld_rows
            )
        }
    }

    # Retrieve values on a band by band basis
    # using parallel processing
    ts_bands <- purrr::map(bands, function(band) {

        # get the scale factors, max, min and missing values
        band_params   <- .tile_band_conf(tile, band)
        missing_value <- .miss_value(band_params)
        minimum_value <- .min_value(band_params)
        maximum_value <- .max_value(band_params)
        scale_factor  <- .scale(band_params)
        offset_value  <- .offset(band_params)

        # get the values of the time series as matrix
        values_band <- .tile_extract(
            tile = tile,
            band = band,
            xy = xy
        )

        # each row of the values matrix is a spatial point
        ts_band_lst <- purrr::map(seq_len(nrow(values_band)), function(i) {
            t_point <- .timeline_during(
                timeline   = timeline,
                start_date = lubridate::as_date(points$start_date[[i]]),
                end_date   = lubridate::as_date(points$end_date[[i]])
            )

            # select the valid dates in the timeline
            start_idx <- which(timeline == t_point[[1]])
            end_idx <- which(timeline == t_point[[length(t_point)]])

            # get only valid values for the timeline
            values_ts <- unlist(values_band[i, start_idx:end_idx],
                                use.names = FALSE
            )

            # include information from cloud band
            if (!purrr::is_null(cld_band)) {
                cld_values <- unlist(cld_values[i, start_idx:end_idx],
                                     use.names = FALSE
                )
                if (.source_cloud_bit_mask(
                    source = .cube_source(cube = tile),
                    collection = .cube_collection(cube = tile)
                )) {
                    values_ts[cld_values > 0] <- NA
                } else {
                    values_ts[cld_values %in% cld_index] <- NA
                }
            }

            # adjust maximum and minimum values
            values_ts[values_ts == missing_value] <- NA
            values_ts[values_ts < minimum_value] <- NA
            values_ts[values_ts > maximum_value] <- NA

            # use linear imputation
            impute_fn = .impute_linear()
            # are there NA values? interpolate them
            if (any(is.na(values_ts))) {
                values_ts <- impute_fn(values_ts)
            }

            # correct the values using the scale factor
            values_ts <- values_ts * scale_factor + offset_value

            # return the values of one band for point xy
            return(values_ts)
        })

        # return the values of all points xy for one band
        return(ts_band_lst)
    })

    # now we have to transpose the data
    ts_samples <- ts_bands |>
        purrr::set_names(bands) |>
        purrr::transpose() |>
        purrr::map(tibble::as_tibble)


    points$time_series <- purrr::map2(
        points$time_series,
        ts_samples,
        dplyr::bind_cols
    )

    class(points) <- c("sits", class(points))
    return(points)
}

#' @title Extract a time series from raster
#' @name .ts_get_raster_class
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Retrieve a set of time series for a raster data cube.
#'
#' @param tile              Metadata describing a tile of a raster data cube.
#' @param points            tibble with points
#' @param band              Band to be retrieved.
#' @param xy                A matrix with longitude as X and latitude as Y.
#' @return                  A sits tibble with the time series.
.ts_get_raster_class <- function(tile,
                                 points,
                                 band,
                                 xy) {

    # set caller to show in errors
    .check_set_caller(".raster_class_get_ts")

    # get timeline
    timeline <- sits_timeline(tile)

    # check timeline
    .check_length(
        x = timeline,
        len_min = 2,
        len_max = 2,
        msg = "invalid classified timeline"
    )

    # get tile labels
    labels <- sits_labels(tile)

    # check for labels
    .check_null(
        x = labels,
        msg = "tiles should have labels field defined"
    )

    # get the values of the time series as matrix
    values_band <- .tile_extract(tile = tile, band = band, xy = xy)

    # each row of the values matrix is a spatial point
    traj_lst <- as.list(unname(unlist(values_band)))

    # check if all values fits the labels
    max_label_index <- max(unlist(traj_lst))
    .check_that(
        x = max_label_index <= length(labels),
        local_msg = paste(
            "cube should have at least", max_label_index, "labels"
        ),
        msg = "pixel values do not correspond to any label"
    )

    # now we have to transpose the data
    traj_samples <- traj_lst |>
        purrr::map(function(x) tibble::tibble(class = labels[x]))

    points$predicted <- purrr::map2(
        points$predicted,
        traj_samples,
        dplyr::bind_cols
    )

    class(points) <- unique(c("predicted", "sits", class(points)))
    return(points)
}
