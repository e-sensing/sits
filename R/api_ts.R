#---- time_series ----

.ts_cols <- c("sample_id", "label")

#' @title Check if data is a time series
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @keywords internal
#' @noRd
#' @param x       R object
#' @return TRUE/FALSE
.is_ts <- function(x) {
    "Index" %in% names(x) && is.data.frame(x)
}
#' @title Check if data includes a time series
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @keywords internal
#' @noRd
#' @param x       R object
#' @return TRUE/FALSE
.has_ts <- function(x) {
    "time_series" %in% names(x) && .is_ts(x[["time_series"]][[1]])
}
#' @title Return the time series for a SITS tibble
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @keywords internal
#' @noRd
#' @param x       R object
#' @return time series
.ts <- function(x) {
    .check_set_caller(".ts")
    # Check time_series column
    .check_that(.has_ts(x))
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
#' @title Assigns a new time series for a SITS tibble
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @keywords internal
#' @noRd
#' @param x       R object
#' @param value   New time series
#' @return new R object with time series
`.ts<-` <- function(x, value) {
    .check_set_caller(".ts_assign")
    .check_that(.is_ts(value))
    # Pack time series
    value <- tidyr::nest(value, time_series = -dplyr::all_of(.ts_cols))
    x <- x[.ts_sample_id(value), ]
    x[["time_series"]] <- value[["time_series"]]
    # Return samples
    x
}
#' @title Return the index of a time series
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @keywords internal
#' @noRd
#' @param ts      Time series
#' @return temporal index
.ts_index <- function(ts) {
    .as_date(ts[["Index"]])
}
#' @title Return the sample id of a time series
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @keywords internal
#' @noRd
#' @param ts      Time series
#' @return sample id
.ts_sample_id <- function(ts) {
    ts[["sample_id"]]
}
#' @title Return the bands of a time series
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @keywords internal
#' @noRd
#' @param ts      Time series
#' @return bands
.ts_bands <- function(ts) {
    setdiff(colnames(ts), c(.ts_cols, "Index"))
}
#' @title Select the bands of a time series
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @keywords internal
#' @noRd
#' @param ts      Time series
#' @param bands   Desired bands
#' @return time series with bands
.ts_select_bands <- function(ts, bands) {
    .check_set_caller(".ts_select_bands")
    # Check missing bands
    .check_that(all(bands %in% .ts_bands(ts)))
    # Select the same bands as in the first sample
    ts <- ts[unique(c(.ts_cols, "Index", bands))]
    # Return time series
    ts
}
#' @title Start date of a time series
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @keywords internal
#' @noRd
#' @param ts      Time series
#' @return start date
.ts_start_date <- function(ts) {
    .as_date(unlist(unname(tapply(
        as.character(.ts_index(ts)), .ts_sample_id(ts), min,
        simplify = FALSE
    ))))
}
#' @title Minimum date of a time series
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @keywords internal
#' @noRd
#' @param ts      Time series
#' @return start date
.ts_min_date <- function(ts) {
    min(.ts_index(ts))
}
#' @title End date of a time series
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @keywords internal
#' @noRd
#' @param ts      Time series
#' @return end date
.ts_end_date <- function(ts) {
    .as_date(unlist(unname(tapply(
        as.character(.ts_index(ts)), .ts_sample_id(ts), max,
        simplify = FALSE
    ))))
}
#' @title Minimum date of a time series
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @keywords internal
#' @noRd
#' @param ts      Time series
#' @return end date
.ts_max_date <- function(ts) {
    max(.ts_index(ts))
}
#' @title Filter time series by interval
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @keywords internal
#' @noRd
#' @param ts            Time series
#' @param start_date    Start date
#' @param end_date      End date
#' @return time series filtered by interval
.ts_filter_interval <- function(ts, start_date, end_date) {
    if (.has_not(start_date)) {
        start_date <- .ts_min_date(ts)
    }
    if (.has_not(end_date)) {
        end_date <- .ts_max_date(ts)
    }
    # Filter the interval period
    ts <- ts[.ts_index(ts) >= start_date & .ts_index(ts) <= end_date, ]
    # Return time series
    ts
}
#' @title Values of a time series
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @keywords internal
#' @noRd
#' @param ts      Time series
#' @param bands   Bands to extract values
#' @return values
.ts_values <- function(ts, bands = NULL) {
    .check_set_caller(".ts_values")
    # Get the time series of samples
    bands <- .default(bands, .ts_bands(ts))
    # Check missing bands
    .check_that(all(bands %in% .ts_bands(ts)))
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
#' @param impute_fn         Imputation function to remove NA.
#' @param xy                A matrix with longitude as X and latitude as Y.
#' @param cld_band          Cloud band (if available)
#' @return                  A sits tibble with the time series.
.ts_get_raster_data <- function(tile,
                                points,
                                bands,
                                impute_fn,
                                xy,
                                cld_band = NULL) {
    # set caller to show in errors
    .check_set_caller(".raster_data_get_ts")

    timeline <- .tile_timeline(tile)

    # retrieve values for the cloud band (if available)
    if (.has(cld_band)) {
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
            cld_values <- matrix(
                bitwAnd(cld_values, sum(2^cld_index)),
                nrow = cld_rows
            )
        }
    }
    # Retrieve values on a band by band basis
    # using parallel processing
    ts_bands <- purrr::map(bands, function(band) {
        # get the scale factors, max, min and missing values
        band_params <- .tile_band_conf(tile, band)
        missing_value <- .miss_value(band_params)
        minimum_value <- .min_value(band_params)
        maximum_value <- .max_value(band_params)
        scale_factor <- .scale(band_params)
        offset_value <- .offset(band_params)

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
                start_date = lubridate::as_date(points[["start_date"]][[i]]),
                end_date   = lubridate::as_date(points[["end_date"]][[i]])
            )
            # select the valid dates in the timeline
            start_idx <- which(timeline == t_point[[1]])
            end_idx <- which(timeline == t_point[[length(t_point)]])
            # get only valid values for the timeline
            values_ts <- unlist(values_band[i, start_idx:end_idx],
                use.names = FALSE
            )
            # include information from cloud band
            if (.has(cld_band)) {
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
            # are there NA values? interpolate them
            if (anyNA(values_ts)) {
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
    # include the time series in the XY points
    points[["time_series"]] <- purrr::map2(
        points[["time_series"]],
        ts_samples,
        dplyr::bind_cols
    )
    # set class of time series
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
    .check_set_caller(".ts_get_raster_class")
    # get timeline
    timeline <- .tile_timeline(tile)
    # get timeline length
    timeline_length <- length(timeline)
    # check timeline
    .check_that(timeline_length == 1 || timeline_length == 2)
    # get tile labels
    labels <- .tile_labels(tile)
    # check for labels
    .check_that(all(.has(labels)))
    # get the values of the time series as matrix
    values_band <- .tile_extract(tile = tile, band = band, xy = xy)
    # each row of the values matrix is a spatial point
    traj_lst <- as.list(unname(unlist(values_band)))
    # check if all values fits the labels
    .check_that(all(unique(unlist(traj_lst)) %in% as.numeric(names(labels))))
    # now we have to transpose the data
    traj_samples <- traj_lst |>
        purrr::map(function(x) tibble::tibble(class = labels[x]))
    # include the time series in the XY points
    points[["predicted"]] <- purrr::map2(
        points[["predicted"]],
        traj_samples,
        dplyr::bind_cols
    )
    # set class of time series
    class(points) <- unique(c("predicted", "sits", class(points)))
    return(points)
}
