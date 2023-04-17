#' @title Extract a time series from raster
#' @name .raster_data_get_ts
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
#' @param impute_fn         Imputation function for NA values
#' @return                  A sits tibble with the time series.
.raster_data_get_ts <- function(tile,
                                points,
                                bands,
                                xy,
                                cld_band = NULL,
                                impute_fn = sits_impute_linear()) {


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
    ts_samples <- ts_bands %>%
        purrr::set_names(bands) %>%
        purrr::transpose() %>%
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
#' @name .raster_class_get_ts
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
.raster_class_get_ts <- function(tile,
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
    traj_samples <- traj_lst %>%
        purrr::map(function(x) tibble::tibble(class = labels[x]))

    points$predicted <- purrr::map2(
        points$predicted,
        traj_samples,
        dplyr::bind_cols
    )

    class(points) <- unique(c("predicted", "sits", class(points)))
    return(points)
}
