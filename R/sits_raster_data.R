#' @title Read a block of values retrieved from a set of raster images
#' @name  .sits_classify_data_read
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  tile            Input tile to read data.
#' @param  block           Bounding box in (col, row, ncols, nrows).
#' @param  ml_model        Model trained by \code{\link[sits]{sits_train}}.
#' @param  filter_fn       Smoothing filter function to be applied to the data.
#' @param  impute_fn       Impute function to replace NA.
#' @param  output_dir      Output directory.
#' @return A matrix with values for classification.
.sits_classify_data_read <- function(tile, block, ml_model, impute_fn,
                                     filter_fn, output_dir) {
    # Read and preprocess values from cloud
    # Get cloud values (NULL if not exists)
    cloud_mask <- .tile_cloud_read_block(tile = tile, block = block)
    # Read and preprocess values from each band
    values <- .ml_foreach_band(ml_model, function(band) {

        # Get band values
        values <- .tile_read_block(tile = tile, band = band, block = block)


        #
        # Log here
        #
        .sits_debug_log(
            output_dir = output_dir,
            event = "start_block_data_process",
            key = "process",
            value = "cloud-impute-filter"
        )

        # Remove cloud masked pixels
        if (!is.null(cloud_mask)) {
            values[cloud_mask] <- NA
        }
        # Remove NA pixels
        if (!is.null(impute_fn)) {
            values <- impute_fn(values)
        }
        # Filter the time series
        if (!is.null(filter_fn)) {
            values <- filter_fn(values)
        }
        # Normalize values
        stats <- .ml_stats(ml_model)
        q02 <- .stats_q02_band(stats, band)
        q98 <- .stats_q98_band(stats, band)
        if (!is.null(q02) && !is.null(q98)) {
            values <- normalize_data(values, q02, q98)
        }


        #
        # Log here
        #
        .sits_debug_log(
            output_dir = output_dir,
            event = "end_block_data_process",
            key = "band",
            value = band
        )

        # Return values
        values
    })
    # Compose final values
    values <- do.call(cbind, values)
    colnames(values) <- .ml_attr_names(ml_model)

    return(values)
}
#' @title Extract a time series from raster
#' @name .sits_raster_data_get_ts
#' @keywords internal
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
#' @param output_dir        An output directory to save temporary time series.
#' @return                  A sits tibble with the time series.
.sits_raster_data_get_ts <- function(tile,
                                     points,
                                     bands,
                                     xy,
                                     cld_band = NULL,
                                     impute_fn = sits_impute_linear(),
                                     output_dir = output_dir) {


    # set caller to show in errors
    .check_set_caller(".sits_raster_data_get_ts")

    timeline <- sits_timeline(tile)

    # retrieve values for the cloud band (if available)
    if (!purrr::is_null(cld_band)) {

        # retrieve values that indicate clouds
        cld_index <- .source_cloud_interp_values(
            source = .cube_source(cube = tile),
            collection = .cube_collection(cube = tile)
        )

        # get the values of the time series
        cld_values <- .cube_extract(
            cube = tile,
            band_cube = cld_band,
            xy = xy
        )

        # get information about cloud bitmask
        if (.source_cloud_bit_mask(
            source = .cube_source(cube = tile),
            collection = .cube_collection(cube = tile)
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
        missing_value <- .cube_band_missing_value(cube = tile, band = band)
        minimum_value <- .cube_band_minimum_value(cube = tile, band = band)
        maximum_value <- .cube_band_maximum_value(cube = tile, band = band)
        scale_factor <- .cube_band_scale_factor(cube = tile, band = band)
        offset_value <- .cube_band_offset_value(cube = tile, band = band)

        # get the values of the time series as matrix
        values_band <- .cube_extract(
            cube = tile,
            band_cube = band,
            xy = xy
        )

        # each row of the values matrix is a spatial point
        ts_band_lst <- purrr::map(seq_len(nrow(values_band)), function(i) {
            t_point <- .sits_timeline_during(
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
#' @name .sits_image_classified_get_ts
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Retrieve a set of time series for a raster data cube.
#'
#' @param tile              Metadata describing a tile of a raster data cube.
#' @param points            tibble with points
#' @param band              Band to be retrieved.
#' @param xy                A matrix with longitude as X and latitude as Y.
#' @param output_dir        An output directory to save temporary time series.
#' @return                  A sits tibble with the time series.
.sits_image_classified_get_ts <- function(tile,
                                          points,
                                          band,
                                          xy,
                                          output_dir = output_dir) {

    # set caller to show in errors
    .check_set_caller(".sits_image_classified_get_ts")

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
    values_band <- .cube_extract(
        cube = tile,
        band_cube = band,
        xy = xy
    )

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
