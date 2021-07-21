#' @title Read a block of values retrieved from a set of raster images
#' @name  .sits_raster_data_read
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  cube            input data cube.
#' @param  samples         tibble with samples.
#' @param  extent          bounding box in (i,j) coordinates
#' @param  stats           normalization parameters.
#' @param  filter_fn          smoothing filter to be applied.
#' @param  impute_fn       impute function to replace NA
#' @param  interp_fn       function to interpolate points from cube to
#'                         match samples
#' @param  compose_fn      function to compose points from cube to match samples
#' @return A data.table with values for classification.
.sits_raster_data_read <- function(cube,
                                   samples,
                                   extent,
                                   stats,
                                   filter_fn,
                                   impute_fn,
                                   interp_fn,
                                   compose_fn) {

    # get the bands in the same order as the samples
    bands <- sits_bands(samples)

    # preprocess the input data
    data <- .sits_raster_data_preprocess(
        cube      = cube,
        bands     = bands,
        extent    = extent,
        impute_fn = impute_fn,
        stats     = stats,
        filter_fn = filter_fn
    )

    # create two additional columns for prediction
    two_cols <- data.table::data.table(
        "original_row" = rep(1, nrow(data)),
        "reference" = rep("NoClass", nrow(data))
    )

    # join the two columns with the data values
    data <- data.table::as.data.table(cbind(two_cols, data))

    # get the attribute names set column names for DT
    colnames(data) <- names(.sits_distances(samples[1, ]))

    return(data)
}

#' @title Preprocess a set of values retrieved from a raster
#' @name  .sits_raster_data_preprocess
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  cube             data cube being processed
#' @param  bands            bands to be processed
#' @param  extent           extent to be read
#' @param  filter_fn        smoothing filter to be applied.
#' @param  stats            normalization parameters.
#' @param  impute_fn        imputing function to be applied to replace NA
#' @param  .verbose         prints information about processing times
#' @return Matrix with pre-processed values.
.sits_raster_data_preprocess <- function(cube,
                                         bands,
                                         extent,
                                         filter_fn = NULL,
                                         stats = NULL,
                                         impute_fn,
                                         .verbose = FALSE) {

    # get the file information for the cube
    file_info <- cube$file_info[[1]]

    # does the cube have a cloud band?
    cld_band <- .sits_config_cloud_band(cube)
    if (cld_band %in% sits_bands(cube)) {

        cld_index <- .sits_config_cloud_values(cube)
        cld_files <- dplyr::filter(file_info, band == cld_band)$path
        clouds <- .sits_raster_api_read_stack(files  = cld_files,
                                              block = extent) %in% cld_index
    } else {
        clouds <- NULL
    }

    # read the values from the raster ordered by bands
    values_bands <- purrr::map(bands, function(band_cube) {

        # define the input raster files for band
        bnd_files <- dplyr::filter(file_info, band == band_cube)$path

        # are there bands associated to the files?
        assertthat::assert_that(
            length(bnd_files) > 0,
            msg = paste(".sits_raster_data_preprocess: no files for band",
                        band_cube)
        )

        # read the values
        values <- .sits_raster_api_read_stack(files = bnd_files,
                                              block = extent)

        # get the missing values, minimum values and scale factors
        missing_value <- .sits_config_missing_values(cube, band_cube)
        minimum_value <- .sits_config_minimum_values(cube, band_cube)
        maximum_value <- .sits_config_maximum_values(cube, band_cube)

        # correct NA, minimum, maximum, and missing values
        values[values < minimum_value] <- NA
        values[values > maximum_value] <- NA
        values[values == missing_value] <- NA

        # change the points under clouds to NA
        if (!purrr::is_null(clouds)) {
            values[clouds] <- NA
        }

        # remove NA pixels
        if (!purrr::is_null(impute_fn) && any(is.na(values))) {
            if (.verbose) task_start_time <- lubridate::now()

            values <- impute_fn(values)

            if (.verbose) {
                .sits_processing_task_time(
                    "Impute NA",
                    task_start_time
                )
            }
        }

        # scale the data set
        scale_factor <- .sits_config_scale_factors(cube, band_cube)
        values <- scale_factor * values

        # filter the data
        if (!(purrr::is_null(filter_fn))) {
            values <- filter_fn(values)
        }

        # normalize the data
        if (!purrr::is_null(stats)) {
            values <- .sits_normalize_matrix(values, stats, band_cube)
        }

        #values_dt <- data.table::as.data.table(values)
        #return(values_dt)
        return(values)

    })

    data <- NULL

    # create a data.table joining the values
    data <- do.call(cbind, values_bands)

    return(data)

}

#' @title Split a data.table or a matrix for parallel processing
#' @name .sits_raster_data_split
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function splits a data.table into a
#'              list of chunks for parallel processing.
#'
#' @param data             Data (data.table or matrix).
#' @param ncores           Number of cores for processing.
#' @return                 List of pairs of positions (first row, last row)
#'                         to be assigned to each core.
#'
.sits_raster_data_split <- function(data, ncores) {
    # number of rows in the data
    nrows <- nrow(data)
    # find the number of rows per core
    step <- ceiling(nrows / ncores)

    # create a vector with the initial rows per block
    blocks <- seq(from = 1, to = nrows, by = step)

    # fill the list with the initial and final row per block
    block_lst <- purrr::map2(blocks, 1:ncores, function(blk, i) {
        start <- blk
        end <- start + step - 1
        if (i == ncores) {
            end <- nrows
        }
        return(data[start:end, ])
    })
    return(block_lst)
}

#' @title Extract a time series from raster
#' @name .sits_raster_data_get_ts
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Retrieve a set of time series for a raster data cube.
#'
#' @param cube              Metadata describing a raster data cube.
#' @param points            tibble with points
#' @param bands             Bands to be retrieved.
#' @param cld_band          Cloud band (if available)
#' @param impute_fn         Imputation function for NA values
#' @return                  A sits tibble with the time series.
.sits_raster_data_get_ts <- function(cube, points, bands,
                                     cld_band = NULL,
                                     impute_fn = sits_impute_linear()) {

    # ensure metadata tibble exists
    assertthat::assert_that(
        nrow(cube) >= 1,
        msg = ".sits_raster_data_get_ts: need a valid metadata for data cube"
    )

    names <- c("longitude", "latitude", "label")
    assertthat::assert_that(
        all(names %in% colnames(points)),
        msg = ".sits_raster_data_get_ts: data input is not valid"
    )

    # get the scale factors, max, min and missing values
    missing_values <- .sits_config_missing_values(cube, bands)
    minimum_values <- .sits_config_minimum_values(cube, bands)
    maximum_values <- .sits_config_maximum_values(cube, bands)
    scale_factors <- .sits_config_scale_factors(cube, bands)

    # get the timeline
    timeline <- sits_timeline(cube)

    # make sure we get only the relevant columns
    points <- dplyr::select(points, longitude, latitude,
                            start_date, end_date, label)

    # get XY
    xy_tb <- .sits_latlong_to_proj(
        longitude = points$longitude,
        latitude  = points$latitude,
        crs       = cube$crs
    )

    # join lat-long with XY values in a single tibble
    points <- dplyr::bind_cols(points, xy_tb)

    # filter the points inside the data cube space-time extent
    points <- dplyr::filter(
        points,
        X > cube$xmin & X < cube$xmax &
            Y > cube$ymin & Y < cube$ymax &
            start_date <= as.Date(timeline[length(timeline)]) &
            end_date >= as.Date(timeline[1])
    )

    # are there points to be retrieved from the cube?
    if (nrow(points) == 0) {
        return(NULL)
    }

    # build the time index for the data
    samples_lst <- slider::slide(points, function(point) {

        # get the valid timeline
        dates <- .sits_timeline_during(
            timeline   = timeline,
            start_date = as.Date(point$start_date),
            end_date   = as.Date(point$end_date)
        )
        sample <- tibble::tibble(
            longitude  = point$longitude,
            latitude   = point$latitude,
            start_date = dates[[1]],
            end_date   = dates[[length(dates)]],
            label      = point$label,
            cube       = cube$name
        )

        # put them on a tibble
        ts <- tibble::tibble(Index = dates)

        # store them in the sample tibble
        sample$time_series <- list(ts)

        # return valid row of time series
        return(sample)
    })

    # get all the time series (without the values)
    samples <- dplyr::bind_rows(samples_lst)

    # create a matrix to extract the values
    xy <- matrix(c(points$X, points$Y), nrow = nrow(points), ncol = 2)
    colnames(xy) <- c("X", "Y")

    # retrieve values for the cloud band (if available)
    if (!purrr::is_null(cld_band)) {

        # retrieve values that indicate clouds
        cld_index <- .sits_config_cloud_values(cube)

        # get the values of the time series (terra object)
        cld_values <- .sits_cube_extract(cube, cld_band, xy)

        # get information about cloud bitmask
        cld_mask <- .sits_config_cloud_bitmask(cube)

        # get bitmask values
        if (cld_mask)
            cld_values <- .sits_bitmask_values(cld_values, cld_index)
    }

    # Retrieve values on a band by band basis
    # using parallel processing
    ts_bands <- .sits_parallel_map(bands, function(band) {

            # get the values of the time series as matrix
            values_band <- .sits_cube_extract(cube, band, xy)

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
                                    use.names = FALSE)

                # include information from cloud band
                if (!purrr::is_null(cld_band)) {
                    cld_values <- unlist(cld_values[i, start_idx:end_idx],
                                         use.names = FALSE)
                    values_ts[cld_values %in% cld_index] <- NA
                }

                # adjust maximum and minimum values
                values_ts[values_ts == missing_values[[band]]] <- NA
                values_ts[values_ts < minimum_values[[band]]] <- NA
                values_ts[values_ts > maximum_values[[band]]] <- NA

                # are there NA values? interpolate them
                if (any(is.na(values_ts))) {
                    values_ts <- impute_fn(values_ts)
                }

                # correct the values using the scale factor
                values_ts <- values_ts * scale_factors[[band]]

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

    samples$time_series <- purrr::map2(samples$time_series,
                                       ts_samples,
                                       dplyr::bind_cols)

    class(samples) <- c("sits", class(samples))
    return(samples)
}
