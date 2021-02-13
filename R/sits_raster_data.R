#' @title Read a block of values retrieved from a set of raster bricks
#' @name  .sits_raster_data_read
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  cube            input data cube.
#' @param  samples         tibble with samples.
#' @param  extent          bounding box in (i,j) coordinates
#' @param  stats           normalization parameters.
#' @param  filter          smoothing filter to be applied.
#' @param  impute_fn       impute function to replace NA
#' @param  multicores      number of cores to process the time series.
#' @param  .verbose        print processing information
#' @return A data.table with values for classification.
.sits_raster_data_read <- function(cube,
                                   samples,
                                   extent,
                                   stats,
                                   filter,
                                   impute_fn,
                                   multicores,
                                   .verbose) {

    # get the bands in the same order as the samples
    bands <- sits_bands(samples)

    # read the values from the raster bricks ordered by bands
    values_bands <- purrr::map(bands, function(band) {

        # preprocess the input data
        values <- .sits_raster_data_preprocess(
            cube = cube,
            band_cube = band,
            extent = extent,
            impute_fn = impute_fn,
            stats = stats,
            filter = filter,
            multicores = multicores,
            .verbose = .verbose
        )
        return(values)
    })
    # create a data.table joining the values
    data <- do.call(cbind, values_bands)

    # create two additional columns for prediction
    two_cols <- data.table::data.table(
        "original_row" = rep(1, nrow(data)),
        "reference" = rep("NoClass", nrow(data))
    )
    # join the two columns with the data values
    data <- data.table::as.data.table(cbind(two_cols, data))

    return(data)
}

#' @title Preprocess a set of values retrieved from a raster brick
#' @name  .sits_raster_data_preprocess
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  cube             data cube being processed
#' @param  band_cube        band to be processed
#' @param  extent           extent to be read
#' @param  filter           smoothing filter to be applied.
#' @param  stats            normalization parameters.
#' @param  impute_fn        imputing function to be applied to replace NA
#' @param  multicores       number of cores to process the time series.
#' @param  .verbose         prints information about processing times
#' @return Matrix with pre-processed values.
.sits_raster_data_preprocess <- function(cube,
                                         band_cube,
                                         extent,
                                         filter = NULL,
                                         stats = NULL,
                                         impute_fn,
                                         multicores,
                                         .verbose = FALSE) {

    # get the file information for the cube
    file_info <- cube$file_info[[1]]

    # define the input raster files for band
    bnd_files <- dplyr::filter(file_info, band == band_cube)$path

    # are there bands associated to the files?
    assertthat::assert_that(length(bnd_files) > 0,
        msg = paste0(".sits_raster_data_preprocess:
                                         no files for band ", band_cube)
    )

    # read the values
    values <- .sits_raster_api_read_extent(bnd_files, extent)

    # get the missing values, minimum values and scale factors
    missing_value <- unname(cube$missing_values[[1]][band_cube])
    minimum_value <- unname(cube$minimum_values[[1]][band_cube])
    maximum_value <- unname(cube$maximum_values[[1]][band_cube])

    # correct NA, minimum, maximum, and missing values
    values[values < minimum_value] <- NA
    values[values > maximum_value] <- NA
    values[values == missing_value] <- NA

    # does the cube have a cloud band?
    cld_band <- .sits_config_cloud_band(cube)

    if (cld_band %in% sits_bands(cube)) {
        cld_files <- dplyr::filter(file_info, band == cld_band)$path
        clouds <- .sits_raster_api_read_extent(cld_files, extent)
    }
    else {
          clouds <- NULL
      }

    # change the points under clouds to NA
    if (!purrr::is_null(clouds)) {
        cld_index <- .sits_config_cloud_values(cube)
        values[clouds %in% cld_index] <- NA
    }

    # remove NA pixels
    if (any(is.na(values))) {
        if (.verbose) task_start_time <- lubridate::now()

        values <- .sits_raster_data_na_remove(
            values = values,
            impute_fn = impute_fn,
            multicores = multicores
        )

        if (.verbose) {
            .sits_processing_task_time(
                "Impute NA",
                task_start_time
            )
        }
    }
    # scale the data set
    scale_factor <- cube$scale_factors[[1]][band_cube]
    values <- scale_factor * values

    # filter the data
    if (!(purrr::is_null(filter))) {
        values <- .sits_raster_data_filter(values, filter, multicores)
    }
    # normalize the data
    if (!purrr::is_null(stats)) {
        values <- .sits_normalize_matrix(values, stats, band_cube, multicores)
    }

    values_dt <- data.table::as.data.table(values)
    return(values_dt)
}
#' @title Remove cloud pixels and NA values by imputation
#' @name  .sits_raster_data_na_remove
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  values           matrix of values retrieved from a raster object
#' @param  impute_fn        imputing function to be applied to replace NA
#' @param  multicores       number of cores to process the time series.
#' @return Data.table with pre-processed values.
.sits_raster_data_na_remove <- function(values,
                                        impute_fn,
                                        multicores) {
    cld_remove_block <- function(block) {
        # interpolate NA
        block <- impute_fn(block)
    }
    # use multicores to speed up filtering
    if (multicores > 1) {
        chunks <- .sits_raster_data_split(values, multicores)
        rows <- parallel::mclapply(chunks,
                                   cld_remove_block,
                                   mc.cores = multicores
        )
        values <- do.call(rbind, rows)
    }
    else {
          values <- impute_fn(values)
      }

    return(values)
}
#' @title Filter the time series values in the case of a matrix
#' @name .sits_raster_data_filter
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function filters a matrix.
#'
#' @param  values         matrix of values.
#' @param  filter         Filter function to apply to matrix.
#' @param  multicores     Number of cores.
#' @return Filtered matrix.
.sits_raster_data_filter <- function(values, filter, multicores) {

    # auxiliary function to scale a block of data
    filter_matrix_block <- function(chunk) {
        filtered_block <- filter(chunk)
        return(filtered_block)
    }
    # use multicores to speed up filtering
    if (multicores > 1) {
        chunks <- .sits_raster_data_split(values, multicores)
        rows <- parallel::mclapply(chunks,
                                   filter_matrix_block,
                                   mc.cores = multicores
        )
        values <- do.call(rbind, rows)
    }
    else {
          values <- filter(values)
      }

    return(values)
}

#' @title Split a data.table or a matrix for multicore processing
#' @name .sits_raster_data_split
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function splits a data.table into a
#'              list of chunks for multicore processing.
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
    assertthat::assert_that(NROW(cube) >= 1,
        msg = ".sits_raster_data_get_ts: need a valid metadata for data cube"
    )

    names <- c("longitude", "latitude", "label")
    assertthat::assert_that(all(names %in% colnames(points)),
        msg = ".sits_raster_data_get_ts: data input is not valid"
    )

    # get the scale factors, max, min and missing values
    maximum_values <- unlist(cube$maximum_values)
    minimum_values <- unlist(cube$minimum_values)
    missing_values <- unlist(cube$missing_values)
    scale_factors <- unlist(cube$scale_factors)
    # get the timeline
    timeline <- sits_timeline(cube)
    # make sure we get only the relevant columns
    points <- dplyr::select(
        points, longitude, latitude,
        start_date, end_date, label
    )
    # get XY
    xy_tb <- .sits_latlong_to_proj(points$longitude, points$latitude, cube$crs)
    # join lat-long with XY values in a single tibble
    points <- dplyr::bind_cols(points, xy_tb)
    # filter the points inside the data cube space-time extent
    points <- dplyr::filter(
        points,
        X > cube$xmin & X < cube$xmax &
            Y > cube$ymin & Y < cube$ymax &
            start_date >= as.Date(timeline[1])
    )

    # are there points to be retrieved from the cube?
    if (nrow(points) == 0) {
          return(NULL)
      }

    # build the time index for the data
    samples_lst <- slider::slide(points, function(point) {
      sample <- .sits_tibble()
      sample <- tibble::add_row(sample,
        longitude = point$longitude,
        latitude = point$latitude,
        start_date = as.Date(point$start_date),
        end_date = as.Date(point$end_date),
        label = point$label,
        cube  = cube$name
      )
      # get the valid timeline
      t_idx <- .sits_timeline_indexes(
        timeline = timeline,
        start_date = as.Date(point$start_date),
        end_date = as.Date(point$end_date)
      )
      # select the valid dates in the timeline
      dates <- timeline[t_idx["start_idx"]:t_idx["end_idx"]]
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
        cld_values <- .sits_raster_api_extract(cube, cld_band, xy)
    }
    # Retrieve values on a band by band basis
    ts_bands <- bands %>%
        purrr::map(function(band) {
            # get the values of the time series
            values_band <- .sits_raster_api_extract(cube, band, xy)

            # each row of the values matrix is a spatial point
            ts_band_lst <- seq_len(nrow(values_band)) %>%
                      purrr::map(function(i) {
                        t_idx <- .sits_timeline_indexes(
                          timeline = timeline,
                          start_date = lubridate::as_date(points$start_date[i]),
                          end_date = lubridate::as_date(points$end_date[i])
                        )
                        # select the valid dates in the timeline
                        t_row <- timeline[t_idx["start_idx"]:t_idx["end_idx"]]
                        # get only valid values for the timeline
                        values <- as.numeric(
                          values_band[i, t_idx["start_idx"]:t_idx["end_idx"]]
                        )

                        # include information from cloud band
                        if (!purrr::is_null(cld_band)) {
                          cld_values <- as.numeric(
                            cld_values[i, t_idx["start_idx"]:t_idx["end_idx"]])
                          values[cld_values %in% cld_index] <- NA
                        }
                        # adjust maximum and minimum values
                        values[values == missing_values[band]] <- NA
                        values[values < minimum_values[band]] <- NA
                        values[values > maximum_values[band]] <- NA

                        # are there NA values? interpolate them
                        if (any(is.na(values))) {
                          values <- impute_fn(values)
                        }
                        # correct the values using the scale factor
                        values <- values * scale_factors[band]
                        # create a tibble for each band
                        ts <- tibble::tibble(values = values)
                        colnames(ts) <- c(band)

                        # return the values of one band for point xy
                        return(ts)
                      })
            # return the values of all points xy for one band
            return(ts_band_lst)
        })

    # now we have to untangle the data
    n_bands <- length(bands)
    n_samples <- nrow(points)

    ts_samples <- seq_len(n_samples) %>%
      purrr::map(function(s) {
          b <- 1
          ts_sample <- ts_bands[[b]][[s]]
          while (b < n_bands) {
              b <- b + 1
            ts_sample <- dplyr::bind_cols(ts_sample, ts_bands[[b]][[s]])
          }
          ts_index <- samples[s, ]$time_series[[1]]
          ts <- dplyr::bind_cols(ts_index, ts_sample)
          return(ts)
      })
    # include the time series in the samples
    samples$time_series <- ts_samples

    class(samples) <- c("sits", class(samples))
    return(samples)
}
