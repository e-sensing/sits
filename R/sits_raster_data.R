#' @title Read a block of values retrieved from a set of raster images
#' @name  .sits_raster_data_read
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  cube            input data cube.
#' @param  samples         tibble with samples.
#' @param  extent          bounding box in (i,j) coordinates
#' @param  stats           normalization parameters.
#' @param  filter_fn       smoothing filter to be applied.
#' @param  impute_fn       impute function to replace NA
#' @return A data.table with values for classification.
.sits_raster_data_read <- function(cube,
                                   samples,
                                   extent,
                                   stats,
                                   filter_fn,
                                   impute_fn) {

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
#' @return Matrix with pre-processed values.
.sits_raster_data_preprocess <- function(cube,
                                         bands,
                                         extent,
                                         filter_fn = NULL,
                                         stats = NULL,
                                         impute_fn) {

    # set caller to show in errors
    .check_set_caller(".sits_raster_data_preprocess")

    # get the file information for the cube
    file_info <- .file_info(cube)

    # does the cube have a cloud band?
    cld_band <- .source_cloud()
    if (cld_band %in% sits_bands(cube)) {

        cld_index <- .source_cloud_interp_values(
            source = .cube_source(cube = cube),
            collection = .cube_collection(cube = cube)
        )

        cld_files <- dplyr::filter(file_info, band == cld_band)$path
        clouds <- .raster_read_stack(files  = cld_files, block = extent)

        # get information about cloud bitmask
        if (.source_cloud_bit_mask(
            source = .cube_source(cube = cube),
            collection = .cube_collection(cube = cube))) {

            clouds <- as.matrix(clouds)
            cld_rows <- nrow(clouds)
            clouds <- matrix(bitwAnd(clouds, sum(2 ^ cld_index)),
                             nrow = cld_rows) > 0
        } else {

            clouds <- clouds %in% cld_index
        }

    } else {
        clouds <- NULL
    }

    # read the values from the raster ordered by bands
    values_bands <- purrr::map(bands, function(b) {

        # define the input raster files for band
        bnd_files <- dplyr::filter(file_info, band == b)$path

        # are there bands associated to the files?
        .check_length(
            x = bnd_files,
            len_min = 1,
            msg = paste("no files for band", b)
        )

        # read the values
        values <- .raster_read_stack(files = bnd_files,
                                     block = extent)

        # get the missing values, minimum values and scale factors
        missing_value <- .cube_band_missing_value(cube = cube, band = b)
        minimum_value <- .cube_band_minimum_value(cube = cube, band = b)
        maximum_value <- .cube_band_maximum_value(cube = cube, band = b)

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
            values <- impute_fn(values)
        }

        # scale the data set
        scale_factor <- .cube_band_scale_factor(cube, band = b)
        offset_value <- .cube_band_offset_value(cube = cube, band = b)

        values <- scale_factor * values + offset_value

        # filter the data
        if (!(purrr::is_null(filter_fn))) {
            values <- filter_fn(values)
        }

        # normalize the data
        if (!purrr::is_null(stats)) {
            values <- .sits_ml_normalize_matrix(values, stats, b)
        }

        return(values)

    })

    data <- NULL

    # create a data.table joining the values
    data <- do.call(cbind, values_bands)

    return(data)

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
            collection = .cube_collection(cube = tile))) {

            cld_values <- as.matrix(cld_values)
            cld_rows <- nrow(cld_values)
            cld_values <- matrix(bitwAnd(cld_values, sum(2 ^ cld_index)),
                                 nrow = cld_rows)
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
        values_band <- .cube_extract(cube = tile,
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
                                use.names = FALSE)

            # include information from cloud band
            if (!purrr::is_null(cld_band)) {
                cld_values <- unlist(cld_values[i, start_idx:end_idx],
                                     use.names = FALSE)
                if (.source_cloud_bit_mask(
                    source = .cube_source(cube = tile),
                    collection = .cube_collection(cube = tile)))
                    values_ts[cld_values > 0] <- NA
                else
                    values_ts[cld_values %in% cld_index] <- NA
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

    cube_tile <- .cube_tiles(tile)

    # now we have to transpose the data
    ts_samples <- ts_bands %>%
        purrr::set_names(bands) %>%
        purrr::transpose() %>%
        purrr::map(tibble::as_tibble)


    points$time_series <- purrr::map2(points$time_series,
                                     ts_samples,
                                     dplyr::bind_cols)

    class(points) <- c("sits", class(points))
    return(points)
}
