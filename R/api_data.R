#' @title Dispatch function to get time series from data cubes and cloud
#' services
#' @name .data_get_ts
#' @author Gilberto Camara
#' @keywords internal
#' @noRd
#' @param cube            Data cube from where data is to be retrieved.
#' @param samples         Samples to be retrieved.
#' @param crs             A coordinate reference system of samples.
#'                        The provided crs could be a character
#'                        (e.g, "EPSG:4326" or "WGS84" or a proj4string), or a
#'                        a numeric with the EPSG code (e.g. 4326).
#'                        This parameter only works for 'csv' or data.frame'
#'                        samples. Default is 4326.
#' @param bands           Bands to be retrieved (optional).
#' @param impute_fn       Imputation function to remove NA.
#' @param multicores      Number of threads to process the time series.
#' @param progress        A logical value indicating if a progress bar
#'                        should be shown. Default is \code{FALSE}.
#'
#' @return                A tibble with a set of time series retrieved
#'                        from a data cube.
#'
.data_get_ts <- function(cube,
                         samples, ...,
                         bands,
                         impute_fn,
                         multicores,
                         progress) {
    # Dispatch
    UseMethod(".data_get_ts", cube)
}
#' @name .data_get_ts
#' @keywords internal
#' @noRd
#' @export
.data_get_ts.raster_cube <- function(cube,
                                     samples, ...,
                                     bands,
                                     impute_fn,
                                     multicores,
                                     progress) {
    # Pre-condition
    .check_cube_bands(cube, bands = bands)
    # Is the cloud band available?
    cld_band <- .source_cloud()
    if (cld_band %in% bands) {
        bands <- bands[bands != cld_band]
    } else {
        cld_band <- NULL
    }
    if (.cube_is_base(cube)) {
        bands <- setdiff(bands, .cube_bands(.cube_base_info(cube)))
    }

    # define parallelization strategy
    # find block size
    rast <- .raster_open_rast(.tile_path(cube))
    block <- .raster_file_blocksize(rast)
    # 1st case - split samples by tiles
    if ((.raster_nrows(rast) == block[["nrows"]] &&
        .raster_ncols(rast) == block[["ncols"]]) ||
        inherits(cube, "dem_cube")) {
        # split samples by bands and tile
        ts_tbl <- .data_by_tile(
            cube = cube,
            samples = samples,
            bands = bands,
            impute_fn = impute_fn,
            cld_band = cld_band,
            multicores = multicores,
            progress = progress
        )
    } else {
        # get data by chunks
        ts_tbl <- .data_by_chunks(
            cube = cube,
            samples = samples,
            bands = bands,
            impute_fn = impute_fn,
            cld_band = cld_band,
            multicores = multicores,
            progress = progress
        )
    }
    if (.has(cube[["base_info"]])) {
        # get base info
        cube_base <- .cube_base_info(cube)
        # get bands
        bands_base <- .cube_bands(cube_base)
        # extract data
        base_tbl <- .data_get_ts(
            cube = cube_base,
            samples = samples,
            bands = bands_base,
            impute_fn = impute_fn,
            multicores = multicores,
            progress = progress
        )
        # prepare output data
        base_tbl <- base_tbl |>
                        dplyr::select("longitude", "latitude", "time_series") |>
                        dplyr::rename("base_data" = "time_series")
        # Assuming `ts_tbl` as the source of truth, the size of the following
        # `join` must be the same as the current `ts_tbl`.
        ts_tbl_size <- nrow(ts_tbl)
        # joining samples data from cube and base_cube by longitude / latitude
        ts_tbl <- dplyr::left_join(
            x = ts_tbl,
            y = base_tbl,
            by = c("longitude", "latitude")
        ) |>
            tidyr::drop_na()
        # checking samples consistency
        .data_check(ts_tbl_size, nrow(ts_tbl))
        # add base class (`sits` is added as it is removed in the join above)
        class(ts_tbl) <- unique(c("sits_base", "sits", class(ts_tbl)))
    }
    return(ts_tbl)
}

#' @name .data_get_ts
#' @keywords internal
#' @noRd
#' @export
.data_get_ts.class_cube <- function(cube,
                                    samples, ...,
                                    bands,
                                    crs,
                                    multicores,
                                    progress) {
    # Filter only tiles that intersects with samples
    cube <- .cube_filter_spatial(
        cube = cube,
        roi = .point_as_sf(point = .point(x = samples, crs = crs))
    )
    # pre-condition - check bands
    if (is.null(bands)) {
        bands <- .cube_bands(cube)
    }
    .check_cube_bands(cube, bands = bands)
    # get cubes timeline
    tl <- .cube_timeline(cube)[[1]]
    # create tile-band pairs for parallelization
    tiles_bands <- tidyr::expand_grid(
        tile = .cube_tiles(cube),
        band = bands
    ) |>
        purrr::pmap(function(tile, band) {
            return(list(tile, band))
        })
    # set output_dir
    output_dir <- tempdir()
    if (Sys.getenv("SITS_SAMPLES_CACHE_DIR") != "") {
        output_dir <- Sys.getenv("SITS_SAMPLES_CACHE_DIR")
    }
    # prepare parallelization
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)
    # get the samples in parallel using tile-band combination
    samples_tiles_bands <- .parallel_map(
        tiles_bands,
        function(tile_band) {
            # select tile and band
            tile_id <- tile_band[[1]]
            band <- tile_band[[2]]
            tile <- .select_raster_cube(cube, bands = band, tiles = tile_id)
            # create a hash to store temporary samples file
            hash_bundle <- digest::digest(list(tile, samples), algo = "md5")
            filename <- .file_path(
                "samples", hash_bundle,
                ext = ".rds",
                output_dir = output_dir
            )
            # does the file exist?
            if (file.exists(filename)) {
                tryCatch(
                    { # ensure that the file is not corrupted
                        timeseries <- readRDS(filename)
                        return(timeseries)
                    },
                    error = function(e) {
                        unlink(filename)
                        gc()
                    }
                )
            }
            # get XY
            xy_tb <- .proj_from_latlong(
                longitude = samples[["longitude"]],
                latitude  = samples[["latitude"]],
                crs       = .cube_crs(tile)
            )
            # join lat-long with XY values in a single tibble
            samples <- dplyr::bind_cols(samples, xy_tb)
            # filter the points inside the data cube space-time extent
            samples <- dplyr::filter(
                samples,
                .data[["X"]] > tile[["xmin"]],
                .data[["X"]] < tile[["xmax"]],
                .data[["Y"]] > tile[["ymin"]],
                .data[["Y"]] < tile[["ymax"]],
                .data[["start_date"]] <= as.Date(tl[[length(tl)]]),
                .data[["end_date"]] >= as.Date(tl[[1]])
            )
            # are there points to be retrieved from the cube?
            if (nrow(samples) == 0) {
                return(NULL)
            }
            # create a matrix to extract the values
            xy <- matrix(
                c(samples[["X"]], samples[["Y"]]),
                nrow = nrow(samples),
                ncol = 2
            )
            colnames(xy) <- c("X", "Y")
            # build the sits tibble for the storing the points
            samples_tbl <- slider::slide_dfr(samples, function(point) {
                # get the valid timeline
                dates <- .timeline_during(
                    timeline   = tl,
                    start_date = as.Date(point[["start_date"]]),
                    end_date   = as.Date(point[["end_date"]])
                )
                sample <- tibble::tibble(
                    longitude  = point[["longitude"]],
                    latitude   = point[["latitude"]],
                    start_date = dates[[1]],
                    end_date   = dates[[length(dates)]],
                    label      = point[["label"]],
                    cube       = tile[["collection"]],
                    polygon_id = point[["polygon_id"]]
                )
                # store them in the sample tibble
                sample[["predicted"]] <- list(tibble::tibble(
                    # from 1 to the number of dates (can be more than one)
                    from = dates[[1]], to = dates[[length(dates)]]
                ))
                # return valid row of time series
                return(sample)
            })
            ts <- .ts_get_raster_class(
                tile = tile,
                points = samples_tbl,
                band = "class",
                xy = xy
            )
            ts[["tile"]] <- tile_id
            ts[["#..id"]] <- seq_len(nrow(ts))
            saveRDS(ts, filename)
            return(ts)
        },
        progress = progress
    )
    # reorganise the samples
    ts_tbl <- samples_tiles_bands |>
        dplyr::bind_rows() |>
        tidyr::unnest("predicted") |>
        dplyr::group_by(
            .data[["longitude"]], .data[["latitude"]],
            .data[["start_date"]], .data[["end_date"]],
            .data[["label"]], .data[["cube"]],
            .data[["from"]], .data[["to"]], .data[["tile"]],
            .data[["#..id"]]
        )
    # is there a polygon id? This occurs when we have segments
    if ("polygon_id" %in% colnames(ts_tbl)) {
        ts_tbl <- dplyr::group_by(ts_tbl, .data[["polygon_id"]], .add = TRUE)
    }
    ts_tbl <- ts_tbl |>
        dplyr::summarise(
            dplyr::across(
                dplyr::all_of(bands), stats::na.omit
            )
        ) |>
        dplyr::arrange(.data[["from"]]) |>
        dplyr::ungroup() |>
        tidyr::nest(
            predicted = !!c("from", "to", bands)
        ) |>
        dplyr::select(-c("tile", "#..id"))

    # get the first point that intersect more than one tile
    # eg sentinel 2 mgrs grid
    ts_tbl <- ts_tbl |>
        dplyr::group_by(
            .data[["longitude"]], .data[["latitude"]],
            .data[["start_date"]], .data[["end_date"]],
            .data[["label"]], .data[["cube"]]
        ) |>
        dplyr::slice_head(n = 1) |>
        dplyr::ungroup()

    # recreate hash values
    hash_bundle <- purrr::map_chr(tiles_bands, function(tile_band) {
        tile_id <- tile_band[[1]]
        band <- tile_band[[2]]
        tile <- .select_raster_cube(cube, bands = band, tiles = tile_id)
        digest::digest(list(tile, samples), algo = "md5")
    })
    # recreate file names to delete them
    # samples will be recycled for each hash_bundle
    temp_timeseries <- .file_path(
        "samples", hash_bundle,
        ext = "rds",
        output_dir = output_dir
    )
    # delete temporary rds
    unlink(temp_timeseries)
    gc()
    # check if data has been retrieved
    if (progress) {
        .data_check(nrow(samples), nrow(ts_tbl))
    }
    class(ts_tbl) <- unique(c("predicted", "sits", class(ts_tbl)))
    return(ts_tbl)
}

#' @title Check if all points have been retrieved
#' @name .data_check
#' @keywords internal
#' @noRd
#' @param n_rows_input     Number of rows in input.
#' @param n_rows_output    Number of rows in output.
#'
#' @return No return value, called for side effects.
#'
.data_check <- function(n_rows_input, n_rows_output) {
    # Have all input rows being read?
    if (n_rows_output == 0) {
        message("No points have been retrieved")
        return(invisible(FALSE))
    }

    if (n_rows_output < n_rows_input) {
        message("Some points could not be retrieved")
    } else {
        message("All points have been retrieved")
    }
    return(invisible(n_rows_input))
}

#' @title Extracts the time series average by polygon.
#' @name .data_avg_polygon
#' @keywords internal
#' @noRd
#' @description This function extracts the average of the automatically
#' generated points for each polygon in a shapefile.
#'
#' @param data A sits tibble with points time series.
#'
#' @return A sits tibble with the average of all points by each polygon.
.data_avg_polygon <- function(data) {
    bands <- .samples_bands(data)
    columns_to_avg <- c(bands, "latitude", "longitude")
    data_avg <- data |>
        tidyr::unnest(cols = "time_series") |>
        dplyr::group_by(
            .data[["Index"]],
            .data[["start_date"]],
            .data[["end_date"]],
            .data[["label"]],
            .data[["cube"]],
            .data[["polygon_id"]]
        ) |>
        dplyr::summarise(dplyr::across(!!columns_to_avg, function(x) {
            mean(x, na.rm = TRUE)
        }), .groups = "drop") |>
        tidyr::nest("time_series" = c("Index", dplyr::all_of(bands))) |>
        dplyr::select(!!colnames(data))

    class(data_avg) <- class(data)
    return(data_avg)
}

#' @title get time series from data cubes on tile by tile bassis
#' @name .data_by_tile
#' @keywords internal
#' @noRd
#' @param cube            Data cube from where data is to be retrieved.
#' @param samples         Samples to be retrieved.
#' @param bands           Bands to be retrieved (optional).
#' @param impute_fn       Imputation function to remove NA.
#' @param cld_band        Cloud band
#' @param multicores      Number of threads to process the time series.
#' @param progress        A logical value indicating if a progress bar
#'                        should be shown.
.data_by_tile <- function(cube,
                          samples,
                          bands,
                          impute_fn,
                          cld_band,
                          multicores,
                          progress) {
    .check_set_caller(".data_by_tile")
    # Get cube timeline
    tl <- .cube_timeline(cube)[[1]]
    # Get tile-band combination
    tiles_bands <- .cube_split_tiles_bands(cube = cube, bands = bands)
    # Set output_dir
    output_dir <- tempdir()
    if (Sys.getenv("SITS_SAMPLES_CACHE_DIR") != "") {
        output_dir <- Sys.getenv("SITS_SAMPLES_CACHE_DIR")
    }
    # To avoid open more process than tiles and bands combinations
    if (multicores > length(tiles_bands)) {
        multicores <- length(tiles_bands)
    }
    # Prepare parallelization
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)
    # Get the samples in parallel using tile-band combination
    samples_tiles_bands <- .parallel_map(tiles_bands, function(tile_band) {
        tile_id <- tile_band[[1]]
        band <- tile_band[[2]]

        tile <- .select_raster_cube(
            data = cube,
            bands = c(band, cld_band),
            tiles = tile_id
        )
        hash_bundle <- digest::digest(list(tile, samples), algo = "md5")
        # File to store the samples
        filename <- .file_path(
            "samples", hash_bundle,
            ext = ".rds",
            output_dir = output_dir
        )
        # Does the file exist?
        if (file.exists(filename)) {
            tryCatch(
                { # ensure that the file is not corrupted
                    timeseries <- readRDS(filename)
                    return(timeseries)
                },
                error = function(e) {
                    unlink(filename)
                    gc()
                }
            )
        }
        # get XY
        xy_tb <- .proj_from_latlong(
            longitude = samples[["longitude"]],
            latitude  = samples[["latitude"]],
            crs       = .cube_crs(tile)
        )
        # join lat-long with XY values in a single tibble
        samples <- dplyr::bind_cols(samples, xy_tb)
        # filter the points inside the data cube space-time extent
        samples <- dplyr::filter(
            samples,
            .data[["X"]] > tile[["xmin"]],
            .data[["X"]] < tile[["xmax"]],
            .data[["Y"]] > tile[["ymin"]],
            .data[["Y"]] < tile[["ymax"]],
            .data[["start_date"]] <= as.Date(tl[length(tl)]),
            .data[["end_date"]] >= as.Date(tl[[1]])
        )

        # are there points to be retrieved from the cube?
        if (nrow(samples) == 0) {
            return(NULL)
        }
        # create a matrix to extract the values
        xy <- matrix(
            c(samples[["X"]], samples[["Y"]]),
            nrow = nrow(samples),
            ncol = 2
        )
        colnames(xy) <- c("X", "Y")
        # build the sits tibble for the storing the points
        samples_tbl <- slider::slide_dfr(samples, function(point) {
            # get the valid timeline
            dates <- .timeline_during(
                timeline   = tl,
                start_date = as.Date(point[["start_date"]]),
                end_date   = as.Date(point[["end_date"]])
            )
            sample <- tibble::tibble(
                longitude  = point[["longitude"]],
                latitude   = point[["latitude"]],
                start_date = dates[[1]],
                end_date   = dates[[length(dates)]],
                label      = point[["label"]],
                cube       = tile[["collection"]],
                polygon_id = point[["polygon_id"]]
            )
            # store them in the sample tibble
            sample[["time_series"]] <- list(tibble::tibble(Index = dates))
            # return valid row of time series
            return(sample)
        })
        # extract time series
        ts <- .ts_get_raster_data(
            tile = tile,
            points = samples_tbl,
            bands = band,
            impute_fn = impute_fn,
            xy = xy,
            cld_band = cld_band
        )
        ts[["tile"]] <- tile_id
        ts[["#..id"]] <- seq_len(nrow(ts))
        saveRDS(ts, filename)
        return(ts)
    },
    progress = progress
    )
    # bind rows to get a melted tibble of samples
    ts_tbl <- dplyr::bind_rows(samples_tiles_bands)
    if (!.has_ts(ts_tbl)) {
        warning(.conf("messages", ".data_by_tile"),
                immediate. = TRUE, call. = FALSE
        )
        return(.tibble())
    }
    # reorganise the samples
    ts_tbl <- ts_tbl |>
        tidyr::unnest("time_series") |>
        dplyr::group_by(
            .data[["longitude"]], .data[["latitude"]],
            .data[["start_date"]], .data[["end_date"]],
            .data[["label"]], .data[["cube"]],
            .data[["Index"]], .data[["tile"]], .data[["#..id"]]
        )
    # is there a polygon id? This occurs when we have segments
    if ("polygon_id" %in% colnames(ts_tbl)) {
        ts_tbl <- dplyr::group_by(ts_tbl, .data[["polygon_id"]], .add = TRUE)
    }
    # create time series
    ts_tbl <- ts_tbl |>
        dplyr::reframe(
            dplyr::across(dplyr::all_of(bands), stats::na.omit)
        ) |>
        dplyr::arrange(.data[["Index"]]) |>
        dplyr::ungroup() |>
        tidyr::nest(time_series = !!c("Index", bands)) |>
        dplyr::select(-c("tile", "#..id"))
    # get the first point that intersect more than one tile
    # eg sentinel 2 mgrs grid
    ts_tbl <- ts_tbl |>
        dplyr::group_by(
            .data[["longitude"]], .data[["latitude"]],
            .data[["start_date"]], .data[["end_date"]],
            .data[["label"]], .data[["cube"]]
        ) |>
        dplyr::slice_head(n = 1) |>
        dplyr::ungroup()
    # recreate hash values
    hash_bundle <- purrr::map_chr(tiles_bands, function(tile_band) {
        tile_id <- tile_band[[1]]
        band <- tile_band[[2]]
        tile <- .select_raster_cube(cube, bands = c(band, cld_band),
                                    tiles = tile_id
                                    )
        digest::digest(list(tile, samples), algo = "md5")
    })
    # recreate file names to delete them
    # samples will be recycled for each hash_bundle
    temp_timeseries <- .file_path(
        "samples", hash_bundle,
        ext = "rds",
        output_dir = output_dir
    )
    # delete temporary rds
    unlink(temp_timeseries)
    gc()
    # check if data has been retrieved
    if (progress) {
        .data_check(nrow(samples), nrow(ts_tbl))
    }
    if (!inherits(ts_tbl, "sits")) {
        class(ts_tbl) <- c("sits", class(ts_tbl))
    }
    return(ts_tbl)
}
#' @title get time series from data cubes using chunks
#' @name .data_by_tile
#' @keywords internal
#' @noRd
#' @param cube            Data cube from where data is to be retrieved.
#' @param samples         Samples to be retrieved.
#' @param bands           Bands to be retrieved (optional).
#' @param impute_fn       Imputation function to remove NA.
#' @param cld_band        Cloud band
#' @param multicores      Number of threads to process the time series.
#' @param progress        A logical value indicating if a progress bar
#'                        should be shown.
.data_by_chunks <- function(cube,
                            samples,
                            bands,
                            impute_fn,
                            cld_band,
                            multicores,
                            progress) {
    # Get cube timeline
    tl <- .cube_timeline(cube)[[1]]
    # transform sits tibble to sf
    samples_sf <- sits_as_sf(samples)
    # Get chunks samples
    chunks_samples <- .cube_split_chunks_samples(
        cube = cube, samples_sf = samples_sf
    )
    # Set output_dir
    output_dir <- tempdir()
    if (Sys.getenv("SITS_SAMPLES_CACHE_DIR") != "") {
        output_dir <- Sys.getenv("SITS_SAMPLES_CACHE_DIR")
    }
    # To avoid open more process than chunks and samples combinations
    if (multicores > length(chunks_samples)) {
        multicores <- length(chunks_samples)
    }
    # Prepare parallelization
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)
    # Get the samples in parallel using tile-band combination
    samples_tiles_bands <- .parallel_map(chunks_samples, function(chunk) {
        tile <- .select_raster_cube(
            data = cube,
            bands = c(bands, cld_band),
            tiles = chunk[["tile"]]
        )
        # Get chunk samples
        samples <- chunk[["samples"]][[1]]
        hash_bundle <- digest::digest(list(tile, samples), algo = "md5")
        # Create a file to store the samples
        filename <- .file_path(
            "samples", hash_bundle,
            ext = ".rds",
            output_dir = output_dir
        )
        # Does the file exist?
        if (file.exists(filename)) {
            tryCatch(
                { # ensure that the file is not corrupted
                    timeseries <- readRDS(filename)
                    return(timeseries)
                },
                error = function(e) {
                    unlink(filename)
                    gc()
                }
            )
        }
        # get XY
        xy_tb <- .proj_from_latlong(
            longitude = samples[["longitude"]],
            latitude  = samples[["latitude"]],
            crs       = .cube_crs(tile)
        )
        # join lat-long with XY values in a single tibble
        samples <- dplyr::bind_cols(samples, xy_tb)
        # filter the points inside the data cube space-time extent
        samples <- dplyr::filter(
            samples,
            .data[["X"]] > tile[["xmin"]],
            .data[["X"]] < tile[["xmax"]],
            .data[["Y"]] > tile[["ymin"]],
            .data[["Y"]] < tile[["ymax"]],
            .data[["start_date"]] <= as.Date(tl[[length(tl)]]),
            .data[["end_date"]] >= as.Date(tl[[1]])
        )
        # are there points to be retrieved from the cube?
        if (nrow(samples) == 0) {
            return(NULL)
        }
        # create a matrix to extract the values
        xy <- matrix(
            c(samples[["X"]], samples[["Y"]]),
            nrow = nrow(samples),
            ncol = 2
        )
        colnames(xy) <- c("X", "Y")
        # build the sits tibble for the storing the points
        samples_tbl <- slider::slide_dfr(samples, function(point) {
            # get the valid timeline
            dates <- .timeline_during(
                timeline   = tl,
                start_date = as.Date(point[["start_date"]]),
                end_date   = as.Date(point[["end_date"]])
            )
            sample <- tibble::tibble(
                longitude  = point[["longitude"]],
                latitude   = point[["latitude"]],
                start_date = dates[[1]],
                end_date   = dates[[length(dates)]],
                label      = point[["label"]],
                cube       = tile[["collection"]],
                polygon_id = point[["polygon_id"]]
            )
            # store them in the sample tibble
            sample[["time_series"]] <- list(tibble::tibble(Index = dates))
            # return valid row of time series
            return(sample)
        })
        # extract time series
        ts <- .ts_get_raster_data(
            tile = tile,
            points = samples_tbl,
            bands = bands,
            impute_fn = impute_fn,
            xy = xy,
            cld_band = cld_band
        )
        ts[["tile"]] <- chunk[["tile"]]
        ts[["#..id"]] <- seq_len(nrow(ts))
        saveRDS(ts, filename)
        return(ts)
    }, progress = progress)
    # bind rows to get a melted tibble of samples
    ts_tbl <- dplyr::bind_rows(samples_tiles_bands)
    if (!.has_ts(ts_tbl)) {
        warning(.conf("messages", ".data_by_chunks"),
            immediate. = TRUE, call. = FALSE
        )
        return(.tibble())
    }
    # reorganise the samples
    ts_tbl <- ts_tbl |>
        tidyr::unnest("time_series") |>
        dplyr::group_by(
            .data[["longitude"]], .data[["latitude"]],
            .data[["start_date"]], .data[["end_date"]],
            .data[["label"]], .data[["cube"]],
            .data[["Index"]], .data[["tile"]], .data[["#..id"]]
        )
    # is there a polygon id? This occurs when we have segments
    if ("polygon_id" %in% colnames(ts_tbl)) {
        ts_tbl <- dplyr::group_by(ts_tbl, .data[["polygon_id"]], .add = TRUE)
    }
    # create time series
    ts_tbl <- ts_tbl |>
        dplyr::reframe(
            dplyr::across(dplyr::all_of(bands), stats::na.omit)
        ) |>
        dplyr::arrange(.data[["Index"]]) |>
        dplyr::ungroup() |>
        tidyr::nest(time_series = !!c("Index", bands)) |>
        dplyr::select(-c("tile", "#..id"))
    # get the first point that intersect more than one tile
    # eg sentinel 2 mgrs grid
    ts_tbl <- ts_tbl |>
        dplyr::group_by(
            .data[["longitude"]], .data[["latitude"]],
            .data[["start_date"]], .data[["end_date"]],
            .data[["label"]], .data[["cube"]]
        ) |>
        dplyr::slice_head(n = 1) |>
        dplyr::ungroup()
    # recreate hash values
    hash_bundle <- purrr::map_chr(chunks_samples, function(chunk) {
        tile <- .select_raster_cube(
            data = cube,
            bands = c(bands, cld_band),
            tiles = chunk[["tile"]]
        )
        # Get chunk samples
        samples <- chunk[["samples"]][[1]]
        digest::digest(list(tile, samples), algo = "md5")
    })
    # recreate file names to delete them
    # samples will be recycled for each hash_bundle
    temp_timeseries <- .file_path(
        "samples", hash_bundle,
        ext = "rds",
        output_dir = output_dir
    )
    # delete temporary rds
    unlink(temp_timeseries)
    gc()
    # check if data has been retrieved
    if (progress) {
        .data_check(nrow(samples), nrow(ts_tbl))
    }
    if (!inherits(ts_tbl, "sits")) {
        class(ts_tbl) <- c("sits", class(ts_tbl))
    }
    return(ts_tbl)
}
#' @title get time series from base tiles
#' @name .data_base_tiles
#' @keywords internal
#' @noRd
#' @param cube            Data cube from where data is to be retrieved.
#' @param samples         Samples to be retrieved.
#' @param ts_time         Time series from multitemporal bands
#'
#' @return                Time series information with base tile data
#'
.data_base_tiles <- function(cube, samples) {
    # retrieve values from samples
    #
    # read each tile
    samples <- slider::slide_dfr(cube, function(tile){
        # get XY
        xy_tb <- .proj_from_latlong(
            longitude = samples[["longitude"]],
            latitude  = samples[["latitude"]],
            crs       = .cube_crs(tile)
        )
        # join lat-long with XY values in a single tibble
        samples <- dplyr::bind_cols(samples, xy_tb)
        # filter the points inside the data cube space-time extent
        samples <- dplyr::filter(
            samples,
            .data[["X"]] > tile[["xmin"]],
            .data[["X"]] < tile[["xmax"]],
            .data[["Y"]] > tile[["ymin"]],
            .data[["Y"]] < tile[["ymax"]]
        )

        # are there points to be retrieved from the cube?
        if (nrow(samples) == 0) {
            return(NULL)
        }
        # create a matrix to extract the values
        xy <- matrix(
            c(samples[["X"]], samples[["Y"]]),
            nrow = nrow(samples),
            ncol = 2
        )
        colnames(xy) <- c("X", "Y")

        # get the values of the time series as matrix
        base_bands <- .tile_base_bands(tile)
        samples <- purrr::map_dbl(base_bands, function(band){
            values_base_band <- .tile_base_extract(
                tile = tile,
                band = band,
                xy = xy
            )
            samples[[band]] <- values_base_band
            return(samples)
        })
        return(samples)
    })
}

#' @title function to get class for point in a classified cube
#' @name .data_get_class
#' @author Gilberto Camara
#' @keywords internal
#' @noRd
#' @param cube            Classified data cube from where data is to be retrieved.
#' @param samples         Samples to be retrieved.
#'
#' @return                A tibble with a list of lat/long and respective classes.
#'
.data_get_class <- function(cube, samples){
    data <- slider::slide_dfr(cube, function(tile) {
        # convvert lat/long to tile CRS
        xy_tb <- .proj_from_latlong(
            longitude = samples[["longitude"]],
            latitude  = samples[["latitude"]],
            crs       = .cube_crs(tile)
        )
        # join lat-long with XY values in a single tibble
        samples <- dplyr::bind_cols(samples, xy_tb)
        # filter the points inside the data cube space-time extent
        samples <- dplyr::filter(
            samples,
            .data[["X"]] > tile[["xmin"]],
            .data[["X"]] < tile[["xmax"]],
            .data[["Y"]] > tile[["ymin"]],
            .data[["Y"]] < tile[["ymax"]]
        )

        # are there points to be retrieved from the cube?
        if (nrow(samples) == 0) {
            return(NULL)
        }
        # create a matrix to extract the values
        xy <- matrix(
            c(samples[["X"]], samples[["Y"]]),
            nrow = nrow(samples),
            ncol = 2
        )
        colnames(xy) <- c("X", "Y")

        # open spatial raster object
        rast <- .raster_open_rast(.tile_path(tile))

        # get cells from XY coords
        class_numbers <- dplyr::pull(.raster_extract(rast, xy))
        # convert class numbers in labels
        labels <- .cube_labels(tile)
        classes <- labels[class_numbers]
        # insert classes into samples
        samples[["label"]] <- unname(classes)
        samples <- dplyr::select(samples, .data[["longitude"]],
                                 .data[["latitude"]], .data[["label"]])
        return(samples)
    })
    return(data)
}
