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
#' @param multicores      Number of threads to process the time series.
#' @param progress        A logical value indicating if a progress bar
#'                        should be shown. Default is \code{FALSE}.
#'
#' @return                A tibble with a set of time series retrieved
#'                        from a data cube.
#'
.data_get_ts <- function(cube,
                         samples, ...,
                         bands = NULL,
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
                                     multicores,
                                     progress) {

    # Pre-conditions
    if (is.null(bands)) {
        bands <- .cube_bands(cube)
    }
    .check_cube_bands(cube, bands = bands)

    # Is the cloud band available?
    cld_band <- .source_cloud()

    if (cld_band %in% bands) {
        bands <- bands[bands != cld_band]
    } else {
        cld_band <- NULL
    }

    # get cubes timeline
    tl <- sits_timeline(cube)

    tiles_bands <- tidyr::expand_grid(tile = .cube_tiles(cube),
                                      band = bands) |>
        purrr::pmap(function(tile, band) {
            return(list(tile, band))
        })
    # set output_dir
    output_dir <- tempdir()
    if (Sys.getenv("SITS_SAMPLES_CACHE_DIR") != "")
        output_dir <- Sys.getenv("SITS_SAMPLES_CACHE_DIR")
    # prepare parallelization
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)

    samples_tiles_bands <- .parallel_map(
        tiles_bands,
        function(tile_band) {
            tile_id <- tile_band[[1]]
            band <- tile_band[[2]]

            tile <- sits_select(cube,
                                bands = c(band, cld_band),
                                tiles = tile_id)
            hash_bundle <- digest::digest(list(tile, samples), algo = "md5")
            # create a file to store the samples
            filename <- .file_path(
                "samples", hash_bundle,
                ext = ".rds",
                output_dir = output_dir
            )
            # does the file exist?
            if (file.exists(filename)) {
                tryCatch({
                    # ensure that the file is not corrupted
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
                .data[["X"]] > tile$xmin & .data[["X"]] < tile$xmax &
                    .data[["Y"]] > tile$ymin & .data[["Y"]] < tile$ymax &
                    .data[["start_date"]] <= as.Date(tl[length(tl)]) &
                    .data[["end_date"]] >= as.Date(tl[1])
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
                sample$time_series <- list(tibble::tibble(Index = dates))
                # return valid row of time series
                return(sample)
            })

            # extract time series
            ts <- .ts_get_raster_data(
                tile = tile,
                points = samples_tbl,
                bands = band,
                xy = xy,
                cld_band = cld_band
            )
            ts[["tile"]] <- tile_id
            ts[["#..id"]] <- seq_len(nrow(ts))
            saveRDS(ts, filename)

            return(ts)
        }, progress = progress)

    ts_tbl <- dplyr::bind_rows(samples_tiles_bands)

    if (!.has_ts(ts_tbl)) {
        warning(
            "No time series were extracted. ",
            "Check your samples and your input cube",
            immediate. = TRUE, call. = FALSE
        )
        return(.tibble())
    }

    ts_tbl <- ts_tbl |>
        tidyr::unnest("time_series") |>
        dplyr::group_by(
            .data[["longitude"]], .data[["latitude"]],
            .data[["start_date"]], .data[["end_date"]],
            .data[["label"]], .data[["cube"]],
            .data[["Index"]], .data[["tile"]], .data[["#..id"]]
        )

    if ("polygon_id" %in% colnames(ts_tbl)) {
        ts_tbl <- dplyr::group_by(ts_tbl, .data[["polygon_id"]], .add = TRUE)
    }

    ts_tbl <- ts_tbl |>
        dplyr::reframe(
            dplyr::across(dplyr::all_of(bands), stats::na.omit)) |>
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
            .data[["label"]], .data[["cube"]]) |>
        dplyr::slice_head(n = 1) |>
        dplyr::ungroup()

    # recreate hash values
    hash_bundle <- purrr::map_chr(tiles_bands, function(tile_band) {
        tile_id <- tile_band[[1]]
        band <- tile_band[[2]]
        tile <- sits_select(cube, bands = c(band, cld_band), tiles = tile_id)
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
    if (progress)
        .data_check(nrow(samples), nrow(ts_tbl))

    if (!inherits(ts_tbl, "sits")) {
        class(ts_tbl) <- c("sits", class(ts_tbl))
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
                                    crs = 4326,
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
    tl <- sits_timeline(cube)
    # create tile-band pairs for parallelization
    tiles_bands <- tidyr::expand_grid(tile = .cube_tiles(cube),
                                      band = bands) |>
        purrr::pmap(function(tile, band) {
            return(list(tile, band))
        })
    # set output_dir
    output_dir <- tempdir()
    if (Sys.getenv("SITS_SAMPLES_CACHE_DIR") != "")
        output_dir <- Sys.getenv("SITS_SAMPLES_CACHE_DIR")
    # prepare parallelization
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)

    samples_tiles_bands <- .parallel_map(
        tiles_bands,
        function(tile_band) {

            # select tile and band
            tile_id <- tile_band[[1]]
            band <- tile_band[[2]]
            tile <- sits_select(cube, bands = band, tiles = tile_id)
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
                .data[["X"]] > tile$xmin & .data[["X"]] < tile$xmax &
                    .data[["Y"]] > tile$ymin & .data[["Y"]] < tile$ymax &
                    .data[["start_date"]] <= as.Date(tl[length(tl)]) &
                    .data[["end_date"]] >= as.Date(tl[1])
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
                sample$predicted <- list(tibble::tibble(
                    from = dates[[1]], to = dates[[2]])
                )
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
        }, progress = progress)

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

    if ("polygon_id" %in% colnames(ts_tbl)) {
        ts_tbl <- dplyr::group_by(ts_tbl, .data[["polygon_id"]], .add = TRUE)
    }

    ts_tbl <- ts_tbl |>
        dplyr::summarise(
            dplyr::across(dplyr::all_of(bands), stats::na.omit)) |>
        dplyr::arrange(.data[["from"]]) |>
        dplyr::ungroup() |>
        tidyr::nest(predicted = !!c("from", "to", bands)) |>
        dplyr::select(-c("tile", "#..id"))

    # get the first point that intersect more than one tile
    # eg sentinel 2 mgrs grid
    ts_tbl <- ts_tbl |>
        dplyr::group_by(.data[["longitude"]], .data[["latitude"]],
                        .data[["start_date"]], .data[["end_date"]],
                        .data[["label"]], .data[["cube"]]) |>
        dplyr::slice_head(n = 1) |>
        dplyr::ungroup()

    # recreate hash values
    hash_bundle <- purrr::map_chr(tiles_bands, function(tile_band) {
        tile_id <- tile_band[[1]]
        band <- tile_band[[2]]
        tile <- sits_select(cube, bands = band, tiles = tile_id)
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
    if (progress)
        .data_check(nrow(samples), nrow(ts_tbl))

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
    bands <- sits_bands(data)
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
