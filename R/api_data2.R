.get_data_by_tile <- function(cube,
                              samples,
                              bands,
                              cld_band,
                              multicores,
                              progress) {
    # Get cube timeline
    tl <- sits_timeline(cube)
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

        tile <- sits_select(
            data = cube,
            bands = c(band, cld_band),
            tiles = tile_id
        )
        hash_bundle <- digest::digest(list(tile, samples), algo = "md5")
        # Create a file to store the samples
        filename <- .file_path(
            "samples", hash_bundle,
            ext = ".rds",
            output_dir = output_dir
        )
        # Does the file exist?
        if (.ts_is_valid(filename)) {
            ts <- readRDS(filename)
            return(ts)
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
    },
    progress = progress
    )
    # bind rows to get a melted tibble of samples
    ts_tbl <- dplyr::bind_rows(samples_tiles_bands)
    if (!.has_ts(ts_tbl)) {
        warning(
            "No time series were extracted. ",
            "Check your samples and your input cube",
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
    if (progress) {
        .data_check(nrow(samples), nrow(ts_tbl))
    }
    if (!inherits(ts_tbl, "sits")) {
        class(ts_tbl) <- c("sits", class(ts_tbl))
    }
    return(ts_tbl)
}

.get_data_by_chunks <- function(cube,
                                samples,
                                bands,
                                cld_band,
                                multicores,
                                progress) {
    # Get cube timeline
    tl <- sits_timeline(cube)
    # Get chunks samples
    chunks_samples <- .cube_split_chunks_samples(cube = cube, samples = samples)
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
        tile <- sits_select(
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
        if (.ts_is_valid(filename)) {
            ts <- readRDS(filename)
            return(ts)
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
            bands = bands,
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
        warning(
            "No time series were extracted. ",
            "Check your samples and your input cube",
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
        tile <- sits_select(
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
