#' @title Extract set of time series from supercells
#'
#' @name .segments_get_summary
#' @keywords internal
#' @noRd
#' @description     Using the segments as polygons, get all time series
#'
#' @param cube       regular data cube
#' @param segments   polygons produced by sits_segments
#' @param bands      bands used in time series
#' @param aggreg_fn  Function to compute a summary of each segment
#' @param pol_id     ID attribute for polygons.
#' @param multicores Number of cores to use for processing
#' @param progress   Show progress bar?
#'
.segments_get_summary <- function(cube,
                                  segments,
                                  bands,
                                  aggreg_fn,
                                  pol_id,
                                  multicores,
                                  progress) {
    # verify if exactextractr is installed
    .check_require_packages("exactextractr")
    # get start and end dates
    start_date <- .cube_start_date(cube)
    end_date <- .cube_end_date(cube)

    # combine tiles and bands for parallel processing
    tiles_bands <- tidyr::expand_grid(
        tile = .cube_tiles(cube),
        band = bands
    ) |>
        purrr::pmap(function(tile, band) {
            return(list(tile, band))
        })
    # set output_dir
    output_dir <- tempdir()
    if (nzchar(Sys.getenv("SITS_SAMPLES_CACHE_DIR"))) {
        output_dir <- Sys.getenv("SITS_SAMPLES_CACHE_DIR")
    }
    # prepare parallelization
    multicores <- min(multicores, length(tiles_bands))
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)

    samples_tiles_bands <- .parallel_map(tiles_bands, function(tile_band) {
        tile_id <- tile_band[[1]]
        band <- tile_band[[2]]
        # select a band for a tile
        tile <- sits_select(cube, bands = band, tiles = tile_id)
        # select supercells for the tile
        segs_tile <- segments[[tile_id]]
        # create hash for combination of tile and samples
        hash_bundle <- digest::digest(list(tile, segments), algo = "md5")
        # create a file with a hash code
        filename <- .file_path(
            "samples", hash_bundle,
            ext = ".rds",
            output_dir = output_dir
        )
        # test if file exists
        if (file.exists(filename)) {
            tryCatch(
                {
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
        # build the sits tibble for the storing the points
        samples_tbl <- purrr::pmap_dfr(
            list(segs_tile$x, segs_tile$y, segs_tile[[pol_id]]),
            function(x, y, pid) {
                # convert XY to lat long
                lat_long <- .proj_to_latlong(x, y, .crs(cube))

                # create metadata for the polygons
                sample <- tibble::tibble(
                    longitude  = lat_long[1, "longitude"],
                    latitude   = lat_long[1, "latitude"],
                    start_date = start_date,
                    end_date   = end_date,
                    label      = "NoClass",
                    cube       = tile[["collection"]],
                    polygon_id = pid
                )
                # store them in the sample tibble
                sample$time_series <- list(
                    tibble::tibble(Index = .tile_timeline(tile))
                )
                # return valid row of time series
                return(sample)
            }
        )

        # extract time series per tile and band
        ts <- .segments_get_ts(
            tile = tile,
            band = band,
            samples_tbl = samples_tbl,
            segs_tile = segs_tile,
            aggreg_fn = aggreg_fn
        )

        ts[["tile"]] <- tile_id
        ts[["#..id"]] <- seq_len(nrow(ts))

        return(ts)
    }, progress = progress)
    # join rows to get time series tibble
    ts_tbl <- dplyr::bind_rows(samples_tiles_bands)

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
            dplyr::across(dplyr::all_of(bands), stats::na.omit)
        ) |>
        dplyr::arrange(.data[["Index"]]) |>
        dplyr::ungroup() |>
        tidyr::nest(time_series = !!c("Index", bands)) |>
        dplyr::select(-c("#..id"))

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
        tile <- sits_select(cube, bands = band, tiles = tile_id)
        digest::digest(list(tile, segments), algo = "md5")
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

    if (!inherits(ts_tbl, "sits")) {
        class(ts_tbl) <- c("sits", class(ts_tbl))
    }

    return(ts_tbl)
}
#' @title Extract many time series from each segment
#'
#' @name .segments_get_data
#' @keywords internal
#' @noRd
#' @description     Using the segments as polygons, get all time series
#'
#' @param cube       regular data cube
#' @param segments   polygons produced by sits_segments
#' @param bands      bands used in time series
#' @param pol_id     ID attribute for polygons.
#' @param n_sam_pol  Number of samples per polygon to be read.
#' @param multicores Number of cores to use for processing
#' @param progress   Show progress bar?
#'
.segments_get_data <- function(cube,
                               segments,
                               bands,
                               pol_id,
                               n_sam_pol,
                               multicores,
                               progress) {
    # extract a samples data.frame from sf object
    samples <- slider::slide_dfr(cube, function(tile) {
        samples_tile  <- .sf_get_samples(
            sf_object  = segments[[.tile_name(tile)]],
            label      = "NoClass",
            label_attr = NULL,
            start_date = as.Date(sits_timeline(cube)[1]),
            end_date   = as.Date(sits_timeline(cube)
                                 [length(sits_timeline(cube))]),
            n_sam_pol  = n_sam_pol,
            pol_id     = pol_id
        )
        return(samples_tile)
    })

    # extract time series from a cube given a data.frame
    data <- .data_get_ts(
        cube       = cube,
        samples    = samples,
        bands      = bands,
        multicores = multicores,
        progress   = progress
    )
    return(data)
}
#' @title Extract time series from segments by tile and band
#'
#' @name .segments_get_ts
#' @noRd
#' @description     Using the segments as polygons
#'
#' @param tile        Tile of regular data cube
#' @param band        Band to extract time series
#' @param samples_tbl Samples tibble
#' @param segs_tile   Polygons produced by sits_supercells for the tile
#' @param aggreg_fn   Aggregation function to compute a summary of each segment
#'
.segments_get_ts <- function(tile,
                             band,
                             samples_tbl,
                             segs_tile,
                             aggreg_fn) {
    # get the scale factors, max, min and missing values
    band_params <- .tile_band_conf(tile, band)
    missing_value <- .miss_value(band_params)
    minimum_value <- .min_value(band_params)
    maximum_value <- .max_value(band_params)
    scale_factor <- .scale(band_params)
    offset_value <- .offset(band_params)
    # extract the values
    values <- .tile_extract_segments(tile, band, segs_tile, aggreg_fn)
    # adjust maximum and minimum values
    values[values == missing_value] <- NA
    values[values < minimum_value] <- NA
    values[values > maximum_value] <- NA
    # use linear imputation
    impute_fn <- .impute_linear()
    # are there NA values? interpolate them
    if (any(is.na(values))) {
        values <- impute_fn(values)
    }
    # correct the values using the scale factor
    values <- values * scale_factor + offset_value
    # join new time series with previous values
    samples_tbl <- slider::slide2_dfr(
        samples_tbl, seq_len(nrow(samples_tbl)),
        function(sample, i) {
            old_ts <- sample$time_series[[1]]
            new_ts <- tibble::tibble(ts = values[i, ])
            new_ts <- dplyr::bind_cols(old_ts, new_ts)
            colnames(new_ts) <- c(colnames(old_ts), band)
            sample$time_series[[1]] <- new_ts
            return(sample)
        }
    )
    # set sits class
    class(samples_tbl) <- c("sits", class(samples_tbl))
    return(samples_tbl)
}
