#' @title Extract set of time series from supercells
#'
#' @name .supercells_get_data
#' @noRd
#' @description     Using the segments as polygons, get all time series
#'
#' @param cube       regular data cube
#' @param supercells polygons produced by sits_supercells
#' @param bands      bands used in time series
#' @param impute_fn  Imputation function for NA values.
#' @param aggreg_fn  Aggregation function to compute a summary of each segments
#' @param multicores Number of cores to use for processing
#' @param progress   Show progress bar?
#'
.supercells_get_data <- function(
        cube,
        supercells,
        bands,
        impute_fn,
        aggreg_fn,
        multicores,
        progress
){
    # set start and end dates
    dates <- .cube_timeline(cube)[[1]]
    start_date <- dates[1]
    end_date   <- dates[[length(dates)]]
    # combine tiles and bands for parallel processing
    tiles_bands <- tidyr::expand_grid(tile = .cube_tiles(cube),
                                      band = bands) %>%
        purrr::pmap(function(tile, band) {
            return(list(tile, band))
        })
    # set output_dir
    output_dir <- tempdir()
    if (Sys.getenv("SITS_SAMPLES_CACHE_DIR") != "")
        output_dir <- Sys.getenv("SITS_SAMPLES_CACHE_DIR")
    # prepare parallelization
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)

    samples_tiles_bands <- .sits_parallel_map(tiles_bands, function(tile_band) {
        tile_id <- tile_band[[1]]
        band <- tile_band[[2]]
        # select a band for a tile
        tile <- sits_select(cube, bands = band, tiles = tile_id)
        # select supercells for the tile
        segs_tile <- supercells[[tile_id]]
        # create hash for combination of tile and samples
        hash_bundle <- digest::digest(list(tile, supercells), algo = "md5")
        # create a file with a hash code
        filename <- .file_path(
            "samples", hash_bundle,
            ext = ".rds",
            output_dir = output_dir
        )
        # test if file exists
        if (file.exists(filename)) {
            tryCatch({
                # ensure that the file is not corrupted
                timeseries <- readRDS(filename)
                return(timeseries)
            },
            error = function(e) {
                unlink(filename)
                gc()
            })
        }
        # build the sits tibble for the storing the points
        samples_tbl <- slider::slide_dfr(segs_tile, function(seg) {
            # convert XY to lat long
            lat_long <- .proj_to_latlong(seg$x, seg$y, .crs(cube))

            # create metadata for the polygons
            sample <- tibble::tibble(
                longitude  = lat_long[1, "longitude"],
                latitude   = lat_long[1, "latitude"],
                start_date = start_date,
                end_date   = end_date,
                label      = "NoClass",
                cube       = tile[["collection"]],
                polygon_id = seg[["supercells"]]
            )
            # store them in the sample tibble
            sample$time_series <- list(tibble::tibble(Index = dates))
            # return valid row of time series
            return(sample)
        })
        # extract time series per tile and band
        ts <- .supercells_get_ts(
            tile = tile,
            band = band,
            segs_tile = segs_tile,
            impute_fn  = impute_fn,
            aggreg_fn = aggreg_fn
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

    ts_tbl <- ts_tbl %>%
        tidyr::unnest("time_series") %>%
        dplyr::group_by(
            .data[["longitude"]], .data[["latitude"]],
            .data[["start_date"]], .data[["end_date"]],
            .data[["label"]], .data[["cube"]],
            .data[["Index"]], .data[["tile"]], .data[["#..id"]]
        )

    if ("polygon_id" %in% colnames(ts_tbl)) {
        ts_tbl <- dplyr::group_by(ts_tbl, .data[["polygon_id"]], .add = TRUE)
    }

    ts_tbl <- ts_tbl %>%
        dplyr::reframe(
            dplyr::across(dplyr::all_of(bands), stats::na.omit)) %>%
        dplyr::arrange(.data[["Index"]]) %>%
        dplyr::ungroup() %>%
        tidyr::nest(time_series = !!c("Index", bands)) %>%
        dplyr::select(-c("tile", "#..id"))


    # get the first point that intersect more than one tile
    # eg sentinel 2 mgrs grid
    ts_tbl <- ts_tbl %>%
        dplyr::group_by(
            .data[["longitude"]], .data[["latitude"]],
            .data[["start_date"]], .data[["end_date"]],
            .data[["label"]], .data[["cube"]]) %>%
        dplyr::slice_head(n = 1) %>%
        dplyr::ungroup()

    # recreate hash values
    hash_bundle <- purrr::map_chr(tiles_bands, function(tile_band) {
        tile_id <- tile_band[[1]]
        band <- tile_band[[2]]
        tile <- sits_select(cube, bands = band, tiles = tile_id)
        digest::digest(list(tile, supercells), algo = "md5")
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
    # .sits_get_data_check(nrow(samples), nrow(ts_tbl))

    if (!inherits(ts_tbl, "sits")) {
        class(ts_tbl) <- c("sits", class(ts_tbl))
    }

    return(ts_tbl)
}
#' @title Extract time series from supercells by tile and band
#'
#' @name .supercells_get_ts
#' @noRd
#' @description     Using the segments as polygons
#'
#' @param tile        Tile of regular data cube
#' @param band        Band to extract time series
#' @param segs_tile   Polygons produced by sits_supercells for the tile
#' @param impute_fn  Imputation function for NA values.
#' @param aggreg_fn   Aggregation function to compute a summary of each segment
#'
.supercells_get_ts <- function(
        tile,
        band,
        segs_tile,
        impute_fn,
        aggreg_fn
){
    # get the scale factors, max, min and missing values
    band_params   <- .tile_band_conf(tile, band)
    missing_value <- .miss_value(band_params)
    minimum_value <- .min_value(band_params)
    maximum_value <- .max_value(band_params)
    scale_factor  <- .scale(band_params)
    offset_value  <- .offset(band_params)
    # extract the values
    values <- .tile_extract_segments(tile, band, segs_tile, aggreg_fn)
    # adjust maximum and minimum values
    values[values == missing_value] <- NA
    values[values < minimum_value] <- NA
    values[values > maximum_value] <- NA
    # are there NA values? interpolate them
    if (any(is.na(values))) {
        values <- impute_fn(values)
    }
    # correct the values using the scale factor
    values <- values * scale_factor + offset_value

     # get the time series as a list
    values_ts <- as.list(as.data.frame(values))

    # # now we have to transpose the data
    # ts_samples <- ts_bands %>%
    #     purrr::set_names(bands) %>%
    #     purrr::transpose() %>%
    #     purrr::map(tibble::as_tibble)
    #
    #
    # points$time_series <- purrr::map2(
    #     points$time_series,
    #     ts_samples,
    #     dplyr::bind_cols
    # )

    return(values_ts)
}
