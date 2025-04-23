#' @title Dispatch function to get time series from data cubes and cloud
#' services
#' @name .data_get_ts
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
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
    # Is the cloud band available?
    cld_band <- NULL
    if (.has_cloud(bands)) {
        cld_band <- .source_cloud()
        bands <- setdiff(bands, cld_band)
    }
    # Does the cube have base info?
    if (.cube_is_base(cube)) {
        bands <- setdiff(bands, .cube_bands(.cube_base_info(cube)))
    }
    # Extract samples time series from raster cube
    samples_ts <- .data_extract(
        cube = cube,
        samples = samples,
        bands = bands,
        impute_fn = impute_fn,
        cld_band = cld_band,
        multicores = multicores,
        progress = progress
    )
    # Extract samples time series from base cube
    if (.cube_is_base(cube)) {
        # Get cube base info
        cube_base <- .cube_base_info(cube)
        # Get base bands
        bands_base <- .cube_bands(cube_base)
        # Extract samples time series
        base_ts <- .data_get_ts(
            cube = cube_base,
            samples = samples,
            bands = bands_base,
            impute_fn = impute_fn,
            multicores = multicores,
            progress = progress
        )
        # Combine cube time series with base data
        samples_ts <- .data_combine_ts(
            samples_ts = samples_ts,
            base_ts = base_ts
        )
    }
    samples_ts
}

#' @title get time series from data cubes on tile by tile bassis
#' @name .data_extract
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
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
.data_extract <- function(cube,
                          samples,
                          bands,
                          impute_fn,
                          cld_band,
                          multicores,
                          progress) {
    .check_set_caller(".data_extract")
    # Get cube timeline
    tl <- .dissolve(.cube_timeline(cube))

    # Set output_dir
    output_dir <- tempdir()
    if (Sys.getenv("SITS_SAMPLES_CACHE_DIR") != "") {
        output_dir <- Sys.getenv("SITS_SAMPLES_CACHE_DIR")
    }

    # Reproject the samples and use them on-the-fly without allocate
    samples_rep <- .data_lazy_reproject(samples, cube, output_dir)

    # Get tile-band combination
    tiles_bands <- .cube_split_tiles_bands(cube = cube, bands = bands)
    # To avoid open more process than tiles and bands combinations
    if (multicores > length(tiles_bands)) {
        multicores <- length(tiles_bands)
    }
    # Prepare parallelization
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)

    # Get the samples in parallel using tile-band combination
    ts <- .parallel_map(tiles_bands, function(tile_band) {
        tile_name <- tile_band[[1]]
        band <- tile_band[[2]]
        # Select the tile and band for extracting time series
        tile <- .select_raster_cube(
            data = cube,
            bands = c(band, cld_band),
            tiles = tile_name
        )
        tile_crs <- .tile_crs(tile)
        # Create a hash based on tile and samples
        hash <- digest::digest(list(tile, samples), algo = "md5")
        # File to store the temporary samples
        filename <- .file_samples_name(hash, output_dir)
        # Does the file exist?
        if (file.exists(filename)) {
            timeseries <- .try(
                expr = readRDS(filename),
                .default = unlink(filename)
            )
            if (.has_ts(timeseries)) {
                return(timeseries)
            }
        }
        # Filter samples ...
        samples <- .data_filter_samples(
            samples = samples, cube = cube, samples_rep = samples_rep,
            timeline = tl
        )
        # Create samples ...
        samples <- .data_create_tibble(
            samples = samples,
            tile = tile,
            timeline = tl
        )
        # Are there points to be retrieved from the cube?
        if (nrow(samples) == 0) {
            return(NULL)
        }

        # Extract time series
        samples <- .ts_get_raster_data(
            tile = tile,
            points = samples,
            bands = band,
            impute_fn = impute_fn,
            xy = as.matrix(samples[, c("X", "Y")]),
            cld_band = cld_band
        )
        samples[["tile"]] <- tile_name
        saveRDS(samples, filename)
        samples
    }, progress = progress)
    # bind rows to get a melted tibble of samples
    ts <- dplyr::bind_rows(ts)
    if (!.has_ts(ts)) {
        warning(.conf("messages", ".data_by_tile"),
                immediate. = TRUE, call. = FALSE
        )
        return(.tibble())
    }
    # Post-process the samples
    ts <- .data_reorganise_ts(ts, bands)
    # recreate hash values
    hash_bundle <- purrr::map_chr(tiles_bands, function(tile_band) {
        tile_id <- tile_band[[1]]
        band <- tile_band[[2]]
        tile <- .select_raster_cube(
            cube, bands = c(band, cld_band), tiles = tile_id
        )
        digest::digest(list(tile, samples), algo = "md5")
    })
    # Recreate file names to delete them
    filename <- .file_samples_name(hash_bundle, output_dir)
    # Delete temporary rds
    unlink(filename)
    unlink(.dissolve(samples_rep))
    gc()
    # check if data has been retrieved
    if (progress) {
        .message_data_check(nrow(samples), nrow(ts))
    }
    if (!inherits(ts, "sits")) {
        class(ts) <- c("sits", class(ts))
    }
    ts
}

#' @title function to get class for point in a classified cube
#' @name .data_get_class
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @param cube            Classified data cube
#' @param samples         Samples to be retrieved.
#'
#' @return                A tibble with a lat/long and respective classes.
#'
.data_get_class <- function(cube, samples) {
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
        samples <- dplyr::select(samples, dplyr::all_of("longitude"),
                                 dplyr::all_of("latitude"), dplyr::all_of("label"))
        samples
    })
    data
}

#' @title function to get probability values for a set of given locations
#' @name .data_get_probs
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @param cube            Probability cube from where data is to be retrieved.
#' @param samples         Samples to be retrieved.
#' @param window_size     Size of window around pixel (optional)
#'
#' @return                A tibble with a list of lat/long and respective probs
#'
.data_get_probs <- function(cube, samples, window_size) {
    # get scale and offset
    band_conf <- .conf_derived_band(
        derived_class = "probs_cube",
        band = "probs"
    )
    # return data frame
    slider::slide_dfr(cube, function(tile) {
        # convert lat/long to tile CRS
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

        if (.has(window_size))
            samples <- .data_get_probs_window(tile, samples, xy,
                                              band_conf, window_size)
        else
            samples <- .data_get_probs_pixel(tile, samples, xy, band_conf)

        samples
    })
}
#' @title function to get probability values for a pixel
#' @name .data_get_probs_pixel
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @param tile            Probability cube from where data is to be retrieved.
#' @param samples         Samples to be retrieved.
#' @param xy              Pixel position in the image
#' @param band_conf       Configuration parameters for the raster data
#'
#' @return                A tibble with a list of lat/long and respective probs
#'
.data_get_probs_pixel <- function(tile, samples, xy, band_conf) {
    # open spatial raster object
    rast <- .raster_open_rast(.tile_path(tile))

    # get cells from XY coords
    values <- .raster_extract(rast, xy)

    offset <- .offset(band_conf)
    if (.has(offset) && offset != 0) {
        values <- values - offset
    }
    scale <- .scale(band_conf)
    if (.has(scale) && scale != 1) {
        values <- values * scale
    }
    colnames(values) <- .tile_labels(tile)

    # insert classes into samples
    dplyr::bind_cols(samples, values)
}
#' @title function to get probability values for a window
#' @name .data_get_probs_window
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @param tile            Probability cube from where data is to be retrieved.
#' @param samples         Samples to be retrieved.
#' @param xy              Pixel position in the image
#' @param band_conf       Configuration parameters for the raster data
#' @param window_size     Size of window around a pixel
#'
#' @return                A tibble with a list of lat/long and respective probs
#'
.data_get_probs_window <- function(tile, samples, xy, band_conf, window_size) {
    # open spatial raster object
    rast <- .raster_open_rast(.tile_path(tile))
    # overlap in pixel
    overlap <- ceiling(window_size / 2) - 1
    # number of rows and cols
    nrows <- .raster_nrows(rast)
    ncols <- .raster_ncols(rast)

    # slide for each XY position
    data <- slider::slide2_dfr(xy[, 1], xy[, 2], function(x, y) {
        # find the cells to be retrieved
        center_row <- .raster_row(rast, y)
        center_col <- .raster_col(rast, x)
        top_row <- max(center_row - overlap, 1)
        bottow_row <- min(center_row + overlap, nrows)
        left_col <- max(center_col - overlap, 1)
        right_col <- min(center_col + overlap, ncols)
        # build a vector of cells
        cells <- vector()
        for (row in c(top_row:bottow_row))
            for (col in c(left_col:right_col))
                cells <- c(cells, .raster_cell_from_rowcol(rast, row, col))
        values <- .raster_extract(rast, cells)
        offset <- .offset(band_conf)
        if (.has(offset) && offset != 0) {
            values <- values - offset
        }
        scale <- .scale(band_conf)
        if (.has(scale) && scale != 1) {
            values <- values * scale
        }
        # build a tibble to store the values
        data <- tibble::tibble(
            neighbors = list(values)
        )
        return(data)
    })
    # insert classes into samples
    dplyr::bind_cols(samples, data)
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
    data_avg
}

.timeline_filter <- function(timeline, samples) {
    start_date <- samples[["start_date"]]
    end_date <- samples[["end_date"]]
    timeline[timeline >= start_date & timeline <= end_date]
}

.data_lazy_reproject <- function(samples, cube, output_dir) {
    xy_list <- purrr::map(.cube_crs(cube), function(cube_crs) {
        # Create a hash based on crs and samples
        hash <- digest::digest(list(cube_crs, samples), algo = "md5")
        # File to store the temporary samples
        filename <- .file_samples_name(hash, output_dir)
        xy <- .proj_from_latlong(
            longitude = samples[["longitude"]],
            latitude  = samples[["latitude"]],
            crs       = cube_crs
        )
        saveRDS(xy, filename)
        filename
    })
    names(xy_list) <- .cube_crs(cube)
    xy_list
}

.data_filter_samples <- function(samples, cube, samples_rep, timeline) {
    cube_crs <- .cube_crs(cube)
    # Read the reprojected samples
    samples_rep <- readRDS(samples_rep[[cube_crs]])
    # join lat-long with XY values in a single tibble
    samples <- dplyr::bind_cols(samples, samples_rep)
    # Filter samples extent
    dplyr::filter(
        samples,
        .data[["X"]] > cube[["xmin"]],
        .data[["X"]] < cube[["xmax"]],
        .data[["Y"]] > cube[["ymin"]],
        .data[["Y"]] < cube[["ymax"]],
        .data[["start_date"]] <= as.Date(timeline[[length(timeline)]]),
        .data[["end_date"]] >= as.Date(timeline[[1]])
    )
}

.data_create_tibble <- function(samples, tile, timeline) {
    samples[["#..id"]] <- seq_len(nrow(samples))
    samples[["cube"]] <- .tile_collection(tile)
    # build the sits tibble for the storing the points
    samples <- samples |>
        dplyr::group_by(.data[["#..id"]]) |>
        dplyr::mutate(
            Index = list(Index = .timeline_filter(timeline, .data))
        ) |>
        tidyr::unnest("Index") |>
        dplyr::mutate(
            start_date = min(.data[["Index"]]),
            end_date = max(.data[["Index"]])
        ) |>
        tidyr::nest(time_series = "Index") |>
        dplyr::ungroup()
}

.data_reorganise_ts <- function(ts, bands) {
    # reorganise the samples
    ts <- ts |>
        tidyr::unnest("time_series") |>
        dplyr::group_by(
            .data[["longitude"]], .data[["latitude"]],
            .data[["start_date"]], .data[["end_date"]],
            .data[["label"]], .data[["cube"]],
            .data[["Index"]], .data[["tile"]], .data[["#..id"]]
        )
    # is there a polygon id? This occurs when we have segments
    if ("polygon_id" %in% colnames(ts)) {
        ts <- dplyr::group_by(
            ts, .data[["polygon_id"]], .add = TRUE
        )
    }
    # Verify NA values in time series
    ts <- ts |>
        dplyr::reframe(
            dplyr::across(dplyr::all_of(bands), stats::na.omit)
        ) |>
        dplyr::arrange(.data[["Index"]]) |>
        dplyr::ungroup() |>
        tidyr::nest(time_series = !!c("Index", bands)) |>
        dplyr::select(-c("tile", "#..id"))
    # Get the first point that intersect more than one tile
    # eg sentinel 2 mgrs grid
    ts |>
        dplyr::group_by(
            .data[["longitude"]], .data[["latitude"]],
            .data[["start_date"]], .data[["end_date"]],
            .data[["label"]], .data[["cube"]]
        ) |>
        dplyr::slice_head(n = 1) |>
        dplyr::ungroup()
}

#' @name .data_combine_ts
#' @keywords internal
#' @noRd
#' @export
.data_combine_ts <- function(samples_ts, base_ts) {
    # prepare output data
    base_ts <- base_ts |>
        dplyr::select("longitude", "latitude", "time_series") |>
        dplyr::rename("base_data" = "time_series")
    # Assuming `ts_tbl` as the source of truth, the size of the following
    # `join` must be the same as the current `ts_tbl`.
    ts_tbl_size <- nrow(samples_ts)
    # joining samples data from cube and base_cube by longitude / latitude
    samples_ts <- dplyr::left_join(
        x = samples_ts,
        y = base_ts,
        by = c("longitude", "latitude")
    ) |>
        tidyr::drop_na()
    # checking samples consistency
    .message_data_check(ts_tbl_size, nrow(samples_ts))
    # add base class (`sits` is added as it is removed in the join above)
    class(samples_ts) <- unique(c("sits_base", "sits", class(samples_ts)))
    samples_ts
}
