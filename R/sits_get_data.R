#' @title Get time series from data cubes and cloud services
#' @name sits_get_data
#' @author Gilberto Camara
#'
#' @description Retrieve a set of time series from a data cube or from
#' a time series service. Data cubes and puts it in a "sits tibble".
#' Sits tibbles are the main structures of sits package.
#' They contain both the satellite image time series and their metadata.
#'
#' @note
#' There are four ways of specifying data to be retrieved using the
#' "samples" parameter:
#' \itemize{
#' \item{CSV file: }{Provide a CSV file with columns
#' "longitude", "latitude", "start_date", "end_date" and "label" for
#' each sample}
#' \item{SHP file: }{Provide a shapefile in POINT or POLYGON geometry
#' containing the location of the samples and an attribute to be
#' used as label. Also, provide start and end date for the time series.}
#' \item{sits object: }{A sits tibble.}
#' \item{sf object: }{An "sf" object with POINT or POLYGON geometry.}
#' \item{data.frame: }{A data.frame with with mandatory columns
#' "longitude", "latitude".}
#' }
#
#' @param cube            Data cube from where data is to be retrieved.
#' @param samples         Samples location (sits, sf, or data.frame).
#' @param ...             Specific parameters for specific cases.
#' @param start_date      Start of the interval for the time series
#'                        in "YYYY-MM-DD" format (optional).
#' @param end_date        End of the interval for the time series in
#'                        "YYYY-MM-DD" format (optional).
#' @param label           Label to be assigned to the time series (optional).
#' @param bands           Bands to be retrieved (optional).
#' @param crs             A coordinate reference system of samples.
#'                        The provided crs could be a character
#'                        (e.g, "EPSG:4326" or "WGS84" or a proj4string), or a
#'                        a numeric with the EPSG code (e.g. 4326).
#'                        This parameter only works for 'csv' or data.frame'
#'                        samples. Default is 4326.
#' @param impute_fn       Imputation function for NA values.
#' @param label_attr      Attribute in the shapefile or sf object to be used
#'                        as a polygon label.
#' @param n_sam_pol       Number of samples per polygon to be read
#'                        (for POLYGON or MULTIPOLYGON shapefile).
#' @param pol_avg         Summarize samples for each polygon?
#' @param pol_id          ID attribute for polygons.
#' @param output_dir      Directory where the time series will be saved as rds.
#'                        Default is the current path.
#' @param multicores      Number of threads to process the time series.
#' @param progress        A logical value indicating if a progress bar
#'                        should be shown. Default is \code{FALSE}.
#'
#' @return A tibble with the metadata and data for each time series
#' <longitude, latitude, start_date, end_date, label, cube, time_series>.
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # reading a lat/long from a local cube
#'     # create a cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     raster_cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
#'         data_dir = data_dir,
#'         delim = "_",
#'         parse_info = c("X1", "tile", "band", "date")
#'     )
#'     samples <- tibble::tibble(longitude = -55.66738, latitude = -11.76990)
#'     point_ndvi <- sits_get_data(raster_cube, samples)
#'     #
#'     # reading samples from a cube based on a  CSV file
#'     csv_file <- system.file("extdata/samples/samples_sinop_crop.csv",
#'         package = "sits"
#'     )
#'     points <- sits_get_data(cube = raster_cube, samples = csv_file)
#'
#'     # reading a shapefile from BDC (Brazil Data Cube)
#'     # needs a BDC access key that can be obtained
#'     # for free by registering in the BDC website
#'     if (nchar(Sys.getenv("BDC_ACCESS_KEY")) > 0) {
#'         # create a data cube from the BDC
#'         bdc_cube <- sits_cube(
#'             source = "BDC",
#'             collection = "CB4_64_16D_STK-1",
#'             bands = c("NDVI", "EVI"),
#'             tiles = c("022024", "022025"),
#'             start_date = "2018-09-01",
#'             end_date = "2018-10-28"
#'         )
#'         # define a shapefile to be read from the cube
#'         shp_file <- system.file("extdata/shapefiles/bdc-test/samples.shp",
#'             package = "sits"
#'         )
#'         # get samples from the BDC based on the shapefile
#'         time_series_bdc <- sits_get_data(
#'             cube = bdc_cube,
#'             samples = shp_file)
#'     }
#' }
#'
#' @export
sits_get_data <- function(cube,
                          samples,
                          ...,
                          start_date = NULL,
                          end_date = NULL,
                          label = "NoClass",
                          bands = sits_bands(cube),
                          crs = 4326,
                          impute_fn = sits_impute_linear(),
                          label_attr = NULL,
                          n_sam_pol = 30,
                          pol_avg = FALSE,
                          pol_id = NULL,
                          multicores = 2,
                          output_dir = getwd(),
                          progress = FALSE) {
    # Pre-conditions
    .check_is_raster_cube(cube)
    .check_is_regular(cube)
    .check_bands_in_cube(bands = bands, cube = cube)
    .check_dates_parameter(c(start_date, end_date))
    .check_lgl_parameter(pol_avg)
    .check_num_parameter(n_sam_pol)
    .check_multicores(multicores)
    .check_output_dir(output_dir)
    .check_progress(progress)
    .check_that(
        !(pol_avg && !.has(pol_id)),
        msg = "invalid 'pol_id' parameter."
    )
    # Get cube timeline and start_date and end_date parameters
    tl <- .cube_timeline(cube)
    # Get start_date and end_date as default
    start_date <- .default(start_date, as.Date(tl[1]))
    end_date <- .default(end_date, as.Date(tl[length(tl)]))
    # Transform samples to tibble format
    samples <- .samples_as_tbl(
        samples    = samples,
        start_date = start_date,
        end_date   = end_date,
        label      = label,
        bands      = bands,
        crs        = crs,
        label_attr = label_attr,
        n_sam_pol  = n_sam_pol,
        pol_avg    = pol_avg,
        pol_id     = pol_id
    )
    # Check samples format
    .check_samples(samples)
    # Get time series
    data <- .gd_get_ts(
        cube       = cube,
        samples    = samples,
        bands      = bands,
        crs        = crs,
        impute_fn  = impute_fn,
        multicores = multicores,
        output_dir = output_dir,
        progress   = progress
    )
    # Should the points in the polygons be summarized?
    if (.are_samples_polygons(data, pol_avg)) {
        data <- .samples_avg_polygon(data)
    }
    # Return extracted time series
    return(data)
}

.gd_get_ts <- function(cube,
                       samples, ...,
                       bands = NULL,
                       crs,
                       impute_fn,
                       multicores,
                       output_dir,
                       progress) {
    # Filter only tiles that intersects with samples
    cube <- .cube_filter_spatial(
        cube = cube,
        roi = .point_as_sf(point = .point(x = samples, crs = crs))
    )
    # Filter only bands that are in cube
    cube <- .cube_filter_bands(cube = cube, bands = bands)
    # Format samples to sits format
    samples <- .gd_samples_format(
        samples = samples,
        cube = cube
    )
    # Should the cloud band included in time series extraction?
    include_cloud <- FALSE
    if (.source_cloud() %in% bands) {
        include_cloud <- TRUE
        bands <- bands[bands != .source_cloud()]
    }
    # Create crossing product between tiles and bands as jobs
    tiles_bands <- .cube_split_tiles_bands(
        cube = cube, bands = bands, include_cloud = include_cloud
    )
    # Prepare parallelization
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)
    # Start parallel processing
    samples_tiles <- .jobs_map_parallel_dfr(tiles_bands, function(tile) {
        # Create a temporary samples file name
        out_file <- .file_samples_name(samples, tile, output_dir)
        # Verify if exists valid samples in local directory
        if (are_samples_valid(out_file)) {
            return(readRDS(out_file))
        }
        # Split samples into groups
        samples_groups <- .samples_split_groups(
            samples = samples, multicores = multicores
        )
        # Processing each time series groups
        purrr::map_dfr(samples_groups, function(samples_group) {
            # Add X and Y in samples tibble
            samples <- .samples_add_xy(
                samples = samples_group, crs = crs, as_crs = .tile_crs(tile)
            )
            # Filter samples that are in tile box
            samples <- .samples_filter_spatial(
                samples = samples, tile = tile, crs = crs
            )
            # Are there points to be retrieved from the tile?
            if (are_samples_empty(samples)) {
                return(NULL)
            }
            # Extract time series
            ts <- .gd_extract_data(
                tile = tile,
                samples = samples,
                crs = crs,
                impute_fn = impute_fn
            )
            # Save the temporary sample
            saveRDS(ts, out_file)
            # Return a tibble with extracted time series
            return(ts)
        })
    }, progress = progress)

    if (are_samples_empty(samples_tiles)) {
        message("No time series were retrieved.")
        return(samples_tiles)
    }
    # Join samples for each tile
    ts_tbl <- .gd_join_samples(
        ts_tbl = samples_tiles,
        cube   = cube,
        bands  = bands
    )
    # Recreate temporary hash values and delete them
    .gd_del_temphash(
        tiles_bands = tiles_bands, cube = cube,
        samples = samples, output_dir = output_dir
    )
    # Check if data has been retrieved
    .gd_ts_check(nrow(samples), nrow(ts_tbl))
    # Set samples class
    ts_tbl <- .set_class(ts_tbl, c(.gd_class(cube), class(ts_tbl)))
    # Return extracted time series
    return(ts_tbl)
}

.gd_class <- function(cube) {
    UseMethod(".gd_class", cube)
}

.gd_class.raster_cube <- function(cube) {
    return("sits")
}

.gd_class.class_cube <- function(cube) {
    return(c("predicted", "sits"))
}

#' @title Extract a time series from raster
#' @name .gd_extract_data
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Retrieve a set of time series for a raster data cube.
#' @param tile      Metadata describing a tile of a raster data cube.
#' @param samples   tibble with samples
#' @param crs      Samples origin crs
#' @param impute_fn Imputation function for NA values
#' @return          A sits tibble with the time series.
.gd_extract_data <- function(tile, samples, crs, ...) {
    UseMethod(".gd_extract_data", tile)
}

#' @name .gd_extract_data
#' @keywords internal
#' @noRd
#' @export
.gd_extract_data.raster_cube <- function(tile, samples, crs, ..., impute_fn) {
    # Get tile band
    bands <- .cube_bands(tile, add_cloud = FALSE)
    # Get tile timeline
    timeline <- .tile_timeline(tile)
    # Transform samples to tile cube crs
    xy <- .samples_as_xy(samples)
    # Retrieve values for the cloud band (if available)
    if (.tile_contains_cloud(tile)) {
        cld_band <- .source_cloud()
        # Retrieve values that indicate clouds
        cld_index <- .source_cloud_interp_values(
            source = .tile_source(tile),
            collection = .tile_collection(tile)
        )
        # Get the values of the time series
        cld_values <- .tile_extract(
            tile = tile,
            band = cld_band,
            xy = xy
        )
        # Get information about cloud bitmask
        if (.source_cloud_bit_mask(
            source = .tile_source(tile),
            collection = .tile_collection(tile)
        )) {
            cld_values <- as.matrix(cld_values)
            cld_rows <- nrow(cld_values)
            cld_values <- matrix(bitwAnd(cld_values, sum(2^cld_index)),
                                 nrow = cld_rows
            )
        }
    }

    # Retrieve values on a band by band basis
    ts_bands <- purrr::map(bands, function(band) {

        # get the scale factors, max, min and missing values
        band_params   <- .tile_band_conf(tile, band)
        missing_value <- .miss_value(band_params)
        minimum_value <- .min_value(band_params)
        maximum_value <- .max_value(band_params)
        scale_factor  <- .scale(band_params)
        offset_value  <- .offset(band_params)

        # Get the values of the time series as matrix
        values_band <- .tile_extract(
            tile = tile,
            band = band,
            xy = xy
        )

        # Each row of the values matrix is a spatial point
        ts_band_lst <- purrr::map(seq_len(nrow(values_band)), function(i) {
            t_point <- .timeline_during(
                timeline   = timeline,
                start_date = lubridate::as_date(samples$start_date[[i]]),
                end_date   = lubridate::as_date(samples$end_date[[i]])
            )

            # Select the valid dates in the timeline
            start_idx <- which(timeline == t_point[[1]])
            end_idx <- which(timeline == t_point[[length(t_point)]])

            # Get only valid values for the timeline
            values_ts <- unlist(values_band[i, start_idx:end_idx],
                                use.names = FALSE
            )

            # Include information from cloud band
            if (.tile_contains_cloud(tile)) {
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

            # Adjust maximum and minimum values
            values_ts[values_ts == missing_value] <- NA
            values_ts[values_ts < minimum_value] <- NA
            values_ts[values_ts > maximum_value] <- NA

            # Are there NA values? interpolate them
            if (any(is.na(values_ts))) {
                values_ts <- impute_fn(values_ts)
            }

            # Correct the values using the scale factor
            values_ts <- values_ts * scale_factor + offset_value

            # Return the values of one band for point xy
            return(values_ts)
        })

        # Return the values of all points xy for one band
        return(ts_band_lst)
    })

    # Now we have to transpose the data
    ts_samples <- ts_bands %>%
        purrr::set_names(bands) %>%
        purrr::transpose() %>%
        purrr::map(tibble::as_tibble)

    samples$time_series <- purrr::map2(
        samples$time_series,
        ts_samples,
        dplyr::bind_cols
    )
    samples[["tile"]] <- .tile_name(tile)
    samples[["#..id"]] <- seq_len(nrow(samples))

    class(samples) <- c("sits", class(samples))
    return(samples)
}

#' @name .gd_extract_data
#' @keywords internal
#' @noRd
#' @export
.gd_extract_data.class_cube <- function(tile, samples, crs, ...) {
    # Get tile band
    band <- .tile_bands(tile)
    # Transform samples to tile cube crs
    xy <- .samples_as_xy(samples)
    # Get tile labels
    labels <- sits_labels(tile)
    # Get the values of the time series as matrix
    values_band <- .tile_extract(tile = tile, band = band, xy = xy)
    # Each row of the values matrix is a spatial point
    traj_lst <- as.list(unname(unlist(values_band)))
    # Check if all values fits the labels
    max_label_index <- max(unlist(traj_lst))
    .check_that(
        x = max_label_index <= length(labels),
        local_msg = paste(
            "cube should have at least", max_label_index, "labels"
        ),
        msg = "pixel values do not correspond to any label"
    )
    # Now we have to transpose the data
    traj_samples <- traj_lst %>%
        purrr::map(function(x) tibble::tibble(class = labels[x]))

    samples$predicted <- purrr::map2(
        samples$predicted,
        traj_samples,
        dplyr::bind_cols
    )

    samples[["tile"]] <- .tile_name(tile)
    samples[["#..id"]] <- seq_len(nrow(samples))

    class(samples) <- unique(c("predicted", "sits", class(samples)))
    return(samples)
}

.gd_del_temphash <- function(tiles_bands, cube, samples, output_dir) {
    # Recreate file names to delete them
    temp_timeseries <- slider::slide_chr(tiles_bands, function(tile) {
        .file_samples_name(
            samples = samples, tile = tile, output_dir = output_dir
        )
    })
    # Delete temporary rds
    unlink(temp_timeseries)
    gc()
}

.gd_join_samples <- function(ts_tbl, cube, ...) {
    UseMethod(".gd_join_samples", cube)
}

.gd_join_samples.raster_cube <- function(ts_tbl, cube, ..., bands) {
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
        dplyr::summarise(dplyr::across(dplyr::all_of(bands), stats::na.omit)) %>%
        dplyr::arrange(.data[["Index"]]) %>%
        dplyr::ungroup() %>%
        tidyr::nest(time_series = !!c("Index", bands)) %>%
        dplyr::select(-c("tile", "#..id"))

    # Get the first point that intersect more than one tile
    # eg sentinel 2 mgrs grid
    ts_tbl <- ts_tbl %>%
        dplyr::group_by(
            .data[["longitude"]], .data[["latitude"]],
            .data[["start_date"]], .data[["end_date"]],
            .data[["label"]], .data[["cube"]]) %>%
        dplyr::slice_head(n = 1) %>%
        dplyr::ungroup()

}

.gd_join_samples.class_cube <- function(ts_tbl, cube, ..., bands) {
    ts_tbl <- ts_tbl %>%
        tidyr::unnest("predicted") %>%
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

    ts_tbl <- ts_tbl %>%
        dplyr::summarise(dplyr::across(dplyr::all_of(bands), stats::na.omit)) %>%
        dplyr::arrange(.data[["from"]]) %>%
        dplyr::ungroup() %>%
        tidyr::nest(predicted = !!c("from", "to", bands)) %>%
        dplyr::select(-c("tile", "#..id"))

    # get the first point that intersect more than one tile
    # eg sentinel 2 mgrs grid
    ts_tbl <- ts_tbl %>%
        dplyr::group_by(.data[["longitude"]], .data[["latitude"]],
                        .data[["start_date"]], .data[["end_date"]],
                        .data[["label"]], .data[["cube"]]) %>%
        dplyr::slice_head(n = 1) %>%
        dplyr::ungroup()

}

#' @title Check if all points have been retrieved
#' @name .gd_ts_check
#' @keywords internal
#' @noRd
#' @param n_rows_input     Number of rows in input.
#' @param n_rows_output    Number of rows in output.
#'
#' @return No return value, called for side effects.
#'
.gd_ts_check <- function(n_rows_input, n_rows_output) {
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

.gd_samples_format <- function(samples, cube) {
    UseMethod(".gd_samples_format", cube)
}

.gd_samples_format.raster_cube <- function(samples, cube) {
    # Get cube timeline
    timeline <- .cube_timeline(cube)
    # Build the sits tibble for the storing the points
    slider::slide_dfr(samples, function(point) {
        # Get the valid timeline
        dates <- .timeline_during(
            timeline   = timeline,
            start_date = as.Date(point[["start_date"]]),
            end_date   = as.Date(point[["end_date"]])
        )
        sample <- tibble::tibble(
            longitude  = point[["longitude"]],
            latitude   = point[["latitude"]],
            start_date = dates[[1]],
            end_date   = dates[[length(dates)]],
            label      = point[["label"]],
            cube       = .tile_collection(.tile(cube)),
            polygon_id = point[["polygon_id"]]
        )
        # Store them in the sample tibble
        sample$time_series <- list(tibble::tibble(Index = dates))
        # Return valid row of time series
        return(sample)
    })
}

.gd_samples_format.class_cube <- function(samples, cube) {
    # Get cube timeline
    timeline <- .cube_timeline(cube)
    # Build the sits tibble for the storing the points
    samples_tbl <- slider::slide_dfr(samples, function(point) {
        # Get the valid timeline
        dates <- .timeline_during(
            timeline   = timeline,
            start_date = as.Date(point[["start_date"]]),
            end_date   = as.Date(point[["end_date"]])
        )
        sample <- tibble::tibble(
            longitude  = point[["longitude"]],
            latitude   = point[["latitude"]],
            start_date = dates[[1]],
            end_date   = dates[[length(dates)]],
            label      = point[["label"]],
            cube       = .tile_collection(.tile(cube)),
            polygon_id = point[["polygon_id"]]
        )
        # Store them in the sample tibble
        sample$predicted <- list(tibble::tibble(
            from = dates[[1]], to = dates[[2]])
        )
        # Return valid row of time series
        return(sample)
    })
    return(samples_tbl)
}

.is_sf <- function(data) {
    inherits(data, "sf")
}

.are_sits_samples <- function(samples) {
    inherits(samples, "sits")
}

.are_samples_polygons <- function(samples, pol_avg) {
    pol_avg && "polygon_id" %in% colnames(samples)
}

are_samples_empty <- function(samples) {
    nrow(samples) == 0
}

are_samples_valid <- function(file) {
    # resume processing in case of failure
    if (!all(file.exists(file))) {
        return(FALSE)
    }
    # try to open the file
    r_obj <- .try({
        readRDS(file)
    },
    .default = {
        unlink(file)
        NULL
    })
    # File is not valid
    if (is.null(r_obj)) {
        return(FALSE)
    }
    return(TRUE)
}

.samples_type <- function(samples) {
    if (.is_sf(samples)) {
        return("sf")
    }
    if (is.data.frame(samples)) {
        return("data.frame")
    }
    if (is.character(samples)) {
        .check_file(samples, extensions = c("csv", "shp"))
        return(tolower(.file_ext(samples)))
    }
    if (.are_sits_samples(samples)) {
        return("sits")
    }
    else {
        stop("invalid 'samples' parameter type")
    }
}

.samples_switch <- function(samples, ...) {
    switch(.samples_type(samples), ...)
}

.samples_transform <- function(samples, crs, as_crs) {
    pts_coords <- .point_coords(
        .point(x = samples, crs = crs, as_crs = as_crs)
    )
    samples[, c("longitude", "latitude")] <- pts_coords
    samples
}

.samples_as_tbl <- function(samples,
                            start_date,
                            end_date,
                            label,
                            bands,
                            crs,
                            label_attr,
                            n_sam_pol,
                            pol_avg,
                            pol_id) {
    .samples_switch(
        samples,
        "data.frame" = .samples_from_df(
            samples = samples,
            label = label,
            start_date = start_date,
            end_date = end_date
        ),
        "sits" = samples,
        "csv" = .samples_from_csv(samples),
        "shp" = .samples_from_shp(
            shp_file    = samples,
            label       = label,
            shp_attr    = label_attr,
            start_date  = start_date,
            end_date    = end_date,
            n_shp_pol   = n_sam_pol,
            pol_id      = pol_id
        ),
        "sf" = .samples_from_sf(
            sf_object     = samples,
            label         = label,
            label_attr    = label_attr,
            start_date    = start_date,
            end_date      = end_date,
            n_sam_pol     = n_sam_pol,
            pol_id        = pol_id
        )
    )
}

.samples_from_df <- function(samples, label, start_date, end_date) {
    # check if samples contains all the required columns
    .check_chr_contains(
        x = colnames(samples),
        contains = c("latitude", "longitude"),
        discriminator = "all_of",
        msg = "missing lat/long information in data frame"
    )
    # TODO: Create self assign function .samples_label(samples) <- label
    # Fill missing columns
    if (!("label" %in% colnames(samples))) {
        samples$label <- label
    }
    if (!("start_date" %in% colnames(samples))) {
        samples$start_date <- start_date
    }
    if (!("end_date" %in% colnames(samples))) {
        samples$end_date <- end_date
    }
    class(samples) <- c("sits", class(samples))
    samples
}

.samples_create_hash <- function(samples, ...) {
    digest::digest(list(samples, ...), algo = "md5")
}

.samples_filter_spatial <- function(samples, tile, crs) {
    timeline <- .tile_timeline(tile)
    # Filter the points inside the data cube space-time extent
    dplyr::filter(
        samples,
        .data[["X"]] > .xmin(tile) & .data[["X"]] < .xmax(tile) &
            .data[["Y"]] > .ymin(tile) & .data[["Y"]] < .ymax(tile) &
            .data[["start_date"]] <= as.Date(timeline[length(timeline)]) &
            .data[["end_date"]] >= as.Date(timeline[1])
    )
}

.samples_as_xy <- function(samples) {
    # Create a matrix to extract the values
    matrix(
        c(samples[["X"]], samples[["Y"]]),
        nrow = nrow(samples),
        ncol = 2,
        dimnames = list(NULL, c("X", "Y"))
    )
}

.samples_add_xy <- function(samples, crs, as_crs) {
    # Get samples coordinates
    xy_sf <- .point(samples, crs = crs, as_crs = as_crs)
    xy_tb <- .point_coords(xy_sf)
    # Join lat-long with XY values in a single tibble
    dplyr::bind_cols(samples, xy_tb)
}

#' @title Extracts the time series average by polygon.
#' @name .samples_avg_polygon
#' @keywords internal
#' @noRd
#' @description This function extracts the average of the automatically
#' generated points for each polygon in a shapefile.
#'
#' @param data A sits tibble with points time series.
#'
#' @return A sits tibble with the average of all points by each polygon.
.samples_avg_polygon <- function(samples) {
    if (are_samples_empty(samples)) {
        return(samples)
    }
    bands <- sits_bands(samples)
    columns_to_avg <- c(bands, "latitude", "longitude")

    data_avg <- samples %>%
        tidyr::unnest(cols = "time_series") %>%
        dplyr::group_by(
            .data[["Index"]],
            .data[["start_date"]],
            .data[["end_date"]],
            .data[["label"]],
            .data[["cube"]],
            .data[["polygon_id"]]
        ) %>%
        dplyr::summarise(dplyr::across(!!columns_to_avg, mean, na.rm = TRUE),
                         .groups = "drop"
        ) %>%
        tidyr::nest("time_series" = c("Index", dplyr::all_of(bands))) %>%
        dplyr::select(!!colnames(samples))

    class(data_avg) <- class(samples)

    return(data_avg)
}
