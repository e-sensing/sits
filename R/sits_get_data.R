#' @title Get time series from data cubes and cloud services
#' @name sits_get_data
#' @author Gilberto Camara
#'
#' @description Retrieve a set of time series from a data cube or from
#' a time series service. Data cubes and puts it in a "sits tibble".
#' Sits tibbles are the main structures of sits package.
#' They contain both the satellite image time series and their metadata.
#' There are four ways of specifying data to be retrieved:
#' \itemize{
#' \item{CSV file:}{Provide a CSV file with columns
#' "longitude", "latitude", "start_date", "end_date" and "label" for
#' each sample}
#' \item{SHP file:}{Provide a shapefile in POINT or POLYGON geometry
#' containing the location of the samples and an attribute to be
#' used as label. Also, provide start and end date for the time series.}
#' \item{samples:}{A data.frame with with columns
#' "longitude", "latitude", "start_date", "end_date" and "label" for
#' each sample}
#' \item{single point:}{Provide the values
#' "longitude", "latitude", "start_date", "end_date" and "label" to
#' obtain a time series for a spacetime location}
#' }
#
#' @param cube            Data cube from where data is to be retrieved.
#' @param file            File with information on the data to be retrieved.
#' @param samples         Data.frame with samples location in spacetime.
#' @param multicores      Number of threads to process the time series.
#' @param longitude       Longitude of the chosen location.
#' @param latitude        Latitude of the chosen location.
#' @param start_date      Start of the interval for the time series
#'                        in "YYYY-MM-DD" format (optional).
#' @param end_date        End of the interval for the time series in
#'                        "YYYY-MM-DD" format (optional).
#' @param label           Label to be assigned to the time series (optional).
#' @param bands           Bands to be retrieved (optional).
#' @param impute_fn       Imputation function for NA values.
#' @param shp_attr        Attribute in the shapefile to be used
#'                        as a polygon label.
#' @param .n_pts_csv      Number of points from CSV file to be retrieved.
#' @param .n_shp_pol      Number of samples per polygon to be read
#'                        (for POLYGON or MULTIPOLYGON shapefile).
#' @param .shp_avg        Logical value to summarize samples for a same polygon.
#' @param .shp_id         ID attribute for polygons shapefile.
#' @param output_dir      Directory where the time series will be saved as rds.
#'                        Default is the current path.
#' @param  progress       A logical value indicating if a progress bar
#'                        should be shown. Default is \code{FALSE}.
#'
#' @return A tibble with the metadata and data for each time series
#' <longitude, latitude, start_date, end_date, label, cube, time_series>.
#'
#' @examples
#' if (sits_active_tests()) {
#' # -- Read a point in a raster data cube
#'
#' # Create a data cube based on files
#' data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#' raster_cube <- sits_cube(
#'   source = "BDC",
#'   collection = "MOD13Q1-6",
#'   data_dir = data_dir,
#'   delim = "_",
#'   parse_info = c("X1", "X2", "tile", "band", "date")
#' )
#'
#' # read the time series of the point from the raster
#' point_ts <- sits_get_data(raster_cube,
#'     longitude = -55.554,
#'     latitude = -11.525,
#'     output_dir = tempdir()
#' )
#'
#' # --- Read a set of points described by a CSV file
#'
#' # read data from a CSV file
#' csv_file <- system.file("extdata/samples/samples_sinop_crop.csv",
#'   package = "sits"
#' )
#' points_csv <- sits_get_data(raster_cube,
#'                             file = csv_file,
#'                             output_dir = tempdir())
#' }
#'
#' @export
sits_get_data <- function(cube,
                          file = NULL,
                          samples = NULL,
                          longitude = NULL,
                          latitude = NULL,
                          start_date = NULL,
                          end_date = NULL,
                          label = "NoClass",
                          bands = NULL,
                          impute_fn = sits_impute_linear(),
                          shp_attr = NULL,
                          .n_pts_csv = NULL,
                          .n_shp_pol = 30,
                          .shp_avg = FALSE,
                          .shp_id = NULL,
                          multicores = 1,
                          output_dir = ".",
                          progress = FALSE) {

    # set caller to show in errors
    .check_set_caller("sits_get_data")


    .check_that(
        dir.exists(output_dir),
        msg = "invalid output directory"
    )

    # pre-condition - all tiles have same bands
    is_regular <- .cube_is_regular(cube)

    .check_that(is_regular,
                local_msg = "tiles have different bands and dates",
                msg = "cube is inconsistent"
    )

    # pre-condition - file parameter
    .check_chr(file,
               allow_empty = FALSE, len_min = 1, len_max = 1,
               allow_null = TRUE, msg = "invalid 'file' parameter"
    )

    # no start or end date? get them from the timeline
    timeline <- sits_timeline(cube)

    if (purrr::is_null(start_date)) {
        start_date <- as.Date(timeline[1])
    }
    if (purrr::is_null(end_date)) {
        end_date <- as.Date(timeline[length(timeline)])
    }

    # is there a shapefile or a CSV file?
    if (!purrr::is_null(file)) {

        # get the file extension
        file_ext <- tolower(tools::file_ext(file))

        # sits only accepts "csv" or "shp" files
        .check_chr_within(
            x = file_ext,
            within = .config_get("sample_file_formats"),
            msg = paste0(
                "samples should be of type ",
                paste(.config_get("sample_file_formats"),
                      collapse = " or "
                )
            )
        )
        if (file_ext == "csv") {
            samples <- .sits_get_samples_from_csv(
                csv_file   = file,
                .n_pts_csv = .n_pts_csv
            )
        }

        if (file_ext == "shp") {
            samples <- .sits_get_samples_from_shp(
                shp_file = file,
                label = label,
                shp_attr = shp_attr,
                start_date = start_date,
                end_date = end_date,
                .n_shp_pol = .n_shp_pol,
                .shp_id = .shp_id
            )
        }
    } else {
        if (!purrr::is_null(samples)) {
            # check if samples are a data.frame
            .check_chr_contains(
                x = class(samples),
                contains = "data.frame",
                case_sensitive = TRUE,
                discriminator = "any_of",
                can_repeat = TRUE,
                msg = "samples should be of type data.frame"
            )
        } else {
            if (!purrr::is_null(latitude) &&
                !purrr::is_null(longitude)) {
                samples <- tibble::tibble(
                    longitude  = longitude,
                    latitude   = latitude,
                    start_date = as.Date(start_date),
                    end_date   = as.Date(end_date),
                    label      = label
                )
            } else {
                stop("no valid information about samples")
            }
        }
    }

    # post-condition
    # check if samples contains all the required columns
    .check_chr_contains(
        x = colnames(samples),
        contains = .config_get("df_sample_columns"),
        discriminator = "all_of",
        msg = "data input is not valid"
    )

    data <- .sits_get_ts(
        cube       = cube,
        samples    = samples,
        bands      = bands,
        impute_fn  = impute_fn,
        multicores = multicores,
        output_dir = output_dir,
        progress   = progress
    )

    if (.shp_avg && "polygon_id" %in% colnames(data))
        data <- .sits_shp_avg_polygon(data = data)

    return(data)
}

#' @title Dispatch function to get time series from data cubes and cloud
#' services
#' @name .sits_get_ts
#' @author Gilberto Camara
#' @keywords internal
#' @param cube            Data cube from where data is to be retrieved.
#' @param samples         Samples to be retrieved.
#' @param bands           Bands to be retrieved (optional).
#' @param impute_fn       Imputation function for NA values.
#' @param multicores      Number of threads to process the time series.
#' @param progress        A logical value indicating if a progress bar
#'                        should be shown. Default is \code{FALSE}.
.sits_get_ts <- function(cube,
                         samples, ...,
                         bands = NULL,
                         impute_fn,
                         multicores,
                         output_dir,
                         progress) {

    # Dispatch
    UseMethod(".sits_get_ts", cube)
}

#' @keywords internal
#' @export
.sits_get_ts.raster_cube <- function(cube,
                                     samples, ...,
                                     bands,
                                     impute_fn,
                                     multicores,
                                     output_dir,
                                     progress) {

    samples_sf  <- sf::st_as_sf(
        x = samples,
        coords = c(x = "longitude", y = "latitude"),
        crs = 4326
    )
    are_samples_in_tiles <- purrr::map_lgl(seq_len(nrow(cube)), function(i) {
        .sits_raster_sub_image_intersects(
            cube = cube[i, ],
            roi = samples_sf
        )
    })
    .check_that(
        any(are_samples_in_tiles),
        msg = "The provided tile(s) does not intersects with samples."
    )
    # filter only tiles that intersects with samples
    cube <- cube[are_samples_in_tiles, ]

    .check_chr_within(
        x = .config_get("df_sample_columns"),
        within = colnames(samples),
        msg = "data input is not valid"
    )

    # pre-condition - check bands
    if (is.null(bands)) {
        bands <- .cube_bands(cube)
    }

    .cube_bands_check(cube, bands = bands)

    # is the cloud band available?
    cld_band <- .source_cloud()

    if (cld_band %in% bands) {
        bands <- bands[bands != cld_band]
    } else {
        cld_band <- NULL
    }

    # get cubes timeline
    tl <- sits_timeline(cube)

    tiles_bands <- purrr::cross2(.cube_tiles(cube), bands)

    # prepare parallelization
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)

    samples_tiles_bands <- .sits_parallel_map(tiles_bands, function(tile_band) {

        tile_id <- tile_band[[1]]
        band <- tile_band[[2]]

        tile <- sits_select(cube, bands = c(band, cld_band), tiles = tile_id)

        hash_bundle <- digest::digest(list(tile, samples), algo = "md5")

        filename <- .create_filename(
            "samples", hash_bundle,
            ext = ".rds",
            output_dir = output_dir
        )

        if (file.exists(filename))
            tryCatch({
                # ensuring that the file is not corrupted
                timeseries <- readRDS(filename)

                return(timeseries)
            },
            error = function(e) {
                unlink(filename)
                gc()
            })

        # get XY
        xy_tb <- .sits_proj_from_latlong(
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
            dates <- .sits_timeline_during(
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
        ts <- .sits_raster_data_get_ts(
            tile = tile,
            points = samples_tbl,
            bands = band,
            xy = xy,
            cld_band = cld_band,
            impute_fn = impute_fn,
            output_dir = output_dir
        )

        ts[["tile"]] <- tile_id
        ts[["#..id"]] <- seq_len(nrow(ts))

        saveRDS(ts, filename)

        return(ts)
    }, progress = progress)

    ts_tbl <- samples_tiles_bands %>%
        dplyr::bind_rows() %>%
        tidyr::unnest(.data[["time_series"]]) %>%
        dplyr::group_by(.data[["longitude"]], .data[["latitude"]],
                        .data[["start_date"]], .data[["end_date"]],
                        .data[["label"]], .data[["cube"]],
                        .data[["Index"]], .data[["tile"]], .data[["#..id"]])

    if ("polygon_id" %in% colnames(ts_tbl))
        ts_tbl <- dplyr::group_by(ts_tbl, .data[["polygon_id"]], .add = TRUE)

    ts_tbl <- ts_tbl %>%
        dplyr::summarise(dplyr::across(bands, stats::na.omit)) %>%
        dplyr::arrange(.data[["Index"]]) %>%
        dplyr::ungroup() %>%
        tidyr::nest(time_series = !!c("Index", bands)) %>%
        dplyr::select(-c("tile", "#..id"))

    # recreate hash values
    hash_bundle <- purrr::map_chr(tiles_bands, function(tile_band) {

        tile_id <- tile_band[[1]]
        band <- tile_band[[2]]
        tile <- sits_select(cube, bands = c(band, cld_band), tiles = tile_id)
        digest::digest(list(tile, samples), algo = "md5")
    })

    # recreate file names to delete them
    # samples will be recycled for each hash_bundle
    temp_timeseries <- .create_filename(
        "samples", hash_bundle,
        ext = "rds",
        output_dir = output_dir
    )

    # delete temporary rds
    unlink(temp_timeseries)
    gc()

    # check if data has been retrieved
    .sits_get_data_check(nrow(samples), nrow(ts_tbl))

    if (!inherits(ts_tbl, "sits")) {
        class(ts_tbl) <- c("sits", class(ts_tbl))
    }

    return(ts_tbl)
}

#' @title check if all points have been retrieved
#' @name .sits_get_data_check
#' @keywords internal
#' @param n_rows_input     Number of rows in input.
#' @param n_rows_output    Number of rows in output.
#'
#' @return A logical value
#'
.sits_get_data_check <- function(n_rows_input, n_rows_output) {

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

    return(invisible(TRUE))
}

#' @title Transform a shapefile into a samples file
#' @name .sits_get_samples_from_shp
#' @author Gilberto Camara
#' @keywords internal
#' @param shp_file        Shapefile that describes the data to be retrieved.
#' @param label           Default label for samples.
#' @param shp_attr        Shapefile attribute that describes the label.
#' @param start_date      Start date for the data set.
#' @param end_date        End date for the data set.
#' @param .n_shp_pol      Number of samples per polygon to be read.
#' @param .shp_id         ID attribute for polygons shapefile.
#'                        (for POLYGON or MULTIPOLYGON shapefile).
#' @return                A tibble with information the samples to be retrieved.
#'
.sits_get_samples_from_shp <- function(shp_file,
                                       label,
                                       shp_attr,
                                       start_date,
                                       end_date,
                                       .n_shp_pol,
                                       .shp_id) {

    # pre-condition - check the shape file and its attribute
    sf_shape <- .sits_shp_check_validity(
        shp_file = shp_file,
        shp_attr = shp_attr,
        label = label
    )
    # get the points to be read
    samples <- .sits_shp_to_tibble(
        sf_shape = sf_shape,
        shp_attr = shp_attr,
        label = label,
        .n_shp_pol = .n_shp_pol,
        .shp_id = .shp_id
    )

    samples <- dplyr::mutate(samples,
                             start_date = as.Date(start_date),
                             end_date = as.Date(end_date)
    )

    return(samples)
}

#' @title Transform a shapefile into a samples file
#' @name .sits_get_samples_from_csv
#' @author Gilberto Camara
#' @keywords internal
#' @param csv_file        CSV that describes the data to be retrieved.
#' @param .n_pts_csv      number of points to be retrieved
#' @return                A tibble with information the samples to be retrieved
#'
.sits_get_samples_from_csv <- function(csv_file,
                                       .n_pts_csv) {

    # read sample information from CSV file and put it in a tibble
    samples <- tibble::as_tibble(utils::read.csv(csv_file))

    # pre-condition - check if CSV file is correct
    .sits_csv_check(samples)

    # select valid columns
    samples <- dplyr::select(samples,
                             dplyr::all_of(.config_get("df_sample_columns")))

    if (!purrr::is_null(.n_pts_csv) &&
        .n_pts_csv > 1 && .n_pts_csv < nrow(samples)) {
        samples <- samples[1:.n_pts_csv, ]
    }

    samples <- dplyr::mutate(samples,
                             start_date = as.Date(.data[["start_date"]]),
                             end_date = as.Date(.data[["end_date"]])
    )

    return(samples)
}
