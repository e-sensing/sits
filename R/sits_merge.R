#' @title Merge two data sets (time series or cubes)
#' @name sits_merge
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description To merge two series, we consider that they contain different
#' attributes but refer to the same data cube, and spatiotemporal location.
#' This function is useful to merge different bands of the same locations.
#' For example, one may want to put the raw and smoothed bands
#' for the same set of locations in the same tibble.
#'
#' To merge data cubes, they should share the same sensor, resolution,
#' bounding box, timeline, and have different bands.
#'
#' When the user requests a sits_merge operation for two regular cubes with
#' the same number of time intervals but with timelines that are not equal
#' the system issues a warning and asks the user to provide a \code{tolerance}
#' parameter which will be used in the merging operation.
#' The temporal tolerance parameter should be less than the time interval
#' between two images of both cubes.
#' In this case the second cube will have its timeline and the image
#' file names changed to match the timeline of the first cube.
#' The images of the second cube will be written in \code{output_dir}
#' directory.
#'
#' @param data1      Time series (tibble of class "sits")
#'                   or data cube (tibble of class "raster_cube") .
#' @param data2      Time series (tibble of class "sits")
#'                   or data cube (tibble of class "raster_cube") .
#'
#' @param ...        Additional parameters
#' @param tolerance  Allowable amount of variation in time interval
#'                   between two dates of cubes to be merged
#' @param output_dir Directory where transformed images will be saved
#' @param suffix     If there are duplicate bands in data1 and data2
#'                   these suffixes will be added
#'                   (character vector).
#' @param tolerance  A period tolerance to merge both cubes.
#'                   ISO8601-compliant time period for regular data cubes,
#'                   with number and unit, where "D", "M" and "Y" stand
#'                   for days, month and year; e.g., "P16D" for 16 days.
#'                   The temporal tolerance parameter should be less than
#'                   the time interval between two images of both cubes.
#' @param output_dir Valid directory for storing merged images.
#'
#' @return merged data sets (tibble of class "sits" or
#'         tibble of class "raster_cube")
#' @examples
#' if (sits_run_examples()) {
#'     # Retrieve a time series with values of NDVI
#'     point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
#'
#'     # Filter the point using the Whittaker smoother
#'     point_whit <- sits_filter(point_ndvi, sits_whittaker(lambda = 3.0))
#'     # Merge time series
#'     point_ndvi <- sits_merge(point_ndvi, point_whit, suffix = c("", ".WHIT"))
#'
#'     # Plot the two points to see the smoothing effect
#'     plot(point_ndvi)
#' }
#' @export
#'
sits_merge <- function(data1, data2, ...) {
    UseMethod("sits_merge", data1)
}

#' @rdname sits_merge
#' @export
sits_merge.sits <- function(data1, data2, ..., suffix = c(".1", ".2")) {
    .check_set_caller("sits_merge_sits")
    # precondition - data sets are not empty
    .check_that(nrow(data1) > 0 & nrow(data2) > 0)
    # check that data2 and data1 are sits tibble
    .check_samples_ts(data1)
    .check_samples_ts(data2)
    # verify if data1 and data2 have the same number of rows
    .check_that(nrow(data1) == nrow(data2))
    # are the names of the bands different?
    bands1 <- sits_bands(data1)
    bands2 <- sits_bands(data2)
    coincidences1 <- bands1 %in% bands2
    coincidences2 <- bands2 %in% bands1
    if (any(coincidences1) || any(coincidences2)) {
        bands1_names <- rep(x = suffix[[1]], length(coincidences1))
        bands2_names <- rep(x = suffix[[2]], length(coincidences2))
        bands1[coincidences1] <- paste0(bands1[coincidences1],
                                        bands1_names[coincidences1]
        )
        bands2[coincidences2] <- paste0(bands2[coincidences2],
                                        bands2_names[coincidences2]
        )
        .check_that(!any(bands1 %in% bands2))
        .check_that(!any(bands2 %in% bands1))
        data1 <- .band_rename(data1, bands1)
        data2 <- .band_rename(data2, bands2)
    }
    # prepare result
    result <- data1
    # merge time series
    result[["time_series"]] <- purrr::map2(
        data1[["time_series"]],
        data2[["time_series"]],
        function(ts1, ts2) {
            ts3 <- dplyr::bind_cols(ts1, dplyr::select(ts2, -"Index"))
            return(ts3)
        }
    )
    return(result)
}

#' @rdname sits_merge
#' @export
#'
sits_merge.raster_cube <- function(data1, data2, ...,
                                   tolerance = NULL,
                                   output_dir = NULL) {
    .check_set_caller("sits_merge_raster_cube")
    # pre-condition - check cube type
    .check_is_raster_cube(data1)
    .check_is_raster_cube(data2)
    if (.has(tolerance)) {
        .check_period(tolerance)
    }
    if (.has(output_dir)) {
        .check_output_dir(output_dir)
    }

    # aligning tiles
    data1 <- dplyr::arrange(data1, .data[["tile"]])
    data2 <- dplyr::arrange(data2, .data[["tile"]])
    # Get cubes timeline
    d1_tl <- as.Date(unlist(.cube_timeline(data1)))
    d2_tl <- as.Date(unlist(.cube_timeline(data2)))
    # check timeline interval
    # tl_interval1 <- lubridate::int_diff()
    # join cube tiles
    common_tiles <- intersect(data1[["tile"]], data2[["tile"]])
    data1 <- dplyr::filter(data1, .data[["tile"]] %in% common_tiles)
    data2 <- dplyr::filter(data2, .data[["tile"]] %in% common_tiles)
    .check_that(all(sort(.cube_tiles(data1)) == sort(.cube_tiles(data2))))
    if (inherits(data1, "hls_cube") && inherits(data2, "hls_cube") &&
        (.cube_collection(data1) == "HLSS30" ||
         .cube_collection(data2) == "HLSS30")) {
            data1[["collection"]] <- "HLSS30"
    }

    if (all(d1_tl == d2_tl)) {
        data1 <- .merge_fi(data1, data2)
        return(data1)
    }
    # Get difference in timelines
    diff_timelines <- .merge_diff_timelines(d1_tl, d2_tl)
    # Verify the consistency of each difference
    if (!all(diff_timelines <= lubridate::period(tolerance))) {
        stop(.conf("messages", "sits_merge_raster_cube_error"),
             call. = FALSE
        )
    }
    if (!.has(output_dir)) {
        warning(
            paste("The images with the fixed timeline of the",
                  "second cube will not be written. If you want",
                  "to write it, use the `output_dir` parameter."
            ),
            call. = FALSE
        )
    }

    # Change file name to match reference timeline
    data2 <- slider::slide_dfr(data2, function(y) {
        fi_list <- purrr::map(.tile_bands(y), function(band) {
            fi_band <- .fi_filter_bands(.fi(y), bands = band)
            fi_band[["date"]] <- d1_tl
            if (!.has(output_dir)) {
                return(fi_band)
            }
            fi_paths <- .fi_paths(fi_band)
            file_names <- .file_eo_name(
                tile = y, band = band,
                date = d1_tl, output_dir = output_dir
            )
            file.copy(from = fi_paths, to = file_names)
            fi_band[["path"]] <- file_names
            return(fi_band)
        })
        tile_fi <- dplyr::bind_rows(fi_list)
        tile_fi <- dplyr::arrange(
            tile_fi,
            .data[["date"]],
            .data[["band"]],
            .data[["fid"]]
        )
        y[["file_info"]] <- list(tile_fi)
        y
    })
    # Merge the cubes
    data1 <- .merge_fi(data1, data2)
    # Return cubes merged
    return(data1)
}

#' @rdname sits_merge
#' @export
sits_merge.default <- function(data1, data2, ...) {
    stop(.conf("messages", "sits_merge_default"))
}
