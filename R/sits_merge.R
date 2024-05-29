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
#' @param data1      Time series (tibble of class "sits")
#'                   or data cube (tibble of class "raster_cube") .
#' @param data2      Time series (tibble of class "sits")
#'                   or data cube (tibble of class "raster_cube") .
#'
#' @param ...        Additional parameters
#' @param suffix     If there are duplicate bands in data1 and data2
#'                   these suffixes will be added
#'                   (character vector).
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
sits_merge.sar_cube <- function(data1, data2, ...) {
    .check_set_caller("sits_merge_sar_cube")
    # pre-condition - check cube type
    .check_is_raster_cube(data1)
    .check_is_raster_cube(data2)
    # pre-condition for merge is having the same tiles
    common_tiles <- intersect(data1[["tile"]], data2[["tile"]])
    .check_that(length(common_tiles) > 0)
    # filter cubes by common tiles and arrange them
    data1 <- dplyr::arrange(
        dplyr::filter(data1, .data[["tile"]] %in% common_tiles),
        .data[["tile"]]
    )
    data2 <- dplyr::arrange(
        dplyr::filter(data2, .data[["tile"]] %in% common_tiles),
        .data[["tile"]]
    )
    if (length(.cube_timeline(data2)[[1]]) == 1){
        return(.merge_single_timeline(data1, data2))
    }
    if (inherits(data2, "sar_cube")) {
        return(.merge_equal_cube(data1, data2))
    } else {
        return(.merge_distinct_cube(data1, data2))
    }
}

#' @rdname sits_merge
#' @export
sits_merge.raster_cube <- function(data1, data2, ...) {
    .check_set_caller("sits_merge_raster_cube")
    # pre-condition - check cube type
    .check_is_raster_cube(data1)
    .check_is_raster_cube(data2)
    # pre-condition for merge is having the same tiles
    common_tiles <- intersect(data1[["tile"]], data2[["tile"]])
    .check_that(length(common_tiles) > 0)
    # filter cubes by common tiles and arrange them
    data1 <- dplyr::arrange(
        dplyr::filter(data1, .data[["tile"]] %in% common_tiles),
        .data[["tile"]]
    )
    data2 <- dplyr::arrange(
        dplyr::filter(data2, .data[["tile"]] %in% common_tiles),
        .data[["tile"]]
    )
    if (length(.cube_timeline(data2)[[1]]) == 1){
        return(.merge_single_timeline(data1, data2))
    }
    if (inherits(data2, "sar_cube")) {
        return(.merge_distinct_cube(data1, data2))
    } else {
        return(.merge_equal_cube(data1, data2))
    }
}

.merge_equal_cube <- function(data1, data2) {
    if (inherits(data1, "hls_cube") && inherits(data2, "hls_cube") &&
        (.cube_collection(data1) == "HLSS30" ||
         .cube_collection(data2) == "HLSS30")) {
        data1[["collection"]] <- "HLSS30"
    }

    data1 <- .cube_merge(data1, data2)
    return(data1)
}

.merge_distinct_cube <- function(data1, data2) {
    # Get cubes timeline
    d1_tl <- unique(as.Date(.cube_timeline(data1)[[1]]))
    d2_tl <- unique(as.Date(.cube_timeline(data2)[[1]]))

    # get intervals
    d1_period <- as.integer(
        lubridate::as.period(lubridate::int_diff(d1_tl)), "days"
    )
    d2_period <- as.integer(
        lubridate::as.period(lubridate::int_diff(d2_tl)), "days"
    )
    # pre-condition - are periods regular?
    .check_that(
        length(unique(d1_period)) == 1 && length(unique(d2_period)) == 1
    )
    # pre-condition - Do cubes have the same periods?
    .check_that(
        unique(d1_period) == unique(d2_period)
    )
    # pre-condition - are the cubes start date less than period timeline?
    .check_that(
        abs(d1_period[[1]] - d2_period[[2]]) <= unique(d2_period)
    )

    # Change file name to match reference timeline
    data2 <- slider::slide_dfr(data2, function(y) {
        fi_list <- purrr::map(.tile_bands(y), function(band) {
            fi_band <- .fi_filter_bands(.fi(y), bands = band)
            fi_band[["date"]] <- d1_tl
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
    data1 <- .cube_merge(data1, data2)
    # Return cubes merged
    return(data1)
}

.merge_single_timeline <- function(data1, data2){
    # Get data1 timeline.
    d1_tl <- unique(as.Date(.cube_timeline(data1)[[1]]))
    # Create new `file_info` using dates from `data1` timeline.
    fi_new <- purrr::map(sits_timeline(data1), function(date_row) {
        fi <- .fi(data2)
        fi[["date"]] <- as.Date(date_row)
        fi
    })
    # Assign the new `file_into` into `data2`
    data2[["file_info"]] <- list(dplyr::bind_rows(fi_new))
    # Merge cubes and return
    .cube_merge(data1, data2)
}

#' @rdname sits_merge
#' @export
sits_merge.default <- function(data1, data2, ...) {
    stop(.conf("messages", "sits_merge_default"))
}
