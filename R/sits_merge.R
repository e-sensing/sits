#' @title Merge two data sets (time series or cubes)
#' @name sits_merge
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos,   \email{efelipecarlos@@gmail.com}
#'
#' @description To merge two series, we consider that they contain different
#' attributes but refer to the same data cube and spatiotemporal location.
#' This function is useful for merging different bands of the same location.
#' For example, one may want to put the raw and smoothed bands for the same set
#' of locations in the same tibble.
#'
#' In the case of data cubes, the function merges the images based on the
#' following conditions:
#' \enumerate{
#' \item If the two cubes have different bands but compatible timelines, the
#' bands are combined, and the timeline is adjusted to overlap. To create the
#' overlap, we align the timelines like a "zipper": for each interval defined
#' by a pair of consecutive dates in the first timeline, we include matching
#' dates from the second timeline. If the second timeline has multiple dates
#' in the same interval, only the minimum date is kept. This ensures the final
#' timeline avoids duplicates and is consistent. This is useful when merging
#' data from different sensors (e.g., Sentinel-1 with Sentinel-2).
#' \item If the bands are the same, the cube will have the combined timeline of
#' both cubes. This is useful for merging data from the same sensors from
#' different satellites (e.g., Sentinel-2A with Sentinel-2B).
#' \item otherwise, the function will produce an error.
#' }
#'
#' @param data1      Time series (tibble of class "sits")
#'                   or data cube (tibble of class "raster_cube") .
#' @param data2      Time series (tibble of class "sits")
#'                   or data cube (tibble of class "raster_cube") .
#'
#' @param ...        Additional parameters
#' @param suffix     If data1 and data2 are tibble with duplicate bands, this
#'                   suffix will be added (character vector).
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
    .check_content_data_frame(data1)
    .check_content_data_frame(data2)
    # check that data2 and data1 are sits tibble
    .check_samples_ts(data1)
    .check_samples_ts(data2)
    data2 <- .samples_convert_to_sits(data2)
    # verify if data1 and data2 have the same number of rows
    .check_that(nrow(data1) == nrow(data2))
    # are the names of the bands different?
    bands1 <- .samples_bands(data1)
    bands2 <- .samples_bands(data2)
    coincidences1 <- bands1 %in% bands2
    coincidences2 <- bands2 %in% bands1
    if (any(coincidences1) || any(coincidences2)) {
        bands1_names <- rep(x = suffix[[1L]], length(coincidences1))
        bands2_names <- rep(x = suffix[[2L]], length(coincidences2))
        bands1[coincidences1] <- paste0(
            bands1[coincidences1],
            bands1_names[coincidences1]
        )
        bands2[coincidences2] <- paste0(
            bands2[coincidences2],
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
            dplyr::bind_cols(ts1, dplyr::select(ts2, -"Index"))
        }
    )
    result
}

#' @rdname sits_merge
#' @export
sits_merge.raster_cube <- function(data1, data2, ...) {
    .check_set_caller("sits_merge_raster_cube")
    # pre-condition - check cube type
    .check_is_raster_cube(data1)
    .check_is_raster_cube(data2)
    # merge cubes
    .merge(data1, data2)
}

#' @rdname sits_merge
#' @export
sits_merge.default <- function(data1, data2, ...) {
    data1 <- tibble::as_tibble(data1)
    if (all(.conf("sits_cube_cols") %in% colnames(data1))) {
        data1 <- .cube_find_class(data1)
    } else if (all(.conf("sits_tibble_cols") %in% colnames(data1))) {
        class(data1) <- c("sits", class(data1))
    } else {
        stop(.conf("messages", "sits_merge_default"))
    }
    sits_merge(data1, data2, ...)
}
