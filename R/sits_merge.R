#' @title Merge two data sets (time series or cubes)
#'
#' @name sits_merge
#'
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
#' @param data1      Time series or cube to be merged.
#' @param data2      Time series or cube to be merged.
#' @param ...        Additional parameters
#' @param suffix     If there are duplicate bands in data1 and data2
#'                   these suffixes will be added.
#'
#' @return merged data sets
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
sits_merge <- function(data1, data2, ..., suffix = c(".1", ".2")) {
    UseMethod("sits_merge", data1)
}
#' @rdname sits_merge
#' @export
sits_merge.sits <- function(data1, data2, ..., suffix = c(".1", ".2")) {
    # precondition - data sets are not empty
    .check_that(
        x = nrow(data1) > 0 & nrow(data2) > 0,
        msg = "invalid input data"
    )
    # check that data2 is a sits tibble
    .check_samples_ts(data2)
    # verify if data1 and data2 have the same number of rows
    .check_that(
        x = nrow(data1) == nrow(data2),
        msg = "cannot merge tibbles of different sizes"
    )
    # are the names of the bands different?
    bands1 <- sits_bands(data1)
    bands2 <- sits_bands(data2)
    coincidences1 <- bands1 %in% bands2
    coincidences2 <- bands2 %in% bands1
    if (any(coincidences1) || any(coincidences2)) {
        bands1[coincidences1] <- paste0(bands1, suffix[[1]][coincidences1])
        bands2[coincidences2] <- paste0(bands2, suffix[[2]][coincidences2])
        .check_that(
            !any(bands1 %in% bands2),
            local_msg = "duplicated band names",
            msg = "invalid band names"
        )
        .check_that(
            !any(bands2 %in% bands1),
            local_msg = "duplicated band names",
            msg = "invalid band names"
        )
        data1 <- .band_rename(data1, bands1)
        data2 <- .band_rename(data2, bands2)
    }
    # prepare result
    result <- data1
    # merge time series
    result$time_series <- purrr::map2(
        data1$time_series,
        data2$time_series,
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
sits_merge.raster_cube <- function(data1, data2, ...) {
    # pre-condition - check cube type
    .check_is_raster_cube(data1)
    .check_is_raster_cube(data2)
    # aligning tiles
    data1 <- dplyr::arrange(data1, .data[["tile"]])
    data2 <- dplyr::arrange(data2, .data[["tile"]])
    .check_that(
        x = all(.cube_tiles(data1) == .cube_tiles(data2)),
        msg = "merge cubes requires same tiles"
    )
    if (inherits(data1, "hls_cube") && inherits(data2, "hls_cube")) {
        if (.cube_collection(data1) == "HLSS30" ||
            .cube_collection(data2) == "HLSS30") {
            data1$collection <- "HLSS30"
        }
    }
    data1 <- slider::slide2_dfr(data1, data2, function(x, y) {
        .fi(x) <- dplyr::arrange(
            dplyr::bind_rows(.fi(x), .fi(y)),
            .data[["date"]],
            .data[["band"]],
            .data[["fid"]]
        )
        # remove duplicates
        .fi(x) <- dplyr::distinct(
            .fi(x),
            .data[["band"]],
            .data[["date"]],
            .keep_all = TRUE
        )

        return(x)
    })
    return(data1)
}
#' @rdname sits_merge
#' @export
sits_merge.tbl_df <- function(data1, data2, ...) {
    if (all(.conf("sits_cube_cols") %in% colnames(data1))) {
        class(data) <- c("raster_cube", class(data1))
    } else if (all(.conf("sits_tibble_cols") %in% colnames(data1))) {
        class(data1) <- c("sits", class(data1))
    } else
    data1 <- sits_merge(data1, data2, ...)
    return(data1)
}
#' @rdname sits_merge
#' @export
sits_merge.default <- function(data1, data2, ...){
    data1 <- tibble::as_tibble(data1)
    data2 <- tibble::as_tibble(data2)
    data1 <- sits_merge(data1, data2, ...)
    return(data1)
}
