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
#'
#' @export
#'
sits_merge <- function(data1, data2, ..., suffix = c(".1", ".2")) {

    # set caller to show in errors
    .check_set_caller("sits_merge")
    # get the meta-type (sits or cube)
    data1 <- .config_data_meta_type(data1)
    UseMethod("sits_merge", data1)
}

#' @export
sits_merge.sits <- function(data1, data2, ..., suffix = c(".1", ".2")) {

    # precondition
    .sits_tibble_test(data1)
    .sits_tibble_test(data2)

    # if some parameter is empty returns the another one
    .check_that(
        x = nrow(data1) > 0 & nrow(data2) > 0,
        msg = "invalid input data"
    )

    # verify if data1.tb and data2.tb has the same number of rows
    .check_that(
        x = nrow(data1) == nrow(data2),
        msg = "cannot merge tibbles of different sizes"
    )

    # are the names of the bands different?
    # if they are not
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
        data1 <- .sits_rename_bands(data1, bands1)
        data2 <- .sits_rename_bands(data2, bands2)
    }
    # prepare result
    result <- data1

    # merge time series
    result$time_series <- purrr::map2(
        data1$time_series,
        data2$time_series,
        function(ts1, ts2) {
            ts3 <- dplyr::bind_cols(ts1, dplyr::select(ts2, -.data[["Index"]]))
            return(ts3)
        }
    )
    return(result)
}

#' @export
#'
sits_merge.raster_cube <- function(data1, data2, ..., suffix = c(".1", ".2")) {

    # pre-condition - check cube type
    .cube_check(data1)
    .cube_check(data2)

    .check_that(
        x = data1$satellite == data2$satellite,
        msg = "cubes from different satellites"
    )
    .check_that(
        x = data1$sensor == data2$sensor,
        msg = "cubes from different sensors"
    )
    .check_that(
        all(.cube_resolution(data1) == .cube_resolution(data2)),
        msg = "merge cubes requires same resolution"
    )
    .check_that(
        length(.cube_tiles(data1)) == length(.cube_tiles(data2)),
        msg = "merge cubes requires same number of tiles"
    )

    # para manter a pariedade
    data1 <- dplyr::arrange(data1, .data[["tile"]])
    data2 <- dplyr::arrange(data2, .data[["tile"]])
    .check_that(
        x = all(.cube_tiles(data1) == .cube_tiles(data2)),
        msg = "merge cubes requires same tiles"
    )

    data1 <- slider::slide2_dfr(data1, data2, function(x, y) {
        .check_that(
            x = all(sits_timeline(x) == sits_timeline(y)),
            msg = "merge cubes requires same timeline"
        )
        # are the names of the bands different?
        bands1 <- sits_bands(x)
        bands2 <- sits_bands(y)

        coincidences1 <- bands1 %in% bands2
        coincidences2 <- bands2 %in% bands1
        if (any(coincidences1) || any(coincidences2)) {
            bands1[coincidences1] <- paste0(bands1[coincidences1], suffix[[1]])
            bands2[coincidences2] <- paste0(bands2[coincidences2], suffix[[2]])
            .check_that(
                !any(bands1 %in% bands2),
                local_msg = "use suffix to avoid band duplication",
                msg = "duplicated band names"
            )
            .check_that(
                !any(bands2 %in% bands1),
                local_msg = "use suffix to avoid band duplication",
                msg = "duplicated band names"
            )
            x <- .sits_rename_bands(x, bands1)
            y <- .sits_rename_bands(y, bands2)
        }

        x[["file_info"]][[1]] <- dplyr::arrange(
            dplyr::bind_rows(.file_info(x), .file_info(y)),
            .data[["date"]], .data[["band"]]
        )

        return(x)
    })

    return(data1)
}
