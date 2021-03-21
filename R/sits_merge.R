#' @title Merge two data sets (time series or cubes)
#' @name sits_merge
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description To merge two series, we consider that they contain different
#' attributes but refer to the same data cube, and spatio-temporal location.
#' This function is useful to merge different bands of the same locations.
#' For example, one may want to put the raw and smoothed bands
#' for the same set of locations in the same tibble.
#'
#' To merge data cubes, they should share the same sensor, resolution,
#' bounding box, timeline, and have different bands.
#'
#' @param data1      sits tibble or cube to be merged.
#' @param data2      sits tibble or cube to be merged.
#' @return merged data sets
#'
#' @examples {
#' # Retrieve a time series with values of NDVI
#' data(point_ndvi)
#' # Filter the point using the whittaker smoother
#' point_ws.tb <- sits_whittaker(point_ndvi, lambda = 3.0)
#' # Plot the two points to see the smoothing effect
#' plot(sits_merge(point_ndvi, point_ws.tb))
#'
#'
#' }
#' @export
sits_merge <- function(data1, data2) {
    # get the meta-type (sits or cube)
    data1 <- .sits_config_data_meta_type(data1)

    UseMethod("sits_merge", data1)
}
#' @rdname sits_merge
#'
sits_merge.sits <- function(data1, data2) {

    # precondition
    .sits_test_tibble(data1)
    .sits_test_tibble(data2)

    # backward compatibility
    data1 <- .sits_tibble_rename(data1)
    data2 <- .sits_tibble_rename(data2)

    # if some parameter is empty returns the another one
    assertthat::assert_that(nrow(data1) > 0 & nrow(data2) > 0,
                            msg = "sits_merge: invalid input data"
    )

    # verify if data1.tb and data2.tb has the same number of rows
    assertthat::assert_that(NROW(data1) == NROW(data2),
                            msg = "sits_merge: cannot merge tibbles of different sizes"
    )

    # are the names of the bands different?
    # if they are not
    bands1 <- sits_bands(data1)
    bands2 <- sits_bands(data2)
    if (any(bands1 %in% bands2) || any(bands2 %in% bands1)) {
        if (!(any(".new" %in% bands1)) & !(any(".new" %in% bands2))) {
            bands2 <- paste0(bands2, ".new")
        } else {
            bands2 <- paste0(bands2, ".nw")
        }
        sits_bands(data2) <- bands2
    }
    # prepare result
    result <- data1

    # merge time series
    result$time_series <- purrr::map2(
        data1$time_series,
        data2$time_series,
        function(ts1, ts2) {
            ts3 <- dplyr::bind_cols(ts1, dplyr::select(ts2, -Index))
            return(ts3)
        }
    )
    return(result)
}
#' @rdname sits_merge
#'
sits_merge.cube <- function(data1, data2) {
    # preconditions
    assertthat::assert_that(nrow(data1) == 1 & nrow(data2) == 1,
                            msg = "merge only works from simple cubes (one tibble row)"
    )
    assertthat::assert_that(data1$satellite == data2$satellite,
                            msg = "cubes from different satellites"
    )
    assertthat::assert_that(data1$sensor == data2$sensor,
                            msg = "cubes from different sensors"
    )
    assertthat::assert_that(all(sits_bands(data1) != sits_bands(data2)),
                            msg = "merge cubes requires different bands in each cube"
    )
    assertthat::assert_that(all(sits_bbox(data1) == sits_bbox(data2)),
                            msg = "merge cubes requires same bounding boxes"
    )
    assertthat::assert_that(data1$xres == data2$xres &
                                data1$yres == data2$yres,
                            msg = "merge cubes requires same resolution"
    )
    assertthat::assert_that(all(sits_timeline(data1) == sits_timeline(data2)),
                            msg = "merge cubes requires same timeline"
    )

    # get the file information
    file_info_1 <- data1$file_info[[1]]
    file_info_2 <- data2$file_info[[1]]

    file_info_1 <- file_info_1 %>%
        dplyr::bind_rows(file_info_2) %>%
        dplyr::arrange(date)
    # merge the file info and the bands
    data1$file_info[[1]] <- file_info_1
    data1$bands[[1]] <- c(sits_bands(data1), sits_bands(data2))

    return(data1)
}
