.merge_diff_timelines <- function(t1, t2) {
    abs(as.Date(t1) - as.Date(t2))
}

.cube_merge <- function(data1, data2) {
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

.merge_irregular_cube <- function(data1, data2) {
    merged_cube <- dplyr::bind_rows(data1, data2)
    class(merged_cube) <- c("combined_cube", class(data1))
    return(merged_cube)
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

.merge_single_timeline <- function(data1, data2) {
    tiles <- .cube_tiles(data1)
    # update the timeline of the cube with single time step (`data2`)
    data2 <- .map_dfr(tiles, function(tile_name) {
        tile_data1 <- .cube_filter_tiles(data1, tile_name)
        tile_data2 <- .cube_filter_tiles(data2, tile_name)
        # Get data1 timeline.
        d1_tl <- unique(as.Date(.cube_timeline(tile_data1)[[1]]))
        # Create new `file_info` using dates from `data1` timeline.
        fi_new <- purrr::map(.tile_timeline(tile_data1), function(date_row) {
            fi <- .fi(tile_data2)
            fi[["date"]] <- as.Date(date_row)
            fi
        })
        # Assign the new `file_into` into `data2`
        tile_data2[["file_info"]] <- list(dplyr::bind_rows(fi_new))
        tile_data2
    })
    # Merge cubes and return
    .cube_merge(data1, data2)
}
