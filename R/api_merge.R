# ---- General utilities ----
.merge_bands_intersects <- function(data1, data2) {
    # Extract bands
    d1_bands <- .cube_bands(data1)
    d2_bands <- .cube_bands(data2)
    # Extract overlaps
    intersect(d1_bands, d2_bands)
}

.merge_tiles_overlaps <- function(data1, data2) {
    # Extract common tiles
    d1_tiles <- .cube_tiles(data1)
    d2_tiles <- .cube_tiles(data2)
    # Extract overlaps
    intersect(d1_tiles, d2_tiles)
}

.merge_adjust_timeline <- function(data1, data2) {
    # reference timeline
    reference_tl <- .cube_timeline(data1)
    # Adjust dates / bands
    slider::slide_dfr(data2, function(y) {
        fi_list <- purrr::map(.tile_bands(y), function(band) {
            fi_band <- .fi_filter_bands(.fi(y), bands = band)
            fi_band[["date"]] <- reference_tl
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
}

.merge_get_ts_within <- function(data1, data2) {
    # extract timelines
    d1_tl <- .cube_timeline(data1)
    d2_tl <- .cube_timeline(data2)

    # Check if all dates in A are in B
    if (all(datesA %in% datesB)) {
        return(datesA)  # A is contained in B
    }

    # Check if all dates in B are in A
    if (all(datesB %in% datesA)) {
        return(datesB)  # B is contained in A
    }

    return(NULL)  # Neither is contained
}

.merge_check_bands_intersects <- function(data1, data2) {
    # Extract band intersects
    bands_intersects <- .merge_bands_intersects(data1, data2)
    # Check if there are intersects
    .check_that(length(bands_intersects) >= 1)
}

.merge_check_band_sensor <- function(data1, data2) {
    # Extract band intersects
    bands_intersects <- .merge_bands_intersects(data1, data2)
    # If has overlaps, the sensor must be the same
    if (length(bands_intersects) >= 1) {
        .check_that(data1[["sensor"]] == data2[["sensor"]])
    }
}

# ---- Merge strategies ----
.merge_strategy_file <- function(data1, data2, adjust_timeline) {
    # adjust second cube timeline, based on the first cube
    if (adjust_timeline) {
        data2 <- .merge_adjust_timeline(data1, data2)
    }
    # extract tiles
    tiles <- .merge_tiles_overlaps(data1, data2)
    # merge cubes
    .map_dfr(tiles, function(tile) {
        # select data in the selected tile
        data1_in_tile <- .select_raster_tiles(data1, tile)
        data2_in_tile <- .select_raster_tiles(data2, tile)
        # change file name to match reference timeline
        slider::slide2_dfr(data1_in_tile, data2_in_tile, function(x, y) {
            # arrange by `date`, `band` and `fid`
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
            # return
            return(x)
        })
    })
}

.merge_strategy_bind <- function(data1, data2, adjust_timeline) {
    # Adjust second cube timeline, based on the first cube
    if (adjust_timeline) {
        data2 <- .merge_adjust_timeline(data1, data2)
    }
    # Extract band intersects
    bands_intersects <- .merge_bands_intersects(data1, data2)
    # Use only intersect bands
    data1 <- .select_raster_bands(data1, bands_intersects)
    data2 <- .select_raster_bands(data2, bands_intersects)
    # Merge
    dplyr::bind_rows(data1, data2)
}

# ---- Regular cubes ----
# .merge_regular_cut_timeline <- function(data1, data2) {
#     # extract timelines
#     d1_tl <- .cube_timeline(data1)
#     d2_tl <- .cube_timeline(data2)
#     # extract tiles
#     tiles <- .merge_tiles_overlaps(data1, data2)
#     # merge cubes
#     .map_dfr(tiles, function(tile) {
#         # select data in the selected tile
#         data1_in_tile <- .select_raster_tiles(data1, tile)
#         data2_in_tile <- .select_raster_tiles(data2, tile)
#         # extract timelines
#         d1_tl <- .tile_timeline(data1_in_tile)
#         d2_tl <- .tile_timeline(data2_in_tile)
#         # get min/max dates
#         min_tl <- min(
#             min(d1_tl), min(d2_tl)
#         )
#         max_tl <- max(
#             max(d1_tl), max(d2_tl)
#         )
#         # cut timeline
#         .tile_filter_interval(tile, min_tl, max_tl)
#     })
# }

.merge_regular_check_timeline_overlaps <- function(data1, data2) {
    # extract timelines
    d1_tl <- .cube_timeline(data1)
    d2_tl <- .cube_timeline(data2)
    # check overlaps
    slider::slide2_vec(d1_tl, d2_tl, function(x, y) {
        x <- .dissolve(x)
        y <- .dissolve(y)

        .check_that(length(.timeline_has_overlap(x, y)) >= 1)
    })
}

.merge_regular_check_periods <- function(data1, data2) {
    # get cubes timeline
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
}

.merge_regular_cube <- function(data1, data2) {
    # pre-condition - timelines overlaps
    # in case of regular cube it is assumed the timeline must overlap
    # to avoid the creation of inconsistent / irregular cubes
    .merge_regular_check_timeline_overlaps(data1, data2)
    # pre-condition - timelines with same period
    .merge_regular_check_periods(data1, data2)
    # pre-condition - equal bands must be from the same sensor
    # bands with the same name, must be from the same sensor to avoid confusion
    .merge_check_band_sensor(data1, data2)
    # ToDo: Cut timeline at overlapping intervals when length(ts1) != length(ts2)
    # get tile overlaps
    tiles_overlaps <- .merge_tiles_overlaps(data1, data2)
    # define the strategy (default - merge tiles)
    merge_strategy <- NULL
    # case: same tiles, merge file info
    tiles_overlaps <- .merge_tiles_overlaps(data1, data2)
    if (.has(tiles_overlaps)) {
        merge_strategy <- .merge_strategy_file
    # case 2: different tiles, merge tile rows
    } else {
        merge_strategy <- .merge_strategy_bind
    }
    # merge
    merge_strategy(data1, data2, TRUE)
}

# ---- Irregular cubes ----
.merge_irregular_cube <- function(data1, data2) {
    # pre-condition - equal bands from the same sensor
    # bands with the same name, must be from the same sensor to avoid confusion
    .merge_check_band_sensor(data1, data2)
    # get tile overlaps
    tiles_overlaps <- .merge_tiles_overlaps(data1, data2)
    # define the strategy (default - merge tiles)
    merge_strategy <- NULL
    # case: same tiles, merge file info
    if (.has(tiles_overlaps)) {
        merged_cube <- .merge_strategy_file(data1, data2, FALSE)
    # case 2: different tiles, merge tile rows
    } else {
        merged_cube <- .merge_strategy_bind(data1, data2, FALSE)
        class(merged_cube) <- c("combined_cube", class(data1))
    }
    # return
    return(merged_cube)
}

# .merge_irregular_cube <- function(data1, data2) {
#     # pre-condition - intersecting bands
#     .merge_check_bands_intersects(data1, data2)
#     # pre-condition - equal bands from the same sensor
#     # bands with the same name, must be from the same sensor to avoid confusion
#     .merge_regular_check_band_sensor(data1, data2)
#     # merge
#     merged_cube <- .merge_strategy_tile(data1, data2, FALSE)
#     # assign a new class, meaning the cube must be regularized to be used
#     class(merged_cube) <- c("combined_cube", class(data1))
#     # return
#     return(merged_cube)
# }

# Already incorporated
# .cube_merge <- function(data1, data2) {
#     data1 <- slider::slide2_dfr(data1, data2, function(x, y) {
#         .fi(x) <- dplyr::arrange(
#             dplyr::bind_rows(.fi(x), .fi(y)),
#             .data[["date"]],
#             .data[["band"]],
#             .data[["fid"]]
#         )
#         # remove duplicates
#         .fi(x) <- dplyr::distinct(
#             .fi(x),
#             .data[["band"]],
#             .data[["date"]],
#             .keep_all = TRUE
#         )
#
#         return(x)
#     })
#     return(data1)
# }

# ToDo: Special case
.merge_equal_cube <- function(data1, data2) {
    if (inherits(data1, "hls_cube") && inherits(data2, "hls_cube") &&
        (.cube_collection(data1) == "HLSS30" ||
         .cube_collection(data2) == "HLSS30")) {
        data1[["collection"]] <- "HLSS30"
    }

    data1 <- .cube_merge(data1, data2)
    return(data1)
}

# ToDo: Special case
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

# Generalized case
# .merge_distinct_cube <- function(data1, data2) {
#     # Get cubes timeline
#     d1_tl <- unique(as.Date(.cube_timeline(data1)[[1]]))
#     d2_tl <- unique(as.Date(.cube_timeline(data2)[[1]]))
#
#     # get intervals
#     d1_period <- as.integer(
#         lubridate::as.period(lubridate::int_diff(d1_tl)), "days"
#     )
#     d2_period <- as.integer(
#         lubridate::as.period(lubridate::int_diff(d2_tl)), "days"
#     )
#     # pre-condition - are periods regular?
#     .check_that(
#         length(unique(d1_period)) == 1 && length(unique(d2_period)) == 1
#     )
#     # pre-condition - Do cubes have the same periods?
#     .check_that(
#         unique(d1_period) == unique(d2_period)
#     )
#     # pre-condition - are the cubes start date less than period timeline?
#     .check_that(
#         abs(d1_period[[1]] - d2_period[[2]]) <= unique(d2_period)
#     )
#
#     # Change file name to match reference timeline
#     data2 <- slider::slide_dfr(data2, function(y) {
#         fi_list <- purrr::map(.tile_bands(y), function(band) {
#             fi_band <- .fi_filter_bands(.fi(y), bands = band)
#             fi_band[["date"]] <- d1_tl
#             return(fi_band)
#         })
#         tile_fi <- dplyr::bind_rows(fi_list)
#         tile_fi <- dplyr::arrange(
#             tile_fi,
#             .data[["date"]],
#             .data[["band"]],
#             .data[["fid"]]
#         )
#         y[["file_info"]] <- list(tile_fi)
#         y
#     })
#     # Merge the cubes
#     data1 <- .cube_merge(data1, data2)
#     # Return cubes merged
#     return(data1)
# }

# not used anywhere!
# .merge_diff_timelines <- function(t1, t2) {
#     abs(as.Date(t1) - as.Date(t2))
# }


