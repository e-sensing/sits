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
    reference_tl <- .cube_timeline(data1)[[1]]
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

.merge_timeline_has_same_length <- function(data1, data2) {
    length(.cube_timeline(data1)[[1]]) == length(.cube_timeline(data2)[[1]])
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
    # Merge
    dplyr::bind_rows(data1, data2)
}

# ---- Regular cubes ----
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
    # get tile overlaps
    tiles_overlaps <- .merge_tiles_overlaps(data1, data2)
    # pre-condition - timelines overlaps
    # in case of regular cube it is assumed the timeline must overlap
    # to avoid the creation of inconsistent / irregular cubes
    .merge_regular_check_timeline_overlaps(data1, data2)
    # pre-condition - timelines with same period
    .merge_regular_check_periods(data1, data2)
    # pre-condition - equal bands must be from the same sensor
    # bands with the same name, must be from the same sensor to avoid confusion
    .merge_check_band_sensor(data1, data2)
    # pre-condition - bands must intersect if more then 1 tile is provided
    if (length(tiles_overlaps) > 1) {
        .merge_check_bands_intersects(data1, data2)
    }
    # Extract band intersects
    bands_intersects <- .merge_bands_intersects(data1, data2)
    # Use only intersect bands
    data1 <- .select_raster_bands(data1, bands_intersects)
    data2 <- .select_raster_bands(data2, bands_intersects)
    # Check timeline consistency
    if (!.merge_timeline_has_same_length(data1, data2)) {
        # TODO: warning avisando o usuário que os cubos tem timelines
        # com lengths diferentes
        t1 <- .cube_timeline(data1)[[1]]
        t2 <- .cube_timeline(data2)[[1]]

        if (length(t1) > length(t2)) {
            ref <- t1[t1 >= min(t2) & t1 <= max(t2)]
        } else {
            ref <- t2[t2 >= min(t1) & t2 <= max(t1)]
        }

        .check_that(.has(ref))

        data1 <- .cube_filter_interval(
            data1, start_date = min(ref), end_date = max(ref)
        )

        data2 <- .cube_filter_interval(
            data2, start_date = min(ref), end_date = max(ref)
        )

        if (length(.cube_timeline(data1)) !=  length(.cube_timeline(data2))) {
            min_length <- min(c(length(.cube_timeline(data1)),
                                length(.cube_timeline(data2))))

            data1 <- .cube_filter_dates(
                data1, .cube_timeline(data1)[[1]][seq_len(min_length)]
            )
            data2 <- .cube_filter_dates(
                data2, .cube_timeline(data2)[[1]][seq_len(min_length)]
            )
        }
    }
    # define the strategy (default - merge tiles)
    merge_strategy <- NULL
    # case: same tiles, merge file info
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
    # get tile overlaps
    tiles_overlaps <- .merge_tiles_overlaps(data1, data2)
    # pre-condition - equal bands from the same sensor
    # bands with the same name, must be from the same sensor to avoid confusion
    .merge_check_band_sensor(data1, data2)
    # pre-condition - bands must intersect if more then 1 tile is provided
    if (length(tiles_overlaps) > 1) {
        .merge_check_bands_intersects(data1, data2)
    }
    # Extract band intersects
    bands_intersects <- .merge_bands_intersects(data1, data2)
    # Use only intersect bands
    data1 <- .select_raster_bands(data1, bands_intersects)
    data2 <- .select_raster_bands(data2, bands_intersects)
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

# ---- Special case: DEM Cube ----
.merge_dem_cube <- function(data1, data2) {
    # define cubes
    dem_cube <- data1
    other_cube <- data2
    # check which cube is the DEM
    if (inherits(data2, "dem_cube")) {
        # move DEM cube (de)
        dem_cube <- data2
        other_cube <- data1
    }
    tiles <- .cube_tiles(other_cube)
    # update the timeline of the cube with single time step
    dem_cube <- .map_dfr(tiles, function(tile_name) {
        tile_other <- .cube_filter_tiles(other_cube, tile_name)
        tile_dem <- .cube_filter_tiles(dem_cube, tile_name)
        # Get data1 timeline.
        d1_tl <- unique(as.Date(.cube_timeline(tile_other)[[1]]))
        # Create new `file_info` using dates from `data1` timeline.
        fi_new <- purrr::map(.tile_timeline(tile_other), function(date_row) {
            fi <- .fi(tile_dem)
            fi[["date"]] <- as.Date(date_row)
            fi
        })
        # Assign the new `file_into` into `data2`
        tile_dem[["file_info"]] <- list(dplyr::bind_rows(fi_new))
        tile_dem
    })
    # merge cubes and return
    .merge_strategy_file(other_cube, dem_cube, FALSE)
}

.merge_hls_cube <- function(data1, data2) {
    if ((.cube_collection(data1) == "HLSS30" ||
         .cube_collection(data2) == "HLSS30")) {
        data1[["collection"]] <- "HLSS30"
    }

    # merge cubes and return
    .merge_strategy_file(data1, data2, FALSE)
}
