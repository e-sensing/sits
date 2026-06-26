# ---- General utilities ----
#' @title  Check if two cube have the same bands
#' @name   .merge_has_equal_bands
#' @author Felipe Carvalho, \email{filipe.carvalho@@inpe.br}
#' @author Felipe Carlos,   \email{efelipecarlos@@gmail.com}
#' @noRd
#' @param  data1     Data cube
#' @param  data2     Data cube
#' @return TRUE/FALSE
.merge_has_equal_bands <- function(data1, data2) {
    # get cube bands
    data1_bands <- .cube_bands(data1)
    data2_bands <- .cube_bands(data2)
    # verify if both cubes have the same bands
    has_same_bands <- all(data1_bands %in% data2_bands)
    # if has the same bands, do check for consistency
    if (has_same_bands) {
        # get bands intersects
        bands_intersects <- setdiff(data1_bands, data2_bands)
        # no extra bands are allowed when the same bands are defined
        .check_that(length(bands_intersects) == 0L)
        # same sensor is required when bands with the same names are defined
        .check_that(all(.cube_sensor(data1) %in% .cube_sensor(data2)))
    }
    # return
    has_same_bands
}
#' @title  Check if two cube have common tiles
#' @name   .merge_get_common_tiles
#' @author Felipe Carvalho, \email{filipe.carvalho@@inpe.br}
#' @author Felipe Carlos,   \email{efelipecarlos@@gmail.com}
#' @noRd
#' @param  data1     Data cube
#' @param  data2     Data cube
#' @return Tiles that are part of both cubes
.merge_get_common_tiles <- function(data1, data2) {
    # Extract common tiles
    d1_tiles <- .cube_tiles(data1)
    d2_tiles <- .cube_tiles(data2)
    # Extract overlaps
    intersect(d1_tiles, d2_tiles)
}
#' @title  Adjust timeline strategies strategies
#' @name   .merge_zipper_strategy
#' @author Felipe Carvalho, \email{filipe.carvalho@@inpe.br}
#' @author Felipe Carlos,   \email{efelipecarlos@@gmail.com}
#' @noRd
#' @param  t1     Timeline of data cube 1
#' @param  t2     Timeline of data cube 2
#' @return Common timeline for both cubes
.merge_zipper_strategy <- function(t1, t2) {
    # define vector to store overlapping dates
    t_overlap <- NULL
    # define the size of the `for` - size of the reference time-series
    ts_reference_len <- length(t1) - 1L
    # search the overlapping dates
    for (idx in seq_len(ts_reference_len)) {
        # reference interval (`t1`)
        reference_interval <- t1[idx:(idx + 1L)]
        # verify which dates are in the reference interval
        t2_in_interval <- t2 >= t1[idx] & t2 <= t1[idx + 1L]
        # get the interval dates
        t2_interval_dates <- t2[t2_in_interval]
        # if have interval, process them
        if (.has(t2_interval_dates)) {
            # if all t2 dates are in the interval, just save them
            if (all(reference_interval %in% t2_interval_dates)) {
                t_overlap <- c(t_overlap, t2_interval_dates)
            } else {
                # if not, save the reference interval and the min value of
                # the t2 interval dates.
                # this ensures there are not two dates in the same interval
                t_overlap <- c(
                    t_overlap, # dates storage
                    reference_interval, # current interval
                    min(t2_interval_dates) # min t2 interval date
                )
            }
        }
    }
    # sort and remove duplicated values
    sort(unique(as.Date(t_overlap)))
}

# ---- Merge strategies ----
#' @title  Define merge strategy based on combining files
#' @name   .merge_strategy_file
#' @author Felipe Carvalho, \email{filipe.carvalho@@inpe.br}
#' @author Felipe Carlos,   \email{efelipecarlos@@gmail.com}
#' @noRd
#' @param  data1     Data cube
#' @param  data2     Data cube
#' @return           Merged data cube
.merge_strategy_file <- function(data1, data2) {
    # extract tiles
    tiles <- .merge_get_common_tiles(data1, data2)
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
            x
        })
    })
}
#' @title  Define merge strategy based on binding tiles
#' @name   .merge_strategy_bind
#' @author Felipe Carvalho, \email{filipe.carvalho@@inpe.br}
#' @author Felipe Carlos,   \email{efelipecarlos@@gmail.com}
#' @noRd
#' @param  data1     Data cube
#' @param  data2     Data cube
#' @return           Merged data cube
.merge_strategy_bind <- function(data1, data2) {
    # Merge
    dplyr::bind_rows(data1, data2)
}

#' @title  Define merge strategy based on densifying the cube
#' @name   .merge_strategy_densify
#' @author Felipe Carvalho, \email{filipe.carvalho@@inpe.br}
#' @author Felipe Carlos,   \email{efelipecarlos@@gmail.com}
#' @noRd
#' @param  data1     Data cube
#' @param  data2     Data cube
#' @return           Merged data cube
.merge_cube_densify <- function(data1, data2) {
    # get tile overlaps
    common_tiles <- .merge_get_common_tiles(data1, data2)
    # define the strategy (default - merge tiles)
    merge_strategy <- NULL
    # case 1: same tiles, merge file info
    if (.has(common_tiles)) {
        merge_strategy <- .merge_strategy_file
    } else {
        # case 2: different tiles, merge cube rows
        merge_strategy <- .merge_strategy_bind
    }
    # merge
    merged_cube <- merge_strategy(data1, data2)
    # include `combined` in cubes merged with bind
    if (!.has(common_tiles)) {
        class(merged_cube) <- c("combined_cube", class(data1))
    }
    # return
    merged_cube
}

#' @title  Define merge strategy based on increasing the timeline
#' @name   .merge_strategy_compactify
#' @author Felipe Carvalho, \email{filipe.carvalho@@inpe.br}
#' @author Felipe Carlos,   \email{efelipecarlos@@gmail.com}
#' @noRd
#' @param  data1     Data cube
#' @param  data2     Data cube
#' @return           Merged data cube
.merge_cube_compactify <- function(data1, data2) {
    # extract tiles
    tiles <- .merge_get_common_tiles(data1, data2)
    # align timeline tile by tile.
    merged_cube <- .map_dfr(tiles, function(tile) {
        # get tiles
        tile1 <- .cube_filter_tiles(data1, tile)
        tile2 <- .cube_filter_tiles(data2, tile)
        # get tile timelines
        ts1 <- .tile_timeline(tile1)
        ts2 <- .tile_timeline(tile2)
        # adjust timeline using zipper strategy
        ts_overlap <- .merge_zipper_strategy(ts1, ts2)
        # filter cubes in the overlapping dates
        tile1 <- .cube_filter_dates(tile1, ts_overlap)
        tile2 <- .cube_filter_dates(tile2, ts_overlap)
        # merge by file
        .merge_strategy_file(tile1, tile2)
    })
}
#' @title  Define merge strategy based on symmetric interleaving of the timeline
#' @name   .merge_strategy_zipper
#' @author Felipe Carvalho, \email{filipe.carvalho@@inpe.br}
#' @author Felipe Carlos,   \email{efelipecarlos@@gmail.com}
#' @noRd
#' @param  data1     Data cube
#' @param  data2     Data cube
#' @return           Merged data cube
.merge_strategy_zipper <- function(data1, data2) {
    # Get data cubes timeline
    t1 <- .cube_timeline(data1)[[1L]]
    t2 <- .cube_timeline(data2)[[1L]]

    # Find common period
    start_date <- max(min(t1), min(t2))
    end_date <- min(max(t1), max(t2))

    t1_overlap <- t1[t1 >= start_date & t1 <= end_date]
    t2_overlap <- t2[t2 >= start_date & t2 <= end_date]

    # Check identical lengths in overlap
    if (length(t1_overlap) != length(t2_overlap) || length(t1_overlap) == 0) {
        stop(.conf("messages", ".merge_regular_interleaved"), call. = FALSE)
    # Get overlapped dates
    for (i in seq_along(t2)) {
        t2_int <- lubridate::interval(
            lubridate::ymd(t2[i]), lubridate::ymd(t2[i]) + t2_period - 1L
        )
        overlapped_dates <- purrr::map(seq_along(t1), function(j) {
            t1_int <- lubridate::interval(
                lubridate::ymd(t1[j]), lubridate::ymd(t1[j]) + t1_period - 1L
            )
            lubridate::int_overlaps(t2_int, t1_int)
        })

        dates <- t1[unlist(overlapped_dates)]
        dates <- setdiff(dates, t1_date)
        if (.has(dates)) {
            t1_date[[i]] <- as.Date(min(dates))
            t2_date[[i]] <- as.Date(t2[i])
        }
    }

    # Check strict interleaving in overlap
    diff1 <- all(diff(c(rbind(t1_overlap, t2_overlap))) >= 0)
    diff2 <- all(diff(c(rbind(t2_overlap, t1_overlap))) >= 0)

    if (!(diff1 || diff2)) {
        stop(.conf("messages", ".merge_regular_interleaved"), call. = FALSE)
    }

    # Symmetric reference timeline
    t_ref <- pmin(t1_overlap, t2_overlap)

    # Transform list to vector date
    t_ref <- as.Date(t_ref)

    # Filter overlapped dates
    data1 <- .cube_filter_dates(data1, t1_overlap)
    data2 <- .cube_filter_dates(data2, t2_overlap)

    # Change file date to match reference timeline for both cubes
    data1 <- slider::slide_dfr(data1, function(y) {
        fi_list <- purrr::map(.tile_bands(y), function(band) {
            fi_band <- .fi_filter_bands(.fi(y), bands = band)
            fi_band[["date"]] <- t_ref
            fi_band
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

    data2 <- slider::slide_dfr(data2, function(y) {
        fi_list <- purrr::map(.tile_bands(y), function(band) {
            fi_band <- .fi_filter_bands(.fi(y), bands = band)
            fi_band[["date"]] <- t_ref
            fi_band
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
    .merge_strategy_file(data1, data2)
}
#' @title  Merges cubes based on adequate strategy
#' @name   .merge
#' @author Felipe Carvalho, \email{filipe.carvalho@@inpe.br}
#' @author Felipe Carlos,   \email{efelipecarlos@@gmail.com}
#' @noRd
#' @param  data1     Data cube
#' @param  data2     Data cube
#' @return           Strategy to be used
.merge <- function(data1, data2) {
    merge_type <- .merge_type(data1, data2)
    class(data1) <- c(merge_type, class(data1))
    UseMethod(".merge", data1)
}
#' @title  Merge strategy when one of the cubes is a DEM
#' @name   .merge.dem_case
#' @author Felipe Carvalho, \email{filipe.carvalho@@inpe.br}
#' @author Felipe Carlos,   \email{efelipecarlos@@gmail.com}
#' @noRd
#' @param  data1     Data cube
#' @param  data2     Data cube
#' @return           Merged data cube
#' @export
.merge.dem_case <- function(data1, data2) {
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
    .merge_strategy_file(other_cube, dem_cube)
}

#' @title  Merge strategy for Harmonized Landsat-Sentinel data
#' @name   .merge.hls_case
#' @author Felipe Carvalho, \email{filipe.carvalho@@inpe.br}
#' @author Felipe Carlos,   \email{efelipecarlos@@gmail.com}
#' @noRd
#' @param  data1     Data cube
#' @param  data2     Data cube
#' @return           Merged data cube
.merge.hls_case <- function(data1, data2) {
    if ((.cube_collection(data1) == "HLSS30" ||
        .cube_collection(data2) == "HLSS30")) {
        data1[["collection"]] <- "HLSS30"
    }

    # merge cubes and return
    .merge_strategy_file(data1, data2)
}

#' @title  Merge strategy for regular cubes
#' @name   .merge.regular_case
#' @author Felipe Carvalho, \email{filipe.carvalho@@inpe.br}
#' @author Felipe Carlos,   \email{efelipecarlos@@gmail.com}
#' @noRd
#' @param  data1     Data cube
#' @param  data2     Data cube
#' @return           Merged data cube
.merge.regular_case <- function(data1, data2) {
    # Rule 1: Do the cubes have same tiles?
    .check_cube_tiles(data1, .cube_tiles(data2))
    .check_cube_tiles(data2, .cube_tiles(data1))

    # Check for conflicts in common bands before any filtering
    common_bands <- intersect(.cube_bands(data1), .cube_bands(data2))
    if (.has(common_bands)) {
        cb1 <- .cube_filter_bands(data1, common_bands)
        cb2 <- .cube_filter_bands(data2, common_bands)

        tiles <- .merge_get_common_tiles(cb1, cb2)
        purrr::walk(tiles, function(tile) {
            fi1 <- .fi(.cube_filter_tiles(cb1, tile))
            fi2 <- .fi(.cube_filter_tiles(cb2, tile))

            fi_bind <- dplyr::bind_rows(fi1, fi2)
            dups <- duplicated(dplyr::select(fi_bind, dplyr::all_of(c("band", "date"))))
            if (any(dups)) {
                dups_keys <- dplyr::distinct(
                    dplyr::select(fi_bind[dups, ], dplyr::all_of(c("band", "date")))
                )
                for (i in seq_len(nrow(dups_keys))) {
                    b <- dups_keys[["band"]][[i]]
                    d <- dups_keys[["date"]][[i]]
                    dup_records <- fi_bind[fi_bind[["band"]] == b & fi_bind[["date"]] == d, ]
                    if (length(unique(dup_records[["fid"]])) > 1) {
                        stop(.conf("messages", ".merge_regular_conflict"), call. = FALSE)
                    }
                }
            }
        })
    }

    # Rule 2: Do they have the same bands?
    if (all(.cube_bands(data1) %in% .cube_bands(data2)) &&
        all(.cube_bands(data2) %in% .cube_bands(data1))) {
        # densify!
        merged_cube <- .merge_cube_densify(data1, data2)
        return(merged_cube)
    }

    # Filter bands to merge
    bands_to_merge <- setdiff(.cube_bands(data2), .cube_bands(data1))
    data2 <- .cube_filter_bands(data2, bands_to_merge)

    # Rule 3: Do the cubes have same timeline?
    if (all(.cube_timeline(data1) %in% .cube_timeline(data2)) &&
        all(.cube_timeline(data2) %in% .cube_timeline(data1))) {
        merged_cube <- .merge_strategy_file(data1, data2)
    } else {
        merged_cube <- .merge_strategy_zipper(data1, data2)
    }
    # Return merged cube
    merged_cube
}

#' @title  Merges cubes based on adequate strategy
#' @name   .merge
#' @author Felipe Carvalho, \email{filipe.carvalho@@inpe.br}
#' @author Felipe Carlos,   \email{efelipecarlos@@gmail.com}
#' @noRd
#' @param  data1     Data cube
#' @param  data2     Data cube
#' @return           Strategy to be used
.merge_type <- function(data1, data2) {
    if (.merge_type_dem(data1, data2)) {
        return("dem_case")
    }
    if (.merge_type_hls(data1, data2)) {
        return("hls_case")
    }
    if (.merge_type_regular(data1, data2)) {
        return("regular_case")
    }
    # find no alternative? error messages
    stop(.conf("messages", ".merge_type"), toString(class(data1)))
}
.merge_type_regular <- function(data1, data2) {
    .cube_is_regular(data1) &&
        .cube_is_regular(data2)
}
.merge_type_dem <- function(data1, data2) {
    any(inherits(data1, "dem_cube"), inherits(data2, "dem_cube"))
}
.merge_type_hls <- function(data1, data2) {
    all(inherits(data1, "hls_cube"), inherits(data2, "hls_cube"))
}
