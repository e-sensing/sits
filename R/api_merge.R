#' @title  Keep the tiles that are part of both cubes
#' @name   .merge_tiles
#' @description Keeps the tiles that both cubes have and stops if there is none.
#'   Warns when a tile of either cube is left out.
#' @author Rolf Simoes,     \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @param  data1  Data cube
#' @param  data2  Data cube
#' @return List with both cubes filtered to their common tiles
.merge_tiles <- function(data1, data2) {
    tiles <- intersect(.cube_tiles(data1), .cube_tiles(data2))
    .check_that(.has(tiles), msg = .conf("messages", ".merge_tiles"))
    if (!all(c(.cube_tiles(data1), .cube_tiles(data2)) %in% tiles)) {
        .message_warnings_merge_tiles_dropped()
    }
    list(
        .cube_filter_tiles(data1, tiles),
        .cube_filter_tiles(data2, tiles)
    )
}
#' @title  Check that two cubes have one file for each band and date
#' @name   .merge_check_files
#' @description Stops if, in the union of the files of both cubes, a band has
#'   two files on one date. It does not merge the cubes.
#' @author Rolf Simoes,     \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @param  data1  Data cube
#' @param  data2  Data cube with the same tiles
#' @return Called for side effects
.merge_check_files <- function(data1, data2) {
    one_file <- slider::slide_lgl(data1, function(tile) {
        tile2 <- .cube_filter_tiles(data2, .tile_name(tile))
        .fi_has_one_file(.fi_merge(.fi(tile), .fi(tile2)))
    })
    .check_that(
        all(one_file),
        msg = .conf("messages", ".merge_regular_conflict")
    )
}

#' @title  Does a cube merge while irregular?
#' @name   .merge_allows_irregular
#' @description Tells if a cube can be merged without being regular.
#'   Only HLS cubes can, as two sensors give images on other dates.
#' @author Rolf Simoes,     \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @param  cube  Data cube
#' @return TRUE or FALSE
.merge_allows_irregular <- function(cube) {
    UseMethod(".merge_allows_irregular", cube)
}
#' @export
.merge_allows_irregular.hls_cube <- function(cube) {
    TRUE
}
#' @export
.merge_allows_irregular.default <- function(cube) {
    FALSE
}
#' @title  Collection of a merged cube
#' @name   .merge_collection
#' @description Gives the collection of the merged cube. It is the collection
#'   of the first cube; for HLS it is HLSS30 when either cube has it.
#' @author Rolf Simoes,     \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @param  data1  Data cube
#' @param  data2  Data cube
#' @return Collection name
.merge_collection <- function(data1, data2) {
    UseMethod(".merge_collection", data1)
}
#' @export
.merge_collection.hls_cube <- function(data1, data2) {
    # an HLS cube with Sentinel-2 images is labelled as the S30 collection
    if (.cube_collection(data1) == "HLSS30" ||
        .cube_collection(data2) == "HLSS30") {
        return("HLSS30")
    }
    .cube_collection(data1)
}
#' @export
.merge_collection.default <- function(data1, data2) {
    .cube_collection(data1)
}

#' @title  Check that regular cubes give a regular cube
#' @name   .merge_check_result
#' @description Stops if the merge of two regular cubes is not a regular cube.
#'   Cubes that merge while irregular are not checked.
#' @author Rolf Simoes,     \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @param  data1   Data cube
#' @param  data2   Data cube
#' @param  merged  Merged data cube
#' @return Called for side effects
.merge_check_result <- function(data1, data2, merged) {
    # safety net: no rule may turn regular inputs into an irregular cube
    if (!(.merge_allows_irregular(data1) && .merge_allows_irregular(data2)) &&
        .cube_is_regular(data1) && .cube_is_regular(data2)) {
        .check_that(
            .cube_is_regular(merged),
            msg = .conf("messages", ".merge_irregular_result")
        )
    }
}

# ---- Merge strategies ----
#' @title  Merge the files of two cubes tile by tile
#' @name   .merge_strategy_file
#' @description Adds to each tile of the first cube the files of the same tile
#'   in the second cube, and sets the file ids of the result.
#' @author Felipe Carvalho, \email{filipe.carvalho@@inpe.br}
#' @author Felipe Carlos,   \email{efelipecarlos@@gmail.com}
#' @author Rolf Simoes,     \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @param  data1  Data cube
#' @param  data2  Data cube with the same tiles
#' @return Merged data cube
.merge_strategy_file <- function(data1, data2) {
    .cube_foreach_tile(data1, function(tile) {
        tile2 <- .cube_filter_tiles(data2, .tile_name(tile))
        .fi(tile) <- .fi_fid_by_date(.fi_merge(.fi(tile), .fi(tile2)))
        tile
    })
}
#' @title  Merge two cubes by pairing their interleaved dates
#' @name   .merge_strategy_step
#' @description Pairs the dates of two cubes with no common date and gives each
#'   pair the earlier date of the two. Then merges the files of the cubes.
#'   Stops if the dates cannot be paired.
#' @author Felipe Carvalho, \email{filipe.carvalho@@inpe.br}
#' @author Felipe Carlos,   \email{efelipecarlos@@gmail.com}
#' @author Rolf Simoes,     \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @param  data1  Data cube
#' @param  data2  Data cube with other bands and no common date
#' @return Merged data cube
.merge_strategy_step <- function(data1, data2) {
    t1 <- .cube_timeline(data1)[[1L]]
    t2 <- .cube_timeline(data2)[[1L]]
    pairs <- .timeline_pairs(t1, t2)
    .check_that(
        .has(pairs),
        msg = .conf("messages", ".merge_regular_interleaved")
    )
    dates1 <- t1[pairs[[1L]]]
    dates2 <- t2[pairs[[2L]]]
    # the earlier date of each pair, so both orders give the same date
    ref_dates <- pmin(dates1, dates2)
    set_dates <- function(cube, dates) {
        .cube_foreach_tile(cube, function(tile) {
            .fi(tile) <- .fi_set_dates(.fi(tile), dates, ref_dates)
            tile
        })
    }
    .merge_strategy_file(set_dates(data1, dates1), set_dates(data2, dates2))
}
#' @title  Merge two cubes with the adequate strategy
#' @name   .merge
#' @description Dispatches on the merge type of two cubes with the same tiles.
#' @author Felipe Carvalho, \email{filipe.carvalho@@inpe.br}
#' @author Felipe Carlos,   \email{efelipecarlos@@gmail.com}
#' @author Rolf Simoes,     \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @param  data1  Data cube
#' @param  data2  Data cube with the same tiles
#' @return Merged data cube
.merge <- function(data1, data2) {
    merge_type <- .merge_type(data1, data2)
    class(data1) <- c(merge_type, class(data1))
    UseMethod(".merge", data1)
}
#' @title  Merge strategy when one of the cubes is a DEM
#' @name   .merge.dem_case
#' @description Copies the DEM to every date of the other cube and merges the
#'   files. Stops if the other cube has a DEM band from another file.
#' @author Felipe Carvalho, \email{filipe.carvalho@@inpe.br}
#' @author Felipe Carlos,   \email{efelipecarlos@@gmail.com}
#' @author Rolf Simoes,     \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @param  data1  Data cube
#' @param  data2  Data cube
#' @return Merged data cube
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
    # a cube that has the DEM band from another file cannot take this DEM
    .merge_check_files(other_cube, dem_cube)
    # merge cubes and return
    .merge_strategy_file(other_cube, dem_cube)
}
#' @title  Merge strategy when both cubes are DEM
#' @name   .merge.dem_dem_case
#' @description Checks that both DEM cubes have the same files and returns the
#'   first. Stops if they differ.
#' @author Rolf Simoes,     \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @param  data1  DEM cube
#' @param  data2  DEM cube
#' @return The first DEM cube
.merge.dem_dem_case <- function(data1, data2) {
    .check_that(
        identical(.cube_file_keys(data1), .cube_file_keys(data2)),
        msg = .conf("messages", ".merge_dem_dem")
    )
    data1
}

#' @title  Merge strategy for Harmonized Landsat-Sentinel data
#' @name   .merge.hls_case
#' @description Binds the files of two HLS cubes and keeps their file ids, as
#'   the sensors may have images on one date. Sets the collection to HLSS30 if
#'   one is.
#' @author Felipe Carvalho, \email{filipe.carvalho@@inpe.br}
#' @author Felipe Carlos,   \email{efelipecarlos@@gmail.com}
#' @author Rolf Simoes,     \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @param  data1  Data cube
#' @param  data2  Data cube
#' @return Merged data cube
.merge.hls_case <- function(data1, data2) {
    data1[["collection"]] <- .merge_collection(data1, data2)
    # two sensors may have images on one date, so file ids are kept
    .cube_foreach_tile(data1, function(tile) {
        tile2 <- .cube_filter_tiles(data2, .tile_name(tile))
        .fi(tile) <- .fi_merge(.fi(tile), .fi(tile2))
        tile
    })
}

#' @title  Merge strategy for regular cubes
#' @name   .merge.regular_case
#' @description Merges regular cubes by union of dates if bands are equal, or by
#'   union of bands on the common dates. With no common date, it pairs
#'   interleaved dates. Stops if a band has two files on one date, or if a
#'   common band has no common date.
#' @author Felipe Carvalho, \email{filipe.carvalho@@inpe.br}
#' @author Felipe Carlos,   \email{efelipecarlos@@gmail.com}
#' @author Rolf Simoes,     \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @param  data1  Data cube
#' @param  data2  Data cube
#' @return Merged data cube
.merge.regular_case <- function(data1, data2) {
    .merge_check_files(data1, data2)
    bands1 <- .cube_bands(data1)
    bands2 <- .cube_bands(data2)
    # same bands: union of dates
    if (setequal(bands1, bands2)) {
        return(.merge_strategy_file(data1, data2))
    }
    t1 <- .cube_timeline(data1)[[1L]]
    t2 <- .cube_timeline(data2)[[1L]]
    dates <- t1[t1 %in% t2]
    # other bands: union of bands on the common dates
    if (.has(dates)) {
        return(.merge_strategy_file(
            .cube_filter_dates(data1, dates),
            .cube_filter_dates(data2, dates)
        ))
    }
    # a common band on other dates has no rule to pair them
    .check_that(
        !.has(intersect(bands1, bands2)),
        msg = .conf("messages", ".merge_common_band_dates")
    )
    .merge_strategy_step(data1, data2)
}

#' @title  Define the merge type of two cubes
#' @name   .merge_type
#' @description Gives the type of merge, from the class of the cubes: HLS, DEM
#'   and DEM, one DEM, or regular. Stops if a cube that is not DEM is
#'   irregular.
#' @author Felipe Carvalho, \email{filipe.carvalho@@inpe.br}
#' @author Felipe Carlos,   \email{efelipecarlos@@gmail.com}
#' @author Rolf Simoes,     \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @param  data1  Data cube
#' @param  data2  Data cube
#' @return Name of the case: hls_case, dem_dem_case, dem_case or regular_case
.merge_type <- function(data1, data2) {
    if (.merge_type_hls(data1, data2)) {
        return("hls_case")
    }
    if (.merge_type_dem_dem(data1, data2)) {
        return("dem_dem_case")
    }
    # only HLS cubes merge while irregular; the others are regularized first
    .check_that(
        .merge_type_regular(data1, data2),
        msg = .conf("messages", ".merge_type")
    )
    if (.merge_type_dem(data1, data2)) {
        return("dem_case")
    }
    "regular_case"
}
#' @title  Tell if the cubes are regular
#' @name   .merge_type_regular
#' @description Tells if the cubes are regular. A static cube, such as a DEM,
#'   is not checked, as it is copied to every date of the other cube.
#' @author Rolf Simoes,     \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @param  data1  Data cube
#' @param  data2  Data cube
#' @return TRUE if the cubes that are not static are regular
.merge_type_regular <- function(data1, data2) {
    # a static cube is copied to every date of the other; it need not be regular
    cubes <- purrr::discard(list(data1, data2), .cube_is_static)
    all(purrr::map_lgl(cubes, .cube_is_regular))
}
#' @title  Tell if both cubes are DEM
#' @name   .merge_type_dem_dem
#' @description Tells if both cubes have the class dem_cube.
#' @author Rolf Simoes,     \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @param  data1  Data cube
#' @param  data2  Data cube
#' @return TRUE if both cubes are DEM
.merge_type_dem_dem <- function(data1, data2) {
    all(inherits(data1, "dem_cube"), inherits(data2, "dem_cube"))
}
#' @title  Tell if one of the cubes is a DEM
#' @name   .merge_type_dem
#' @description Tells if at least one cube has the class dem_cube.
#' @author Rolf Simoes,     \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @param  data1  Data cube
#' @param  data2  Data cube
#' @return TRUE if at least one cube is DEM
.merge_type_dem <- function(data1, data2) {
    any(inherits(data1, "dem_cube"), inherits(data2, "dem_cube"))
}
#' @title  Tell if both cubes are HLS
#' @name   .merge_type_hls
#' @description Tells if both cubes have the class hls_cube.
#' @author Rolf Simoes,     \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @param  data1  Data cube
#' @param  data2  Data cube
#' @return TRUE if both cubes are HLS
.merge_type_hls <- function(data1, data2) {
    all(inherits(data1, "hls_cube"), inherits(data2, "hls_cube"))
}
