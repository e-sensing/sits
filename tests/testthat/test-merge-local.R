# merge reads no pixel, so cubes are made by editing the fixture file_info
.test_merge_local_cube <- function() {
    sits_cube(
        source = "BDC", collection = "MOD13Q1-6.1",
        data_dir = system.file("extdata/raster/mod13q1", package = "sits"),
        progress = FALSE
    )
}

# applies `fn` to the file_info of the first record
.test_merge_local_edit_fi <- function(cube, fn) {
    cube[["file_info"]][[1L]] <- fn(cube[["file_info"]][[1L]])
    cube
}

# sets the band of every file of the first record
.test_merge_local_set_band <- function(cube, band) {
    .test_merge_local_edit_fi(cube, function(fi) {
        fi[["band"]] <- band
        fi
    })
}

# keeps the files of the timeline dates at positions `idx`
.test_merge_local_keep_dates <- function(cube, idx) {
    dates <- sits_timeline(cube)[idx]
    .test_merge_local_edit_fi(cube, function(fi) fi[fi[["date"]] %in% dates, ])
}

# moves every date of the first record by `days`
.test_merge_local_shift_dates <- function(cube, days) {
    .test_merge_local_edit_fi(cube, function(fi) {
        fi[["date"]] <- fi[["date"]] + days
        fi
    })
}

# a cube opened alone numbers its file ids from 1
.test_merge_local_reopen <- function(cube, idx) {
    .test_merge_local_edit_fi(
        .test_merge_local_keep_dates(cube, idx),
        function(fi) {
            fi[["fid"]] <- as.character(
                match(fi[["date"]], sort(unique(fi[["date"]])))
            )
            fi
        }
    )
}

# the cube plus a copy of it as tile 999999
.test_merge_local_two_tiles <- function(cube) {
    other <- cube
    other[["tile"]] <- "999999"
    both <- dplyr::bind_rows(cube, other)
    class(both) <- class(cube)
    both
}

# tile, band, date and file of every record; fid is left out
.test_merge_local_content <- function(cube) {
    rows <- dplyr::bind_rows(
        purrr::map2(cube[["tile"]], cube[["file_info"]], function(tile, fi) {
            tibble::tibble(
                tile = tile, band = fi[["band"]],
                date = fi[["date"]], path = fi[["path"]]
            )
        })
    )
    rows[order(rows[["tile"]], rows[["date"]], rows[["band"]],
               rows[["path"]]), ]
}

# TRUE if, in every record, each fid maps to one date and the reverse
.test_merge_local_one_fid_per_date <- function(cube) {
    all(purrr::map_lgl(cube[["file_info"]], function(fi) {
        pairs <- unique(fi[c("fid", "date")])
        !anyDuplicated(pairs[["fid"]]) && !anyDuplicated(pairs[["date"]])
    }))
}

test_that("Merge - same band", {
    cube <- .test_merge_local_cube()
    full <- .test_merge_local_content(cube)
    expect_equal(.test_merge_local_content(sits_merge(cube, cube)), full)
    pairs <- list(
        disjoint = list(.test_merge_local_reopen(cube, 1:6),
            .test_merge_local_reopen(cube, 7:12)),
        overlapping = list(.test_merge_local_reopen(cube, 1:8),
            .test_merge_local_reopen(cube, 5:12))
    )
    purrr::walk(pairs, function(p) {
        merged <- sits_merge(p[[1L]], p[[2L]])
        expect_equal(.test_merge_local_content(merged), full)
        expect_true(.test_merge_local_one_fid_per_date(merged))
        expect_equal(.test_merge_local_content(sits_merge(p[[2L]], p[[1L]])),
            full)
        # merging a part again adds nothing
        expect_equal(.test_merge_local_content(sits_merge(merged, p[[2L]])),
            full)
    })
    part_a <- .test_merge_local_reopen(cube, 1:4)
    part_b <- .test_merge_local_reopen(cube, 5:8)
    part_c <- .test_merge_local_reopen(cube, 9:12)
    expect_equal(
        .test_merge_local_content(sits_merge(sits_merge(part_a, part_b),
            part_c)),
            full
    )
    expect_equal(
        .test_merge_local_content(
            sits_merge(part_a, sits_merge(part_b, part_c))
        ),
        full
    )
})

test_that("Merge - local cubes with different dates", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    files <- list.files(data_dir, pattern = "NDVI", full.names = TRUE)
    dir_a <- file.path(tempdir(), "merge_dates_a")
    dir_b <- file.path(tempdir(), "merge_dates_b")
    out_dir <- file.path(tempdir(), "merge_dates_out")
    purrr::walk(c(dir_a, dir_b, out_dir), dir.create, showWarnings = FALSE)
    file.copy(files[1:2], dir_a)
    file.copy(files[3:4], dir_b)
    .test_merge_local_open_dir <- function(dir) {
        sits_cube(
            source = "BDC", collection = "MOD13Q1-6.1",
            data_dir = dir, progress = FALSE
        )
    }
    cube <- sits_merge(.test_merge_local_open_dir(dir_a),
        .test_merge_local_open_dir(dir_b))
    fi <- cube[["file_info"]][[1L]]
    # each local cube numbers its file ids from 1
    expect_equal(length(unique(fi[["fid"]])), length(unique(fi[["date"]])))
    sits_apply(
        cube, NDVI2 = NDVI * 1, output_dir = out_dir, progress = FALSE
    )
    expect_length(list.files(out_dir, pattern = "NDVI2.*tif$"), 4L)
    sits_texture(
        cube, TEX = glcm_contrast(NDVI), window_size = 3L,
        output_dir = out_dir, progress = FALSE
    )
    expect_length(list.files(out_dir, pattern = "TEX.*tif$"), 4L)
    unlink(c(dir_a, dir_b, out_dir), recursive = TRUE)
})

test_that("Merge - other band", {
    cube <- .test_merge_local_cube()
    dates <- sits_timeline(cube)
    evi <- .test_merge_local_set_band(cube, "EVI")
    ndwi <- .test_merge_local_set_band(cube, "NDWI")
    # same dates
    merged <- sits_merge(cube, evi)
    expect_equal(sits_bands(merged), c("EVI", "NDVI"))
    expect_equal(sits_timeline(merged), dates)
    expect_true(.test_merge_local_one_fid_per_date(merged))
    expect_equal(.test_merge_local_content(sits_merge(evi, cube)),
        .test_merge_local_content(merged))
    # bands already present add nothing, in both orders
    expect_equal(.test_merge_local_content(sits_merge(merged, evi)),
        .test_merge_local_content(merged))
    expect_equal(.test_merge_local_content(sits_merge(evi, merged)),
        .test_merge_local_content(merged))
    # one common band and one new band
    three <- sits_merge(merged, sits_merge(evi, ndwi))
    expect_equal(sits_bands(three), c("EVI", "NDVI", "NDWI"))
    expect_equal(nrow(three[["file_info"]][[1L]]), 36L)
    # some common dates: the result keeps only those
    odd <- c(1L, 3L, 5L, 7L)
    evi_odd <- .test_merge_local_keep_dates(evi, odd)
    expect_equal(sits_timeline(sits_merge(cube, evi_odd)), dates[odd])
    expect_equal(sits_timeline(sits_merge(evi_odd, cube)), dates[odd])
    # interleaved dates: each pair takes the earlier date
    purrr::walk(c(1L, -10L), function(days) {
        evi_shift <- .test_merge_local_shift_dates(evi, days)
        merged <- sits_merge(cube, evi_shift)
        expect_equal(sits_timeline(merged), pmin(dates, dates + days))
        expect_equal(nrow(merged[["file_info"]][[1L]]), 24L)
        expect_true(.test_merge_local_one_fid_per_date(merged))
        expect_equal(
            .test_merge_local_content(sits_merge(evi_shift, cube)),
                .test_merge_local_content(merged)
        )
    })
    # three cubes within one period give one cube in any order
    evi_1 <- .test_merge_local_shift_dates(evi, 1L)
    ndwi_3 <- .test_merge_local_shift_dates(ndwi, 3L)
    first <- .test_merge_local_content(sits_merge(sits_merge(cube, evi_1),
        ndwi_3))
    expect_equal(.test_merge_local_content(sits_merge(cube, sits_merge(evi_1,
        ndwi_3))), first)
    expect_equal(.test_merge_local_content(sits_merge(sits_merge(cube, ndwi_3),
        evi_1)), first)
})

test_that("Merge - errors", {
    cube <- .test_merge_local_cube()
    evi <- .test_merge_local_set_band(cube, "EVI")
    .test_merge_local_both_orders <- function(cube_a, cube_b, key) {
        msg <- .conf("messages", key)
        expect_error(sits_merge(cube_a, cube_b), msg, fixed = TRUE)
        expect_error(sits_merge(cube_b, cube_a), msg, fixed = TRUE)
    }
    # a missing date leaves two dates of one cube between two of the other
    .test_merge_local_both_orders(
        cube,
        .test_merge_local_keep_dates(
            .test_merge_local_shift_dates(evi, 1L), -6L
        ),
        ".merge_regular_interleaved"
    )
    # dates 5 days apart do not pair with dates a month apart
    dense <- .test_merge_local_edit_fi(evi, function(fi) {
        fi[["date"]] <- fi[["date"]][[1L]] +
            seq(1L, by = 5L, length.out = nrow(fi))
        fi
    })
    .test_merge_local_both_orders(cube, dense, ".merge_regular_interleaved")
    # same band and date with another file
    other_file <- .test_merge_local_edit_fi(cube, function(fi) {
        fi[["path"]][[3L]] <- file.path(tempdir(), "other.tif")
        fi
    })
    .test_merge_local_both_orders(cube, other_file, ".merge_regular_conflict")
    .test_merge_local_both_orders(
        cube, sits_merge(other_file, .test_merge_local_set_band(cube, "NDWI")),
        ".merge_regular_conflict"
    )
    # the same files with other file ids are the same cube
    other_fid <- .test_merge_local_edit_fi(cube, function(fi) {
        fi[["fid"]] <- paste0(fi[["fid"]], "_other")
        fi
    })
    expect_equal(.test_merge_local_content(sits_merge(cube, other_fid)),
        .test_merge_local_content(cube))
    # a common band without a common date
    .test_merge_local_both_orders(
        sits_merge(cube, evi), .test_merge_local_shift_dates(evi, 1L),
        ".merge_common_band_dates"
    )
    # no common tile
    other_tile <- cube
    other_tile[["tile"]] <- "999999"
    .test_merge_local_both_orders(cube, other_tile, ".merge_tiles")
    # one band without one date makes the cube irregular
    date_3 <- sits_timeline(cube)[[3L]]
    irregular <- .test_merge_local_edit_fi(sits_merge(cube, evi), function(fi) {
        fi[!(fi[["band"]] == "EVI" & fi[["date"]] == date_3), ]
    })
    expect_false(.cube_is_regular(irregular))
    .test_merge_local_both_orders(
        irregular, .test_merge_local_set_band(cube, "NDWI"), ".merge_type"
    )
    # raw cubes of two sensors have no common tile; regularizing comes first
    irregular[["tile"]] <- "999999"
    .test_merge_local_both_orders(
        irregular, .test_merge_local_set_band(cube, "NDWI"), ".merge_type"
    )
})

test_that("Merge - DEM, HLS and tiles", {
    cube <- .test_merge_local_cube()
    # a tile in one cube only is left out with a warning
    msg <- .conf("messages", ".merge_tiles_dropped")
    expect_warning(
        merged <- sits_merge(.test_merge_local_two_tiles(cube),
            .test_merge_local_set_band(cube, "EVI")),
        msg, fixed = TRUE
    )
    expect_equal(merged[["tile"]], cube[["tile"]])
    # DEM: one date copied to every date of the other cube
    dem <- .test_merge_local_set_band(.test_merge_local_keep_dates(cube, 1L),
        "ELEVATION")
    class(dem) <- c("dem_cube", class(dem))
    merged <- sits_merge(cube, dem)
    fi <- merged[["file_info"]][[1L]]
    expect_equal(sits_bands(merged), c("ELEVATION", "NDVI"))
    expect_equal(sum(fi[["band"]] == "ELEVATION"), 12L)
    expect_true(.test_merge_local_one_fid_per_date(merged))
    expect_equal(.test_merge_local_content(sits_merge(dem, cube)),
        .test_merge_local_content(merged))
    expect_equal(.test_merge_local_content(sits_merge(merged, dem)),
        .test_merge_local_content(merged))
    expect_equal(.test_merge_local_content(sits_merge(dem, dem)),
        .test_merge_local_content(dem))
    # another DEM file
    dem_other <- .test_merge_local_edit_fi(dem, function(fi) {
        fi[["path"]] <- file.path(tempdir(), "dem.tif")
        fi
    })
    msg <- .conf("messages", ".merge_dem_dem")
    expect_error(sits_merge(dem, dem_other), msg, fixed = TRUE)
    expect_error(sits_merge(dem_other, dem), msg, fixed = TRUE)
    msg <- .conf("messages", ".merge_regular_conflict")
    expect_error(sits_merge(merged, dem_other), msg, fixed = TRUE)
    expect_error(sits_merge(dem_other, merged), msg, fixed = TRUE)
    # STAC can split one acquisition in two items: same band and date, two
    # files; the cube is irregular, as one band lacks a date
    date_3 <- sits_timeline(cube)[[3L]]
    pieces <- .test_merge_local_edit_fi(
        sits_merge(cube, .test_merge_local_set_band(cube, "EVI")),
        function(fi) {
            fi <- fi[!(fi[["band"]] == "EVI" & fi[["date"]] == date_3), ]
            piece <- fi[1L, ]
            piece[["fid"]] <- "piece_2"
            piece[["path"]] <- paste0(piece[["path"]], ".piece_2")
            dplyr::bind_rows(fi, piece)
        }
    )
    expect_false(.cube_is_regular(pieces))
    # only HLS cubes merge while irregular; the others are regularized first
    msg <- .conf("messages", ".merge_type")
    expect_error(sits_merge(pieces, dem), msg, fixed = TRUE)
    expect_error(sits_merge(dem, pieces), msg, fixed = TRUE)
    # HLS: two sensors on one date keep both files
    l30 <- cube
    l30[["collection"]] <- "HLSL30"
    class(l30) <- c("hls_cube", class(l30))
    s30 <- .test_merge_local_edit_fi(cube, function(fi) {
        fi[["path"]] <- paste0(fi[["path"]], ".s30")
        fi
    })
    s30[["collection"]] <- "HLSS30"
    class(s30) <- c("hls_cube", class(s30))
    merged <- sits_merge(l30, s30)
    expect_equal(merged[["collection"]], "HLSS30")
    expect_equal(nrow(merged[["file_info"]][[1L]]), 24L)
    expect_equal(.test_merge_local_content(sits_merge(s30, l30)),
        .test_merge_local_content(merged))
    expect_equal(.test_merge_local_content(sits_merge(merged, s30)),
        .test_merge_local_content(merged))
    expect_warning(
        merged <- sits_merge(.test_merge_local_two_tiles(l30), s30),
        .conf("messages", ".merge_tiles_dropped"), fixed = TRUE
    )
    expect_equal(nrow(merged), 1L)
})
