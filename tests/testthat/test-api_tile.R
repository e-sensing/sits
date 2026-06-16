test_that(".tile_effective_size with no ROI returns full tile size", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    tile <- .tile(cube)

    full_size <- .tile_size(tile)
    eff_size  <- .tile_effective_size(tile, roi = NULL)

    expect_equal(eff_size[["ncols"]], full_size[["ncols"]])
    expect_equal(eff_size[["nrows"]], full_size[["nrows"]])
})

test_that(".tile_effective_size with ROI equal to tile bbox returns full tile size", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    tile <- .tile(cube)
    tile_bbox <- .tile_bbox(tile)
    # ROI exactly matches the tile bbox → effective size ≈ full tile size
    roi <- .bbox_as_sf(tile_bbox)

    full_size <- .tile_size(tile)
    eff_size  <- .tile_effective_size(tile, roi = roi)

    # floor() rounding may shave at most 1 pixel; allow a tolerance of 1
    expect_equal(eff_size[["ncols"]], full_size[["ncols"]])
    expect_equal(eff_size[["nrows"]], full_size[["nrows"]])
})

test_that(".tile_effective_size with half-tile ROI returns smaller dimensions", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    tile <- .tile(cube)
    tile_bbox <- .tile_bbox(tile)

    # ROI covering the bottom-left quadrant (half width, half height)
    half_bbox <- tile_bbox
    half_bbox[["xmax"]] <- (tile_bbox[["xmax"]] + tile_bbox[["xmin"]]) / 2
    half_bbox[["ymax"]] <- (tile_bbox[["ymax"]] + tile_bbox[["ymin"]]) / 2
    roi_half <- .bbox_as_sf(half_bbox)

    full_size <- .tile_size(tile)
    eff_size  <- .tile_effective_size(tile, roi = roi_half)

    # Effective dimensions should be strictly smaller than the full tile
    expect_lt(eff_size[["ncols"]], full_size[["ncols"]])
    expect_lt(eff_size[["nrows"]], full_size[["nrows"]])
})

test_that(".tile_effective_size with very small ROI returns small dimensions", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    tile <- .tile(cube)
    tile_bbox <- .tile_bbox(tile)
    full_size <- .tile_size(tile)

    # ROI covering only 10% of each dimension
    x_range <- tile_bbox[["xmax"]] - tile_bbox[["xmin"]]
    y_range <- tile_bbox[["ymax"]] - tile_bbox[["ymin"]]

    small_bbox <- tile_bbox
    small_bbox[["xmax"]] <- tile_bbox[["xmin"]] + x_range * 0.1
    small_bbox[["ymax"]] <- tile_bbox[["ymin"]] + y_range * 0.1
    roi_small <- .bbox_as_sf(small_bbox)

    eff_size <- .tile_effective_size(tile, roi = roi_small)

    # Effective dimensions should be substantially smaller than full tile
    expect_lt(eff_size[["ncols"]], full_size[["ncols"]] * 0.2)
    expect_lt(eff_size[["nrows"]], full_size[["nrows"]] * 0.2)
})

test_that(".tile_effective_size with ROI larger than tile is clamped to tile", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    tile <- .tile(cube)
    tile_bbox <- .tile_bbox(tile)
    full_size <- .tile_size(tile)

    # ROI that extends beyond every edge of the tile
    x_range <- tile_bbox[["xmax"]] - tile_bbox[["xmin"]]
    y_range <- tile_bbox[["ymax"]] - tile_bbox[["ymin"]]

    large_bbox <- tile_bbox
    large_bbox[["xmin"]] <- tile_bbox[["xmin"]] - x_range
    large_bbox[["xmax"]] <- tile_bbox[["xmax"]] + x_range
    large_bbox[["ymin"]] <- tile_bbox[["ymin"]] - y_range
    large_bbox[["ymax"]] <- tile_bbox[["ymax"]] + y_range
    roi_large <- .bbox_as_sf(large_bbox)

    eff_size <- .tile_effective_size(tile, roi = roi_large)

    # Effective size should not exceed the full tile (the intersection is
    # clamped to the tile bbox)
    expect_equal(eff_size[["ncols"]], full_size[["ncols"]])
    expect_equal(eff_size[["nrows"]], full_size[["nrows"]])
})

test_that(".tile_effective_size falls back to full size when ROI does not intersect", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    tile <- .tile(cube)
    tile_bbox <- .tile_bbox(tile)
    full_size <- .tile_size(tile)

    # ROI entirely outside the tile (shifted two widths to the right)
    x_range <- tile_bbox[["xmax"]] - tile_bbox[["xmin"]]
    outside_bbox <- tile_bbox
    outside_bbox[["xmin"]] <- tile_bbox[["xmax"]] + x_range
    outside_bbox[["xmax"]] <- tile_bbox[["xmax"]] + 2 * x_range
    roi_outside <- .bbox_as_sf(outside_bbox)

    eff_size <- .tile_effective_size(tile, roi = roi_outside)

    # Should fall back to full tile size
    expect_equal(eff_size[["ncols"]], full_size[["ncols"]])
    expect_equal(eff_size[["nrows"]], full_size[["nrows"]])
})

test_that("sits_classify uses ROI-aware block size (verbose check)", {
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor(num_trees = 20))

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )

    bbox <- .bbox(cube)
    # ROI covers only the bottom-left 20% of each dimension (~4% of area).
    # Keep it as a bbox list (with crs) so sits_classify receives the correct
    # projection instead of defaulting to EPSG:4326.
    x_range <- bbox[["xmax"]] - bbox[["xmin"]]
    y_range <- bbox[["ymax"]] - bbox[["ymin"]]
    small_roi <- bbox
    small_roi[["xmax"]] <- bbox[["xmin"]] + x_range * 0.20
    small_roi[["ymax"]] <- bbox[["ymin"]] + y_range * 0.20

    # Helper to extract "nrows x ncols" string from verbose messages
    parse_block_dims <- function(msgs) {
        line <- grep("using blocks of size", msgs, value = TRUE)
        if (length(line) == 0L) return(NULL)
        m <- regmatches(line, regexpr("[0-9]+ x [0-9]+", line))
        if (length(m) == 0L) return(NULL)
        parts <- as.integer(strsplit(m, " x ")[[1L]])
        list(nrows = parts[[1L]], ncols = parts[[2L]])
    }

    # Temporarily disable documentation mode so verbose messages are emitted
    doc_mode <- Sys.getenv("SITS_DOCUMENTATION_MODE")
    Sys.setenv("SITS_DOCUMENTATION_MODE" = "FALSE")
    on.exit(Sys.setenv("SITS_DOCUMENTATION_MODE" = doc_mode))

    out_full <- paste0(tempdir(), "/tile_eff_full")
    if (!dir.exists(out_full)) dir.create(out_full)
    msgs_full <- testthat::capture_messages({
        probs_full <- sits_classify(
            data = cube,
            ml_model = rfor_model,
            output_dir = out_full,
            memsize = 4,
            multicores = 1,
            verbose = TRUE,
            progress = FALSE,
            version = "tile_eff_full"
        )
    })

    out_roi <- paste0(tempdir(), "/tile_eff_roi")
    if (!dir.exists(out_roi)) dir.create(out_roi)
    msgs_roi <- testthat::capture_messages({
        probs_roi <- sits_classify(
            data = cube,
            ml_model = rfor_model,
            roi = small_roi,
            output_dir = out_roi,
            memsize = 4,
            multicores = 1,
            verbose = TRUE,
            progress = FALSE,
            version = "tile_eff_roi"
        )
    })

    dims_full <- parse_block_dims(msgs_full)
    dims_roi  <- parse_block_dims(msgs_roi)

    # Both should have produced a block-size message
    expect_false(is.null(dims_full))
    expect_false(is.null(dims_roi))

    # The ROI classification block should be no larger than the full-tile block
    # (with a small ROI and 4 GB memory it should be strictly smaller)
    full_area <- dims_full[["nrows"]] * dims_full[["ncols"]]
    roi_area  <- dims_roi[["nrows"]]  * dims_roi[["ncols"]]
    expect_lte(roi_area, full_area)

    # Output bbox of the ROI classification should be within the original bbox
    bbox_roi_probs <- sits_bbox(probs_roi)
    expect_gte(bbox_roi_probs[["xmin"]], bbox[["xmin"]] - 1e-5)
    expect_lte(bbox_roi_probs[["xmax"]], bbox[["xmax"]] + 1e-5)
    expect_gte(bbox_roi_probs[["ymin"]], bbox[["ymin"]] - 1e-5)
    expect_lte(bbox_roi_probs[["ymax"]], bbox[["ymax"]] + 1e-5)

    # Cleanup
    unlink(unlist(probs_full$file_info[[1L]]$path))
    unlink(unlist(probs_roi$file_info[[1L]]$path))
})
