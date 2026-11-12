test_that("Smoothing with exclusion mask", {
    # preparation - create cube
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    raster_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        tiles = "012010",
        bands = "NDVI",
        start_date = "2013-09-14",
        end_date = "2014-08-29",
        multicores = 2,
        progress = FALSE
    )
    # preparation - create directory to save NA
    data_dir <- paste0(tempdir(), "/smooth-exclusion-mask-na")
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
    # preparation - create exclusion mask
    exclusion_mask <- sf::st_as_sfc(
        x = sf::st_bbox(
            c(
                xmin = -6057482,
                ymin = -1290723,
                xmax = -6055209,
                ymax = -1288406
            ),
            crs = .cube_crs(raster_cube)
        )
    )
    # preparation - calculate centroid of the exclusion mask
    exclusion_mask_centroid <- sf::st_centroid(exclusion_mask)
    # preparation - create a random forest model
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor(num_trees = 40))
    # test classification with NA
    probs_map <- suppressWarnings(
        sits_classify(
            data = raster_cube,
            ml_model = rfor_model,
            output_dir = data_dir,
            progress = FALSE
        )
    )
    # smoth with exclusion mask
    smooth_map <- sits_smooth(
        cube = probs_map,
        exclusion_mask = exclusion_mask,
        output_dir = data_dir,
        multicores = 2
    )
    # testing original data (no na)
    probs_map_rst <- .raster_open_rast(probs_map[["file_info"]][[1]][["path"]])
    probs_map_value <- .raster_extract(
        probs_map_rst,
        .raster_open_vect(exclusion_mask_centroid)
    )
    expect_false(any(is.na(probs_map_value)))
    # testing smooth data (with na)
    smooth_map_rst <- .raster_open_rast(smooth_map[["file_info"]][[1]][["path"]])
    smooth_map_value <- .raster_extract(
        smooth_map_rst,
        .raster_open_vect(exclusion_mask_centroid)
    )
    expect_true(any(is.na(smooth_map_value)))
    # remove test files
    unlink(data_dir)
})
test_that("Smoothing logs the start and end of each step", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    raster_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        tiles = "012010",
        bands = "NDVI",
        start_date = "2013-09-14",
        end_date = "2014-08-29",
        progress = FALSE
    )
    out_dir <- file.path(tempdir(), "smooth-debug-log")
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor(num_trees = 40))
    probs_map <- sits_classify(
        data = raster_cube, ml_model = rfor_model,
        output_dir = out_dir, progress = FALSE
    )
    .debug(flag = TRUE, output_dir = out_dir)
    on.exit(.debug(flag = FALSE), add = TRUE)
    sits_smooth(
        cube = probs_map, output_dir = out_dir, multicores = 1,
        progress = FALSE
    )
    log_file <- .file_log_name(out_dir)
    events <- utils::read.csv(log_file, strip.white = TRUE)[["event"]]
    # the level is the prefix: one call holds its tiles, a tile its blocks
    steps <- c(
        "call", "tile", "tile_blocks", "tile_merge",
        "block", "block_data_read", "block_kernel", "block_write", "block_gc"
    )
    n <- purrr::map_int(steps, function(step) {
        n_start <- sum(events == paste0("start_", step))
        expect_equal(sum(events == paste0("end_", step)), n_start)
        n_start
    })
    names(n) <- steps
    expect_equal(n[["call"]], 1L)
    expect_equal(n[["tile"]], 1L)
    expect_equal(n[["tile_blocks"]], 1L)
    expect_equal(n[["tile_merge"]], 1L)
    expect_gt(n[["block"]], 0L)
    expect_equal(
        unname(n[c("block_data_read", "block_kernel", "block_write", "block_gc")]),
        rep(n[["block"]], 4L)
    )
    # with one core every event is in this process, in time order
    smooth_events <- events[grepl("^(start|end)_(call|tile|block)", events)]
    expect_equal(smooth_events[[1L]], "start_call")
    expect_equal(smooth_events[[length(smooth_events)]], "end_call")
    expect_lt(
        match("end_tile_blocks", events), match("start_tile_merge", events)
    )
    expect_lt(match("start_tile", events), match("start_tile_blocks", events))
    expect_lt(match("end_tile_merge", events), match("end_tile", events))
})
