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
test_that("Smoothing merges a tile in a worker while the next tile runs", {
    old_force_cpp <- Sys.getenv("SITS_SMOOTH_FORCE_CPP", unset = NA_character_)
    on.exit(
        if (is.na(old_force_cpp)) {
            Sys.unsetenv("SITS_SMOOTH_FORCE_CPP")
        } else {
            Sys.setenv(SITS_SMOOTH_FORCE_CPP = old_force_cpp)
        },
        add = TRUE
    )
    Sys.setenv(SITS_SMOOTH_FORCE_CPP = "TRUE")
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
    in_dir <- file.path(tempdir(), "smooth-two-tiles")
    unlink(in_dir, recursive = TRUE)
    dir.create(in_dir, recursive = TRUE)
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor(num_trees = 40))
    probs_map <- sits_classify(
        data = raster_cube, ml_model = rfor_model,
        output_dir = in_dir, progress = FALSE
    )
    # a second tile: the same probs file under another tile name
    probs_file <- .tile_path(probs_map)
    file.copy(probs_file, sub("_012010_", "_012011_", probs_file))
    probs_two <- sits_cube(
        source = "BDC", collection = "MOD13Q1-6.1", data_dir = in_dir,
        bands = "probs", labels = .cube_labels(probs_map), progress = FALSE
    )
    expect_equal(nrow(probs_two), 2L)
    run <- function(name, multicores) {
        out_dir <- file.path(tempdir(), name)
        unlink(out_dir, recursive = TRUE)
        dir.create(out_dir, recursive = TRUE)
        .debug(flag = TRUE, output_dir = out_dir)
        on.exit(.debug(flag = FALSE), add = TRUE)
        smooth <- sits_smooth(
            probs_two, output_dir = out_dir, multicores = multicores,
            memsize = 4, progress = FALSE
        )
        files <- list.files(
            file.path(out_dir, ".sits"), pattern = "[.]log$", full.names = TRUE
        )
        ev <- do.call(rbind, lapply(files, utils::read.csv, strip.white = TRUE))
        list(smooth = smooth, ev = ev)
    }
    two <- run("smooth-two-workers", 2)
    ev <- two[["ev"]]
    merges <- ev[ev[["event"]] == "start_tile_merge", ]
    expect_equal(nrow(merges), 2L)
    # the merge runs in a worker, not in this process
    expect_true(all(merges[["pid"]] != Sys.getpid()))
    # the blocks of the second tile start before the merge of the first
    blocks_start <- ev[ev[["event"]] == "start_tile_blocks", ]
    expect_equal(nrow(blocks_start), 2L)
    expect_lt(max(blocks_start[["timestamp"]]), min(merges[["timestamp"]]))
    # the same output as one core
    one <- run("smooth-one-core", 1)
    for (i in 1:2) {
        a <- terra::values(.raster_open_rast(.tile_path(one[["smooth"]][i, ])))
        b <- terra::values(.raster_open_rast(.tile_path(two[["smooth"]][i, ])))
        expect_equal(a, b)
    }
})
