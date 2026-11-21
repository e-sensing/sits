test_that("Classify with random forest - single core and multicore", {
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor(num_trees = 40))

    expect_type(rfor_model, "closure")
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    class_ndvi <- sits_classify(
        data = point_ndvi,
        ml_model = rfor_model,
        progress = FALSE
    )

    expect_true(nrow(class_ndvi$predicted[[1]]) == 17)
    expect_true(all(class_ndvi$predicted[[1]]$class %in%
        sits_labels(samples_modis_ndvi)))
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    class_ndvi <- sits_classify(
        data = point_ndvi,
        ml_model = rfor_model,
        multicores = 2,
        progress = FALSE
    )

    expect_true(nrow(class_ndvi$predicted[[1]]) == 17)
    expect_true(all(class_ndvi$predicted[[1]]$class %in%
        sits_labels(samples_modis_ndvi)))
})

test_that("Classification updates workers for internally parallel models", {
    cpu_model <- structure(function() NULL, class = "svm_model")
    xgb_model <- structure(function() NULL, class = "xgb_model")
    torch_model <- structure(
        function() NULL,
        class = c("mlp_model", "torch_model")
    )
    update_workers <- function(model, gpu) {
        testthat::local_mocked_bindings(
            .torch_gpu_classification = function() gpu
        )
        .ml_update_multicores(model, multicores = 4L)
    }

    expect_equal(update_workers(cpu_model, gpu = FALSE), 4L)
    expect_equal(update_workers(cpu_model, gpu = TRUE), 4L)
    expect_equal(update_workers(xgb_model, gpu = FALSE), 1L)
    expect_equal(update_workers(xgb_model, gpu = TRUE), 1L)
    expect_equal(update_workers(torch_model, gpu = FALSE), 4L)
    expect_equal(update_workers(torch_model, gpu = TRUE), 1L)
})

test_that("Vector classification protects XGBoost workers", {
    skip_if_not_installed("snic")

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    output_dir <- tempfile("classify-vector-xgb-")
    dir.create(output_dir)
    on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
    segments <- sits_segment(
        cube = cube,
        seg_fn = sits_snic(spacing = 50L),
        output_dir = output_dir,
        memsize = 24L,
        multicores = 1L,
        progress = FALSE
    )
    xgb_model <- sits_train(
        samples_modis_ndvi,
        sits_xgboost(nrounds = 10L, verbose = FALSE)
    )
    classification_workers <- NULL
    probs <- testthat::with_mocked_bindings(
        sits_classify(
            data = segments,
            ml_model = xgb_model,
            output_dir = output_dir,
            memsize = 24L,
            multicores = 6L,
            version = "xgb",
            progress = FALSE
        ),
        .parallel_start = function(workers, ...) {
            classification_workers <<- workers
            FALSE
        }
    )

    expect_equal(classification_workers, 1L)
    expect_s3_class(probs, "probs_vector_cube")
})

test_that("Classify a set of time series with svm and impute methods", {
    # train model
    svm_model <- sits_train(cerrado_2classes, sits_svm())
    # add NA
    classification_data <- sits_apply(cerrado_2classes,
                                      NDVI = ifelse(NDVI > 0.7, NA, NDVI))
    # define input methods
    input_fncs <- list(
        impute_linear(),
        impute_mean(),
        impute_median(),
        impute_mean_window(weighting = "simple"),
        impute_mean_window(weighting = "simple", k = 3),
        impute_mean_window(weighting = "linear"),
        impute_mean_window(weighting = "linear", k = 5),
        impute_mean_window(weighting = "exponential"),
        impute_mean_window(weighting = "exponential", k = 7),
        impute_mean_window(weighting = "exponential", k = 30)
    )
    # test classification with impute methods
    results <- purrr::map_lgl(seq(input_fncs), function(idx) {
        # get impute function
        impute_fnc <- input_fncs[[idx]]
        # classify
        class1 <- sits_classify(classification_data,
                                ml_model = svm_model,
                                multicores = 2,
                                progress = FALSE)
        # test values
        class1$predicted[[1]]$class %in% sits_labels(cerrado_2classes)
    })
    # check if all results are true
    expect_true(all(results))
})

test_that("Classify error bands 1", {
    model <- sits_train(samples_modis_ndvi, sits_svm())
    point <- sits_select(point_mt_6bands, bands = "EVI")

    expect_error(
        sits_classify(
            data = point,
            ml_model = model
        )
    )
})

test_that("Classify with NA values (using multiple impute methods)", {
    # load cube
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
    data_dir <- paste0(tempdir(), "/na-cube")
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
    # preparation - insert NA in cube
    raster_cube <- sits_apply(
        data = raster_cube,
        NDVI_NA = ifelse(NDVI > 0.5, NA, NDVI),
        output_dir = data_dir
    )
    raster_cube <- sits_select(raster_cube, bands = "NDVI_NA")
    .fi(raster_cube) <- .fi(raster_cube) |>
        dplyr::mutate(band = "NDVI")
    # preparation - create a random forest model
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor(num_trees = 40))
    # define input methods
    input_fncs <- list(
        impute_linear(),
        impute_mean(),
        impute_median(),
        impute_mean_window(weighting = "simple"),
        impute_mean_window(weighting = "simple", k = 3),
        impute_mean_window(weighting = "linear"),
        impute_mean_window(weighting = "linear", k = 5),
        impute_mean_window(weighting = "exponential"),
        impute_mean_window(weighting = "exponential", k = 7),
        impute_mean_window(weighting = "exponential", k = 30)
    )
    # test impute methods
    results <- purrr::map_lgl(seq(input_fncs), function(idx) {
        # get impute function
        impute_fnc <- input_fncs[[idx]]
        # test classification with NA
        class_map <- sits_classify(
            data       = raster_cube,
            ml_model   = rfor_model,
            output_dir = data_dir,
            progress   = FALSE,
            impute_fn  = impute_fnc
        )
        # get map file
        class_map_file <- class_map[["file_info"]][[1]][["path"]]
        # get file raster
        class_map_rst <- .raster_open_rast(class_map_file)
        # test
        result <- anyNA(class_map_rst[])
        # delete file
        unlink(class_map_file)
        # return
        result
    })
    # check if all results are true
    expect_true(all(results))
    # remove test files
    unlink(data_dir)
})

test_that("Classify with exclusion mask", {
    # load cube
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
    data_dir <- paste0(tempdir(), "/exclusion-mask-na")
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
    # preparation - create exclusion mask
    exclusion_mask <- sf::st_as_sfc(
        x = sf::st_bbox(
            c(
                xmin = -55.63478,
                ymin = -11.63328,
                xmax = -55.54080,
                ymax = -11.56978
            ),
            crs = "EPSG:4326"
        )
    )
    # transform object to cube crs
    exclusion_mask <- sf::st_transform(exclusion_mask, .cube_crs(raster_cube))
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
            exclusion_mask = exclusion_mask,
            progress = FALSE
        )
    )
    # testing original data
    probs_map_rst <- .raster_open_rast(probs_map[["file_info"]][[1]][["path"]])
    expect_true(anyNA(probs_map_rst[]))
    # extract values
    probs_map_value <- .raster_extract(
        probs_map_rst,
        .raster_open_vect(exclusion_mask_centroid)
    )

    expect_true(any(is.na(probs_map_value)))
    # remove test files
    unlink(data_dir)
})

# Force the CPU or auto-detected orchestration branch of sits_classify()
# through the public API by toggling SITS_FORCE_CPU (read by
# .torch_gpu_classification()), restoring the variable afterwards. Both
# branches run on CPU tensors when no CUDA/MPS device is present, so this
# exercises both code paths on any machine.
classify_forcing <- function(force, cube, ml_model, output_dir) {
    old <- Sys.getenv("SITS_FORCE_CPU", unset = NA)
    Sys.setenv(SITS_FORCE_CPU = force)
    on.exit(
        if (is.na(old)) {
            Sys.unsetenv("SITS_FORCE_CPU")
        } else {
            Sys.setenv(SITS_FORCE_CPU = old)
        }
    )
    sits_classify(
        data = cube, ml_model = ml_model, memsize = 4L, multicores = 2L,
        output_dir = output_dir, progress = FALSE
    )
}

test_that("Classify a torch model returns the same probs across CPU/GPU", {
    skip_on_cran()
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")

    set.seed(2971)
    torch::torch_manual_seed(2971)
    # cheap torch model; the CPU/GPU branch selection is independent of it
    ml_model <- .try(
        sits_train(
            samples_modis_ndvi,
            sits_mlp(
                layers = c(64L, 64L),
                dropout_rates = c(0.2, 0.3),
                epochs = 2L,
                batch_size = 64L,
                verbose = FALSE
            )
        ),
        .default = NULL
    )
    skip_if(is.null(ml_model), "torch training failed")

    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = system.file("extdata/raster/mod13q1", package = "sits"),
        progress = FALSE
    )

    cpu_dir <- file.path(tempdir(), "classify_torch_cpu")
    gpu_dir <- file.path(tempdir(), "classify_torch_gpu")
    unlink(c(cpu_dir, gpu_dir), recursive = TRUE)
    dir.create(cpu_dir)
    dir.create(gpu_dir)
    on.exit(unlink(c(cpu_dir, gpu_dir), recursive = TRUE), add = TRUE)

    # Drive both pipelines through the public API by forcing each branch
    probs_cpu <- classify_forcing("YES", cube, ml_model, cpu_dir)
    probs_gpu <- classify_forcing("NO", cube, ml_model, gpu_dir)

    # Both branches yield the same probs_cube structure and labels
    expect_s3_class(probs_cpu, "probs_cube")
    expect_s3_class(probs_gpu, "probs_cube")
    expect_equal(nrow(probs_cpu), nrow(probs_gpu))
    expect_equal(.cube_labels(probs_cpu), .cube_labels(probs_gpu))

    # Both branches run on CPU tensors here, so probabilities must agree.
    # The CPU and GPU orchestrations partition the data differently, which
    # can shift the integer-scaled probabilities by at most one unit.
    probs_cpu_rst <- .raster_open_rast(.fi(probs_cpu)[["path"]])
    probs_gpu_rst <- .raster_open_rast(.fi(probs_gpu)[["path"]])
    expect_equal(dim(probs_cpu_rst[]), dim(probs_gpu_rst[]))
    expect_lte(max(abs(probs_cpu_rst[] - probs_gpu_rst[])), 1L)
})
test_that("Classify with another timeline - cube refused", {
    cube <- sits_cube(
        source = "BDC", collection = "MOD13Q1-6.1",
        data_dir = system.file("extdata/raster/mod13q1", package = "sits"),
        progress = FALSE
    )
    # the model is trained on 10 of the 12 dates of the cube
    dates <- sits_timeline(samples_modis_ndvi)[1L:10L]
    samples <- sits_select(samples_modis_ndvi, dates = dates)
    model <- sits_train(samples, sits_rfor(num_trees = 20L))
    expect_error(
        sits_classify(cube, model, output_dir = tempdir(), progress = FALSE),
        "tile timeline does not match samples timeline", fixed = TRUE
    )
})

test_that("Classify two tiles - merge in a worker while the next tile runs", {
    # workers log only from a cluster that sits_classify() starts
    suppressMessages(sits_parallel(workers = 0L))
    root <- file.path(tempdir(), "classify-queue")
    unlink(root, recursive = TRUE)
    dir.create(root, recursive = TRUE)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    cube <- gpu_tiles_cube(root)
    set.seed(42L)
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor(num_trees = 40L))
    roi <- sf::st_as_sfc(sf::st_bbox(c(
        xmin = cube[["xmin"]][[1L]] + 40L * .tile_xres(cube[1L, ]),
        xmax = cube[["xmax"]][[2L]] - 40L * .tile_xres(cube[1L, ]),
        ymin = cube[["ymin"]][[1L]] + 30L * .tile_yres(cube[1L, ]),
        ymax = cube[["ymax"]][[1L]] - 30L * .tile_yres(cube[1L, ])
    ), crs = sf::st_crs(cube[["crs"]][[1L]])))
    run <- function(name, multicores, roi = NULL) {
        out_dir <- file.path(root, name)
        dir.create(out_dir, showWarnings = FALSE)
        on.exit(.debug(flag = FALSE), add = TRUE)
        probs <- suppressMessages(sits_classify(
            cube, rfor_model, roi = roi,
            block_size = c(nrows = 64L, ncols = 64L),
            multicores = multicores, memsize = 4L, output_dir = out_dir,
            verbose = TRUE, progress = FALSE
        ))
        files <- list.files(
            file.path(out_dir, ".sits"), pattern = "[.]log$", full.names = TRUE
        )
        ev <- do.call(rbind, lapply(files, utils::read.csv, strip.white = TRUE))
        list(probs = probs, ev = ev)
    }
    values_of <- function(probs, i) {
        terra::values(.raster_open_rast(.tile_path(probs[i, ])))
    }
    # the file names of a tile hold its name between underscores
    times <- function(ev, event, tile) {
        hit <- ev[["event"]] == event &
            grepl(paste0("_", tile, "_"), ev[["value"]], fixed = TRUE)
        ev[["timestamp"]][hit]
    }
    # blocks of 64 x 64 per tile, counted from the input files
    n_expected <- purrr::map_int(1:2, function(i) {
        r <- terra::rast(.tile_path(cube[i, ]))
        as.integer(ceiling(nrow(r) / 64L) * ceiling(ncol(r) / 64L))
    })
    two <- run("two", 2L)
    ev <- two[["ev"]]
    merges <- ev[ev[["event"]] == "start_tile_merge", ]
    expect_equal(nrow(merges), 2L)
    # the merge runs in a worker, not in this process
    expect_true(all(merges[["pid"]] != Sys.getpid()))
    # the blocks of B start before the first merge, and some before the
    # merge of A ends: a worker merges A while others classify B
    expect_lt(
        max(times(ev, "start_tile_blocks", "B")),
        min(merges[["timestamp"]])
    )
    expect_true(any(
        times(ev, "start_block", "B") < times(ev, "end_tile_merge", "A")
    ))
    # the events of each tile come in order, once each
    steps <- c("start_tile", "start_tile_blocks", "end_tile_blocks",
               "start_tile_merge", "end_tile_merge", "end_tile")
    for (tile in c("A", "B")) {
        at <- purrr::map(steps, function(step) times(ev, step, tile))
        expect_equal(lengths(at), rep(1L, length(steps)))
        expect_false(is.unsorted(unlist(at)))
        # the merge starts only after the last block of its tile ends
        expect_lt(
            max(times(ev, "end_block", tile)),
            times(ev, "start_tile_merge", tile)
        )
    }
    # each block of each tile logs its start and its end
    for (i in 1:2) {
        tile <- c("A", "B")[[i]]
        expect_length(times(ev, "start_block", tile), n_expected[[i]])
        expect_length(times(ev, "end_block", tile), n_expected[[i]])
    }
    # the same probs as one core, tile by tile
    one <- run("one", 1L)
    expect_equal(two[["probs"]][["tile"]], c("A", "B"))
    for (i in 1:2) {
        expect_equal(values_of(two[["probs"]], i), values_of(one[["probs"]], i))
    }
    # a second call resumes the done tiles: no block runs again
    again <- run("two", 2L)
    for (tile in c("A", "B")) {
        expect_equal(
            length(times(again[["ev"]], "start_block", tile)),
            length(times(ev, "start_block", tile))
        )
        # the second call logged: each tile started twice
        expect_length(times(again[["ev"]], "start_tile", tile), 2L)
    }
    # a cube with A done and B not: only the blocks of B run again
    b_values <- values_of(two[["probs"]], 2L)
    unlink(.tile_path(two[["probs"]][2L, ]))
    partial <- run("two", 2L)
    expect_equal(
        length(times(partial[["ev"]], "start_block", "A")),
        n_expected[[1L]]
    )
    expect_equal(
        length(times(partial[["ev"]], "start_block", "B")),
        2L * n_expected[[2L]]
    )
    expect_equal(values_of(partial[["probs"]], 2L), b_values)
    # an ROI over both tiles: cropped tiles, the same as one core
    roi_two <- run("roi-two", 2L, roi = roi)
    roi_one <- run("roi-one", 1L, roi = roi)
    # after the crop no block or merged file is left in .sits
    expect_length(list.files(
        file.path(root, "roi-two", ".sits"), pattern = "[.]tif$"
    ), 0L)
    # blocks of 64 x 64 that touch the ROI, counted from the bboxes; a
    # worker that cropped and lost its debug flag would log fewer blocks.
    # It cannot see the loss if that worker runs no block after its crop
    roi_box <- sf::st_bbox(roi)
    n_touch <- function(lo, hi, origin, res) {
        first <- floor((lo - origin) / (64L * res))
        last <- ceiling((hi - origin) / (64L * res)) - 1L
        last - first + 1L
    }
    for (i in 1:2) {
        xres <- .tile_xres(cube[i, ])
        yres <- .tile_yres(cube[i, ])
        cols <- n_touch(
            max(roi_box[["xmin"]], cube[["xmin"]][[i]]),
            min(roi_box[["xmax"]], cube[["xmax"]][[i]]),
            cube[["xmin"]][[i]], xres
        )
        # rows count down from the top of the tile
        rows <- n_touch(
            cube[["ymax"]][[i]] - roi_box[["ymax"]],
            cube[["ymax"]][[i]] - roi_box[["ymin"]],
            0, yres
        )
        expect_length(
            times(roi_two[["ev"]], "start_block", c("A", "B")[[i]]),
            as.integer(cols * rows)
        )
    }
    for (i in 1:2) {
        # the ROI crops: the tile is smaller than the whole tile
        whole <- sits_bbox(two[["probs"]][i, ])
        cropped <- sits_bbox(roi_two[["probs"]][i, ])
        expect_lt(
            cropped[["ymax"]] - cropped[["ymin"]],
            whole[["ymax"]] - whole[["ymin"]]
        )
        expect_equal(
            sits_bbox(roi_two[["probs"]][i, ]),
            sits_bbox(roi_one[["probs"]][i, ])
        )
        expect_equal(
            values_of(roi_two[["probs"]], i), values_of(roi_one[["probs"]], i)
        )
    }
})

test_that("Classify with exclusion mask - tiles in two CRSs", {
    suppressMessages(sits_parallel(workers = 0L))
    root <- file.path(tempdir(), "classify-queue-crs")
    unlink(root, recursive = TRUE)
    dir.create(root, recursive = TRUE)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    cube <- gpu_tiles_cube_two_crs(root)
    set.seed(42L)
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor(num_trees = 40L))
    # a strip in the CRS of tile A across the border with tile B
    xres <- .tile_xres(cube[1L, ])
    mask <- sf::st_sf(geometry = sf::st_as_sfc(sf::st_bbox(c(
        xmin = cube[["xmax"]][[1L]] - 30L * xres,
        xmax = cube[["xmax"]][[1L]] + 30L * xres,
        ymin = cube[["ymin"]][[1L]] + 20L * xres,
        ymax = cube[["ymax"]][[1L]] - 20L * xres
    ), crs = sf::st_crs(cube[["crs"]][[1L]]))))
    classify <- function(data, name) {
        out_dir <- file.path(root, name)
        dir.create(out_dir)
        sits_classify(
            data, rfor_model, exclusion_mask = mask,
            block_size = c(nrows = 64L, ncols = 64L), multicores = 2L,
            memsize = 4L, output_dir = out_dir, progress = FALSE
        )
    }
    probs <- classify(cube, "both")
    expect_equal(probs[["tile"]], c("A", "B"))
    # inside the strip every label is NA in both tiles; outside it is not
    y <- (cube[["ymin"]][[1L]] + cube[["ymax"]][[1L]]) / 2
    inside <- sf::st_sfc(
        sf::st_point(c(cube[["xmax"]][[1L]] - 10L * xres, y)),
        sf::st_point(c(cube[["xmax"]][[1L]] + 10L * xres, y)),
        crs = sf::st_crs(cube[["crs"]][[1L]])
    )
    outside <- sf::st_sfc(
        sf::st_point(c(cube[["xmin"]][[1L]] + 10L * xres, y)),
        sf::st_point(c(cube[["xmax"]][[1L]] + 60L * xres, y)),
        crs = sf::st_crs(cube[["crs"]][[1L]])
    )
    value_at <- function(i, points) {
        r <- terra::rast(.tile_path(probs[i, ]))
        xy <- sf::st_coordinates(sf::st_transform(points, terra::crs(r)))
        as.matrix(terra::extract(r, xy))
    }
    expect_true(all(is.na(value_at(1L, inside[1L]))))
    expect_true(all(is.na(value_at(2L, inside[2L]))))
    expect_false(anyNA(value_at(1L, outside[1L])))
    expect_false(anyNA(value_at(2L, outside[2L])))
    # each tile alone is the reference: its jobs are not stacked with others
    for (i in 1:2) {
        alone <- classify(cube[i, ], paste0("alone_", i))
        expect_equal(
            .raster_get_values(.raster_open_rast(.tile_path(probs[i, ]))),
            .raster_get_values(.raster_open_rast(.tile_path(alone)))
        )
    }
})

test_that("Classify - worker dies in a merge, merge resent", {
    skip_on_cran()
    skip_on_os("windows")
    suppressMessages(sits_parallel(workers = 0L))
    root <- file.path(tempdir(), "classify-merge-death")
    unlink(root, recursive = TRUE)
    dir.create(root, recursive = TRUE)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    cube <- gpu_tiles_cube(root)
    set.seed(42L)
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor(num_trees = 40L))
    classify <- function(name, multicores) {
        out_dir <- file.path(root, name)
        dir.create(out_dir)
        sits_classify(
            cube, rfor_model, block_size = c(nrows = 64L, ncols = 64L),
            multicores = multicores, memsize = 4L, output_dir = out_dir,
            progress = FALSE
        )
    }
    reference <- classify("sequential", 1L)
    suppressMessages(sits_parallel(workers = 2L))
    on.exit(suppressMessages(sits_parallel(workers = 0L)), add = TRUE)
    # the first merge writes its file, then kills its worker; a new worker
    # has no trace, so the resent merge runs as usual
    flag <- file.path(root, "killed")
    kill_after_merge <- function(flag) {
        trace(
            ".raster_merge_blocks",
            exit = bquote(if (!file.exists(.(flag))) {
                file.create(.(flag))
                tools::pskill(Sys.getpid(), tools::SIGKILL)
            }),
            where = asNamespace("sits"), print = FALSE
        )
        NULL
    }
    environment(kill_after_merge) <- globalenv()
    parallel::clusterCall(sits_env[["cluster"]], kill_after_merge, flag)
    probs <- suppressMessages(classify("queue", 2L))
    expect_true(file.exists(flag))
    for (i in 1:2) {
        expect_equal(
            .raster_get_values(.raster_open_rast(.tile_path(probs[i, ]))),
            .raster_get_values(.raster_open_rast(.tile_path(reference[i, ])))
        )
    }
})

test_that("Classify with ROI - worker dies after a crop, merge resent", {
    # with a ROI the merge writes to .sits, crops into output_dir and only
    # then removes the merged file; the kill comes after the crop
    skip_on_cran()
    skip_on_os("windows")
    suppressMessages(sits_parallel(workers = 0L))
    root <- file.path(tempdir(), "classify-crop-death")
    unlink(root, recursive = TRUE)
    dir.create(root, recursive = TRUE)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    cube <- gpu_tiles_cube(root)
    set.seed(42L)
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor(num_trees = 40L))
    xres <- .tile_xres(cube[1L, ])
    roi <- sf::st_as_sfc(sf::st_bbox(c(
        xmin = cube[["xmin"]][[1L]] + 40L * xres,
        xmax = cube[["xmax"]][[2L]] - 40L * xres,
        ymin = cube[["ymin"]][[1L]] + 30L * xres,
        ymax = cube[["ymax"]][[1L]] - 30L * xres
    ), crs = sf::st_crs(cube[["crs"]][[1L]])))
    classify <- function(name, multicores) {
        out_dir <- file.path(root, name)
        dir.create(out_dir)
        sits_classify(
            cube, rfor_model, roi = roi,
            block_size = c(nrows = 64L, ncols = 64L),
            multicores = multicores, memsize = 4L, output_dir = out_dir,
            progress = FALSE
        )
    }
    reference <- classify("sequential", 1L)
    suppressMessages(sits_parallel(workers = 2L))
    on.exit(suppressMessages(sits_parallel(workers = 0L)), add = TRUE)
    # the first crop in a worker writes its file, then kills the worker;
    # the flag keeps the other traced worker from killing itself
    flag <- file.path(root, "killed")
    kill_after_crop <- function(flag) {
        trace(
            ".crop",
            exit = bquote(if (!file.exists(.(flag))) {
                file.create(.(flag))
                tools::pskill(Sys.getpid(), tools::SIGKILL)
            }),
            where = asNamespace("sits"), print = FALSE
        )
        NULL
    }
    environment(kill_after_crop) <- globalenv()
    parallel::clusterCall(sits_env[["cluster"]], kill_after_crop, flag)
    probs <- suppressMessages(classify("queue", 2L))
    expect_true(file.exists(flag))
    for (i in 1:2) {
        expect_equal(sits_bbox(probs[i, ]), sits_bbox(reference[i, ]))
        expect_equal(
            .raster_get_values(.raster_open_rast(.tile_path(probs[i, ]))),
            .raster_get_values(.raster_open_rast(.tile_path(reference[i, ])))
        )
    }
})
