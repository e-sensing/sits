test_that("streaming pipeline flag gates correctly", {
    old <- Sys.getenv("SITS_GPU_PIPELINE", unset = NA)
    on.exit({
        if (is.na(old)) {
            Sys.unsetenv("SITS_GPU_PIPELINE")
        } else {
            Sys.setenv(SITS_GPU_PIPELINE = old)
        }
    }, add = TRUE)

    Sys.unsetenv("SITS_GPU_PIPELINE")
    expect_false(.stream_enabled())
    Sys.setenv(SITS_GPU_PIPELINE = "staged")
    expect_false(.stream_enabled())
    Sys.setenv(SITS_GPU_PIPELINE = "stream")
    expect_identical(
        .stream_enabled(),
        requireNamespace("siphon", quietly = TRUE)
    )
})

test_that("streaming classification matches the CPU pipeline", {
    skip_on_cran()
    skip_if_not_installed("siphon")
    skip_if_not_installed("torch")

    set.seed(42)
    torch::torch_manual_seed(42)
    cpu_dir <- file.path(tempdir(), "stream_test_cpu")
    stream_dir <- file.path(tempdir(), "stream_test_stream")
    unlink(c(cpu_dir, stream_dir), recursive = TRUE)
    dir.create(cpu_dir)
    dir.create(stream_dir)
    on.exit(unlink(c(cpu_dir, stream_dir), recursive = TRUE), add = TRUE)

    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = system.file("extdata/raster/mod13q1", package = "sits"),
        progress = FALSE
    )
    tile <- .tile(cube)
    ml_model <- sits_train(
        samples_modis_ndvi,
        sits_tempcnn(epochs = 2L, verbose = FALSE)
    )
    # several chunks so the pipeline actually streams
    block <- c(ncols = 255L, nrows = 50L)

    probs_cpu <- .classify_tile_cpu(
        tile = tile, out_band = "probs", bands = "NDVI", base_bands = NULL,
        ml_model = ml_model, block = block, roi = NULL,
        exclusion_mask = NULL, impute_fn = impute_linear(),
        output_dir = cpu_dir, version = "v1",
        verbose = FALSE, progress = FALSE
    )
    # direct call: dispatch via sits_classify() requires a GPU device,
    # but the streaming pipeline itself is device-agnostic
    probs_stream <- .classify_tile_stream(
        tile = tile, out_band = "probs", bands = "NDVI", base_bands = NULL,
        ml_model = ml_model, block = block, roi = NULL,
        exclusion_mask = NULL, impute_fn = impute_linear(),
        output_dir = stream_dir, version = "v1", multicores = 2L,
        verbose = FALSE, progress = FALSE
    )

    v_cpu <- terra::values(terra::rast(.tile_path(probs_cpu)))
    v_stream <- terra::values(terra::rast(.tile_path(probs_stream)))
    expect_identical(v_stream, v_cpu)
})

test_that("streaming classification reuses a persistent sits cluster", {
    # NOTE: run against an *installed* dev sits. Stage closures are
    # serialized to workers with a namespace reference, so workers resolve
    # sits internals in the installed package; under pkgload::load_all()
    # over an older installed sits, workers execute mixed code.
    skip_on_cran()
    skip_if_not_installed("siphon")
    skip_if_not_installed("torch")

    set.seed(42)
    torch::torch_manual_seed(42)
    cpu_dir <- file.path(tempdir(), "stream_pers_cpu")
    stream_dir <- file.path(tempdir(), "stream_pers_stream")
    unlink(c(cpu_dir, stream_dir), recursive = TRUE)
    dir.create(cpu_dir)
    dir.create(stream_dir)
    on.exit(unlink(c(cpu_dir, stream_dir), recursive = TRUE), add = TRUE)
    on.exit(.parallel_stop(TRUE), add = TRUE)

    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = system.file("extdata/raster/mod13q1", package = "sits"),
        progress = FALSE
    )
    tile <- .tile(cube)
    ml_model <- sits_train(
        samples_modis_ndvi,
        sits_tempcnn(epochs = 2L, verbose = FALSE)
    )
    # several chunks so the pipeline actually streams
    block <- c(ncols = 255L, nrows = 50L)

    probs_cpu <- .classify_tile_cpu(
        tile = tile, out_band = "probs", bands = "NDVI", base_bands = NULL,
        ml_model = ml_model, block = block, roi = NULL,
        exclusion_mask = NULL, impute_fn = impute_linear(),
        output_dir = cpu_dir, version = "v1",
        verbose = FALSE, progress = FALSE
    )

    # persistent cluster as created by sits_parallel(workers = 3)
    .parallel_start(workers = 3L)
    bk <- .stream_backend_start(multicores = 8L)
    # attached to the sits cluster, not owned by siphon
    expect_false(bk[["owned"]])
    # read slots leave one node for the write slot
    expect_identical(.stream_read_workers(bk, 8L), 2L)
    probs_stream <- .classify_tile_stream(
        tile = tile, out_band = "probs", bands = "NDVI", base_bands = NULL,
        ml_model = ml_model, block = block, roi = NULL,
        exclusion_mask = NULL, impute_fn = impute_linear(),
        output_dir = stream_dir, version = "v1", multicores = 8L,
        bk = bk, verbose = FALSE, progress = FALSE
    )
    .stream_backend_stop(bk)
    # the cluster survives the stream run and remains usable by sits
    expect_true(.parallel_is_open())
    expect_identical(
        unlist(parallel::clusterApply(
            sits_env[["cluster"]], 1:3, function(i) i
        )),
        1:3
    )
    # no stage runners left behind on the persistent workers
    leftover <- grep(
        "^\\.siphon_stage_",
        unique(unlist(parallel::clusterEvalQ(
            sits_env[["cluster"]], ls(globalenv(), all.names = TRUE)
        ))),
        value = TRUE
    )
    expect_length(leftover, 0L)

    v_cpu <- terra::values(terra::rast(.tile_path(probs_cpu)))
    v_stream <- terra::values(terra::rast(.tile_path(probs_stream)))
    expect_identical(v_stream, v_cpu)
})
