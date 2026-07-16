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
    expect_false(sits:::.stream_enabled())
    Sys.setenv(SITS_GPU_PIPELINE = "staged")
    expect_false(sits:::.stream_enabled())
    Sys.setenv(SITS_GPU_PIPELINE = "stream")
    expect_identical(
        sits:::.stream_enabled(),
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
    tile <- sits:::.tile(cube)
    ml_model <- sits_train(
        samples_modis_ndvi,
        sits_tempcnn(epochs = 2L, verbose = FALSE)
    )
    # several chunks so the pipeline actually streams
    block <- c(ncols = 255L, nrows = 50L)

    probs_cpu <- sits:::.classify_tile_cpu(
        tile = tile, out_band = "probs", bands = "NDVI", base_bands = NULL,
        ml_model = ml_model, block = block, roi = NULL,
        exclusion_mask = NULL, filter_fn = NULL, impute_fn = impute_linear(),
        output_dir = cpu_dir, version = "v1",
        verbose = FALSE, progress = FALSE
    )
    # direct call: dispatch via sits_classify() requires a GPU device,
    # but the streaming pipeline itself is device-agnostic
    probs_stream <- sits:::.classify_tile_stream(
        tile = tile, out_band = "probs", bands = "NDVI", base_bands = NULL,
        ml_model = ml_model, block = block, roi = NULL,
        exclusion_mask = NULL, filter_fn = NULL, impute_fn = impute_linear(),
        output_dir = stream_dir, version = "v1", multicores = 2L,
        verbose = FALSE, progress = FALSE
    )

    v_cpu <- terra::values(terra::rast(sits:::.tile_path(probs_cpu)))
    v_stream <- terra::values(terra::rast(sits:::.tile_path(probs_stream)))
    expect_identical(v_stream, v_cpu)
})

test_that("streaming classification propagates stage errors and cleans up", {
    skip_on_cran()
    skip_if_not_installed("siphon")
    skip_if_not_installed("torch")

    out_dir <- file.path(tempdir(), "stream_test_err")
    unlink(out_dir, recursive = TRUE)
    dir.create(out_dir)
    on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = system.file("extdata/raster/mod13q1", package = "sits"),
        progress = FALSE
    )
    tile <- sits:::.tile(cube)
    ml_model <- sits_train(
        samples_modis_ndvi,
        sits_tempcnn(epochs = 1L, verbose = FALSE)
    )
    expect_error(
        sits:::.classify_tile_stream(
            tile = tile, out_band = "probs", bands = "NDVI",
            base_bands = NULL, ml_model = ml_model,
            block = c(ncols = 255L, nrows = 50L), roi = NULL,
            exclusion_mask = NULL,
            filter_fn = function(x) stop("stream test failure"),
            impute_fn = impute_linear(), output_dir = out_dir,
            version = "v1", multicores = 2L,
            verbose = FALSE, progress = FALSE
        ),
        "stream test failure"
    )
})
