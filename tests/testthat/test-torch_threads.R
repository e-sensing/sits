test_that(".torch_set_threads(NULL) uses .parallel_cores, then restores", {
    skip_if_not_installed("torch")
    skip_if_not(torch::torch_is_installed())
    before <- torch::torch_get_num_threads()
    on.exit(torch::torch_set_num_threads(before), add = TRUE)
    torch::torch_set_num_threads(1L)
    local_mocked_bindings(.parallel_cores = function(...) 3L)
    old <- .torch_set_threads(NULL)
    expect_equal(torch::torch_get_num_threads(), 3L)
    .torch_set_threads(old)
    expect_equal(torch::torch_get_num_threads(), 1L)
    # a second value rules out a constant that happens to match
    local_mocked_bindings(.parallel_cores = function(...) 2L)
    old <- .torch_set_threads(NULL)
    expect_equal(torch::torch_get_num_threads(), 2L)
    .torch_set_threads(old)
    expect_equal(torch::torch_get_num_threads(), 1L)
})

test_that(".torch_threads is gone; the CPU count lives in .parallel_cores", {
    expect_false(
        exists(".torch_threads", envir = asNamespace("sits"), inherits = FALSE)
    )
})

test_that(".torch_set_threads does nothing when torch is not functional", {
    skip_if_not_installed("torch")
    local_mocked_bindings(.torch_is_functional = function() FALSE)
    # the stops make any call into torch fail the test
    local_mocked_bindings(
        torch_is_installed = function() stop("torch_is_installed called"),
        torch_set_num_threads = function(...) stop("threads changed"),
        .package = "torch"
    )
    old <- 0L
    expect_no_error(old <- .torch_set_threads(2L))
    expect_null(old)
    expect_no_error(.torch_set_threads(old))
})

test_that(paste(
    "sits_train trains with the CPU quota of torch threads,",
    "then restores"
), {
    skip_if_not_installed("torch")
    skip_if_not(torch::torch_is_installed())
    before <- torch::torch_get_num_threads()
    on.exit(torch::torch_set_num_threads(before), add = TRUE)
    torch::torch_set_num_threads(3L)
    seen <- NA_integer_
    # a torch training method that records the threads it was given;
    # torch factories define `optimizer`, which marks them as torch
    ml_method <- local({
        optimizer <- NULL
        function(samples) {
            seen <<- torch::torch_get_num_threads()
            function(values) values
        }
    })
    local_mocked_bindings(.parallel_cores = function(...) 2L)
    sits_train(samples_modis_ndvi, ml_method)
    expect_equal(seen, 2L)
    expect_equal(torch::torch_get_num_threads(), 3L)
    # threads given by the caller win over the quota
    sits_train(samples_modis_ndvi, ml_method, threads = 1L)
    expect_equal(seen, 1L)
    expect_equal(torch::torch_get_num_threads(), 3L)
    # restored when the training fails
    fails <- local({
        optimizer <- NULL
        function(samples) stop("fails")
    })
    expect_error(sits_train(samples_modis_ndvi, fails))
    expect_equal(torch::torch_get_num_threads(), 3L)
    expect_error(sits_train(samples_modis_ndvi, ml_method, threads = 0L))
})

test_that("sits_train leaves torch threads alone for a non-torch method", {
    skip_if_not_installed("torch")
    skip_if_not(torch::torch_is_installed())
    before <- torch::torch_get_num_threads()
    on.exit(torch::torch_set_num_threads(before), add = TRUE)
    torch::torch_set_num_threads(3L)
    seen <- NA_integer_
    rfor <- sits_rfor(num_trees = 20L)
    ml_method <- function(samples) {
        seen <<- torch::torch_get_num_threads()
        rfor(samples)
    }
    local_mocked_bindings(.parallel_cores = function(...) 2L)
    sits_train(samples_modis_ndvi, ml_method)
    expect_equal(seen, 3L)
    expect_equal(torch::torch_get_num_threads(), 3L)
    sits_train(samples_modis_ndvi, ml_method, threads = 1L)
    expect_equal(seen, 3L)
    # the real method must not reach the torch threads at all
    local_mocked_bindings(.torch_set_threads = function(...) stop("torch"))
    expect_no_error(sits_train(samples_modis_ndvi, rfor))
    expect_equal(torch::torch_get_num_threads(), 3L)
})

# Wrap internal functions that run next to the model in the calling process,
# so a test sees the torch threads in use while the model predicts
record_threads <- function(fns, env = parent.frame()) {
    seen <- new.env()
    seen[["threads"]] <- integer(0L)
    mocks <- lapply(fns, function(fn) {
        orig <- get(fn, envir = asNamespace("sits"))
        function(...) {
            seen[["threads"]] <- c(
                seen[["threads"]], torch::torch_get_num_threads()
            )
            orig(...)
        }
    })
    names(mocks) <- fns
    do.call(local_mocked_bindings, c(mocks, list(.env = env)))
    seen
}

test_that("sits_classify uses the given torch threads, then restores", {
    skip_on_cran()
    skip_if_not_installed("torch")
    skip_if_not(torch::torch_is_installed())
    skip_if_not_installed("luz")
    before <- torch::torch_get_num_threads()
    on.exit(torch::torch_set_num_threads(before), add = TRUE)
    set.seed(42L)
    torch::torch_manual_seed(42L)
    ml_model <- sits_train(
        samples_modis_ndvi,
        sits_mlp(
            layers = c(16L, 16L), dropout_rates = c(0.1, 0.1),
            epochs = 2L, batch_size = 64L, verbose = FALSE
        )
    )
    points <- samples_modis_ndvi[1:10, ]
    torch::torch_set_num_threads(1L)
    # time series, CPU branch
    seen <- record_threads(".pred_features")
    sits_classify(points, ml_model, multicores = 1L, threads = 3L,
                  progress = FALSE)
    expect_equal(unique(seen[["threads"]]), 3L)
    expect_equal(torch::torch_get_num_threads(), 1L)
    # NULL uses the CPU quota of the cgroup
    local_mocked_bindings(.parallel_cores = function(...) 2L)
    seen[["threads"]] <- integer(0L)
    sits_classify(points, ml_model, multicores = 1L, progress = FALSE)
    expect_equal(unique(seen[["threads"]]), 2L)
    expect_equal(torch::torch_get_num_threads(), 1L)
    # GPU branch, where multicores becomes 1 and sets its own on.exit
    local({
        local_mocked_bindings(.torch_gpu_classification = function() TRUE)
        seen[["threads"]] <- integer(0L)
        sits_classify(points, ml_model, multicores = 2L, threads = 3L,
                      progress = FALSE)
        expect_equal(unique(seen[["threads"]]), 3L)
        expect_equal(torch::torch_get_num_threads(), 1L)
    })
    # restored when the classification fails
    local({
        local_mocked_bindings(.pred_features = function(...) stop("fails"))
        expect_error(sits_classify(points, ml_model, multicores = 1L,
                                   threads = 3L, progress = FALSE))
        expect_equal(torch::torch_get_num_threads(), 1L)
    })
    expect_error(sits_classify(points, ml_model, threads = 0L,
                               progress = FALSE))
    # a model that is not torch leaves the threads alone
    rf_model <- sits_train(samples_modis_ndvi, sits_rfor(num_trees = 20L))
    seen[["threads"]] <- integer(0L)
    sits_classify(points, rf_model, multicores = 1L, threads = 3L,
                  progress = FALSE)
    expect_equal(unique(seen[["threads"]]), 1L)
    # raster cube
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = system.file("extdata/raster/mod13q1", package = "sits"),
        progress = FALSE
    )
    output_dir <- tempfile("threads-classify-")
    dir.create(output_dir)
    on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
    seen_rst <- record_threads(".check_processed_values")
    sits_classify(cube, ml_model, memsize = 4L, multicores = 1L,
                  threads = 3L, output_dir = output_dir, progress = FALSE)
    expect_equal(unique(seen_rst[["threads"]]), 3L)
    expect_equal(torch::torch_get_num_threads(), 1L)
    # vector cube
    skip_if_not_installed("snic")
    segments <- sits_segment(
        cube = cube, seg_fn = sits_snic(spacing = 50L),
        output_dir = output_dir, memsize = 4L, multicores = 1L,
        progress = FALSE
    )
    seen_rst[["threads"]] <- integer(0L)
    sits_classify(segments, ml_model, memsize = 4L, multicores = 1L,
                  threads = 3L, output_dir = output_dir, version = "vec",
                  progress = FALSE)
    expect_equal(unique(seen_rst[["threads"]]), 3L)
    expect_equal(torch::torch_get_num_threads(), 1L)
})

test_that("sits_encode uses the given torch threads, then restores", {
    skip_on_cran()
    skip_if_not_installed("torch")
    skip_if_not(torch::torch_is_installed())
    skip_if_not_installed("luz")
    before <- torch::torch_get_num_threads()
    on.exit(torch::torch_set_num_threads(before), add = TRUE)
    set.seed(42L)
    torch::torch_manual_seed(42L)
    encoder <- sits_pre_train(
        samples = samples_modis_ndvi,
        rl_method = sits_ssl_mae(
            embedding_dim = 4L, decoder_width = 16L, mask_ratio = 0.5,
            epochs = 2L, batch_size = 64L, verbose = FALSE
        )
    )
    points <- samples_modis_ndvi[1:10, ]
    torch::torch_set_num_threads(1L)
    seen <- record_threads(c(".pred_features", ".check_processed_values"))
    sits_encode(points, encoder, multicores = 1L, threads = 3L,
                progress = FALSE)
    expect_equal(unique(seen[["threads"]]), 3L)
    expect_equal(torch::torch_get_num_threads(), 1L)
    # NULL uses the CPU quota of the cgroup
    local_mocked_bindings(.parallel_cores = function(...) 2L)
    seen[["threads"]] <- integer(0L)
    sits_encode(points, encoder, multicores = 1L, progress = FALSE)
    expect_equal(unique(seen[["threads"]]), 2L)
    expect_equal(torch::torch_get_num_threads(), 1L)
    expect_error(sits_encode(points, encoder, threads = 0L, progress = FALSE))
    # raster cube
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = system.file("extdata/raster/mod13q1", package = "sits"),
        progress = FALSE
    )
    output_dir <- tempfile("threads-encode-")
    dir.create(output_dir)
    on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
    seen[["threads"]] <- integer(0L)
    sits_encode(cube, encoder, memsize = 4L, multicores = 1L, threads = 3L,
                output_dir = output_dir, progress = FALSE)
    expect_equal(unique(seen[["threads"]]), 3L)
    expect_equal(torch::torch_get_num_threads(), 1L)
})

test_that("validation and tuning train torch with the CPU quota", {
    skip_on_cran()
    skip_if_not_installed("torch")
    skip_if_not(torch::torch_is_installed())
    skip_if_not_installed("luz")
    skip_if_not_installed("caret")
    before <- torch::torch_get_num_threads()
    on.exit(torch::torch_set_num_threads(before), add = TRUE)
    set.seed(42L)
    torch::torch_manual_seed(42L)
    torch::torch_set_num_threads(1L)
    local_mocked_bindings(.parallel_cores = function(...) 2L)
    mlp <- sits_mlp(
        layers = c(16L, 16L), dropout_rates = c(0.1, 0.1),
        epochs = 2L, batch_size = 64L, verbose = FALSE
    )
    # .torch_serialize_model runs once at the end of each torch training
    seen <- record_threads(".torch_serialize_model")
    sits_kfold_validate(samples_modis_ndvi, folds = 2L, ml_method = mlp,
                        multicores = 1L, progress = FALSE)
    expect_equal(unique(seen[["threads"]]), 2L)
    expect_equal(torch::torch_get_num_threads(), 1L)
    seen[["threads"]] <- integer(0L)
    sits_validate(samples_modis_ndvi, ml_method = mlp)
    expect_equal(unique(seen[["threads"]]), 2L)
    expect_equal(torch::torch_get_num_threads(), 1L)
    # trials train in this process when multicores is 1
    hparams <- sits_tuning_hparams(
        layers = c(16L, 16L), dropout_rates = c(0.1, 0.1),
        epochs = 2L, batch_size = 64L, verbose = FALSE
    )
    seen[["threads"]] <- integer(0L)
    sits_tuning(samples_modis_ndvi, ml_method = sits_mlp(),
                params = hparams, trials = 1L, multicores = 1L)
    expect_equal(unique(seen[["threads"]]), 2L)
    expect_equal(torch::torch_get_num_threads(), 1L)
    # GPU forces multicores to 1
    local({
        local_mocked_bindings(.torch_gpu_classification = function() TRUE)
        seen[["threads"]] <- integer(0L)
        sits_tuning(samples_modis_ndvi, ml_method = sits_mlp(),
                    params = hparams, trials = 1L, multicores = 2L)
        expect_equal(unique(seen[["threads"]]), 2L)
        expect_equal(torch::torch_get_num_threads(), 1L)
    })
    # restored when the validation fails
    local({
        local_mocked_bindings(.predictors = function(...) stop("fails"))
        expect_error(sits_validate(samples_modis_ndvi, ml_method = mlp))
        expect_equal(torch::torch_get_num_threads(), 1L)
    })
    # a method that is not torch leaves the threads alone
    seen_rf <- record_threads(".predictors")
    sits_validate(samples_modis_ndvi, ml_method = sits_rfor(num_trees = 20L))
    expect_equal(unique(seen_rf[["threads"]]), 1L)
    seen_rf[["threads"]] <- integer(0L)
    sits_tuning(samples_modis_ndvi, ml_method = sits_rfor(),
                params = sits_tuning_hparams(num_trees = 20L),
                trials = 1L, multicores = 1L)
    expect_equal(unique(seen_rf[["threads"]]), 1L)
})
