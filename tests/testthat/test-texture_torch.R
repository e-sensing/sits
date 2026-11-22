test_that("torch glcm_compute matches C++ for all metrics (single angle)", {
    skip_if_not_installed("torch")

    set.seed(7321)
    nrows       <- 20L
    ncols       <- 20L
    window_size <- 5L
    n_grey      <- 64L   # small so the batched ASM loop is fast and comparable
    angles      <- 0.0

    # Synthetic grey-level image; values in [0, n_grey - 1] (matching C++ check)
    img <- as.integer(sample(0L:(n_grey - 1L), nrows * ncols, replace = TRUE))

    check_metric <- function(metric, cpp_fn) {
        cpp_result <- cpp_fn(
            x           = img,
            angles      = angles,
            nrows       = nrows,
            ncols       = ncols,
            n_grey      = n_grey,
            window_size = window_size
        )
        torch_result <- .texture_torch_glcm_compute(
            m           = img,
            metric      = metric,
            window_size = window_size,
            angles      = angles,
            img_nrow    = nrows,
            img_ncol    = ncols,
            n_grey      = n_grey
        )
        # the C++ mirror and torch reflect agree, border included
        max_diff <- max(abs(cpp_result - torch_result[, 1L]))
        expect_lt(max_diff, 1e-3,
            label = paste("max|diff| for", metric))
    }

    check_metric("contrast",      C_glcm_contrast)
    check_metric("dissimilarity", C_glcm_dissimilarity)
    check_metric("homogeneity",   C_glcm_homogeneity)
    check_metric("mean",          C_glcm_mean)
    check_metric("variance",      C_glcm_variance)
    check_metric("std",           C_glcm_std)
    check_metric("correlation",   C_glcm_correlation)
    check_metric("asm",           C_glcm_asm)
    check_metric("energy",        C_glcm_energy)
})

test_that("torch glcm_compute matches C++ for multiple angles", {
    skip_if_not_installed("torch")

    set.seed(8103)
    nrows       <- 20L
    ncols       <- 20L
    window_size <- 5L
    n_grey      <- 64L
    angles      <- c(0.0, pi / 2)

    img <- as.integer(sample(0L:(n_grey - 1L), nrows * ncols, replace = TRUE))

    cpp_result <- C_glcm_contrast(
        x           = img,
        angles      = angles,
        nrows       = nrows,
        ncols       = ncols,
        n_grey      = n_grey,
        window_size = window_size
    )
    torch_result <- .texture_torch_glcm_compute(
        m           = img,
        metric      = "contrast",
        window_size = window_size,
        angles      = angles,
        img_nrow    = nrows,
        img_ncol    = ncols,
        n_grey      = n_grey
    )

    max_diff <- max(abs(cpp_result - torch_result[, 1L]))
    expect_lt(max_diff, 1e-3)
})

test_that("sits_texture runs torch on the GPU in one worker, C++ otherwise", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source     = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir   = data_dir,
        progress   = FALSE
    )
    feature <- sits_select(cube, bands = "NDVI", dates = "2013-09-14")
    real_start <- .parallel_start
    real_feature <- .texture_feature
    run <- function(use_gpu) {
        out_dir <- normalizePath(
            file.path(tempdir(), paste0("texdispatch", use_gpu)),
            mustWork = FALSE
        )
        unlink(out_dir, recursive = TRUE)
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
        on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
        torch_calls <- 0L
        workers <- NA_integer_
        picked <- NULL
        # the torch factory hands back the C++ functions, so the test
        # needs no torch and sees only which factory the dispatch picks
        testthat::local_mocked_bindings(
            .texture_use_gpu = function() use_gpu,
            .texture_torch_functions = function(...) {
                torch_calls <<- torch_calls + 1L
                .texture_functions(...)
            },
            .parallel_start = function(workers, ...) {
                workers <<- workers
                real_start(workers = workers, ...)
            },
            # runs in the main process, so it sees the factory the workers get
            .texture_feature = function(..., texture_fn) {
                picked <<- texture_fn
                real_feature(..., texture_fn = texture_fn)
            }
        )
        sits_texture(
            feature,
            NDVICONTRAST = glcm_contrast(NDVI),
            window_size  = 5L,
            multicores   = 2L,
            output_dir   = out_dir,
            progress     = FALSE
        )
        list(
            torch_calls = torch_calls, workers = workers,
            cpp = identical(picked, .texture_functions)
        )
    }
    gpu <- run(use_gpu = TRUE)
    expect_gt(gpu[["torch_calls"]], 0L)
    expect_false(gpu[["cpp"]])
    expect_equal(gpu[["workers"]], 1L)
    cpu <- run(use_gpu = FALSE)
    expect_true(cpu[["cpp"]])
    expect_equal(cpu[["workers"]], 2L)
})

test_that(".texture_use_gpu follows SITS_FORCE_CPU on a GPU machine", {
    skip_if_not_installed("torch")
    old_env <- Sys.getenv("SITS_FORCE_CPU", unset = NA_character_)
    on.exit(
        if (is.na(old_env)) {
            Sys.unsetenv("SITS_FORCE_CPU")
        } else {
            Sys.setenv(SITS_FORCE_CPU = old_env)
        }
    )
    # a GPU machine as torch sees it
    testthat::local_mocked_bindings(.torch_is_functional = function() TRUE)
    testthat::local_mocked_bindings(
        cuda_is_available = function() TRUE, .package = "torch"
    )
    Sys.setenv(SITS_FORCE_CPU = "FALSE")
    expect_true(.texture_use_gpu())
    Sys.setenv(SITS_FORCE_CPU = "TRUE")
    expect_false(.texture_use_gpu())
})

test_that("sits_texture torch and C++ backends produce close results on real data", {
    skip_on_cran()
    skip_if_not_installed("torch")

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source     = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir   = data_dir,
        progress   = FALSE
    )
    feature <- sits_select(cube, bands = "NDVI", dates = "2013-09-14")
    class(feature) <- "data.frame"

    window_size <- 5L

    # Use normalizePath so the output dir and internally computed file paths
    # both use the same separator, avoiding a mixed-slash issue on Windows
    # where .file_eo_name normalises paths while file.path uses forward slashes.
    out_cpp   <- normalizePath(file.path(tempdir(), "texcpp"),   mustWork = FALSE)
    out_torch <- normalizePath(file.path(tempdir(), "textorch"), mustWork = FALSE)
    unlink(out_cpp,   recursive = TRUE)
    unlink(out_torch, recursive = TRUE)
    dir.create(out_cpp,   recursive = TRUE, showWarnings = FALSE)
    dir.create(out_torch, recursive = TRUE, showWarnings = FALSE)
    on.exit({
        unlink(out_cpp,   recursive = TRUE)
        unlink(out_torch, recursive = TRUE)
    }, add = TRUE)

    cpp_cube <- testthat::with_mocked_bindings(
        sits_texture(
            cube         = feature,
            NDVICONTRAST = glcm_contrast(NDVI),
            window_size  = window_size,
            multicores   = 1L,
            output_dir   = out_cpp,
            progress     = FALSE
        ),
        .texture_use_gpu = function() FALSE
    )

    torch_cube <- testthat::with_mocked_bindings(
        sits_texture(
            cube         = feature,
            NDVICONTRAST = glcm_contrast(NDVI),
            window_size  = window_size,
            multicores   = 1L,
            output_dir   = out_torch,
            progress     = FALSE
        ),
        .texture_use_gpu = function() TRUE
    )

    expect_true(all(sits_bands(cpp_cube)   %in% c("NDVI", "NDVICONTRAST")))
    expect_true(all(sits_bands(torch_cube) %in% c("NDVI", "NDVICONTRAST")))

    cpp_rast   <- .raster_open_rast(
        .fi_paths(.fi(sits_select(cpp_cube,   bands = "NDVICONTRAST")))
    )
    torch_rast <- .raster_open_rast(
        .fi_paths(.fi(sits_select(torch_cube, bands = "NDVICONTRAST")))
    )

    cpp_vals   <- .raster_get_values(cpp_rast)
    torch_vals <- .raster_get_values(torch_rast)

    # same NA mask; values differ only by the INT2S rounding of each path
    expect_identical(is.na(cpp_vals), is.na(torch_vals))
    expect_lte(max(abs(cpp_vals - torch_vals), na.rm = TRUE), 1)
})

test_that("sits_texture torch backend produces all nine GLCM bands", {
    skip_on_cran()
    skip_if_not_installed("torch")

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source     = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir   = data_dir,
        progress   = FALSE
    )
    feature <- sits_select(cube, bands = "NDVI", dates = "2013-09-14")
    class(feature) <- "data.frame"

    out_dir <- normalizePath(file.path(tempdir(), "texbands"), mustWork = FALSE)
    unlink(out_dir, recursive = TRUE)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

    run <- function(texture_cube) {
        testthat::with_mocked_bindings(
            texture_cube,
            .texture_use_gpu = function() TRUE
        )
    }

    expect_true("NDVICONTRAST" %in% sits_bands(run(sits_texture(
        feature, NDVICONTRAST = glcm_contrast(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir, progress = FALSE
    ))))
    expect_true("NDVIDISSIM" %in% sits_bands(run(sits_texture(
        feature, NDVIDISSIM = glcm_dissimilarity(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir, progress = FALSE
    ))))
    expect_true("NDVIHOMOG" %in% sits_bands(run(sits_texture(
        feature, NDVIHOMOG = glcm_homogeneity(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir, progress = FALSE
    ))))
    expect_true("NDVIENERGY" %in% sits_bands(run(sits_texture(
        feature, NDVIENERGY = glcm_energy(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir, progress = FALSE
    ))))
    expect_true("NDVIASM" %in% sits_bands(run(sits_texture(
        feature, NDVIASM = glcm_asm(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir, progress = FALSE
    ))))
    expect_true("NDVIMEAN" %in% sits_bands(run(sits_texture(
        feature, NDVIMEAN = glcm_mean(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir, progress = FALSE
    ))))
    expect_true("NDVIVAR" %in% sits_bands(run(sits_texture(
        feature, NDVIVAR = glcm_variance(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir, progress = FALSE
    ))))
    expect_true("NDVISTD" %in% sits_bands(run(sits_texture(
        feature, NDVISTD = glcm_std(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir, progress = FALSE
    ))))
    expect_true("NDVICORRELATION" %in% sits_bands(run(sits_texture(
        feature, NDVICORRELATION = glcm_correlation(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir, progress = FALSE
    ))))
})

test_that("sits_texture rejects windows without neighbour pairs", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source     = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir   = data_dir,
        progress   = FALSE
    )
    expect_error(
        sits_texture(
            cube,
            NDVIVAR = glcm_variance(NDVI),
            window_size = 1L,
            output_dir = tempdir(),
            progress = FALSE
        ),
        "window_size"
    )
})
