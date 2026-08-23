test_that("torch bayes smoother matches C++ output (neigh_fraction = 1.0)", {
    set.seed(42)
    nrows <- 20L
    ncols <- 20L
    nbands <- 4L
    window_size <- 5L
    smoothness <- rep(20.0, nbands)
    neigh_fraction <- 1.0

    # Synthetic logit matrix — values in a range that avoids ±Inf
    logits <- matrix(
        runif(nrows * ncols * nbands, min = -3, max = 3),
        nrow = nrows * ncols,
        ncol = nbands
    )

    cpp_result <- bayes_smoother_fraction(
        logits         = logits,
        nrows          = nrows,
        ncols          = ncols,
        window_size    = window_size,
        smoothness     = smoothness,
        neigh_fraction = neigh_fraction
    )

    torch_result <- .torch_smooth_bayes_fraction(
        logits         = logits,
        nrows          = nrows,
        ncols          = ncols,
        window_size    = window_size,
        smoothness     = smoothness,
        neigh_fraction = neigh_fraction
    )

    # Interior pixels (excluding the leg-width border) must match closely
    leg      <- window_size %/% 2L
    row_ids  <- (leg + 1L):(nrows - leg)
    col_ids  <- (leg + 1L):(ncols - leg)
    interior <- as.vector(
        outer(row_ids - 1L, col_ids, FUN = function(r, c) r * ncols + c)
    )

    max_diff <- max(abs(cpp_result[interior, ] - torch_result[interior, ]))
    expect_lt(max_diff, 1e-3)
})

test_that("torch bayes smoother matches C++ output (neigh_fraction = 0.5)", {
    set.seed(123)
    nrows <- 20L
    ncols <- 20L
    nbands <- 4L
    window_size <- 5L
    smoothness <- c(10.0, 20.0, 15.0, 25.0)
    neigh_fraction <- 0.5

    logits <- matrix(
        runif(nrows * ncols * nbands, min = -3, max = 3),
        nrow = nrows * ncols,
        ncol = nbands
    )

    cpp_result <- bayes_smoother_fraction(
        logits         = logits,
        nrows          = nrows,
        ncols          = ncols,
        window_size    = window_size,
        smoothness     = smoothness,
        neigh_fraction = neigh_fraction
    )

    torch_result <- .torch_smooth_bayes_fraction(
        logits         = logits,
        nrows          = nrows,
        ncols          = ncols,
        window_size    = window_size,
        smoothness     = smoothness,
        neigh_fraction = neigh_fraction
    )

    leg      <- window_size %/% 2L
    row_ids  <- (leg + 1L):(nrows - leg)
    col_ids  <- (leg + 1L):(ncols - leg)
    interior <- as.vector(
        outer(row_ids - 1L, col_ids, FUN = function(r, c) r * ncols + c)
    )

    max_diff <- max(abs(cpp_result[interior, ] - torch_result[interior, ]))
    expect_lt(max_diff, 1e-3)
})

test_that("torch smoother availability supports fallback and override", {
    old_force_cpp <- Sys.getenv("SITS_SMOOTH_FORCE_CPP", unset = NA_character_)
    on.exit(
        if (is.na(old_force_cpp)) {
            Sys.unsetenv("SITS_SMOOTH_FORCE_CPP")
        } else {
            Sys.setenv(SITS_SMOOTH_FORCE_CPP = old_force_cpp)
        }
    )
    smooth_available <- function(functional, force_cpp = "FALSE") {
        testthat::local_mocked_bindings(
            .torch_is_functional = function() functional
        )
        Sys.setenv(SITS_SMOOTH_FORCE_CPP = force_cpp)
        .torch_smooth_available()
    }
    expect_true(smooth_available(functional = TRUE))
    expect_false(smooth_available(functional = FALSE))
    expect_false(smooth_available(functional = TRUE, force_cpp = "TRUE"))
})

test_that("torch smoother accounts for materialized neighborhoods", {
    expect_equal(
        .torch_smooth_block_memsize(
            block_size = 100,
            window_size = 9L
        ),
        0.000324
    )
})

test_that("Bayesian smoothers validate neighborhood parameters", {
    probs_cube <- structure(list(), class = "probs_cube")
    probs_vector_cube <- structure(list(), class = "probs_vector_cube")
    testthat::local_mocked_bindings(
        .check_raster_cube_files = function(cube) invisible(cube)
    )

    expect_error(
        sits_smooth(
            probs_cube,
            window_size = 3L,
            output_dir = tempdir()
        )
    )
    expect_error(
        sits_smooth(
            probs_cube,
            window_size = 23L,
            output_dir = tempdir()
        )
    )
    expect_error(
        sits_smooth(
            probs_cube,
            neigh_fraction = 0.0,
            output_dir = tempdir()
        )
    )
    expect_error(
        sits_smooth(
            probs_vector_cube,
            neigh_fraction = 0.0,
            output_dir = tempdir()
        )
    )
    expect_error(
        sits_smooth(
            probs_cube,
            progress = "TRUE",
            output_dir = tempdir()
        )
    )
})

test_that("torch functional check disables automatic installation", {
    old_torch_install <- Sys.getenv("TORCH_INSTALL", unset = NA_character_)
    on.exit(
        if (is.na(old_torch_install)) {
            Sys.unsetenv("TORCH_INSTALL")
        } else {
            Sys.setenv(TORCH_INSTALL = old_torch_install)
        }
    )
    Sys.setenv(TORCH_INSTALL = "1")
    testthat::local_mocked_bindings(
        .torch_package_is_installed = function() TRUE,
        .torch_native_is_installed = function() {
            expect_equal(Sys.getenv("TORCH_INSTALL"), "0")
            TRUE
        }
    )
    expect_true(.torch_is_functional())
    expect_equal(Sys.getenv("TORCH_INSTALL"), "1")
})

test_that("sits_smooth selects CPU and torch implementations", {
    skip_on_cran()
    skip_if_not_installed("torch")

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    raster_cube <- sits_cube(
        source     = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir   = data_dir,
        tiles      = "012010",
        bands      = "NDVI",
        start_date = "2013-09-14",
        end_date   = "2014-08-29",
        multicores = 1L,
        progress   = FALSE
    )
    rfor_model <- sits_train(
        samples_modis_ndvi,
        sits_rfor(num_trees = 40L)
    )

    out_dir <- file.path(tempdir(), "smooth-torch-comparison")
    # Purge stale cache files from any previous run
    unlink(out_dir, recursive = TRUE)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

    probs_cube <- suppressWarnings(
        sits_classify(
            data = raster_cube,
            ml_model = rfor_model,
            multicores = 1L,
            output_dir = out_dir,
            progress = FALSE
        )
    )

    # Exercise both implementations through the public API. Torch falls back
    # to CPU tensors when no CUDA or MPS device is available.
    smooth_backend <- function(use_torch, version) {
        testthat::local_mocked_bindings(
            .torch_smooth_available = function() use_torch,
            .torch_gpu_available = function() FALSE
        )
        sits_smooth(
            cube = probs_cube,
            output_dir = out_dir,
            version = version,
            multicores = 1L,
            progress = FALSE
        )
    }
    smooth_cpp <- smooth_backend(use_torch = FALSE, version = "v1-cpp")
    smooth_torch <- smooth_backend(use_torch = TRUE, version = "v1-torch")

    # Compare interior pixels only.
    # The absolute tile boundary (leg = window_size %/% 2 = 4 pixels) uses
    # reflect padding in torch vs. a boundary-inclusive mirror in C++, so
    # those edge pixels are expected to differ — they are cropped by the
    # overlap mechanism for all interior chunks anyway.
    cpp_rast <- .raster_open_rast(.fi_paths(.fi(smooth_cpp)))
    torch_rast <- .raster_open_rast(.fi_paths(.fi(smooth_torch)))
    nrow_r <- .raster_nrows(cpp_rast)
    ncol_r <- .raster_ncols(cpp_rast)

    leg <- 9L %/% 2L
    row_ids <- (leg + 1L):(nrow_r - leg)
    col_ids <- (leg + 1L):(ncol_r - leg)
    interior <- as.vector(
        outer(row_ids - 1L, col_ids, FUN = function(r, c) r * ncol_r + c)
    )

    cpp_vals <- .raster_get_values(cpp_rast)
    torch_vals <- .raster_get_values(torch_rast)

    valid <- is.finite(cpp_vals[interior, 1L]) &
        is.finite(torch_vals[interior, 1L])
    pix <- interior[valid]

    max_diff <- max(abs(cpp_vals[pix, ] - torch_vals[pix, ]))
    expect_lt(max_diff, 5.0)
})
