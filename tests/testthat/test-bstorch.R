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

test_that("sits_smooth_torch produces results close to sits_smooth", {
    testthat::skip_if_not_installed("torch")
    testthat::skip_if_not_installed("xgboost")

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
            data       = raster_cube,
            ml_model   = rfor_model,
            output_dir = out_dir,
            progress   = FALSE
        )
    )

    # Smooth with C++ backend
    smooth_cpp <- sits_smooth(
        cube       = probs_cube,
        output_dir = out_dir,
        version    = "v1-cpp",
        multicores = 1L,
        progress   = FALSE
    )

    # Smooth with torch backend
    smooth_torch <- sits_smooth_torch(
        cube       = probs_cube,
        output_dir = out_dir,
        version    = "v1-torch",
        multicores = 1L,
        progress   = FALSE
    )

    # Compare interior pixels only.
    # The absolute tile boundary (leg = window_size %/% 2 = 4 pixels) uses
    # reflect padding in torch vs. a boundary-inclusive mirror in C++, so
    # those edge pixels are expected to differ — they are cropped by the
    # overlap mechanism for all interior chunks anyway.
    cpp_rast   <- .raster_open_rast(.fi_paths(.fi(smooth_cpp)))
    torch_rast <- .raster_open_rast(.fi_paths(.fi(smooth_torch)))
    nrow_r <- terra::nrow(cpp_rast)
    ncol_r <- terra::ncol(cpp_rast)

    leg      <- 9L %/% 2L   # default window_size = 9
    row_ids  <- (leg + 1L):(nrow_r - leg)
    col_ids  <- (leg + 1L):(ncol_r - leg)
    interior <- as.vector(
        outer(row_ids - 1L, col_ids, FUN = function(r, c) r * ncol_r + c)
    )

    cpp_vals   <- terra::values(cpp_rast,   mat = TRUE)
    torch_vals <- terra::values(torch_rast, mat = TRUE)

    valid <- is.finite(cpp_vals[interior, 1L]) & is.finite(torch_vals[interior, 1L])
    pix   <- interior[valid]

    max_diff <- max(abs(cpp_vals[pix, ] - torch_vals[pix, ]))
    expect_lt(max_diff, 5.0)  # < 5 raw units ≈ 0.0005 probability
})
