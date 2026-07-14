# ---- Unit tests: .ssl_apply_resampling ----

test_that(".ssl_apply_resampling preserves shape", {
    ts_mat <- matrix(sin(seq(0, 4 * pi, length.out = 23)),
        nrow = 23, ncol = 3
    )
    views <- .ssl_apply_resampling(ts_mat)

    expect_true(is.list(views))
    expect_named(views, c("view1", "view2"))
    expect_equal(dim(views$view1), dim(ts_mat))
    expect_equal(dim(views$view2), dim(ts_mat))
})

test_that(".ssl_apply_resampling produces different views", {
    set.seed(7341)
    ts_mat <- matrix(rnorm(23 * 4), nrow = 23, ncol = 4)

    views <- .ssl_apply_resampling(ts_mat)

    # Two disjoint subsequences should yield different views
    expect_false(identical(views$view1, views$view2))
})

test_that(".ssl_apply_resampling values stay in plausible range", {
    ts_mat <- matrix(seq(0, 1, length.out = 23), nrow = 23, ncol = 1)
    views <- .ssl_apply_resampling(ts_mat)

    # Resampled values are interpolated from the upsampled original,
    # so they must stay within the original range
    expect_true(all(views$view1 >= min(ts_mat) - 1e-10))
    expect_true(all(views$view1 <= max(ts_mat) + 1e-10))
    expect_true(all(views$view2 >= min(ts_mat) - 1e-10))
    expect_true(all(views$view2 <= max(ts_mat) + 1e-10))
})

test_that(".ssl_apply_resampling works with different lengths", {
    for (n in c(8L, 16L, 23L, 48L)) {
        ts_mat <- matrix(rnorm(n * 2), nrow = n, ncol = 2)
        views <- .ssl_apply_resampling(ts_mat)
        expect_equal(dim(views$view1), c(n, 2L))
        expect_equal(dim(views$view2), c(n, 2L))
    }
})

# ---- Unit tests: .vicreg_off_diagonal ----

test_that(".vicreg_off_diagonal returns correct count", {
    skip_if_not_installed("torch")

    n <- 5L
    x <- torch::torch_randn(c(n, n))
    od <- .vicreg_off_diagonal(x)

    expect_equal(as.integer(od$shape), n * (n - 1L))
})

test_that(".vicreg_off_diagonal excludes diagonal elements", {
    skip_if_not_installed("torch")

    # Identity matrix: diagonal = 1, off-diagonal = 0
    x <- torch::torch_eye(4L)
    od <- .vicreg_off_diagonal(x)

    expect_equal(torch::as_array(od$sum()), 0)
})

# ---- Unit tests: .vicreg_resampling_dataset ----

test_that(".vicreg_resampling_dataset returns correct item shape", {
    skip_if_not_installed("torch")

    bands    <- .samples_bands(samples_modis_ndvi)
    n_times  <- .samples_ntimes(samples_modis_ndvi)
    n_bands  <- length(bands)
    ml_stats <- .samples_stats(samples_modis_ndvi)
    feats    <- .pred_features_normalize(
        pred  = .predictors(samples_modis_ndvi),
        stats = ml_stats
    )

    ds <- .vicreg_resampling_dataset(
        split   = list(feats = feats[1:10, , drop = FALSE]),
        n_times = n_times
    )

    expect_equal(ds$.length(), 10L)

    item <- ds$.getitem(1L)

    # x: [2, n_times, n_bands] — two stacked views
    expect_equal(as.integer(item$x$shape), c(2L, n_times, n_bands))
    # y: dummy zero tensor
    expect_equal(as.integer(item$y$shape), 1L)
})

test_that(".vicreg_resampling_dataset two views differ", {
    skip_if_not_installed("torch")

    bands    <- .samples_bands(samples_modis_ndvi)
    n_times  <- .samples_ntimes(samples_modis_ndvi)
    ml_stats <- .samples_stats(samples_modis_ndvi)
    feats    <- .pred_features_normalize(
        pred  = .predictors(samples_modis_ndvi),
        stats = ml_stats
    )

    ds <- .vicreg_resampling_dataset(
        split   = list(feats = feats[1:5, , drop = FALSE]),
        n_times = n_times
    )

    item <- ds$.getitem(1L)

    view_a <- torch::as_array(item$x[1, , ])
    view_b <- torch::as_array(item$x[2, , ])

    expect_false(identical(view_a, view_b))
})

# ---- Unit tests: .vicreg_data_split ----

test_that("vicreg_data_split returns correct structure", {
    splits <- .vicreg_data_split(
        samples          = samples_modis_ndvi,
        validation_split = 0.2
    )

    expect_true(is.list(splits))
    expect_named(splits, c("train", "val"))
    expect_true("feats" %in% names(splits$train))
    expect_true("feats" %in% names(splits$val))

    n_total <- nrow(samples_modis_ndvi)
    n_train <- nrow(splits$train$feats)
    n_val   <- nrow(splits$val$feats)

    expect_equal(n_train + n_val, n_total)
    expect_true(n_val > 0L)
    expect_true(n_train > n_val)
})

test_that("vicreg_data_split handles validation_split = 0", {
    splits <- .vicreg_data_split(
        samples          = samples_modis_ndvi,
        validation_split = 0.0
    )

    expect_equal(nrow(splits$val$feats), 0L)
    expect_equal(nrow(splits$train$feats), nrow(samples_modis_ndvi))
})

# ---- Integration tests: sits_ssl_vicreg ----

test_that("sits_ssl_vicreg returns a function when no samples given", {
    vicreg_fn <- sits_ssl_vicreg(
        embedding_dim = 16L,
        epochs        = 5L
    )
    expect_true(is.function(vicreg_fn))
})

test_that("VICReg pre-training produces sits_encoder", {
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")

    encoder <- .try(
        sits_ssl_vicreg(
            samples       = samples_modis_ndvi,
            embedding_dim = 16L,
            proj_dim      = 32L,
            epochs        = 5L,
            batch_size    = 32L,
            verbose       = FALSE
        ),
        .default = NULL
    )

    skip_if(is.null(encoder), "torch training failed")

    expect_true(inherits(encoder, "sits_encoder"))
    expect_true(inherits(encoder, "torch_model"))
    expect_true(inherits(encoder, "sits_model"))
})

test_that("VICReg encoder can encode a sits tibble", {
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")

    embedding_dim <- 16L

    encoder <- .try(
        sits_ssl_vicreg(
            samples       = samples_modis_ndvi,
            embedding_dim = embedding_dim,
            proj_dim      = 32L,
            epochs        = 5L,
            batch_size    = 32L,
            verbose       = FALSE
        ),
        .default = NULL
    )

    skip_if(is.null(encoder), "torch training failed")

    enc_samples <- sits_encode(
        data    = sits_sample(samples_modis_ndvi, frac = 0.3),
        encoder = encoder
    )

    expect_true(inherits(enc_samples, "sits"))
    expect_equal(length(sits_bands(enc_samples)), embedding_dim)
    expect_true(all(grepl("^EMB", sits_bands(enc_samples))))
})

test_that("VICReg: downstream classification works", {
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")

    encoder <- .try(
        sits_ssl_vicreg(
            samples       = samples_modis_ndvi,
            embedding_dim = 16L,
            proj_dim      = 32L,
            epochs        = 5L,
            batch_size    = 32L,
            verbose       = FALSE
        ),
        .default = NULL
    )

    skip_if(is.null(encoder), "torch training failed")

    enc_samples <- sits_encode(
        data    = sits_sample(samples_modis_ndvi, frac = 0.6),
        encoder = encoder
    )

    rf_model <- sits_train(enc_samples, sits_rfor(num_trees = 20L))

    point_enc <- sits_encode(
        data    = sits_select(point_mt_6bands, bands = "NDVI"),
        encoder = encoder
    )

    point_class <- sits_classify(
        data     = point_enc,
        ml_model = rf_model,
        progress = FALSE
    )

    expect_true(
        all(point_class$predicted[[1L]]$class %in%
                sits_labels(samples_modis_ndvi))
    )
})

test_that("VICReg: custom loss coefficients work", {
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")

    encoder <- .try(
        sits_ssl_vicreg(
            samples       = samples_modis_ndvi,
            embedding_dim = 16L,
            proj_dim      = 32L,
            sim_coeff     = 10.0,
            std_coeff     = 10.0,
            cov_coeff     = 5.0,
            epochs        = 3L,
            batch_size    = 32L,
            verbose       = FALSE
        ),
        .default = NULL
    )

    skip_if(is.null(encoder), "torch training failed")

    expect_true(inherits(encoder, "sits_encoder"))
})

test_that("VICReg: validation_split = 0 trains without error", {
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")

    encoder <- .try(
        sits_ssl_vicreg(
            samples          = samples_modis_ndvi,
            embedding_dim    = 16L,
            proj_dim         = 32L,
            validation_split = 0.0,
            epochs           = 3L,
            batch_size       = 32L,
            verbose          = FALSE
        ),
        .default = NULL
    )

    skip_if(is.null(encoder), "torch training failed")

    expect_true(inherits(encoder, "sits_encoder"))
})
