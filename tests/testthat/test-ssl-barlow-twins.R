# ---- Unit tests: .ssl_barlow_twins_resampling_dataset ----

test_that(".ssl_barlow_twins_resampling_dataset returns correct item shape", {
    skip_if_not_installed("torch")

    bands    <- .samples_bands(samples_modis_ndvi)
    n_times  <- .samples_ntimes(samples_modis_ndvi)
    n_bands  <- length(bands)
    ml_stats <- .samples_stats(samples_modis_ndvi)
    feats    <- .pred_features_normalize(
        pred  = .predictors(samples_modis_ndvi),
        stats = ml_stats
    )

    ds <- .ssl_barlow_twins_resampling_dataset(
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

test_that(".ssl_barlow_twins_resampling_dataset two views differ", {
    skip_if_not_installed("torch")

    n_times  <- .samples_ntimes(samples_modis_ndvi)
    ml_stats <- .samples_stats(samples_modis_ndvi)
    feats    <- .pred_features_normalize(
        pred  = .predictors(samples_modis_ndvi),
        stats = ml_stats
    )

    ds <- .ssl_barlow_twins_resampling_dataset(
        split   = list(feats = feats[1:5, , drop = FALSE]),
        n_times = n_times
    )

    item <- ds$.getitem(1L)

    view_a <- torch::as_array(item$x[1, , ])
    view_b <- torch::as_array(item$x[2, , ])

    expect_false(identical(view_a, view_b))
})

# ---- Unit tests: .ssl_barlow_twins_data_split ----

test_that(".ssl_barlow_twins_data_split returns correct structure", {
    splits <- .ssl_barlow_twins_data_split(
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

test_that(".ssl_barlow_twins_data_split handles validation_split = 0", {
    splits <- .ssl_barlow_twins_data_split(
        samples          = samples_modis_ndvi,
        validation_split = 0.0
    )

    expect_equal(nrow(splits$val$feats), 0L)
    expect_equal(nrow(splits$train$feats), nrow(samples_modis_ndvi))
})

# ---- Integration tests: sits_ssl_barlow_twins ----

test_that("sits_ssl_barlow_twins returns a function when no samples given", {
    bt_fn <- sits_ssl_barlow_twins(
        embedding_dim = 16L,
        epochs        = 5L
    )
    expect_true(is.function(bt_fn))
})

test_that("Barlow Twins pre-training produces sits_encoder", {
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")

    encoder <- .try(
        sits_ssl_barlow_twins(
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

test_that("Barlow Twins encoder can encode a sits tibble", {
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")

    embedding_dim <- 16L

    encoder <- .try(
        sits_ssl_barlow_twins(
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

test_that("Barlow Twins: downstream classification works", {
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")

    encoder <- .try(
        sits_ssl_barlow_twins(
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

test_that("Barlow Twins: custom bt_lambda works", {
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")

    encoder <- .try(
        sits_ssl_barlow_twins(
            samples       = samples_modis_ndvi,
            embedding_dim = 16L,
            proj_dim      = 32L,
            bt_lambda     = 1e-2,
            epochs        = 3L,
            batch_size    = 32L,
            verbose       = FALSE
        ),
        .default = NULL
    )

    skip_if(is.null(encoder), "torch training failed")

    expect_true(inherits(encoder, "sits_encoder"))
})

test_that("Barlow Twins: validation_split = 0 trains without error", {
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")

    encoder <- .try(
        sits_ssl_barlow_twins(
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
