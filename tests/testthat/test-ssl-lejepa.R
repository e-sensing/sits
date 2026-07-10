# ---- Unit tests: .lejepa_apply_resampling ----

test_that(".lejepa_apply_resampling preserves shape", {
    ts_mat <- matrix(sin(seq(0, 4 * pi, length.out = 23)),
        nrow = 23, ncol = 3
    )
    views <- .lejepa_apply_resampling(ts_mat)

    expect_true(is.list(views))
    expect_named(views, c("view1", "view2"))
    expect_equal(dim(views$view1), dim(ts_mat))
    expect_equal(dim(views$view2), dim(ts_mat))
})

test_that(".lejepa_apply_resampling produces different views", {
    set.seed(2847)
    ts_mat <- matrix(rnorm(23 * 4), nrow = 23, ncol = 4)
    views <- .lejepa_apply_resampling(ts_mat)
    expect_false(identical(views$view1, views$view2))
})

# ---- Unit tests: .lejepa_sigreg ----

test_that(".lejepa_sigreg returns a scalar", {
    skip_if_not_installed("torch")

    sigreg <- .lejepa_sigreg(num_knots = 17L, num_slices = 64L)
    # proj: [V=2, B=16, D=8]
    proj <- torch::torch_randn(c(2L, 16L, 8L))
    loss <- sigreg(proj)

    expect_equal(as.integer(loss$shape), integer(0))
    expect_true(is.finite(torch::as_array(loss)))
})

test_that(".lejepa_sigreg is lower for Gaussian data", {
    skip_if_not_installed("torch")

    sigreg <- .lejepa_sigreg(num_knots = 17L, num_slices = 128L)

    set.seed(5123)
    # Gaussian input (should have low SIGReg)
    torch::torch_manual_seed(5123L)
    gaussian_proj <- torch::torch_randn(c(2L, 256L, 16L))
    loss_gaussian <- torch::as_array(sigreg(gaussian_proj))

    # Uniform input (should have higher SIGReg)
    torch::torch_manual_seed(5124L)
    uniform_proj <- torch::torch_rand(c(2L, 256L, 16L)) * 6 - 3
    loss_uniform <- torch::as_array(sigreg(uniform_proj))

    expect_true(loss_gaussian < loss_uniform)
})

# ---- Unit tests: .lejepa_resampling_dataset ----

test_that(".lejepa_resampling_dataset returns correct item shape", {
    skip_if_not_installed("torch")

    bands    <- .samples_bands(samples_modis_ndvi)
    n_times  <- .samples_ntimes(samples_modis_ndvi)
    n_bands  <- length(bands)
    ml_stats <- .samples_stats(samples_modis_ndvi)
    preds    <- .pred_normalize(.predictors(samples_modis_ndvi),
        stats = ml_stats
    )
    feats <- as.matrix(.pred_features(preds))

    ds <- .lejepa_resampling_dataset(
        split   = list(feats = feats[1:10, , drop = FALSE]),
        n_times = n_times
    )

    expect_equal(ds$.length(), 10L)
    item <- ds$.getitem(1L)
    expect_equal(as.integer(item$x$shape), c(2L, n_times, n_bands))
    expect_equal(as.integer(item$y$shape), 1L)
})

test_that(".lejepa_resampling_dataset two views differ", {
    skip_if_not_installed("torch")

    n_times  <- .samples_ntimes(samples_modis_ndvi)
    ml_stats <- .samples_stats(samples_modis_ndvi)
    preds    <- .pred_normalize(.predictors(samples_modis_ndvi),
        stats = ml_stats
    )
    feats <- as.matrix(.pred_features(preds))

    ds <- .lejepa_resampling_dataset(
        split   = list(feats = feats[1:5, , drop = FALSE]),
        n_times = n_times
    )

    item <- ds$.getitem(1L)
    view_a <- torch::as_array(item$x[1, , ])
    view_b <- torch::as_array(item$x[2, , ])
    expect_false(identical(view_a, view_b))
})

# ---- Unit tests: .lejepa_data_split ----

test_that(".lejepa_data_split returns correct structure", {
    ml_stats <- .samples_stats(samples_modis_ndvi)
    splits <- .lejepa_data_split(
        samples          = samples_modis_ndvi,
        ml_stats         = ml_stats,
        validation_split = 0.2
    )

    expect_true(is.list(splits))
    expect_named(splits, c("train", "val"))
    n_total <- nrow(samples_modis_ndvi)
    n_train <- nrow(splits$train$feats)
    n_val   <- nrow(splits$val$feats)
    expect_equal(n_train + n_val, n_total)
})

test_that(".lejepa_data_split handles validation_split = 0", {
    ml_stats <- .samples_stats(samples_modis_ndvi)
    splits <- .lejepa_data_split(
        samples          = samples_modis_ndvi,
        ml_stats         = ml_stats,
        validation_split = 0.0
    )
    expect_equal(nrow(splits$val$feats), 0L)
    expect_equal(nrow(splits$train$feats), nrow(samples_modis_ndvi))
})

# ---- Integration tests: sits_ssl_lejepa ----

test_that("sits_ssl_lejepa returns a function when no samples given", {
    lejepa_fn <- sits_ssl_lejepa(
        embedding_dim = 16L,
        epochs        = 5L
    )
    expect_true(is.function(lejepa_fn))
})

test_that("LeJEPA pre-training produces sits_encoder", {
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")

    encoder <- .try(
        sits_pre_train(
            samples        = samples_modis_ndvi,
            encoder_method = sits_ssl_lejepa(
                embedding_dim = 16L,
                proj_dim      = 32L,
                epochs        = 5L,
                batch_size    = 32L,
                verbose       = FALSE
            )
        ),
        .default = NULL
    )

    skip_if(is.null(encoder), "torch training failed")

    expect_true(inherits(encoder, "sits_encoder"))
    expect_true(inherits(encoder, "torch_model"))
    expect_true(inherits(encoder, "sits_model"))
})

test_that("LeJEPA encoder can encode a sits tibble", {
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")

    embedding_dim <- 16L

    encoder <- .try(
        sits_ssl_lejepa(
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

test_that("LeJEPA: downstream classification works", {
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")

    encoder <- .try(
        sits_ssl_lejepa(
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

test_that("LeJEPA: custom lambda works", {
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")

    encoder <- .try(
        sits_ssl_lejepa(
            samples       = samples_modis_ndvi,
            embedding_dim = 16L,
            proj_dim      = 32L,
            lambda        = 0.1,
            epochs        = 3L,
            batch_size    = 32L,
            verbose       = FALSE
        ),
        .default = NULL
    )

    skip_if(is.null(encoder), "torch training failed")
    expect_true(inherits(encoder, "sits_encoder"))
})

test_that("LeJEPA: validation_split = 0 trains without error", {
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")

    encoder <- .try(
        sits_ssl_lejepa(
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
