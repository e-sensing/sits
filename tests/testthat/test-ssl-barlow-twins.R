# ---- Unit tests: .ssl_bt_data_split ----

test_that(".ssl_bt_data_split returns correct structure", {
    result <- .ssl_bt_data_split(
        samples          = samples_modis_ndvi,
        validation_split = 0.2
    )

    n_times <- .samples_ntimes(samples_modis_ndvi)
    n_bands <- length(.samples_bands(samples_modis_ndvi))
    n_feats <- n_times * n_bands

    train <- result[["train"]]
    val   <- result[["val"]]

    # Both splits should have feature matrices with correct columns
    expect_equal(ncol(train[["feats"]]), n_feats)
    expect_equal(ncol(val[["feats"]]),   n_feats)
    # Total rows should equal number of samples
    expect_equal(
        nrow(train[["feats"]]) + nrow(val[["feats"]]),
        nrow(samples_modis_ndvi)
    )
    # Validation set should be non-empty
    expect_true(nrow(val[["feats"]]) > 0L)
})

test_that(".ssl_bt_data_split with zero validation", {
    result <- .ssl_bt_data_split(
        samples          = samples_modis_ndvi,
        validation_split = 0.0
    )

    expect_equal(nrow(result[["train"]][["feats"]]), nrow(samples_modis_ndvi))
    expect_equal(nrow(result[["val"]][["feats"]]),   0L)
})

# ---- Unit tests: .ssl_bt_augment_dataset ----

test_that(".ssl_bt_augment_dataset returns correct item shape", {
    skip_if_not_installed("torch")

    n_times <- .samples_ntimes(samples_modis_ndvi)
    n_bands <- length(.samples_bands(samples_modis_ndvi))

    result <- .ssl_bt_data_split(
        samples          = samples_modis_ndvi,
        validation_split = 0.0
    )

    ds <- .ssl_bt_augment_dataset(
        result[["train"]],
        n_times          = n_times,
        augment_mean     = 1.0,
        augment_variance = 0.1
    )
    item <- ds$.getitem(1L)

    # x must be [2, n_times, n_bands] (two augmented views)
    expect_equal(as.integer(item$x$shape), c(2L, n_times, n_bands))
    # y is a dummy zero tensor
    expect_equal(as.integer(item$y$shape), 1L)
    expect_equal(ds$.length(), nrow(samples_modis_ndvi))
})

test_that(".ssl_bt_augment_dataset produces different views", {
    skip_if_not_installed("torch")

    n_times <- .samples_ntimes(samples_modis_ndvi)

    result <- .ssl_bt_data_split(
        samples          = samples_modis_ndvi,
        validation_split = 0.0
    )

    ds <- .ssl_bt_augment_dataset(
        result[["train"]],
        n_times          = n_times,
        augment_mean     = 1.0,
        augment_variance = 0.1
    )
    item <- ds$.getitem(1L)

    view_a <- torch::as_array(item$x[1, , ])
    view_b <- torch::as_array(item$x[2, , ])

    # Two views of the same sample should differ
    expect_false(all(view_a == view_b))
})

# ---- Integration tests: sits_ssl_barlow_twins ----

test_that("sits_ssl_barlow_twins returns a function when no samples given", {
    ssl_bt_fn <- sits_ssl_barlow_twins(
        embedding_dim = 16L,
        epochs        = 5L
    )
    expect_true(is.function(ssl_bt_fn))
})

test_that("sits_ssl_barlow_twins pre-training produces sits_encoder", {
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")

    encoder <- .try(
        sits_pre_train(
            samples        = samples_modis_ndvi,
            encoder_method = sits_ssl_barlow_twins(
                embedding_dim = 16L,
                proj_dim      = 32L,
                epochs        = 5L,
                batch_size    = 32L,
                verbose       = FALSE
            )
        ),
        .default = NULL
    )

    skip_if(is.null(encoder),
            "torch training failed (likely no GPU/CPU resources)")

    expect_true(inherits(encoder, "sits_encoder"))
    expect_true(inherits(encoder, "torch_model"))
})

test_that("SSL Barlow Twins encoder can encode a sits tibble", {
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")

    embedding_dim <- 16L

    encoder <- .try(
        sits_pre_train(
            samples        = samples_modis_ndvi,
            encoder_method = sits_ssl_barlow_twins(
                embedding_dim = embedding_dim,
                proj_dim      = 32L,
                epochs        = 5L,
                batch_size    = 32L,
                verbose       = FALSE
            )
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
    # Band names must follow the "EMB" prefix convention
    expect_true(all(grepl("^EMB", sits_bands(enc_samples))))
})
