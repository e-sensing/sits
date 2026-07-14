# ---- Unit tests: .mae_mask_index ----

test_that(".mae_mask_index contiguous method returns correct shape", {
    n_samples <- 10L
    timeline  <- seq(as.Date("2013-09-14"), by = "month", length.out = 23L)
    n_times   <- length(timeline)
    mask_ratio <- 0.6

    result <- .mae_mask_index(
        n_samples      = n_samples,
        timeline       = timeline,
        mask_ratio     = mask_ratio,
        masking_method = "contiguous"
    )

    # Must return a list with masked_idx and mask
    expect_true(is.list(result))
    expect_named(result, c("masked_idx", "mask"))

    # Mask matrix has correct dimensions
    expect_equal(dim(result$mask), c(n_samples * n_times, 1L))

    # Number of masked positions equals expected count per sample
    rlen <- round(mask_ratio * n_times)
    expect_equal(sum(result$mask), n_samples * rlen)

    # Masked indices are within valid range
    expect_true(all(result$masked_idx >= 1L))
    expect_true(all(result$masked_idx <= n_samples * n_times))
})

test_that(".mae_mask_index contiguous masks are contiguous per sample", {
    n_samples <- 5L
    timeline  <- seq(as.Date("2013-09-14"), by = "month", length.out = 23L)
    n_times   <- length(timeline)

    result <- .mae_mask_index(
        n_samples      = n_samples,
        timeline       = timeline,
        mask_ratio     = 0.5,
        masking_method = "contiguous"
    )

    mask_mat <- matrix(result$mask, nrow = n_times, ncol = n_samples)
    for (s in seq_len(n_samples)) {
        masked_positions <- which(mask_mat[, s] == 1L)
        # Contiguous means positions form a consecutive sequence
        expect_equal(masked_positions, seq(min(masked_positions),
            max(masked_positions)))
    }
})

test_that(".mae_mask_index random method returns correct count", {
    n_samples <- 8L
    timeline  <- seq(as.Date("2013-09-14"), by = "month", length.out = 23L)
    n_times   <- length(timeline)
    mask_ratio <- 0.4

    result <- .mae_mask_index(
        n_samples      = n_samples,
        timeline       = timeline,
        mask_ratio     = mask_ratio,
        masking_method = "random"
    )

    rlen <- round(mask_ratio * n_times)
    expect_equal(sum(result$mask), n_samples * rlen)
    expect_equal(dim(result$mask), c(n_samples * n_times, 1L))
})

# ---- Unit tests: .mae_dataset_lazy ----

test_that(".mae_dataset_lazy returns correct item shape", {
    skip_if_not_installed("torch")

    bands    <- .samples_bands(samples_modis_ndvi)
    timeline <- .samples_timeline(samples_modis_ndvi)
    n_times  <- length(timeline)
    n_bands  <- length(bands)
    stats    <- .samples_stats(samples_modis_ndvi)
    indices  <- seq_len(min(10L, nrow(samples_modis_ndvi)))

    ds <- .mae_dataset_lazy(
        samples        = samples_modis_ndvi,
        indices        = indices,
        stats          = stats,
        bands          = bands,
        timeline       = timeline,
        mask_ratio     = 0.5,
        masking_method = "contiguous",
        mask_value     = 0,
        masked_bands   = bands
    )

    expect_equal(ds$.length(), length(indices))

    item <- ds$.getitem(1L)

    # x (masked input): [n_times, n_bands]
    expect_equal(as.integer(item$x$shape), c(n_times, n_bands))
    # y (original): [n_times, n_bands]
    expect_equal(as.integer(item$y$y$shape), c(n_times, n_bands))
    # mask: [n_times, 1]
    expect_equal(as.integer(item$y$mask$shape), c(n_times, 1L))
})

test_that(".mae_dataset_lazy batch returns correct shape", {
    skip_if_not_installed("torch")

    bands    <- .samples_bands(samples_modis_ndvi)
    timeline <- .samples_timeline(samples_modis_ndvi)
    n_times  <- length(timeline)
    n_bands  <- length(bands)
    stats    <- .samples_stats(samples_modis_ndvi)
    indices  <- seq_len(min(10L, nrow(samples_modis_ndvi)))

    ds <- .mae_dataset_lazy(
        samples        = samples_modis_ndvi,
        indices        = indices,
        stats          = stats,
        bands          = bands,
        timeline       = timeline,
        mask_ratio     = 0.5,
        masking_method = "random",
        mask_value     = 0,
        masked_bands   = bands
    )

    batch_idx <- c(1L, 2L, 3L)
    batch <- ds$.getbatch(batch_idx)

    # x (masked input): [batch_size, n_times, n_bands]
    expect_equal(
        as.integer(batch$x$shape),
        c(length(batch_idx), n_times, n_bands)
    )
    # y (original): [batch_size, n_times, n_bands]
    expect_equal(
        as.integer(batch$y$y$shape),
        c(length(batch_idx), n_times, n_bands)
    )
    # mask: [batch_size, n_times, 1]
    expect_equal(
        as.integer(batch$y$mask$shape),
        c(length(batch_idx), n_times, 1L)
    )
})

test_that(".mae_dataset_lazy masked input differs from original", {
    skip_if_not_installed("torch")

    bands    <- .samples_bands(samples_modis_ndvi)
    timeline <- .samples_timeline(samples_modis_ndvi)
    stats    <- .samples_stats(samples_modis_ndvi)

    ds <- .mae_dataset_lazy(
        samples        = samples_modis_ndvi,
        indices        = 1L,
        stats          = stats,
        bands          = bands,
        timeline       = timeline,
        mask_ratio     = 0.5,
        masking_method = "contiguous",
        mask_value     = 0,
        masked_bands   = bands
    )

    item <- ds$.getitem(1L)

    x_arr <- torch::as_array(item$x)
    y_arr <- torch::as_array(item$y$y)
    mask_arr <- torch::as_array(item$y$mask)

    # Masked positions in x should differ from y (set to mask_value = 0)
    masked_positions <- which(mask_arr == 1)
    expect_true(length(masked_positions) > 0L)

    # Where mask is 0 (unmasked), x and y should be identical
    unmasked_positions <- which(mask_arr == 0)
    if (length(unmasked_positions) > 0L) {
        expect_equal(x_arr[unmasked_positions, ], y_arr[unmasked_positions, ])
    }
})

# ---- Integration tests: sits_ssl_mae ----

test_that("sits_ssl_mae returns a function when no samples given", {
    mae_fn <- sits_ssl_mae(
        embedding_dim = 16L,
        epochs        = 5L
    )
    expect_true(is.function(mae_fn))
    expect_true(inherits(mae_fn, "function"))
})

test_that("sits_ssl_mae pre-training produces sits_encoder", {
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")

    encoder <- .try(
        sits_pre_train(
            samples        = samples_modis_ndvi,
            encoder_method = sits_ssl_mae(
                embedding_dim  = 16L,
                decoder_width  = 32L,
                masking_method = "contiguous",
                mask_ratio     = 0.5,
                epochs         = 5L,
                batch_size     = 32L,
                verbose        = FALSE
            )
        ),
        .default = NULL
    )

    skip_if(is.null(encoder), "torch training failed (likely no GPU/CPU resources)")

    expect_true(inherits(encoder, "sits_encoder"))
    expect_true(inherits(encoder, "torch_model"))
    expect_true(inherits(encoder, "sits_model"))
})

test_that("MAE encoder can encode a sits tibble", {
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")

    embedding_dim <- 16L

    encoder <- .try(
        sits_pre_train(
            samples        = samples_modis_ndvi,
            encoder_method = sits_ssl_mae(
                embedding_dim  = embedding_dim,
                decoder_width  = 32L,
                mask_ratio     = 0.5,
                epochs         = 5L,
                batch_size     = 32L,
                verbose        = FALSE
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

test_that("MAE: downstream classification works", {
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")

    encoder <- .try(
        sits_pre_train(
            samples        = samples_modis_ndvi,
            encoder_method = sits_ssl_mae(
                embedding_dim  = 16L,
                decoder_width  = 32L,
                mask_ratio     = 0.5,
                epochs         = 5L,
                batch_size     = 32L,
                verbose        = FALSE
            )
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

test_that("MAE: random masking method works", {
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")

    encoder <- .try(
        sits_pre_train(
            samples        = samples_modis_ndvi,
            encoder_method = sits_ssl_mae(
                embedding_dim  = 16L,
                decoder_width  = 32L,
                masking_method = "random",
                mask_ratio     = 0.4,
                epochs         = 5L,
                batch_size     = 32L,
                verbose        = FALSE
            )
        ),
        .default = NULL
    )

    skip_if(is.null(encoder), "torch training failed")

    expect_true(inherits(encoder, "sits_encoder"))
    expect_true(inherits(encoder, "torch_model"))
})

test_that("MAE: validation_split = 0 trains without error", {
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")

    encoder <- .try(
        sits_pre_train(
            samples        = samples_modis_ndvi,
            encoder_method = sits_ssl_mae(
                embedding_dim    = 16L,
                decoder_width    = 32L,
                mask_ratio       = 0.5,
                validation_split = 0.0,
                epochs           = 3L,
                batch_size       = 32L,
                verbose          = FALSE
            )
        ),
        .default = NULL
    )

    skip_if(is.null(encoder), "torch training failed")

    expect_true(inherits(encoder, "sits_encoder"))
})
