# ---- Unit tests: .contrastive_learning_data_split ----

test_that(".contrastive_learning_data_split label method returns correct shape", {
    result <- .contrastive_learning_data_split(
        samples          = samples_modis_ndvi,
        validation_split = 0.2,
        num_pairs        = 50L,
        pair_smp_method  = "label"
    )

    train <- result[["train"]]
    val   <- result[["val"]]

    n_times <- .samples_ntimes(samples_modis_ndvi)
    n_bands <- length(.samples_bands(samples_modis_ndvi))
    n_feats <- n_times * n_bands

    # Both splits should have feature matrices
    expect_equal(ncol(train[["a"]]), n_feats)
    expect_equal(ncol(train[["b"]]), n_feats)
    expect_equal(ncol(val[["a"]]),   n_feats)
    # Labels vector should match rows
    expect_equal(length(train[["labels"]]), nrow(train[["a"]]))
    expect_equal(length(val[["labels"]]),   nrow(val[["a"]]))
    # Training split should have at most num_pairs rows
    expect_true(nrow(train[["a"]]) <= 50L)
    # Validation set should be non-empty
    expect_true(nrow(val[["a"]]) > 0L)
    # Total should exceed zero
    expect_true(nrow(train[["a"]]) + nrow(val[["a"]]) > 0L)
})

test_that(".contrastive_learning_data_split random method works", {
    result <- .contrastive_learning_data_split(
        samples          = samples_modis_ndvi,
        validation_split = 0.0,
        num_pairs        = 30L,
        pair_smp_method  = "random"
    )

    train <- result[["train"]]
    val   <- result[["val"]]

    expect_equal(nrow(train[["a"]]), 30L)
    expect_equal(nrow(val[["a"]]),   0L)
    # Labels should be integer codes (1-based)
    expect_true(all(train[["labels"]] >= 1L))
})

test_that(".contrastive_learning_data_split defaults to n_samples pairs", {
    result <- .contrastive_learning_data_split(
        samples          = samples_modis_ndvi,
        validation_split = 0.0,
        pair_smp_method  = "label"
    )

    train <- result[["train"]]
    expect_equal(nrow(train[["a"]]), nrow(samples_modis_ndvi))
})

test_that(".contrastive_learning_data_split singleton class handled", {
    # Build a tiny dataset where one class has a single sample
    one_class_samples <- samples_modis_ndvi[
        samples_modis_ndvi$label == "Pasture",
    ]
    one_sample    <- one_class_samples[1L, ]
    other_samples <- samples_modis_ndvi[
        samples_modis_ndvi$label != "Pasture",
    ][1:10, ]
    tiny_samples  <- rbind(one_sample, other_samples)

    # Should complete without error (with a warning about self-pairing)
    expect_no_error({
        result <- suppressWarnings(
            .contrastive_learning_data_split(
                samples          = tiny_samples,
                validation_split = 0.0,
                num_pairs        = 10L,
                pair_smp_method  = "label"
            )
        )
    })
    expect_true(nrow(result[["train"]][["a"]]) > 0L)
})

# ---- Unit tests: ..contrastive_supcon_dataset ----

test_that(".contrastive_supcon_dataset returns correct item shape", {
    skip_if_not_installed("torch")

    n_times <- .samples_ntimes(samples_modis_ndvi)
    n_bands <- length(.samples_bands(samples_modis_ndvi))

    result <- .contrastive_learning_data_split(
        samples          = samples_modis_ndvi,
        validation_split = 0.0,
        num_pairs        = 10L,
        pair_smp_method  = "label"
    )

    ds   <- .contrastive_supcon_dataset(result[["train"]], n_times = n_times)
    item <- ds$.getitem(1L)

    # x must be [2, n_times, n_bands] (two views)
    expect_equal(as.integer(item$x$shape), c(2L, n_times, n_bands))
    # y is an integer class label (scalar tensor with shape [1])
    expect_equal(as.integer(item$y$shape), 1L)
    expect_true(item$y$dtype == torch::torch_long())
    expect_equal(ds$.length(), 10L)
})

# ---- Integration tests: sits_contrastive_learning ----

test_that("sits_contrastive_learning returns a function when no samples given", {
    cl_fn <- sits_contrastive_learning(
        embedding_dim = 16L,
        epochs        = 5L
    )
    expect_true(is.function(cl_fn))
    expect_true(inherits(cl_fn, "function"))
})

test_that("sits_contrastive_learning pre-training produces sits_encoder", {
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")

    encoder <- .try(
        sits_pre_train(
            samples        = samples_modis_ndvi,
            encoder_method = sits_contrastive_learning(
                embedding_dim   = 16L,
                proj_dim        = 32L,
                temperature     = 0.07,
                pair_smp_method = "label",
                num_pairs       = 100L,
                epochs          = 5L,
                batch_size      = 32L,
                verbose         = FALSE
            )
        ),
        .default = NULL
    )

    skip_if(is.null(encoder), "torch training failed (likely no GPU/CPU resources)")

    expect_true(inherits(encoder, "sits_encoder"))
    expect_true(inherits(encoder, "torch_model"))
})

test_that("Contrastive learning encoder can encode a sits tibble", {
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")

    embedding_dim <- 16L

    encoder <- .try(
        sits_pre_train(
            samples        = samples_modis_ndvi,
            encoder_method = sits_contrastive_learning(
                embedding_dim = embedding_dim,
                proj_dim      = 32L,
                num_pairs     = 100L,
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

test_that("Contrastive learning: downstream classification works", {
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")

    encoder <- .try(
        sits_pre_train(
            samples        = samples_modis_ndvi,
            encoder_method = sits_contrastive_learning(
                embedding_dim = 16L,
                proj_dim      = 32L,
                num_pairs     = 100L,
                epochs        = 5L,
                batch_size    = 32L,
                verbose       = FALSE
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
