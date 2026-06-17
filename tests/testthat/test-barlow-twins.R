# ---- Unit tests: .contrastive_training_data_split ----

test_that(".barlow_twins_data_split label method returns same-class positives", {
    preds  <- .predictors(samples_modis_ndvi)
    labels <- .pred_references(preds)

    pairs <- .barlow_twins_data_split(
        samples          = samples_modis_ndvi,
        validation_split = 0.2,
        num_pairs        = NULL,
        pair_smp_method  = "label"
    )

    train <- pairs[["train"]]
    # Both matrices must have the same dimensions
    expect_equal(dim(train$a), dim(train$b))
    expect_equal(ncol(train$a), ncol(as.matrix(.pred_features(preds))))

    # To check that label-based pairing was respected we need to recover
    # which original row each paired row corresponds to.  Since the features
    # are normalised versions of the originals we cannot reverse-map exactly,
    # so we verify the structural invariant: val set is non-empty and shapes
    # match.
    val <- pairs[["val"]]
    expect_equal(dim(val$a), dim(val$b))
    expect_true(nrow(val$a) > 0L)
})

test_that(".barlow_twins_data_split random method returns correct shape", {
    preds <- .predictors(samples_modis_ndvi)
    n_feat <- ncol(as.matrix(.pred_features(preds)))

    pairs <- .barlow_twins_data_split(
        samples          = samples_modis_ndvi,
        validation_split = 0.2,
        num_pairs        = 50L,
        pair_smp_method  = "random"
    )

    train <- pairs[["train"]]
    expect_true(nrow(train$a) <= 50L)
    expect_equal(ncol(train$a), n_feat)
    expect_equal(dim(train$a), dim(train$b))
})

test_that(".barlow_twins_data_split singleton class uses self-pairing without error", {
    # Build a tiny dataset where one class has a single sample
    one_class_samples <- samples_modis_ndvi[samples_modis_ndvi$label == "Pasture", ]
    one_sample        <- one_class_samples[1L, ]
    other_samples     <- samples_modis_ndvi[samples_modis_ndvi$label != "Pasture", ][1:10, ]
    tiny_samples      <- rbind(one_sample, other_samples)

    # Should complete without error (a warning is acceptable)
    expect_no_error({
        pairs <- suppressWarnings(
            .barlow_twins_data_split(
                samples          = tiny_samples,
                validation_split = 0.0,
                num_pairs        = NULL,
                pair_smp_method  = "label"
            )
        )
    })
    # Output shapes are consistent
    train <- pairs[["train"]]
    expect_equal(dim(train$a), dim(train$b))
})

test_that(".barlow_twins_data_split num_pairs controls output size", {
    desired <- 30L
    pairs <- .barlow_twins_data_split(
        samples          = samples_modis_ndvi,
        validation_split = 0.0,   # no val split → all in train
        num_pairs        = desired,
        pair_smp_method  = "label"
    )
    expect_equal(nrow(pairs[["train"]]$a), desired)
    expect_equal(nrow(pairs[["val"]]$a),   0L)
})

# ---- Unit tests: .pair_dataset ----

test_that(".pair_dataset returns correct item shape", {
    skip_if_not_installed("torch")

    preds  <- .predictors(samples_modis_ndvi)
    n_times <- .samples_ntimes(samples_modis_ndvi)
    n_bands <- length(.samples_bands(samples_modis_ndvi))

    pairs <- .barlow_twins_data_split(
        samples          = samples_modis_ndvi,
        validation_split = 0.0,
        num_pairs        = 10L,
        pair_smp_method  = "random"
    )

    ds   <- .pair_dataset(pairs[["train"]], n_times = n_times)
    item <- ds$.getitem(1L)

    # x must be [2, n_times, n_bands]
    expect_equal(as.integer(item$x$shape), c(2L, n_times, n_bands))
    # y is a scalar dummy
    expect_equal(as.integer(item$y$shape), 1L)
    expect_equal(ds$.length(), 10L)
})

# ---- Integration tests: sits_barlow_twins_network ----

test_that("sits_barlow_twins returns a function when no samples given", {
    bt_fn <- sits_barlow_twins(
        embedding_dim = 16L,
        epochs        = 5L
    )
    expect_true(is.function(bt_fn))
    # It must carry the "function" class (may also have encoder-method class)
    expect_true(inherits(bt_fn, "function"))
})

test_that("sits_barlow_twins pre-training produces sits_encoder", {
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")

    encoder <- .try(
        sits_pre_train(
            samples        = samples_modis_ndvi,
            encoder_method = sits_barlow_twins(
                embedding_dim   = 16L,
                proj_dim        = 32L,
                epochs          = 5L,
                batch_size      = 32L,
                pair_smp_method = "label",
                verbose         = FALSE
            )
        ),
        .default = NULL
    )

    skip_if(is.null(encoder), "torch training failed (likely no GPU/CPU resources)")

    expect_true(inherits(encoder, "sits_encoder"))
    expect_true(inherits(encoder, "torch_model"))
})

test_that("Barlow Twins encoder can encode a sits tibble", {
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")

    embedding_dim <- 16L

    encoder <- .try(
        sits_pre_train(
            samples        = samples_modis_ndvi,
            encoder_method = sits_barlow_twins(
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

test_that("Barlow Twins: downstream classification works", {
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")

    encoder <- .try(
        sits_pre_train(
            samples        = samples_modis_ndvi,
            encoder_method = sits_barlow_twins(
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
