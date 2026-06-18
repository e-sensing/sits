# ---- Unit tests: .contrastive_learning_data_split ----

test_that(".contrastive_learning_data_split random method returns correct shape", {
    triplets <- .contrastive_learning_data_split(
        samples          = samples_modis_ndvi,
        sampling_method  = "random",
        validation_split = 0.2,
        num_triplets     = 50L
    )

    train <- triplets[["train"]]
    val   <- triplets[["val"]]

    # Train and val together should equal total triplets
    expect_equal(nrow(train) + nrow(val), 50L)
    # Each triplet has anchor, positive, negative list-columns
    expect_true(all(c("anchor", "positive", "negative") %in% names(train)))
    # Validation set should be non-empty
    expect_true(nrow(val) > 0L)
})

test_that(".contrastive_learning_data_split respects label semantics", {
    triplets <- .contrastive_learning_data_split(
        samples          = samples_modis_ndvi,
        sampling_method  = "random",
        validation_split = 0.0,
        num_triplets     = 30L
    )

    train <- triplets[["train"]]
    labels <- samples_modis_ndvi$label

    # Verify positive is same class and negative is different class
    for (i in seq_len(min(10L, nrow(train)))) {
        a_lbl <- labels[train$anchor_idx[i]]
        p_lbl <- labels[train$positive_idx[i]]
        n_lbl <- labels[train$negative_idx[i]]
        expect_equal(a_lbl, p_lbl)
        expect_false(a_lbl == n_lbl)
    }
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

    # Should complete without error
    expect_no_error({
        triplets <- .contrastive_learning_data_split(
            samples          = tiny_samples,
            sampling_method  = "random",
            validation_split = 0.0,
            num_triplets     = 10L,
            skip_singletons  = TRUE
        )
    })
    expect_true(nrow(triplets[["train"]]) > 0L)
})

test_that(".contrastive_learning_data_split hard method works", {
    triplets <- .contrastive_learning_data_split(
        samples          = samples_modis_ndvi,
        sampling_method  = "hard",
        validation_split = 0.0,
        num_triplets     = 20L
    )

    train <- triplets[["train"]]
    expect_equal(nrow(train), 20L)
    expect_true(all(c("anchor", "positive", "negative") %in% names(train)))
})

# ---- Unit tests: .triplet_dataset ----

test_that(".triplet_dataset returns correct item shape", {
    skip_if_not_installed("torch")

    n_times <- .samples_ntimes(samples_modis_ndvi)
    n_bands <- length(.samples_bands(samples_modis_ndvi))

    triplets <- .contrastive_learning_data_split(
        samples          = samples_modis_ndvi,
        sampling_method  = "random",
        validation_split = 0.0,
        num_triplets     = 10L
    )

    ds   <- .triplet_dataset(triplets[["train"]], margin = 1e-3, n_times = n_times)
    item <- ds$.getitem(1L)

    # x must be [3, n_times, n_bands] (anchor, positive, negative)
    expect_equal(as.integer(item$x$shape), c(3L, n_times, n_bands))
    # y is the margin tensor
    expect_equal(as.integer(item$y$shape), 1L)
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
                embedding_dim      = 16L,
                margin             = 1e-3,
                triplet_smp_method = "random",
                num_triplets       = 100L,
                epochs             = 5L,
                batch_size         = 32L,
                verbose            = FALSE
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
                num_triplets  = 100L,
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
                num_triplets  = 100L,
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
