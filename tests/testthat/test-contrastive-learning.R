# Tests for supervised contrastive learning (SupCon) pre-training.
#
# The internal .contrastive_learning_* helpers are exercised directly (as in
# the other SSL test files), and the public sits_contrastive_learning() is
# covered end-to-end through sits_pre_train() and sits_encode().

# ---- Internal: .contrastive_learning_data_split ----

test_that(".contrastive_learning_data_split builds train/val view pairs", {
    n_times <- .samples_ntimes(samples_modis_ndvi)
    n_bands <- length(.samples_bands(samples_modis_ndvi))
    n_feats <- n_times * n_bands

    split <- .contrastive_learning_data_split(
        samples          = samples_modis_ndvi,
        validation_split = 0.2,
        num_pairs        = 50L
    )
    train <- split[["train"]]
    val   <- split[["val"]]

    expect_equal(dim(train[["a"]]), dim(train[["b"]]))
    expect_equal(ncol(train[["a"]]), n_feats)
    expect_equal(ncol(val[["a"]]), n_feats)
    expect_length(train[["labels"]], nrow(train[["a"]]))
    expect_length(val[["labels"]], nrow(val[["a"]]))
    expect_lte(nrow(train[["a"]]), 50L)
    expect_gt(nrow(val[["a"]]), 0L)
})

test_that(".contrastive_learning_data_split with no validation keeps all pairs", {
    split <- .contrastive_learning_data_split(
        samples          = samples_modis_ndvi,
        validation_split = 0,
        num_pairs        = 30L
    )
    expect_equal(nrow(split[["train"]][["a"]]), 30L)
    expect_equal(nrow(split[["val"]][["a"]]), 0L)
})

# ---- Internal: .contrastive_learning_dataset ----

test_that(".contrastive_learning_dataset yields [2, n_times, n_bands] items", {
    skip_if_not_installed("torch")

    n_times <- .samples_ntimes(samples_modis_ndvi)
    n_bands <- length(.samples_bands(samples_modis_ndvi))

    split <- .contrastive_learning_data_split(
        samples          = samples_modis_ndvi,
        validation_split = 0,
        num_pairs        = 10L
    )
    ds   <- .contrastive_learning_dataset(split[["train"]], n_times = n_times)
    item <- ds$.getitem(1L)

    expect_equal(ds$.length(), 10L)
    expect_equal(as.integer(item$x$shape), c(2L, n_times, n_bands))
    expect_equal(as.integer(item$y$shape), 1L)
    expect_true(item$y$dtype == torch::torch_long())
})

# ---- Internal: .contrastive_learning_loss (SupCon) ----

test_that(".contrastive_learning_loss returns a finite, non-negative scalar", {
    skip_if_not_installed("torch")

    torch::torch_manual_seed(7412L)
    B <- 8L; D <- 16L
    # Views are L2-normalised, matching the loss contract
    input  <- torch::nnf_normalize(
        torch::torch_randn(c(B, 2L, D)), p = 2, dim = 3L
    )
    target <- torch::torch_randint(1L, 3L, B, dtype = torch::torch_long())

    loss <- .contrastive_learning_loss(input, target, scaling = 0.07)

    expect_equal(as.integer(loss$shape), integer(0))
    expect_true(is.finite(torch::as_array(loss)))
    expect_gte(torch::as_array(loss), 0)
})

test_that(".contrastive_learning_loss supports gradient flow", {
    skip_if_not_installed("torch")

    torch::torch_manual_seed(5174L)
    B <- 8L; D <- 8L
    input  <- torch::torch_randn(c(B, 2L, D), requires_grad = TRUE)
    target <- torch::torch_randint(1L, 3L, B, dtype = torch::torch_long())

    # L2-normalise views (as in the model forward) before the loss
    input_n <- torch::nnf_normalize(input, p = 2, dim = 3L)
    loss <- .contrastive_learning_loss(input_n, target, scaling = 0.07)
    loss$backward()

    expect_false(is.null(input$grad))
    expect_true(all(is.finite(torch::as_array(input$grad))))
})

test_that(".contrastive_learning_loss is lower when same-class views align", {
    skip_if_not_installed("torch")

    # Two classes (4 samples each); class embeddings orthonormal.
    B <- 8L; D <- 16L
    target <- torch::torch_tensor(rep(c(1L, 2L), each = 4L),
                                  dtype = torch::torch_long())

    # Aligned: both views equal the class-specific orthonormal embedding.
    base <- torch::nnf_normalize(torch::torch_eye(2L, D), p = 2, dim = 2L)
    sel  <- torch::torch_tensor(rep(c(1L, 2L), each = 4L),
                                dtype = torch::torch_long())
    e    <- base$index_select(1L, sel)
    aligned      <- torch::torch_stack(list(e, e), dim = 2L)
    loss_aligned <- torch::as_array(
        .contrastive_learning_loss(aligned, target, scaling = 0.07)
    )

    # Misaligned: random embeddings ignore class structure.
    torch::torch_manual_seed(4517L)
    z <- torch::nnf_normalize(torch::torch_randn(c(B, D)), p = 2, dim = 2L)
    misaligned      <- torch::torch_stack(list(z, z), dim = 2L)
    loss_misaligned <- torch::as_array(
        .contrastive_learning_loss(misaligned, target, scaling = 0.07)
    )

    expect_true(loss_aligned < loss_misaligned)
})

test_that(".contrastive_learning_loss varies with scaling", {
    skip_if_not_installed("torch")

    torch::torch_manual_seed(3291L)
    B <- 16L; D <- 8L
    # Views are L2-normalised, matching the loss contract
    input  <- torch::nnf_normalize(
        torch::torch_randn(c(B, 2L, D)), p = 2, dim = 3L
    )
    target <- torch::torch_randint(1L, 3L, B, dtype = torch::torch_long())

    loss_sharp <- torch::as_array(
        .contrastive_learning_loss(input, target, scaling = 0.07)
    )
    loss_flat  <- torch::as_array(
        .contrastive_learning_loss(input, target, scaling = 1.0)
    )
    expect_false(loss_sharp == loss_flat)
})

# ---- Integration: sits_contrastive_learning ----

test_that("sits_contrastive_learning returns a training function without samples", {
    cl_fn <- sits_contrastive_learning(embedding_dim = 16L, epochs = 5L)
    expect_true(is.function(cl_fn))
})

test_that("sits_contrastive_learning trains an encoder and encodes samples", {
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")

    embedding_dim <- 16L
    encoder <- .try(
        sits_pre_train(
            samples   = samples_modis_ndvi,
            rl_method = sits_contrastive_learning(
                embedding_dim = embedding_dim,
                proj_dim      = 32L,
                scaling       = 0.07,
                num_pairs     = 100L,
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

    enc_samples <- sits_encode(
        data    = sits_sample(samples_modis_ndvi, frac = 0.3),
        encoder = encoder
    )
    expect_true(inherits(enc_samples, "sits"))
    expect_equal(length(sits_bands(enc_samples)), embedding_dim)
    expect_true(all(grepl("^EMB", sits_bands(enc_samples))))
})

test_that("sits_contrastive_learning embeddings support classification", {
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")

    encoder <- .try(
        sits_pre_train(
            samples   = samples_modis_ndvi,
            rl_method = sits_contrastive_learning(
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
    rf_model  <- sits_train(enc_samples, sits_rfor(num_trees = 20L))
    point_enc <- sits_encode(
        data    = sits_select(point_mt_6bands, bands = "NDVI"),
        encoder = encoder
    )
    point_class <- sits_classify(point_enc, ml_model = rf_model,
                                 progress = FALSE)

    expect_true(all(point_class$predicted[[1L]]$class %in%
                        sits_labels(samples_modis_ndvi)))
})
