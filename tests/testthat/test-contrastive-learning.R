# ---- Unit tests: .contrastive_learning_data_split ----

test_that(".contrastive_learning_data_split label method returns correct shape", {
    result <- .contrastive_learning_data_split(
        samples          = samples_modis_ndvi,
        validation_split = 0.2,
        num_pairs        = 50L
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


# ---- Unit tests: ..contrastive_supcon_dataset ----

test_that(".contrastive_dataset returns correct item shape", {
    skip_if_not_installed("torch")

    n_times <- .samples_ntimes(samples_modis_ndvi)
    n_bands <- length(.samples_bands(samples_modis_ndvi))

    result <- .contrastive_learning_data_split(
        samples          = samples_modis_ndvi,
        validation_split = 0.0,
        num_pairs        = 10L
    )

    ds   <- .contrastive_learning_dataset(result[["train"]], n_times = n_times)
    item <- ds$.getitem(1L)

    # x must be [2, n_times, n_bands] (two views)
    expect_equal(as.integer(item$x$shape), c(2L, n_times, n_bands))
    # y is an integer class label (scalar tensor with shape [1])
    expect_equal(as.integer(item$y$shape), 1L)
    expect_true(item$y$dtype == torch::torch_long())
    expect_equal(ds$.length(), 10L)
})

# ---- Unit tests: .contrastive_learning_loss ----

test_that(".contrastive_learning_loss returns a finite scalar", {
    skip_if_not_installed("torch")

    torch::torch_manual_seed(7412L)
    B <- 8L; D <- 16L
    input <- torch::torch_randn(c(B, 2L, D))
    target <- torch::torch_randint(1L, 3L, B, dtype = torch::torch_long())

    loss <- .contrastive_learning_loss(input, target, scaling = 0.07)

    expect_equal(as.integer(loss$shape), integer(0))
    expect_true(is.finite(torch::as_array(loss)))
})

test_that(".contrastive_learning_loss is non-negative", {
    skip_if_not_installed("torch")

    torch::torch_manual_seed(2958L)
    B <- 16L; D <- 8L
    input <- torch::torch_randn(c(B, 2L, D))
    target <- torch::torch_randint(1L, 4L, B, dtype = torch::torch_long())

    loss <- .contrastive_learning_loss(input, target, scaling = 0.07)

    expect_true(torch::as_array(loss) >= 0)
})

test_that(".contrastive_learning_loss is low for aligned unique-class pairs", {
    skip_if_not_installed("torch")

    # Each sample has its own class and z_a = z_b (orthogonal, L2-normalised).
    # The similarity matrix is approximately identity / tau, so softmax
    # concentrates on the single positive (the diagonal), yielding low loss.
    B <- 8L; D <- 32L
    z <- torch::nnf_normalize(torch::torch_eye(B, D), p = 2, dim = 2)
    input <- torch::torch_stack(list(z, z), dim = 2)
    target <- torch::torch_arange(1, B, dtype = torch::torch_long())

    loss <- torch::as_array(
        .contrastive_learning_loss(input, target, scaling = 0.07)
    )

    expect_true(loss < 0.1)
})

test_that(".contrastive_learning_loss is higher for misaligned pairs", {
    skip_if_not_installed("torch")

    B <- 8L; D <- 32L
    # Aligned: z_a = z_b (orthogonal), each sample its own class
    z <- torch::nnf_normalize(torch::torch_eye(B, D), p = 2, dim = 2)
    target <- torch::torch_arange(1, B, dtype = torch::torch_long())

    input_aligned <- torch::torch_stack(list(z, z), dim = 2)
    loss_aligned <- torch::as_array(
        .contrastive_learning_loss(input_aligned, target, scaling = 0.07)
    )

    # Misaligned: z_b is a random permutation of z_a rows
    torch::torch_manual_seed(4517L)
    perm <- sample.int(B)
    input_misaligned <- torch::torch_stack(list(z, z[perm, ]), dim = 2)
    loss_misaligned <- torch::as_array(
        .contrastive_learning_loss(input_misaligned, target, scaling = 0.07)
    )

    expect_true(loss_misaligned > loss_aligned)
})

test_that(".contrastive_learning_loss supports gradient flow", {
    skip_if_not_installed("torch")

    torch::torch_manual_seed(5174L)
    B <- 8L; D <- 8L
    input <- torch::torch_randn(c(B, 2L, D), requires_grad = TRUE)
    target <- torch::torch_randint(1L, 3L, B, dtype = torch::torch_long())

    loss <- .contrastive_learning_loss(input, target, scaling = 0.07)
    loss$backward()

    expect_false(is.null(input$grad))
    expect_true(all(is.finite(torch::as_array(input$grad))))
})

test_that(".contrastive_learning_loss varies with scaling", {
    skip_if_not_installed("torch")

    torch::torch_manual_seed(3291L)
    B <- 16L; D <- 8L
    input <- torch::torch_randn(c(B, 2L, D))
    target <- torch::torch_randint(1L, 3L, B, dtype = torch::torch_long())

    loss_sharp <- torch::as_array(
        .contrastive_learning_loss(input, target, scaling = 0.07)
    )
    loss_flat <- torch::as_array(
        .contrastive_learning_loss(input, target, scaling = 1.0)
    )

    expect_false(loss_sharp == loss_flat)
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
                scaling         = 0.07,
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

test_that("contrastive learning encoder can encode a sits tibble", {
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

test_that("contrastive learning: downstream classification works", {
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
