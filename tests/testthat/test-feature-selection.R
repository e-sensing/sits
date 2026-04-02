test_that("Feature selection from samples", {
    # ---- Selecting samples by bands ----
    set.seed(123)
    samples <- sits::samples_l8_rondonia_2bands

    samples_selected_mda <- suppressMessages(sits_feature_selection(
        samples = samples,
        ml_method = sits_rfor(),
        metric = "mda",
        n_iter = 1,
        acc_loss = 0.005,
        multicores = 2L
    ))
    expect_equal(
        object = sits_bands(samples_selected_mda),
        expected = "NDVI"
    )
    expect_s3_class(
        object = samples_selected_mda,
        class = "sits"
    )
    # Test with mean decreased gini
    samples_selected_mdg <- suppressMessages(sits_feature_selection(
        samples = samples,
        ml_method = sits_rfor(),
        metric = "mdg",
        n_iter = 1,
        acc_loss = 0.005,
        multicores = 2L
    ))
    expect_equal(
        object = sits_bands(samples_selected_mdg),
        expected = "NDVI"
    )
    expect_s3_class(
        object = samples_selected_mdg,
        class = "sits"
    )
    expect_error(
        sits_feature_selection(
            samples = samples,
            ml_method = sits_tempcnn(),
            n_iter = 1,
            acc_loss = 0.02,
            multicores = 2L
        )
    )
    expect_error(
        sits_feature_selection(
            samples = samples,
            ml_method = sits_rfor(),
            n_iter = 3,
            acc_loss = 0.02,
            multicores = 2L
        )
    )
    expect_error(
        sits_feature_selection(
            samples = samples,
            ml_method = sits_rfor(),
            n_iter = 1,
            acc_loss = 1.2,
            multicores = 2L
        )
    )

    expect_error(
        sits_feature_selection(
            samples = samples,
            ml_method = sits_rfor(),
            n_iter = 1,
            metric = "test-metric",
            acc_loss = 0.005,
            multicores = 2L
        )
    )
})
