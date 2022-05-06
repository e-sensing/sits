test_that("Sample", {
    testthat::skip_on_cran()

    data(cerrado_2classes)

    data <- sits_sample(cerrado_2classes, n = 10)
    expect_true(nrow(data) == 20)

    data <- sits_sample(cerrado_2classes, frac = 0.1)
    expect_true(nrow(dplyr::filter(data, label == "Cerrado")) == 40)
    expect_true(nrow(dplyr::filter(data, label == "Pasture")) == 34)

    data2 <- sits_sample(cerrado_2classes, frac = 1.3, oversample = TRUE)
    expect_true(nrow(data2) > nrow(cerrado_2classes))
})

test_that("Sample reduce imbalance", {
    # print the labels summary for a sample set
    sits_labels_summary(samples_modis_4bands)
    # reduce the sample imbalance
    new_samples <- sits_reduce_imbalance(samples_modis_4bands,
        n_samples_over = 200, n_samples_under = 200,
        multicores = 4
    )
    # print the labels summary for the rebalanced set
    sits_labels_summary(new_samples)
})
