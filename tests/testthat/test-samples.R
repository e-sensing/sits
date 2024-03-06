test_that("Sample", {
    data(cerrado_2classes)

    data <- sits_sample(cerrado_2classes, frac = 0.1)
    expect_true(nrow(dplyr::filter(data, label == "Cerrado")) == 40)
    expect_true(nrow(dplyr::filter(data, label == "Pasture")) == 34)

    data2 <- sits_sample(cerrado_2classes, frac = 1.3, oversample = TRUE)
    expect_true(nrow(data2) > nrow(cerrado_2classes))
})

test_that("Sample reduce imbalance", {
    # print the labels summary for a sample set
    sum_ori_samples <- summary(samples_modis_ndvi)
    # reduce the sample imbalance
    new_samples <- sits_reduce_imbalance(samples_modis_ndvi,
        n_samples_over = 200, n_samples_under = 200,
        multicores = 2
    )
    # print the labels summary for the rebalanced set
    sum_new_samples <- summary(new_samples)
    expect_true(nrow(new_samples) < nrow(samples_modis_ndvi))
    expect_true(sd(sum_new_samples[["count"]]) < sd(sum_ori_samples[["count"]]))
})

test_that("Sampling design", {
    # create a random forest model
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
    # create a data cube from local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir
    )
    # classify a data cube
    probs_cube <- sits_classify(
        data = cube, ml_model = rfor_model, output_dir = tempdir()
    )
    # label the probability cube
    label_cube <- sits_label_classification(
        probs_cube,
        output_dir = tempdir()
    )
    # estimated UA for classes
    expected_ua <- c(Cerrado = 0.75, Forest = 0.9,
                     Pasture = 0.8, Soy_Corn = 0.8)
    sampling_design <- sits_sampling_design(label_cube, expected_ua)

    expect_true(all(c("prop", "expected_ua", "std_dev", "equal",
                      "alloc_100", "alloc_75", "alloc_50", "alloc_prop")
                %in% colnames(sampling_design)))

    expect_equal(sampling_design["Cerrado", "equal"][[1]], 334)
    expect_equal(sampling_design["Pasture", "alloc_75"][[1]], 153)
})
