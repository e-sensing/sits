test_that("sits summary", {
    sum <- summary(samples_modis_ndvi)
    expect_equal(sum$label, c("Cerrado", "Forest", "Pasture", "Soy_Corn"))
    expect_equal(sum$count, c(379, 131, 344, 364))
})
test_that("summary sits accuracy", {
    data(cerrado_2classes)
    # split training and test data
    train_data <- sits_sample(cerrado_2classes, n = 200)
    test_data <- sits_sample(cerrado_2classes, n = 200)
    # train a random forest model
    rfor_model <- sits_train(train_data, sits_rfor())
    # classify test data
    points_class <- sits_classify(
        data = test_data,
        ml_model = rfor_model,
        progress = FALSE
    )
    # measure accuracy
    acc <- sits_accuracy(points_class)
    sum <- capture.output(summary(acc))
    expect_true(grepl("Accuracy", sum[2]))
    expect_true(grepl("Kappa", sum[4]))
})
test_that("summary sits area accuracy", {
    # create a data cube from local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        progress = FALSE
    )
    sum_cube <- capture.output(suppressWarnings(summary(cube)))
    expect_true(grepl("class", sum_cube[1]))
    expect_true(grepl("dimensions", sum_cube[2]))
    expect_true(grepl("resolution", sum_cube[3]))
    expect_true(grepl("extent", sum_cube[4]))
    expect_true(grepl("coord", sum_cube[5]))
    expect_true(grepl("Min", sum_cube[7]))

    # create a random forest model
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
    # classify a data cube
    probs_cube <- sits_classify(
        data = cube,
        ml_model = rfor_model,
        output_dir = tempdir(),
        progress = FALSE
    )
    sum_probs <- capture.output(suppressWarnings(summary(probs_cube)))
    expect_true(grepl("class", sum_probs[1]))
    expect_true(grepl("dimensions", sum_probs[2]))
    expect_true(grepl("resolution", sum_probs[3]))
    expect_true(grepl("extent", sum_probs[4]))
    expect_true(grepl("coord", sum_probs[5]))
    expect_true(grepl("Min", sum_probs[7]))

    # get the variance cube
    variance_cube <- sits_variance(
        probs_cube,
        output_dir = tempdir()
    )
    sum_var <- capture.output(suppressWarnings(summary(variance_cube)))
    expect_true(grepl("class", sum_var[1]))
    expect_true(grepl("dimensions", sum_var[2]))
    expect_true(grepl("resolution", sum_var[3]))
    expect_true(grepl("extent", sum_var[4]))
    expect_true(grepl("coord", sum_var[5]))
    expect_true(grepl("Min", sum_var[7]))

    # label the probability cube
    label_cube <- sits_label_classification(
        probs_cube,
        output_dir = tempdir(),
        progress = FALSE
    )
    sum_label <- capture.output(suppressWarnings(summary(label_cube)))
    expect_true(grepl("class", sum_label[1]))
    expect_true(grepl("dimensions", sum_label[2]))
    expect_true(grepl("resolution", sum_label[3]))
    expect_true(grepl("extent", sum_label[4]))
    expect_true(grepl("coord", sum_label[5]))
    expect_true(grepl("area_km2", sum_label[6]))

    # obtain the ground truth for accuracy assessment
    ground_truth <- system.file("extdata/samples/samples_sinop_crop.csv",
        package = "sits"
    )
    # make accuracy assessment
    as <- sits_accuracy(label_cube, validation = ground_truth)
    sum_as <- capture.output(summary(as))
    expect_true(grepl("Accuracy", sum_as[2]))
    expect_true(grepl("Mapped", sum_as[11]))
    expect_true(grepl("Cerrado", sum_as[13]))
})
