test_that("sits summary", {
    sum <- summary(samples_modis_ndvi)
    expect_equal(sum$label, c("Cerrado", "Forest", "Pasture", "Soy_Corn"))
    expect_equal(sum$count, c(379, 131, 344, 364))
    sum1 <- suppressWarnings(sits_labels_summary(samples_modis_ndvi))
    expect_equal(sum1$label, c("Cerrado", "Forest", "Pasture", "Soy_Corn"))
    expect_equal(sum1$count, c(379, 131, 344, 364))
})

test_that("summary cube",{
    # create a data cube from local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    sum <- capture.output(summary(cube))
    expect_true(grepl("MODIS", sum[1]))
    expect_true(grepl("Median", sum[4]))

})

test_that("summary sits accuracy", {
    data(cerrado_2classes)
    # split training and test data
    train_data <- sits_sample(cerrado_2classes, frac = 0.5)
    test_data  <- sits_sample(cerrado_2classes, frac = 0.5)
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
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    sum_cube <- capture.output(suppressWarnings(summary(cube)))
    expect_true(grepl("TERRA", sum_cube[1]))

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
    expect_true(any(grepl("Min", sum_probs)))

    # get the variance cube
    variance_cube <- sits_variance(
        probs_cube,
        output_dir = tempdir(),
        progress = FALSE
    )
    sum_var <- capture.output(suppressWarnings(summary(variance_cube)))
    expect_true(any(grepl("80%", sum_var)))

    # label the probability cube
    label_cube <- sits_label_classification(
        probs_cube,
        output_dir = tempdir(),
        progress = FALSE
    )
    sum_label <- capture.output(suppressWarnings(summary(label_cube)))
    expect_true(any(grepl("area_km2", sum_label)))

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

test_that("summary BDC cube",{

    tiles <- c("007004", "007005")
    start_date <- "2022-05-01"
    end_date <- "2022-08-29"
    bands <- c("NDVI", "EVI", "B13", "B14", "B15", "B16", "CLOUD")
    # create a raster cube file from BDC
    cbers_cube_8d <- .try(
        {
            sits_cube(
                source = "BDC",
                collection = "CBERS-WFI-8D",
                tiles = tiles,
                start_date = start_date,
                end_date = end_date,
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(purrr::is_null(cbers_cube_8d),
                      message = "BDC cube CBERS-WFI-8D is not accessible"
    )
    sum2 <- capture.output(summary(cbers_cube_8d, tile = "007004"))
    expect_true(grepl("007004", sum2[4]))
    expect_true(grepl("007004", sum2[48]))
})
