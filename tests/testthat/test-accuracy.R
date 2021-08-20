test_that("conf_matrix -2 classes", {
    data(cerrado_2classes)
    train_data <- sits_sample(cerrado_2classes, n = 100)
    test_data <- sits_sample(cerrado_2classes, n = 25)
    xgb_model <- sits_train(train_data, sits_xgboost(verbose = FALSE))
    points_class <- sits_classify(test_data, xgb_model)
    invisible(capture.output(acc <- sits_accuracy(points_class)))
    expect_true(acc$overall["Accuracy"] > 0.70)
    expect_true(acc$overall["Kappa"] > 0.70)
    p <- capture.output(sits_accuracy_summary(acc))
    expect_true(grepl("Accuracy", p[4]))
})
test_that("conf_matrix - more than 2 classes", {
    data(samples_modis_4bands)
    train_data <- sits_sample(samples_modis_4bands, n = 25)
    test_data <- sits_sample(samples_modis_4bands, n = 25)
    xgb_model <- sits_train(train_data, sits_xgboost(verbose = FALSE))
    points_class <- sits_classify(test_data, xgb_model)
    invisible(capture.output(acc <- sits_accuracy(points_class)))
    expect_true(acc$overall["Accuracy"] > 0.70)
    expect_true(acc$overall["Kappa"] > 0.70)
})
test_that("XLS", {
    data(cerrado_2classes)
    acc <- sits_kfold_validate(cerrado_2classes, folds = 2,
                                       ml_method = sits_rfor(num_trees = 100))
    results <- list()
    acc$name <- "cerrado_2classes"
    results[[length(results) + 1]] <- acc
    xls_file <- paste0(tempdir(), "/accuracy.xlsx")
    sits_to_xlsx(results, file = xls_file)

    expect_true(file.remove(xls_file))
})

test_that("Accuracy - more than 2 classes", {
    data("samples_modis_4bands")
    samples <- sits_select(samples_modis_4bands, bands = c("NDVI", "EVI"))
    acc <- sits_kfold_validate(samples, folds = 2,
                                    ml_method = sits_rfor(num_trees = 100))

    expect_true(acc$overall["Accuracy"] > 0.90)
    expect_true(acc$overall["Kappa"] > 0.90)
})
test_that("Accuracy areas", {
    samples_mt_2bands <- sits_select(samples_modis_4bands,
                                     bands = c("NDVI", "EVI"))

    xgb_model <- sits_train(samples_mt_2bands, sits_xgboost(verbose = FALSE))

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "LOCAL",
        name = "sinop-2014",
        satellite = "TERRA",
        sensor = "MODIS",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "X2", "tile", "band", "date")
    )

    probs_cube <- sits_classify(cube,
                                xgb_model,
                                output_dir = tempdir(),
                                memsize = 4,
                                multicores = 2)


    expect_true(all(file.exists(unlist(probs_cube$file_info[[1]]$path))))
    tc_obj <- .raster_open_rast(probs_cube$file_info[[1]]$path[[1]])
    expect_true(nrow(tc_obj) == probs_cube$nrows)

    label_cube <- sits_label_classification(
        probs_cube,
        output_dir = tempdir()
    )

    ground_truth <- system.file("extdata/samples/samples_sinop_crop.csv",
                                package = "sits")
    invisible(capture.output(as <- suppressWarnings(
        sits_accuracy(label_cube, validation_csv = ground_truth)))
    )

    expect_true(as.numeric(as$area_pixels["Forest"]) >
                    as$area_pixels["Pasture"])
    expect_true(as.numeric(as$accuracy$overall) > 0.75)
})
