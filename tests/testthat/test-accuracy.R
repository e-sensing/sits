test_that("conf_matrix -2 classes", {
    data(cerrado_2classes)
    set.seed(1234)
    train_data <- sits_sample(cerrado_2classes, frac = 0.5)
    test_data  <- sits_sample(cerrado_2classes, frac = 0.5)
    rfor_model <- sits_train(train_data, sits_rfor(verbose = FALSE))
    points_class <- sits_classify(
        data = test_data,
        ml_model = rfor_model,
        progress = FALSE
    )
    invisible(capture.output(acc <- sits_accuracy(points_class)))
    expect_true(acc$overall["Accuracy"] > 0.90)
    expect_true(acc$overall["Kappa"] > 0.90)
    p <- capture.output(sits_accuracy_summary(acc))
    expect_true(grepl("Accuracy", p[2]))

    p1 <- capture.output(acc)
    expect_true(grepl("Confusion Matrix", p1[1]))
    expect_true(grepl("Kappa", p1[11]))
})
test_that("conf_matrix - more than 2 classes", {
    set.seed(1234)
    data(samples_modis_ndvi)
    train_data <- sits_sample(samples_modis_ndvi, frac = 0.5)
    test_data  <- sits_sample(samples_modis_ndvi, frac = 0.5)
    rfor_model <- sits_train(train_data, sits_rfor())
    points_class <- sits_classify(
        data = test_data,
        ml_model = rfor_model,
        progress = FALSE
    )
    invisible(capture.output(acc <- sits_accuracy(points_class)))
    expect_true(acc$overall["Accuracy"] > 0.70)
    expect_true(acc$overall["Kappa"] > 0.70)
    p1 <- capture.output(acc)
    expect_true(grepl("Confusion Matrix", p1[1]))
    expect_true(grepl("Cerrado", p1[5]))
    expect_true(grepl("Kappa", p1[15]))
})
test_that("samples_validation", {
    set.seed(1234)
    samples <- samples_modis_ndvi
    samples$id <- seq_len(nrow(samples))
    train_data <- sits_sample(samples, frac = 0.8)
    # Remove the lines used for validation
    sel <- !samples$id %in% train_data$id
    val_samples <- samples[sel, ]
    samples_val <-
        .check_samples_validation(
            samples_validation = val_samples,
            labels = sits_labels(samples),
            timeline = sits_timeline(samples),
            bands = sits_bands(samples)
        )
    expect_true(nrow(samples_val) == nrow(val_samples))
})
test_that("XLS", {
    set.seed(1234)
    data(cerrado_2classes)
    acc <- sits_kfold_validate(cerrado_2classes,
        folds = 2,
        ml_method = sits_rfor(num_trees = 100)
    )
    results <- list()
    acc$name <- "cerrado_2classes"
    results[[length(results) + 1]] <- acc
    xls_file <- paste0(tempdir(), "/accuracy.xlsx")
    suppressMessages(sits_to_xlsx(results, file = xls_file))

    expect_true(file.remove(xls_file))
})

test_that("K-fold validate", {
    set.seed(1234)
    acc <- sits_kfold_validate(samples_modis_ndvi,
        folds = 2,
        ml_method = sits_rfor(num_trees = 100)
    )

    expect_true(acc$overall["Accuracy"] > 0.70)
    expect_true(acc$overall["Kappa"] > 0.70)

    results <- list()
    acc$name <- "modis_ndvi"
    results[[length(results) + 1]] <- acc
    xls_file <- paste0(tempdir(), "/accuracy.xlsx")
    suppressMessages(sits_to_xlsx(results, file = xls_file))

    expect_true(file.remove(xls_file))
})
test_that("Accuracy areas", {
    set.seed(1234)
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        progress = FALSE
    )
    probs_cube <- sits_classify(
        data = cube,
        ml_model = rfor_model,
        output_dir = tempdir(),
        memsize = 4,
        multicores = 1,
        progress = FALSE
    )


    expect_true(all(file.exists(unlist(probs_cube$file_info[[1]]$path))))
    tc_obj <- .raster_open_rast(probs_cube$file_info[[1]]$path[[1]])
    expect_true(nrow(tc_obj) == .tile_nrows(probs_cube))

    label_cube <- sits_label_classification(
        probs_cube,
        memsize = 4,
        multicores = 1,
        output_dir = tempdir(),
        progress = FALSE
    )

    ground_truth <- system.file("extdata/samples/samples_sinop_crop.csv",
        package = "sits"
    )
    invisible(capture.output(as <- suppressWarnings(
        sits_accuracy(label_cube, validation = ground_truth)
    )))

    expect_true(as.numeric(as$area_pixels["Forest"]) >
        as$area_pixels["Pasture"])
    expect_equal(as.numeric(as$accuracy$overall),
        expected = 0.75,
        tolerance = 0.5
    )

    p1 <- capture.output(as)

    expect_true(grepl("Area Weigthed Statistics", p1[1]))
    expect_true(grepl("Overall Accuracy", p1[2]))
    expect_true(grepl("Cerrado", p1[6]))
    expect_true(grepl("Mapped Area", p1[11]))

    # alternative: use a sits tibble
    samples_csv <- tibble::as_tibble(
        utils::read.csv(
            ground_truth,
            stringsAsFactors = FALSE
        )
    )
    as2 <- sits_accuracy(label_cube, validation = samples_csv)

    expect_true(as.numeric(as2$area_pixels["Forest"]) >
        as2$area_pixels["Pasture"])
    expect_equal(as.numeric(as2$accuracy$overall),
        expected = 0.75,
        tolerance = 0.5
    )
})
