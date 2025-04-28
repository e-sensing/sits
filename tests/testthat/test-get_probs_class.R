test_that("Getting data for probs and classified cube", {
    # train a random forest model
    rf_model <- sits_train(samples_modis_ndvi, ml_method = sits_rfor)
    # Example of classification of a data cube
    # create a data cube from local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    # classify a data cube
    probs_cube <- sits_classify(
        data = cube,
        ml_model = rf_model,
        output_dir = tempdir(),
        version = "probs_get",
        progress = FALSE
    )
    samples_sinop <- paste0(system.file(
        "extdata/samples/samples_sinop_crop.csv",
        package = "sits"
    ))
    probs_values <- sits_get_probs(
        cube = probs_cube,
        samples = samples_sinop
    )
    expect_true(all(c(
        "longitude", "latitude",
        "X", "Y", "Cerrado", "Forest", "Pasture",
        "Soy_Corn"
    ) %in% colnames(probs_values)))
    probs <- probs_values[1, c(5:8)]
    expect_true(sum(probs) > 0.99)
    probs2 <- probs_values[2, c(5:8)]
    expect_true(sum(probs2) > 0.99)

    probs_neigh <- sits_get_probs(
        cube = probs_cube,
        samples = samples_sinop,
        window_size = 5L
    )
    expect_true(all(c(
        "longitude", "latitude", "X", "Y",
        "neighbors"
    ) %in% colnames(probs_neigh)))

    probs_mat1 <- probs_neigh[1, ]$neighbors[[1]]
    expect_true(nrow(probs_mat1) == 25)
    expect_true(sum(probs_mat1[1, ]) > 0.99)

    class_cube <- sits_label_classification(
        cube = probs_cube,
        output_dir = tempdir(),
        version = "class_get",
        progress = FALSE
    )
    class_values <- sits_get_class(
        cube = class_cube,
        samples = samples_sinop
    )
    expect_true(all(c("longitude", "latitude", "label")
    %in% colnames(class_values)))
    expect_true(all(unique(class_values[["label"]]) %in%
        c("Forest", "Cerrado", "Pasture", "Soy_Corn")))
})
