test_that("Getting data for probs and classified cube", {
    # train a random forest model
    rf_model <- sits_train(samples_modis_ndvi, ml_method = sits_rfor)
    # Example of classification of a data cube
    # create a data cube from local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir
    )
    # classify a data cube
    probs_cube <- sits_classify(
        data = cube,
        ml_model = rf_model,
        output_dir = tempdir(),
        version = "probs_get"
    )
    samples_sinop <- paste0(system.file("extdata/samples/samples_sinop_csv",
                                        package = "sits"))
    probs_values <- sits_get_probs(
        cube = probs_cube,
        samples = samples_sinop
    )
    probs_neigh <- sits_get_probs(
        cube = probs_cube,
        samples = samples_sinop,
        window_size = 5L
    )

    class_cube <- sits_label_classification(
        cube = probs_cube,
        output_dir = tempdir(),
        version = "class_get"
    )
    class_values <- sits_get_class(
        cube = class_cube,
        samples = samples_sinop
    )

})
