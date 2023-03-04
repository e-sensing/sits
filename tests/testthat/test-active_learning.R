test_that("Suggested samples have low confidence, high entropy", {

    # Get uncertaintly cube.
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    out_dir <- tempdir()
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "tile", "band", "date")
    )
    set.seed(123)
    rfor_model <- sits_train(samples_modis_ndvi,
        ml_method = sits_xgboost(verbose = FALSE)
    )
    probs_cube <- sits_classify(
        cube,
        ml_model = rfor_model,
        output_dir = tempdir(),
        memsize = 4, multicores = 2
    )
    uncert_cube <- sits_uncertainty(
        probs_cube,
        type = "least",
        output_dir = out_dir
    )

    # Get sample suggestions.
    samples_df <- suppressWarnings(sits_uncertainty_sampling(
        uncert_cube,
        min_uncert = 0.5,
        n = 100,
        sampling_window = 10
    ))

    expect_true(nrow(samples_df) <= 100)
    expect_true(all(colnames(samples_df) %in% c(
        "longitude", "latitude",
        "start_date", "end_date",
        "label", "uncertainty"
    )))
    expect_true(all(samples_df[["label"]] == "NoClass"))
    expect_true(all(samples_df[["uncertainty"]] >= 0.5))
})

test_that("Increased samples have high confidence, low entropy", {
    # Get uncertaintly cube.
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    out_dir <- tempdir()
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "tile", "band", "date")
    )
    rfor_model <- sits_train(samples_modis_ndvi,
        ml_method = sits_rfor()
    )
    probs_cube <- sits_classify(
        cube,
        ml_model = rfor_model,
        output_dir = out_dir,
        memsize = 4, multicores = 2
    )
    # Get sample suggestions based on high confidence
    samples_df <- suppressWarnings(
        sits_confidence_sampling(
            probs_cube = probs_cube,
            n = 20,
            min_margin = 0.5,
            sampling_window = 10
        )
    )
    labels <- sits_labels(probs_cube)

    samples_count <- dplyr::count(samples_df, .data[["label"]])
    expect_true(
        nrow(dplyr::filter(samples_count, .data[["n"]] <= 20)) == 4
    )

    expect_true(all(colnames(samples_df) %in% c(
        "longitude", "latitude",
        "start_date", "end_date",
        "label", "confidence"
    )))
})
