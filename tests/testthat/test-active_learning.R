test_that("Suggested samples have low confidence, high entropy", {

    # Get uncertaintly cube.
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    out_dir <- tempdir()
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "X2", "tile", "band", "date")
    )
    samples_ndvi <- sits_select(
        sits::samples_modis_4bands,
        bands = c("NDVI")
    )
    set.seed(123)
    xgb_model <- sits_train(samples_ndvi,
        ml_method = sits_xgboost(verbose = FALSE)
    )
    probs_cube <- sits_classify(
        cube,
        ml_model = xgb_model,
        output_dir = tempdir(),
        memsize = 4, multicores = 2
    )
    uncert_cube <- sits_uncertainty(probs_cube,
        type = "least",
        output_dir = out_dir
    )

    # Get sample suggestions.
    samples_df <- suppressWarnings(sits_uncertainty_sampling(
        uncert_cube,
        n = 100,
        min_dist_pixels = 0
    ))

    expect_true(nrow(samples_df) == 100)
    expect_true(all(colnames(samples_df) %in% c(
        "longitude", "latitude",
        "start_date", "end_date",
        "label"
    )))
    expect_true(all(samples_df[["label"]] == "NoClass"))

    unc_raster <- .raster_open_rast(sits:::.file_info_path(uncert_cube))
    samples_sf <- sf::st_as_sf(samples_df,
        coords = c("longitude", "latitude"),
        crs = 4326
    )
    samples_sf <- sf::st_transform(samples_sf, crs = terra::crs(unc_raster))
    var_df <- .raster_extract(unc_raster, terra::vect(samples_sf))
    samples_df <- cbind(samples_df, var_df)

    expect_true(min(samples_df$focal_median) > mean(unc_raster[]))
    expect_true(max(samples_df$focal_median) == max(unc_raster[]))
    expect_true(mean(samples_df$focal_median) > mean(unc_raster[]))
})

test_that("Increased samples have high confidence, low entropy", {
    testthat::skip_on_cran()

    # Get uncertaintly cube.
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    out_dir <- tempdir()
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "X2", "tile", "band", "date")
    )
    samples_ndvi <- sits_select(
        sits::samples_modis_4bands,
        bands = c("NDVI")
    )
    xgb_model <- sits_train(samples_ndvi,
        ml_method = sits_xgboost(verbose = FALSE)
    )
    probs_cube <- sits_classify(
        cube,
        ml_model = xgb_model,
        output_dir = out_dir,
        memsize = 4, multicores = 2
    )
    # Get sample suggestions based on high confidence
    samples_df <- suppressWarnings(
        sits_confidence_samples(
            probs_cube = probs_cube,
            n = 20,
            min_margin = 0.9,
            min_dist_pixels = 10
        )
    )
    labels <- sits_labels(probs_cube)

    expect_true(nrow(samples_df) <= 20 * length(labels))
    expect_true(all(colnames(samples_df) %in% c(
        "longitude", "latitude",
        "start_date", "end_date",
        "label"
    )))
    expect_true(all(samples_df[["label"]] != "NoClass"))
    expect_true(all(samples_df[["label"]] != ""))
    expect_true(sum(is.na(samples_df[["label"]])) == 0)
    expect_true(all(samples_df[["label"]] != character(0)))
})
