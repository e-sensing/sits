test_that("Suggested samples have low confidence, high entropy", {

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
    samples_2bands <- sits_select(
        sits::samples_modis_4bands,
        bands = c("NDVI")
    )
    xgb_model <- sits_train(samples_2bands,
        ml_method = sits_xgboost(verbose = FALSE)
    )
    probs_cube <- sits_classify(
        cube,
        ml_model = xgb_model,
        output_dir = tempdir(),
        memsize = 4, multicores = 2
    )
    cube <- sits_uncertainty(probs_cube,
                             type = "entropy",
                             output_dir = out_dir)

    # Get sample suggestions.
    samples_df <- sits_suggest_samples(cube,  n = 100, min_dist_pixels = 0)

    expect_true(nrow(samples_df)  == 100)
    expect_true(all(colnames(samples_df)  %in% c("longitude", "latitude",
                                                 "start_date", "end_date",
                                                 "label")))
    expect_true(all(samples_df[["label"]] == "NoClass"))
    expect_warning(
        # Large distance between pixels in an small raster.
        sits_suggest_samples(cube,  n = 100, min_dist_pixels = 100) < 100
    )

    unc_raster <- terra::rast(sits:::.file_info_path(cube))
    samples_sf <- sf::st_as_sf(samples_df,
                               coords = c("longitude", "latitude"),
                               crs = 4326)
    samples_sf <- sf::st_transform(samples_sf, crs = terra::crs(unc_raster))
    var_df <- terra::extract(unc_raster, terra::vect(samples_sf))
    samples_df <- cbind(samples_df, var_df)

    expect_true(min(samples_df$lyr1)  > mean(unc_raster[]))
    expect_true(max(samples_df$lyr1)  == max(unc_raster[]))
    expect_true(mean(samples_df$lyr1) > mean(unc_raster[]))
})



test_that("increased samples have high confidence, low entropy", {

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
    samples_2bands <- sits_select(
        sits::samples_modis_4bands,
        bands = c("NDVI")
    )
    xgb_model <- sits_train(samples_2bands,
        ml_method = sits_xgboost(verbose = FALSE)
    )
    probs_cube <- sits_classify(
        cube,
        ml_model = xgb_model,
        output_dir = tempdir(),
        memsize = 4, multicores = 2
    )
    c_cube <- sits_label_classification(probs_cube,
                                        output_dir = out_dir)
    u_cube <- sits_uncertainty(probs_cube,
                               type = "entropy",
                               output_dir = out_dir)

    # Get sample suggestions.
    samples_df <- sits_increase_samples(u_cube = u_cube,
                                        c_cube = c_cube,
                                        n = 100,
                                        min_dist_pixels = 0)

    expect_true(nrow(samples_df)  == 100)
    expect_true(all(colnames(samples_df)  %in% c("longitude", "latitude",
                                                 "start_date", "end_date",
                                                 "label")))
    expect_true(all(samples_df[["label"]] != "NoClass"))
    expect_true(all(samples_df[["label"]] != ""))
    expect_true(sum(is.na(samples_df[["label"]])) == 0)
    expect_true(all(samples_df[["label"]] != character(0)))
    expect_warning(
        # Large distance between pixels in an small raster.
        sits_increase_samples(u_cube = u_cube,
                              c_cube = c_cube,
                              n = 100,
                              min_dist_pixels = 100)
    )

    unc_raster <- terra::rast(sits:::.file_info_path(u_cube))
    samples_sf <- sf::st_as_sf(samples_df,
                               coords = c("longitude", "latitude"),
                               crs = 4326)
    samples_sf <- sf::st_transform(samples_sf, crs = terra::crs(unc_raster))
    var_df <- terra::extract(unc_raster, terra::vect(samples_sf))
    samples_df <- cbind(samples_df, var_df)

    expect_true(min(samples_df$lyr1)  == min(unc_raster[]))
    expect_true(max(samples_df$lyr1)  < max(unc_raster[]))
    expect_true(mean(samples_df$lyr1) < mean(unc_raster[]))
})
