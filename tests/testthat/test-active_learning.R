test_that("Suggested samples have different confidence levels", {

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
    low_conf <- sits_suggest_samples(cube,  n = 100)
    high_conf <- sits_suggest_samples(cube, n = 100, confidence = "high")

    expect_true(nrow(low_conf)  == 100)
    expect_true(nrow(high_conf) == 100)
    expect_error(sits_suggest_samples(cube, confidence = "INVALID"))

    exp_colnames <- c("longitude", "latitude", "start_date",
                      "end_date", "label")
    expect_true(all(colnames(low_conf)  %in% exp_colnames))
    expect_true(all(colnames(high_conf) %in% exp_colnames))

    unc_raster <- terra::rast(sits:::.file_info_path(cube))
    low_conf["type"]  <- "low"
    high_conf["type"] <- "high"
    conf_df <- rbind(low_conf, high_conf)
    conf_sf <- sf::st_as_sf(conf_df,
                            coords = c("longitude", "latitude"),
                            crs = 4326)
    conf_sf <- sf::st_transform(conf_sf, crs = terra::crs(unc_raster))
    var_df <- terra::extract(unc_raster, terra::vect(conf_sf))
    conf_df <- cbind(conf_df, var_df)

    conf <- conf_df %>%
        dplyr::as_tibble() %>%
        dplyr::arrange(type) %>%
        dplyr::group_by(type) %>%
        dplyr::summarize(min = min(lyr1),
                         max = max(lyr1),
                         mean = mean(lyr1))
    # NOTE: The value of the metrics for the suggested samples of low
    # confidence must be larger than those of the samples of high confidence.
    expect_true(conf[conf$type == "low", "min"]  > conf[conf$type == "high", "min"])
    expect_true(conf[conf$type == "low", "max"]  > conf[conf$type == "high", "max"])
    expect_true(conf[conf$type == "low", "mean"] > conf[conf$type == "high", "mean"])
}

