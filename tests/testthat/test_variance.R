test_that("One-year, single core classification", {
    # create a rfor model
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
    # create a data cube from local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "tile", "band", "date")
    )
    # classify a data cube
    probs_cube <- sits_classify(data = cube,
                                ml_model = rfor_model,
                                output_dir = tempdir())
    # smooth the probability cube using Bayesian statistics
    var_cube <- sits_variance(probs_cube, output_dir = tempdir())

    r_obj <- .raster_open_rast(var_cube$file_info[[1]]$path[[1]])

    max_lyr1 <- max(.raster_get_values(r_obj)[, 1], na.rm = TRUE)
    expect_true(max_lyr1 <= 4000)

    max_lyr3 <- max(.raster_get_values(r_obj)[, 3], na.rm = TRUE)
    expect_true(max_lyr3 <= 4000)

    p <- plot(var_cube, labels = "Cerrado")

    fn <- p$tm_shape$shp

    t <- fn$`TERRA_MODIS_h12v10_2013-09-14_2014-08-29_variance_v1.tif`
    expect_true(max(t) <= 50)
    expect_true(min(t) >= 0)

    expect_true(p$tm_raster$style == "cont")

    p <- plot(var_cube, labels = "Cerrado", type = "hist")
    expect_true(all(p$data_labels %in% c("Cerrado", "Forest",
                                         "Pasture", "Soy_Corn")))
    v <- p$data$variance
    expect_true(max(v) <= 50)
    expect_true(min(v) >= 0)

    expect_true(all(file.remove(unlist(probs_cube$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(var_cube$file_info[[1]]$path))))
})
