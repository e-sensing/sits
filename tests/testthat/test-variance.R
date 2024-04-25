test_that("Variance cube", {
    # create a rfor model
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
    # create a data cube from local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        progress = FALSE
    )
    expect_error(sits_variance(cube, output_dir = tempdir()))
    # classify a data cube
    probs_cube <- sits_classify(
        data = cube,
        ml_model = rfor_model,
        output_dir = tempdir(),
        version = "var1",
        progress = FALSE
    )
    # smooth the probability cube using Bayesian statistics
    var_cube <- sits_variance(probs_cube, output_dir = tempdir())
    # check is variance cube
    .check_is_variance_cube(var_cube)

    varc <- var_cube
    class(varc) <- "data.frame"
    new_cube <- .cube_find_class(varc)
    expect_true("raster_cube" %in% class(new_cube))
    expect_true("derived_cube" %in% class(new_cube))
    expect_true("variance_cube" %in% class(new_cube))


    r_obj <- .raster_open_rast(var_cube$file_info[[1]]$path[[1]])

    max_lyr1 <- max(.raster_get_values(r_obj)[, 1], na.rm = TRUE)
    expect_true(max_lyr1 <= 4000)

    max_lyr3 <- max(.raster_get_values(r_obj)[, 3], na.rm = TRUE)
    expect_true(max_lyr3 <= 4000)

    p <- plot(var_cube, sample_size = 10000, labels = "Cerrado")

    expect_true(p$tm_raster$style == "cont")

    p <- plot(var_cube, sample_size = 10000, labels = "Cerrado", type = "hist")
    expect_true(all(p$data_labels %in% c(
        "Cerrado", "Forest",
        "Pasture", "Soy_Corn"
    )))
    v <- p$data$variance
    expect_true(max(v) <= 100)
    expect_true(min(v) >= 0)

    # test Recovery
    Sys.setenv("SITS_DOCUMENTATION_MODE" = "FALSE")
    expect_message({
        obj <- sits_variance(
            cube = probs_cube,
            output_dir = tempdir()
        )
    })
    class_cube <- sits_label_classification(
        probs_cube,
        output_dir = tempdir(),
        version = "var1"
    )
    expect_error(sits_variance(class_cube, output_dir = tempdir()))

    probs_df <- probs_cube
    class(probs_df) <- "data.frame"
    # test reading cube as data frame
    df_var <- sits_variance(
        cube = probs_df,
        output_dir = tempdir(),
        version = "vardf"
    )
    r_obj <- .raster_open_rast(df_var$file_info[[1]]$path[[1]])

    max_lyr1 <- max(.raster_get_values(r_obj)[, 1], na.rm = TRUE)
    expect_true(max_lyr1 <= 4000)

    max_lyr3 <- max(.raster_get_values(r_obj)[, 3], na.rm = TRUE)
    expect_true(max_lyr3 <= 4000)

    expect_true(all(file.remove(unlist(probs_cube$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(df_var$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(var_cube$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(class_cube$file_info[[1]]$path))))
})
