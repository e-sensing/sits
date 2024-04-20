test_that("Combine predictions", {
    # create a data cube from local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        progress = FALSE
    )
    # create a random forest model
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
    # classify a data cube using rfor model
    output_dir <- paste0(tempdir(), "/comb")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
    probs_rfor_cube <- sits_classify(
        data = cube, ml_model = rfor_model,
        output_dir = output_dir,
        version = "rfor",
        progress = FALSE
    )
    # create an XGBoost model
    xgb_model <- sits_train(samples_modis_ndvi, sits_xgboost())
    # classify a data cube using xgboost model
    probs_xgb_cube <- sits_classify(
        data = cube, ml_model = xgb_model,
        output_dir = output_dir,
        version = "xgb",
        progress = FALSE
    )
    # create a list of predictions to be combined
    pred_cubes <- list(probs_rfor_cube, probs_xgb_cube)
    # combine predictions
    comb_probs_cube_avg <- sits_combine_predictions(
        cubes = pred_cubes,
        type = "average",
        output_dir = output_dir,
        version = "comb_rfor_xgb_avg",
        multicores = 1
    )
    expect_equal(sits_labels(comb_probs_cube_avg), sits_labels(probs_xgb_cube))
    expect_equal(sits_bbox(comb_probs_cube_avg), sits_bbox(probs_xgb_cube))
    expect_equal(nrow(comb_probs_cube_avg), nrow(probs_xgb_cube))

    rfor_obj <- .raster_open_rast(.tile_path(probs_rfor_cube))
    xgb_obj <- .raster_open_rast(.tile_path(probs_xgb_cube))
    avg_obj <- .raster_open_rast(.tile_path(comb_probs_cube_avg))

    vls_rfor <- terra::values(rfor_obj)
    vls_xgb <- terra::values(xgb_obj)
    vls_avg <- terra::values(avg_obj)

    rfor <- as.vector(vls_rfor[1:10, 1])
    xgb <- as.vector(vls_xgb[1:10, 1])
    avg <- purrr::map2_int(rfor, xgb, function(r, x) {
        as.integer(mean(c(r, x)))
    })
    avg2 <- as.vector(vls_avg[1:10, 1])

    expect_true(all(abs(avg - avg2)) < 3)

    # Recovery
    doc_mode <- Sys.getenv("SITS_DOCUMENTATION_MODE")
    if (doc_mode)
        Sys.setenv("SITS_DOCUMENTATION_MODE" = FALSE)
    # test Recovery
    out <- capture_messages({
        expect_message(
            object = {
                sits_combine_predictions(
                    cubes = pred_cubes,
                    type = "average",
                    output_dir = output_dir,
                    version = "comb_rfor_xgb_avg"
                )
            },
            regexp = "recovery mode: data already exists. To produce new data, change output_dir or version"
        )
    })
    if (doc_mode)
        Sys.setenv("SITS_DOCUMENTATION_MODE" = TRUE)
    # combine predictions
    uncert_rfor <- sits_uncertainty(
        cube = probs_rfor_cube,
        output_dir = output_dir,
        version = "uncert-rfor"
    )
    uncert_xgboost <- sits_uncertainty(
        cube = probs_xgb_cube,
        output_dir = output_dir,
        version = "uncert-xgb"
    )
    uncert_cubes <- list(uncert_rfor, uncert_xgboost)

    comb_probs_cube_uncert <- sits_combine_predictions(
        cubes = pred_cubes,
        type = "uncertainty",
        uncert_cubes = uncert_cubes,
        output_dir = output_dir,
        version = "comb_rfor_xgb_uncert"
    )
    expect_equal(
        sits_labels(comb_probs_cube_uncert),
        sits_labels(probs_xgb_cube)
    )
    expect_equal(
        sits_bbox(comb_probs_cube_uncert),
        sits_bbox(probs_xgb_cube)
    )
    expect_equal(
        nrow(comb_probs_cube_uncert),
        nrow(probs_xgb_cube)
    )

    unlink(probs_rfor_cube$file_info[[1]]$path)
    unlink(probs_xgb_cube$file_info[[1]]$path)
    unlink(uncert_rfor$file_info[[1]]$path)
    unlink(uncert_xgboost$file_info[[1]]$path)
    unlink(comb_probs_cube_avg$file_info[[1]]$path)
    unlink(comb_probs_cube_uncert$file_info[[1]]$path)
})
