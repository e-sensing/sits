test_that("uncertainty", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")

    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    xgb_model <- sits_train(samples_modis_ndvi,
        ml_method = sits_xgboost(verbose = FALSE)
    )
    probs_cube <- sits_classify(
        cube,
        ml_model = xgb_model,
        output_dir = tempdir(),
        memsize = 4,
        multicores = 2,
        version = "xgb",
        progress = FALSE
    )

    entropy_cube <- sits_uncertainty(
        probs_cube,
        type = "entropy",
        output_dir = tempdir(),
        version = "xgb_entropy"
    )
    least_cube <- sits_uncertainty(
        probs_cube,
        type = "least",
        output_dir = tempdir(),
        version = "xgb_least"
    )
    margin_cube <- sits_uncertainty(
        probs_cube,
        type = "margin",
        output_dir = tempdir(),
        version = "xgb_margin"
    )

    e_cnames <- c(
        "source", "collection", "satellite", "sensor", "tile",
        "xmin", "xmax", "ymin", "ymax", "crs", "labels", "file_info"
    )

    expect_true(all(colnames(entropy_cube %in% e_cnames)))
    expect_true(all(colnames(least_cube %in% e_cnames)))
    expect_true(all(colnames(margin_cube %in% e_cnames)))
    expect_true(all(
        dim(entropy_cube) == dim(least_cube),
        dim(entropy_cube) == dim(margin_cube)
    ))

    entropy_fi <- entropy_cube[["file_info"]][[1]]
    least_fi <- least_cube[["file_info"]][[1]]
    margin_fi <- margin_cube[["file_info"]][[1]]

    e_cnames <- c(
        "band", "start_date", "end_date", "xmin", "ymin", "xmax",
        "ymax", "xres", "yres", "nrows", "ncols", "path"
    )

    expect_true(all(colnames(entropy_fi %in% e_cnames)))
    expect_true(all(colnames(least_fi %in% e_cnames)))
    expect_true(all(colnames(margin_fi %in% e_cnames)))
    expect_true(all(
        dim(entropy_fi) == dim(least_fi),
        dim(entropy_fi) == dim(margin_fi)
    ))

    entropy_r <- .raster_open_rast(entropy_fi[["path"]])
    expect_true(all(range(entropy_r[]) > 0))
    expect_true(range(entropy_r[])[2] > range(entropy_r[])[1])

    least_r <- .raster_open_rast(least_fi[["path"]])
    expect_true(all(range(least_r[]) >= 0))
    expect_true(range(least_r[])[2] > range(least_r[])[1])

    margin_r <- .raster_open_rast(margin_fi[["path"]])
    expect_true(all(range(margin_r[]) >= 0))
    expect_true(range(margin_r[])[2] > range(margin_r[])[1])

    expect_true(all(file.remove(unlist(probs_cube$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(entropy_cube$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(least_cube$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(margin_cube$file_info[[1]]$path))))
})
