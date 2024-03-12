test_that("One-year, multicores processing reclassify", {
    # Open mask map
    data_dir <- system.file("extdata/raster/prodes", package = "sits")
    prodes2021 <- sits_cube(
        source = "USGS",
        collection = "LANDSAT-C2L2-SR",
        data_dir = data_dir,
        parse_info = c(
            "X1", "X2", "tile", "start_date", "end_date",
            "band", "version"
        ),
        bands = "class",
        version = "v20220606",
        labels = c("1" = "Forest", "11" = "d2012", "16" = "d2017",
                   "17" = "d2018", "27" = "d2019", "29" = "d2020",
                   "32" = "Clouds2021", "33" = "d2021"),
        progress = FALSE
    )
    # Open classification map
    data_dir <- system.file("extdata/raster/classif", package = "sits")
    ro_class <- sits_cube(
        source = "MPC",
        collection = "SENTINEL-2-L2A",
        data_dir = data_dir,
        parse_info = c(
            "X1", "X2", "tile", "start_date", "end_date",
            "band", "version"
        ),
        bands = "class",
        labels = c(
            "1" = "ClearCut_Fire", "2" = "ClearCut_Soil",
            "3" = "ClearCut_Veg", "4" = "Forest"
        ),
        progress = FALSE
    )
    # Reclassify cube
    ro_mask <- sits_reclassify(
        cube = ro_class,
        mask = prodes2021,
        rules = list(
            "Old_Deforestation" = mask %in% c(
                "d2012", "d2017", "d2018",
                "d2019", "d2020", "d2021"
            )
        ),
        memsize = 4,
        multicores = 2,
        output_dir = tempdir(),
        version = "reclass"
    )

    expect_equal(
        sits_labels(ro_mask),
        c(
            "1" = "ClearCut_Fire", "2" =  "ClearCut_Soil",
            "3" =  "ClearCut_Veg", "4" = "Forest",
            "5" = "Old_Deforestation"
        )
    )
    ro_class_obj <- .raster_open_rast(.tile_path(ro_class))
    prodes2021_obj <- .raster_open_rast(.tile_path(prodes2021))
    ro_mask_obj <- .raster_open_rast(.tile_path(ro_mask))

    vls_ro_class <- terra::values(ro_class_obj)
    vls_prodes2021 <- terra::values(prodes2021_obj)
    vls_ro_mask <- terra::values(ro_mask_obj)

    # ro_class is "ClearCut_Veg"
    expect_equal(vls_ro_class[2000], 3)
    # prodes2021 is ""d2018"
    expect_equal(vls_prodes2021[2000], 1)
    # ro_class is "Old_Deforestation"
    expect_equal(vls_ro_mask[2000], 3)

    out <- capture_messages({
        expect_message(
            object = {
                sits_reclassify(
                    cube = ro_class,
                    mask = prodes2021,
                    rules = list(
                        "Old_Deforestation" = mask %in% c(
                            "d2012", "d2017", "d2018",
                            "d2019", "d2020", "d2021"
                        )
                    ),
                    memsize = 4,
                    multicores = 2,
                    output_dir = tempdir(),
                    version = "reclass"
                )
            },
            regexp = "Recovery: "
        )
    })

    expect_true(grepl("output_dir", out[1]))

    unlink(ro_mask$file_info[[1]]$path)
})

test_that("One-year, reclassify different rules", {
    rf_model <- sits_train(samples_modis_ndvi, ml_method = sits_rfor)

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")

    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir
    )

    probs_cube <- sits_classify(
        data = cube,
        ml_model = rf_model,
        output_dir = tempdir(),
        version = "ex_classify",
        multicores = 2,
        memsize = 4
    )

    label_cube <- sits_label_classification(
        probs_cube,
        output_dir = tempdir(),
        multicores = 2,
        memsize = 4
    )

    reclass <- sits_reclassify(
        cube = label_cube,
        mask = label_cube,
        rules = list(
            Cerrado = mask %in% c("Pasture", "Cerrado")
        ),
        output_dir = tempdir(),
        version = "reclass",
        multicores = 2,
        memsize = 4
    )
    expect_error({
        sits_reclassify(
            cube = probs_cube,
            mask = label_cube,
            rules = list(
                Cerrado = mask %in% c("Pasture", "Cerrado")
            ),
            output_dir = tempdir(),
            version = "reclass",
            multicores = 2,
            memsize = 4
        )
    })

    expect_equal(
        object = sits_labels(reclass),
        expected = c("1" =  "Cerrado", "2" = "Forest", "4" = "Soy_Corn")
    )

    reclassv2 <- sits_reclassify(
        cube = label_cube,
        mask = label_cube,
        rules = list(
            CerradoNew = mask %in% c("Pasture", "Cerrado")
        ),
        output_dir = tempdir(),
        version = "v2",
        multicores = 2,
        memsize = 4
    )

    expect_equal(
        object = sits_labels(reclassv2),
        expected = c("2" = "Forest", "4" = "Soy_Corn", "5" = "CerradoNew")
    )

    unlink(reclassv2$file_info[[1]]$path)
    unlink(reclass$file_info[[1]]$path)
    unlink(label_cube$file_info[[1]]$path)
    unlink(probs_cube$file_info[[1]]$path)
})
