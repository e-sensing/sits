test_that("One-year, multicores processing reclassify", {
    # Open mask map
    data_dir <- system.file("extdata/raster/prodes", package = "sits")
    prodes2021 <- sits_cube(
        source = "USGS",
        collection = "LANDSAT-C2L2-SR",
        data_dir = data_dir,
        parse_info = c("X1", "X2", "tile", "start_date", "end_date",
                       "band", "version"),
        bands = "class",
        version = "v20220606",
        labels = c("Forest", "Water", "NonForest",
                   "NonForest2", "NoClass", "d2007", "d2008",
                   "d2009", "d2010", "d2011", "d2012",
                   "d2013", "d2014", "d2015", "d2016",
                   "d2017", "d2018", "r2010", "r2011",
                   "r2012", "r2013", "r2014", "r2015",
                   "r2016", "r2017", "r2018", "d2019",
                   "r2019", "d2020", "NoClass", "r2020",
                   "Clouds2021", "d2021", "r2021")
    )
    # Open classification map
    data_dir <- system.file("extdata/raster/classif", package = "sits")
    ro_class <- sits_cube(
        source = "MPC",
        collection = "SENTINEL-2-L2A",
        data_dir = data_dir,
        parse_info = c("X1", "X2", "tile", "start_date", "end_date",
                       "band", "version"),
        bands = "class",
        labels = c("ClearCut_Fire", "ClearCut_BareSoil",
                   "ClearCut_Veg", "Forest")
    )
    # Reclassify cube
    ro_mask <- sits_reclassify(
        cube = ro_class,
        mask = prodes2021,
        rules = list(
            "Old_Deforestation" = mask %in% c(
                "d2007", "d2008", "d2009",
                "d2010", "d2011", "d2012",
                "d2013", "d2014", "d2015",
                "d2016", "d2017", "d2018",
                "r2010", "r2011", "r2012",
                "r2013", "r2014", "r2015",
                "r2016", "r2017", "r2018",
                "d2019", "r2019", "d2020",
                "r2020", "r2021"
            ),
            "Water_Mask" = mask == "Water",
            "NonForest_Mask" = mask %in% c("NonForest", "NonForest2")
        ),
        memsize = 4,
        multicores = 2,
        output_dir = tempdir()
    )

    expect_equal(
        sits_labels(ro_mask),
        c("ClearCut_Fire", "ClearCut_BareSoil",
          "ClearCut_Veg", "Forest", "Old_Deforestation",
          "Water_Mask", "NonForest_Mask")
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
    expect_equal(vls_prodes2021[2000], 17)
    # ro_class is "Old_Deforestation"
    expect_equal(vls_ro_mask[2000], 5)

    documentation <- FALSE
    if (Sys.getenv("SITS_DOCUMENTATION_MODE") == "true") {
        documentation <- TRUE
        Sys.setenv("SITS_DOCUMENTATION_MODE" = "false")
    }
    out <- capture_messages({
        expect_message(
            object = { sits_reclassify(
                cube = ro_class,
                mask = prodes2021,
                rules = list(
                    "Old_Deforestation" = mask %in% c(
                        "d2007", "d2008", "d2009",
                        "d2010", "d2011", "d2012",
                        "d2013", "d2014", "d2015",
                        "d2016", "d2017", "d2018",
                        "r2010", "r2011", "r2012",
                        "r2013", "r2014", "r2015",
                        "r2016", "r2017", "r2018",
                        "d2019", "r2019", "d2020",
                        "r2020", "r2021"
                    ),
                    "Water_Mask" = mask == "Water",
                    "NonForest_Mask" = mask %in% c("NonForest", "NonForest2")
                ),
                memsize = 4,
                multicores = 2,
                output_dir = tempdir()
            )},
            regexp = "Recovery: "
        )
    })
    if (documentation) {
        Sys.setenv("SITS_DOCUMENTATION_MODE" = "true")
    }
    expect_true(grepl("output_dir", out[1]))

    unlink(ro_mask$file_info[[1]]$path)
})
