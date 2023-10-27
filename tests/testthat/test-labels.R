test_that("Labels", {
    labels <- summary(samples_modis_ndvi)
    expect_true("Cerrado" %in% sits_labels(samples_modis_ndvi))
    expect_equal(sum(labels$count), 1218)
    expect_equal(labels$label[1], "Cerrado")
    expect_equal(sum(labels$prop), 1)
})

test_that("Relabel", {
    # copy result
    new_data <- samples_modis_ndvi
    sits_labels(new_data)

    sits_labels(new_data) <- c("Cerrado", "Forest", "Pasture", "Cropland")

    sum <- summary(new_data)

    expect_true("Cropland" %in% sits_labels(new_data))
    expect_equal(length(sum$label), 4)
    expect_equal(sum$label[1], "Cerrado")
    expect_equal(sum(sum$prop), 1)
})

test_that("Relabel cubes", {
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
            "1" = "ClearCut_Fire", "2" = "ClearCut_BareSoil",
            "3" = "ClearCut_Veg", "4" = "Forest"
        ),
        progress = FALSE
    )
    sits_labels(ro_class) <- c(
        "Queimadas", "Solo Exposto",
        "Vegetacao", "Floresta"
    )
    expect_true("Queimadas" %in% sits_labels(ro_class))
    expect_true("Floresta" %in% sits_labels(ro_class))
})

test_that("Models and patterns", {
    lab <- sits_patterns(cerrado_2classes) |>
        sits_labels()
    expect_true(all(lab %in% c("Cerrado", "Pasture")))

    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
    lab2 <- sits_labels(rfor_model)
    expect_true(all(lab2 %in% c("Cerrado", "Pasture", "Forest", "Soy_Corn")))
})
