test_that("View", {

    v <- sits_view(cerrado_2classes)
    expect_true("leaflet" %in% class(v))
    expect_true(all(v$x$calls[[6]]$args[[1]]$labels %in%
        c("Cerrado", "Pasture")))

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")

    modis_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir
    )

    timeline <- sits_timeline(modis_cube)

    # plot the data cube
    v2 <- sits_view(modis_cube,
        band = "NDVI",
        dates = timeline[[1]]
    )

    expect_true("leaflet" %in% class(v2))
    expect_true(grepl("EPSG3857", v2$x$options$crs$crsClass))

    # plot the data cube RGB
    vrgb <- sits_view(modis_cube,
                    red = "NDVI",
                    green = "NDVI",
                    blue = "NDVI",
                    dates = timeline[[1]]
    )
    expect_true("leaflet" %in% class(vrgb))
    expect_true(grepl("EPSG3857", vrgb$x$options$crs$crsClass))

    rf_model <- sits_train(samples_modis_ndvi, sits_rfor())

    modis_probs <- sits_classify(
        data = modis_cube,
        ml_model = rf_model,
        output_dir = tempdir(),
        memsize = 4,
        multicores = 1,
        verbose = FALSE
    )
    modis_label <- sits_label_classification(modis_probs,
        output_dir = tempdir()
    )

    v3 <- sits_view(modis_label)
    expect_true(grepl("EPSG3857", v3$x$options$crs$crsClass))
    expect_true(
        all(v3$x$calls[[6]]$args[[1]]$labels %in% c(
            "Cerrado", "Pasture",
            "Forest", "Soy_Corn"
        ))
    )


    v4 <- sits_view(modis_cube,
        band = "NDVI",
        class_cube = modis_label,
        dates = timeline[[1]]
    )

    expect_true(grepl("EPSG3857", v4$x$options$crs$crsClass))
    expect_equal(v4$x$calls[[1]]$method, "addProviderTiles")

    expect_true(all(file.remove(unlist(modis_probs$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(modis_label$file_info[[1]]$path))))
})
test_that("View SOM map", {
    set.seed(2903)
    som_map <- sits_som_map(
        samples_modis_ndvi,
        grid_xdim = 4,
        grid_ydim = 4
    )
    v <- suppressMessages(sits_view(som_map, label = "Forest",
                                    prob_min = 0))

    expect_true(grepl("EPSG3857", v$x$options$crs$crsClass))
    expect_equal(v$x$calls[[1]]$method, "addProviderTiles")
})
