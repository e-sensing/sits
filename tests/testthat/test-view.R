test_that("View", {
    v <- sits_view(cerrado_2classes)
    expect_true("leaflet" %in% class(v))
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")

    modis_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        progress = FALSE
    )

    timeline <- sits_timeline(modis_cube)

    # plot the data cube
    v2 <- sits_view(modis_cube,
        band = "NDVI",
        dates = timeline[[1]],
        palette = "RdYlGn"
    )

    expect_true("leaflet" %in% class(v2))
    expect_true(grepl("EPSG3857", v2$x$options$crs$crsClass))
    expect_equal(v2$x$calls[[6]]$args[[2]], "012010 2013-09-14")

    # plot the data cube RGB
    vrgb <- sits_view(modis_cube,
        red = "NDVI",
        green = "NDVI",
        blue = "NDVI",
        dates = timeline[[1]]
    )
    expect_true("leaflet" %in% class(vrgb))
    expect_true(grepl("EPSG3857", vrgb$x$options$crs$crsClass))
    expect_equal(vrgb$x$calls[[6]]$args[[2]], "012010 2013-09-14")

    rf_model <- sits_train(samples_modis_ndvi, sits_rfor())

    modis_probs <- sits_classify(
        data = modis_cube,
        ml_model = rf_model,
        output_dir = tempdir(),
        memsize = 4,
        multicores = 1,
        progress = FALSE,
        version = "v_view"
    )
    v2_probs <- sits_view(modis_probs)
    expect_true("leaflet" %in% class(v2_probs))
    expect_true(grepl("EPSG3857", v2_probs$x$options$crs$crsClass))
    expect_equal(v2_probs$x$calls[[6]]$args[[6]], "probs Forest")

    modis_label <- sits_label_classification(modis_probs,
        output_dir = tempdir(),
        progress = FALSE
    )

    v3 <- sits_view(modis_label)
    expect_true(grepl("EPSG3857", v3$x$options$crs$crsClass))
    expect_true(
        all(v3$x$calls[[6]]$args[[1]]$labels %in% c(
            "Cerrado", "Pasture",
            "Forest", "Soy_Corn"
        ))
    )
    v3_probs <- sits_view(modis_probs, class_cube = modis_label)
    expect_true(grepl("EPSG3857", v3_probs$x$options$crs$crsClass))
    expect_equal(v3_probs$x$calls[[6]]$args[[6]], "probs Forest")

    v4 <- sits_view(modis_cube,
        band = "NDVI",
        class_cube = modis_label,
        dates = timeline[[1]]
    )
    expect_true(grepl("EPSG3857", v4$x$options$crs$crsClass))
    expect_equal(v4$x$calls[[1]]$method, "addProviderTiles")

    v4rgb <- sits_view(modis_cube,
        red = "NDVI",
        green = "NDVI",
        blue = "NDVI",
        dates = timeline[[1]],
        class_cube = modis_label
    )

    expect_true(grepl("EPSG3857", v4rgb$x$options$crs$crsClass))
    expect_equal(v4rgb$x$calls[[1]]$method, "addProviderTiles")

    modis_uncert <- sits_uncertainty(
        cube = modis_probs,
        output_dir = tempdir(),
        memsize = 4,
        multicores = 1
    )
    v5 <- sits_view(modis_uncert)
    expect_true(grepl("EPSG3857", v5$x$options$crs$crsClass))
    expect_equal(v5$x$calls[[1]]$method, "addProviderTiles")
    expect_equal(v5$x$calls[[6]]$args[[2]], "012010 entropy")

    v6 <- sits_view(modis_uncert, class_cube = modis_label)

    expect_true(grepl("EPSG3857", v6$x$options$crs$crsClass))
    expect_equal(v6$x$calls[[1]]$method, "addProviderTiles")
    expect_equal(v6$x$calls[[1]]$args[[1]], "GeoportailFrance.orthos")
    expect_equal(v6$x$calls[[5]]$args[[5]], "012010 entropy")
    expect_equal(v6$x$calls[[6]]$args[[5]], "classification")

    # include segments
    segments <- sits_segment(
        cube = modis_cube,
        tile = "012010",
        bands = "NDVI",
        date = sits_timeline(modis_cube)[1],
        seg_fn = sits_supercells(step = 20)
    )
    samples <- sits_get_data(
        cube = modis_cube,
        samples = segments,
        progress = FALSE
    )
    # get the average value per segment
    # classify the segments
    seg_class <- sits_classify(
        data = samples,
        ml_model = rf_model,
        progress = FALSE
    )
    # add a column to the segments by class
    sf_seg <- sits_join_segments(
        data = seg_class,
        segments = segments
    )

    v7 <- sits_view(modis_cube,
                    red = "NDVI",
                    green = "NDVI",
                    blue = "NDVI",
                    dates = timeline[[1]],
                    class_cube = modis_label,
                    segments = sf_seg)

    expect_true(grepl("EPSG3857", v7$x$options$crs$crsClass))
    expect_equal(v7$x$calls[[1]]$method, "addProviderTiles")
    expect_equal(v7$x$calls[[1]]$args[[1]], "GeoportailFrance.orthos")
    expect_equal(v7$x$calls[[5]]$args[[5]], "012010 2013-09-14")
    expect_equal(v7$x$calls[[6]]$args[[5]], "classification")
    expect_equal(v7$x$calls[[7]]$method, "addPolygons")


    expect_true(all(file.remove(unlist(modis_uncert$file_info[[1]]$path))))
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
    v <- sits_view(som_map, id_neurons = c(1:5))

    expect_true(grepl("EPSG3857", v$x$options$crs$crsClass))
    expect_equal(v$x$calls[[1]]$method, "addProviderTiles")
})
