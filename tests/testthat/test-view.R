test_that("View", {
    v <- sits_view(cerrado_2classes)
    expect_true("leaflet" %in% class(v))
    expect_error(
        sits_view(cerrado_2classes,
                  legend = c("Cerrado" = "green"))
    )
    expect_error(
        .view_set_max_mb(1024)
    )

    # create a data cube
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    modis_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        progress = FALSE
    )
    timeline <- sits_timeline(modis_cube)

    # view the data cube
    v2 <- sits_view(modis_cube,
        band = "NDVI",
        dates = timeline[[1]],
        palette = "RdYlGn"
    )
    expect_true("leaflet" %in% class(v2))
    expect_true(grepl("EPSG3857", v2$x$options$crs$crsClass))
    expect_equal(v2$x$calls[[6]]$args[[2]], "012010 2013-09-14")

    # view the data cube RGB
    vrgb <- sits_view(modis_cube,
        red = "NDVI",
        green = "NDVI",
        blue = "NDVI"
    )
    expect_true("leaflet" %in% class(vrgb))
    expect_true(grepl("EPSG3857", vrgb$x$options$crs$crsClass))
    expect_equal(vrgb$x$calls[[6]]$args[[2]], "012010 2013-09-14")

    # create a probs cube
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

    # create a class cube
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

    # view false color data cube and class cube together
    v4 <- sits_view(modis_cube,
        band = "NDVI",
        class_cube = modis_label,
        dates = timeline[[1]]
    )
    expect_true(grepl("EPSG3857", v4$x$options$crs$crsClass))
    expect_equal(v4$x$calls[[1]]$method, "addProviderTiles")

    # view RGB data cube and class cube together
    v4rgb <- sits_view(modis_cube,
        red = "NDVI",
        green = "NDVI",
        blue = "NDVI",
        dates = timeline[[1]],
        class_cube = modis_label
    )
    expect_true(grepl("EPSG3857", v4rgb$x$options$crs$crsClass))
    expect_equal(v4rgb$x$calls[[1]]$method, "addProviderTiles")

    # create uncert cube
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

    # view uncert cube and class cube
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
    v_segs <- sits_view(modis_cube,
                    band = "NDVI",
                    dates = sits_timeline(modis_cube)[[1]],
                    palette = "RdYlGn",
                    segments = segments
    )
    expect_equal(v_segs$x$calls[[5]]$args[[5]], "012010 2013-09-14")
    expect_equal(v_segs$x$calls[[6]]$args[[3]], "segments")

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

    cbers_cube <- tryCatch(
        {
            sits_cube(
                source = "BDC",
                collection = "CBERS-WFI-16D",
                bands = c("B13", "B15", "B16"),
                tiles = c("007004", "007005"),
                start_date = "2018-09-01",
                end_date = "2018-09-28",
                progress = FALSE
            )
        },
        error = function(e) {
            return(NULL)
        }
    )

    testthat::skip_if(purrr::is_null(cbers_cube),
                      message = "BDC is not accessible"
    )
    v8 <- sits_view(cbers_cube,
                    tiles = c("007004", "007005"),
                    red = "B15",
                    green = "B16",
                    blue = "B13",
                    dates = "2018-08-29")

    expect_equal(v8$x$options$crs$crsClass, "L.CRS.EPSG3857")
    expect_equal(v8$x$calls[[1]]$args[[1]], "GeoportailFrance.orthos")
    expect_equal(v8$x$calls[[5]]$method, "addRasterImage")
    expect_equal(v8$x$calls[[6]]$args[[5]], "007005 2018-08-29")

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
