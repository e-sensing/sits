test_that("Labels", {
    labels <- summary(samples_modis_ndvi)
    expect_true("Cerrado" %in% sits_labels(samples_modis_ndvi))
    expect_equal(sum(labels$count), 1218)
    expect_equal(labels$label[1], "Cerrado")
    expect_equal(sum(labels$prop), 1)
})
test_that("Labels from a STAC class cube", {
    # define roi
    roi <- c(
        "lon_min" = -55.80259, "lon_max" = -55.19900,
        "lat_min" = -11.80208, "lat_max" = -11.49583
    )
    # create world cover from stac
    class_cube <- .try(
        {
            sits_cube(
                source     = "TERRASCOPE",
                collection = "WORLD-COVER-2021",
                roi        = roi,
                progress   = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(class_cube),
        message = "TERRASCOPE is not accessible"
    )

    # download class cube
    class_cube <- sits_cube_copy(
        cube       = class_cube,
        roi        = roi,
        output_dir = tempdir(),
        multicores = 2,
        progress   = FALSE,
        res        = 0.000269
    )

    labels <- summary(class_cube)
    expect_true("Tree_Cover" %in% sits_labels(class_cube))
    expect_equal(labels$class[2], "Shrubland")
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
test_that("Relabel class cube from STAC", {
    # define roi
    roi <- c(
        "lon_min" = -55.80259, "lon_max" = -55.19900,
        "lat_min" = -11.80208, "lat_max" = -11.49583
    )
    # create world cover from stac
    class_cube <- .try(
        {
            sits_cube(
                source     = "TERRASCOPE",
                collection = "WORLD-COVER-2021",
                roi        = roi,
                progress   = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(purrr::is_null(class_cube),
        message = "TERRASCOPE is not accessible"
    )
    sits_labels(class_cube) <- c(
        "Class A", "Class B", "Class C", "Class D", "Class E", "Class F",
        "Class G", "Class H", "Class I", "Class J", "Class K"
    )
    expect_true("Class F" %in% sits_labels(class_cube))
    expect_true("Class D" %in% sits_labels(class_cube))
})

test_that("Models and patterns", {
    lab <- sits_patterns(cerrado_2classes) |>
        sits_labels()
    expect_true(all(lab %in% c("Cerrado", "Pasture")))

    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
    lab2 <- sits_labels(rfor_model)
    expect_true(all(lab2 %in% c("Cerrado", "Pasture", "Forest", "Soy_Corn")))
})

test_that("Relabel class_vector_cube", {
    # Get the classification result which includes a class_vector_cube
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir
    )

    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())

    # Segment the cube
    segs_cube <- sits_segment(
        cube = cube,
        output_dir = tempdir(),
        version = "v1"
    )

    # Classify the segments
    probs_segs <- sits_classify(
        data = segs_cube,
        ml_model = rfor_model,
        output_dir = tempdir(),
        version = "v1"
    )

    # Label the segments
    class_segs <- sits_label_classification(
        cube = probs_segs,
        label_method = "mean",
        version = "label",
        output_dir = tempdir()
    )

    # Original labels
    original_labels <- unname(sits_labels(class_segs))
    expect_equal(original_labels, c("Cerrado", "Forest", "Pasture", "Soy_Corn"))

    # Change labels
    new_labels <- c("Savanna", "Trees", "Grassland", "Crops")
    names(new_labels) <- c(1:4)
    sits_labels(class_segs) <- new_labels

    # Verify new labels
    updated_labels <- sits_labels(class_segs)
    expect_equal(updated_labels, new_labels)
    expect_true("Savanna" %in% updated_labels)
    expect_true("Trees" %in% updated_labels)
    expect_true("Grassland" %in% updated_labels)
    expect_true("Crops" %in% updated_labels)
})

test_that("Relabel probs_vector_cube", {
    # Get the probability result which includes a probs_vector_cube
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir
    )

    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())

    # Segment the cube
    segs_cube <- sits_segment(
        cube = cube,
        output_dir = tempdir(),
        version = "v2"
    )

    # Classify the segments (produces probs_vector_cube)
    probs_segs <- sits_classify(
        data = segs_cube,
        ml_model = rfor_model,
        output_dir = tempdir(),
        version = "v2"
    )

    # Original labels
    original_labels <- sits_labels(probs_segs)
    expect_equal(original_labels, c("Cerrado", "Forest", "Pasture", "Soy_Corn"))

    # Change labels on probs_vector_cube
    new_labels <- c("A", "B", "C", "D")
    sits_labels(probs_segs) <- new_labels

    # Verify new labels
    updated_labels <- sits_labels(probs_segs)
    expect_equal(updated_labels, new_labels)
    expect_true("A" %in% updated_labels)
    expect_true("B" %in% updated_labels)
    expect_true("C" %in% updated_labels)
    expect_true("D" %in% updated_labels)
})

test_that("Label probs_vector_cube with mean, median and majority", {
    # Load cube
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir
    )
    # Train model
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
    # Segment
    segs_cube <- sits_segment(
        cube = cube,
        output_dir = tempdir(),
        version = "methods"
    )
    # Classify
    probs_segs <- sits_classify(
        segs_cube, rfor_model,
        output_dir = tempdir(),
        version = "methods"
    )
    # Define expected labels
    expected_labels <- c("Cerrado", "Forest", "Pasture", "Soy_Corn")
    # Test label methods
    purrr::map(c("mean", "median", "majority"), function(method) {
        # Label classification
        class_segs <- sits_label_classification(
            cube = probs_segs,
            label_method = method,
            version = method,
            output_dir = tempdir()
        )
        # Class must be correct
        expect_s3_class(class_segs, "class_vector_cube")
        # Labels must be the expected ones
        expect_equal(unname(sits_labels(class_segs)), expected_labels)
    })
})
