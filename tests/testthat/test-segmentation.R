test_that("Segmentation", {
    # Example of classification of a data cube
    # Create a data cube from local files
    set.seed(29031956)
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        progress = FALSE
    )
    # create output dir
    output_dir <- paste0(tempdir(), "/seg")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
    # Segment the cube
    segments <- sits_segment(
        cube = cube,
        output_dir = output_dir,
        multicores = 2,
        memsize = 24,
        progress = FALSE,
        version = "vt"
    )

    expect_s3_class(object = segments, class = "vector_cube")
    expect_true("vector_info" %in% colnames(segments))
    # Read segments as sf object
    vector_segs <- .segments_read_vec(segments)
    expect_equal(
        as.character(unique(sf::st_geometry_type(vector_segs))),
        expected = "POLYGON"
    )
    vector_obj <- .vector_open_vec(segments$vector_info[[1]]$path)

    expect_true("sf" %in% class(vector_obj))

    crs_wkt <- .vector_crs(vector_obj, wkt = TRUE)
    expect_equal(class(crs_wkt), "character")
    expect_true(grepl("PROJCRS", crs_wkt))

    crs_nowkt <- .vector_crs(vector_obj, wkt = FALSE)
    expect_equal(class(crs_nowkt), "crs")
    expect_true(grepl("PROJCRS", crs_nowkt$wkt))

    p1 <- plot(segments)
    expect_equal(p1[[1]]$shp_name, "stars_obj")
    expect_equal(p1$tm_grid$grid.projection, 4326)
    expect_equal(p1$tm_layout$legend.bg.alpha, 0.5)

    # test resume feature
    # testing resume feature
    out <- capture_messages({
        expect_message(
            object = {
                sits_segment(
                    cube = cube,
                    output_dir = output_dir,
                    multicores = 1,
                    memsize = 24,
                    progress = FALSE,
                    version = "vt"
                )
            },
            regexp = "Recovery: "
        )
    })

    # test read vector cube
    segment_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        vector_dir = output_dir,
        vector_band = "segments",
        version = "vt",
        progress = FALSE
    )
    expect_s3_class(object = segment_cube, class = "vector_cube")
    expect_true("vector_info" %in% colnames(segment_cube))


    # Train a rf model
    rf_model <- sits_train(samples_modis_ndvi, ml_method = sits_rfor)
    # Create a probability vector cube
    probs_segs <- sits_classify(
        data = segments,
        ml_model = rf_model,
        output_dir = output_dir,
        n_sam_pol = 20,
        multicores = 6,
        memsize = 24,
        version = "vt2"
    )
    p2 <- plot(probs_segs)
    expect_equal(p2$tm_shape$shp_name, "sf_seg")
    expect_equal(ncol(p2$tm_shape$shp), 9)

    expect_s3_class(probs_segs, class = "probs_vector_cube")
    expect_true(
        "vector_info" %in% colnames(probs_segs)
    )
    # Read segments of a probability cube
    vector_probs <- .segments_read_vec(probs_segs)
    expect_true(
        all(sits_labels(probs_segs) %in% colnames(vector_probs))
    )
    # test resume feature
    # testing resume feature
    out2 <- capture_messages({
        expect_message(
            object = {
                sits_classify(
                    data = segments,
                    ml_model = rf_model,
                    output_dir = output_dir,
                    n_sam_pol = 20,
                    multicores = 6,
                    memsize = 24,
                    version = "vt2"
                )
            },
            regexp = "Recovery: "
        )
    })
    # Create a classified vector cube
    class_segs <- sits_label_classification(
        cube = probs_segs,
        output_dir = output_dir,
        multicores = 2,
        memsize = 4
    )
    expect_s3_class(object = class_segs, class = "class_vector_cube")
    expect_true(
        "vector_info" %in% colnames(class_segs)
    )
    # Read segments of a classified cube
    vector_class <- .segments_read_vec(class_segs)
    expect_equal(nrow(vector_probs), nrow(vector_class))
    expect_true(all(sits_labels(rf_model) %in% colnames(vector_probs)))
    expect_true(all(sits_labels(rf_model) %in% colnames(vector_class)))
    expect_true(
        "class" %in% colnames(vector_class)
    )
    p3 <- plot(class_segs)
    expect_equal(p3$tm_shape$shp_name, "sf_seg")
    expect_equal(ncol(p3$tm_shape$shp), 2)
    expect_equal(p2$tm_compass$compass.show.labels, 1)

    # test resume feature
    # testing resume feature
    out3 <- capture_messages({
        expect_message(
            object = {
                sits_label_classification(
                    cube = probs_segs,
                    output_dir = output_dir,
                    multicores = 2,
                    memsize = 4
                )
            },
            regexp = "Recovery: "
        )
    })

    uncert_vect <- sits_uncertainty(probs_segs,
                                    output_dir = output_dir)

    p4 <- plot(uncert_vect)
    expect_equal(p4$tm_shape$shp_name, "sf_seg")

    sf_uncert <- .segments_read_vec(uncert_vect)
    expect_true("entropy" %in% colnames(sf_uncert))
    expect_equal(nrow(sf_uncert), nrow(vector_class))
    expect_true(all(sits_labels(rf_model) %in% colnames(sf_uncert)))
})
test_that("Segmentation of large files",{

    modis_cube <- .try(
        {
            sits_cube(
                source = "BDC",
                collection = "MOD13Q1-6",
                bands = c("NDVI", "CLOUD"),
                tiles = "012010",
                start_date = "2018-09-14",
                end_date = "2019-08-29",
                progress = FALSE
            )
        },
        .default = NULL
    )
    output_dir <- paste0(tempdir(), "/segs")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
    modis_cube_local <- sits_regularize(
        cube = modis_cube,
        period = "P1M",
        res = 1000,
        multicores = 6,
        output_dir = output_dir
    )
    segments <- sits_segment(
        cube = modis_cube_local,
        seg_fn = sits_slic(
            step = 50,
            iter = 10,
            minarea = 100
        ),
        output_dir = output_dir,
        multicores = 4,
        memsize = 16,
        progress = TRUE,
        version = "res1000-step50-iter10-minarea100-m4"
    )
    # Train a rf model
    rf_model <- sits_train(samples_modis_ndvi, ml_method = sits_rfor)
    probs_segs <- sits_classify(
        data = segments,
        ml_model = rf_model,
        output_dir = output_dir,
        n_sam_pol = 10,
        multicores = 6,
        memsize = 24,
        version = "res1000"
    )

})
