test_that("Local vector cube", {
    # --- Create a cube based on a local MODIS data
    # MODIS local files have names such as
    # "TERRA_MODIS_012010_NDVI_2013-09-14.jp2"
    # see the parse info parameter as an example on how to
    # decode local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    modis_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        parse_info = c("satellite", "sensor", "tile", "band", "date")
    )
    # segment the vector cube
    segs_cube <- sits_segment(
        cube = modis_cube,
        seg_fn = sits_slic(
            step = 10,
            compactness = 1,
            dist_fun = "euclidean",
            avg_fun = "median",
            iter = 30,
            minarea = 10
        ),
        output_dir = tempdir()
    )

    # recover the local segmented cube
    local_segs_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        raster_cube = modis_cube,
        vector_dir = tempdir(),
        vector_band = "segments"
    )
    # check
    expect_true(all(c("segs_cube", "vector_cube") %in% class(local_segs_cube)))

    # classify the segments
    # create a random forest model
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
    probs_vector_cube <- sits_classify(
        data = segs_cube,
        ml_model = rfor_model,
        output_dir = tempdir(),
        n_sam_pol = 10
    )

    # recover vector cube
    local_probs_vector_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        raster_cube = modis_cube,
        vector_dir = tempdir(),
        vector_band = "probs"
    )
    expect_true(all(c("probs_vector_cube", "segs_cube", "vector_cube")
                    %in% class(local_probs_vector_cube)))

    # label the segments
    class_vector_cube <- sits_label_classification(
        cube = probs_vector_cube,
        output_dir = tempdir(),
    )

    # recover vector cube
    local_class_vector_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        raster_cube = modis_cube,
        vector_dir = tempdir(),
        vector_band = "class"
    )
    expect_true(all(c("class_vector_cube", "segs_cube", "vector_cube")
                    %in% class(local_class_vector_cube)))
})
