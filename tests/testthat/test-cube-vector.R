test_that("Local vector cube", {
    # --- Create a cube based on a local MODIS data
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
        seg_fn = sits_snic(
            grid_seeding = "hexagonal",
            spacing = 10,
            compactness = 0.5,
            padding = 2
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
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())

    # n_sam_pol should trigger a deprecation warning
    expect_warning(
        sits_classify(
            data = segs_cube,
            ml_model = rfor_model,
            output_dir = tempdir(),
            n_sam_pol = 10
        ),
        regexp = "n_sam_pol.*deprecated"
    )

    # classify without deprecated param
    probs_cube <- sits_classify(
        data = segs_cube,
        ml_model = rfor_model,
        output_dir = tempdir(),
        version = "v2"
    )

    # output should have combined class chain:
    # probs_vector_cube + probs_cube (raster-first OBIA)
    expect_true("probs_vector_cube" %in% class(probs_cube))
    expect_true("probs_cube" %in% class(probs_cube))
    expect_true("vector_cube" %in% class(probs_cube))
    expect_true("raster_cube" %in% class(probs_cube))
    # vector_info must be preserved
    expect_true("vector_info" %in% colnames(probs_cube))

    # label the segments using segment-based aggregation
    # should emit deprecation warning
    expect_warning(
        class_cube <- sits_label_classification(
            cube = probs_cube,
            output_dir = tempdir()
        ),
        regexp = "deprecated"
    )

    # output should have combined class chain:
    # class_vector_cube + class_cube
    expect_true("class_vector_cube" %in% class(class_cube))
    expect_true("class_cube" %in% class(class_cube))
    expect_true("vector_cube" %in% class(class_cube))
    expect_true("raster_cube" %in% class(class_cube))
    # vector_info must be preserved
    expect_true("vector_info" %in% colnames(class_cube))
})
