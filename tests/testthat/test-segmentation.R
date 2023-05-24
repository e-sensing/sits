test_that("Segmentation", {
    # Example of classification of a data cube
    # create a data cube from local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir
    )
    # segment the image
    segments <- sits_supercells(
        cube = cube,
        tile = "012010",
        bands = "NDVI",
        date = sits_timeline(cube)[1],
        step = 10
    )
    sf_seg <- segments[[1]]

    bbox <- sits_bbox(cube)
    expect_true(all(sf_seg$x > bbox[["xmin"]]))
    expect_true(all(sf_seg$x < bbox[["xmax"]]))
    expect_true(all(sf_seg$y > bbox[["ymin"]]))
    expect_true(all(sf_seg$y < bbox[["ymax"]]))
    samples <- sits_get_data(
        cube = cube,
        samples = segments
    )
    expect_true(all(samples$polygon_id %in% c(1:nrow(samples))))
    expect_equal(nrow(samples$time_series[[1]]), 12)
    # test sits_segments
    segments2 <- sits_segment(
        cube = cube,
        tile = "012010",
        bands = "NDVI",
        date = sits_timeline(cube)[1],
        seg_fn = sits_slic(step = 10)
    )
    sf_seg2 <- segments2[[1]]

    bbox <- sits_bbox(cube)
    expect_true(all(sf_seg2$x > bbox[["xmin"]]))
    expect_true(all(sf_seg2$x < bbox[["xmax"]]))
    expect_true(all(sf_seg2$y > bbox[["ymin"]]))
    expect_true(all(sf_seg2$y < bbox[["ymax"]]))
    samples <- sits_get_data(
        cube = cube,
        samples = segments2
    )
    expect_equal(nrow(samples$time_series[[1]]), 12)

    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
    # get the average value per segment
    # classify the segments
    seg_class <- sits_classify(
        data = samples,
        ml_model = rfor_model
    )
    # add a column to the segments by class
    sf_seg <- sits_join_segments(
        data = seg_class,
        segments = segments
    )
    sf_obj <- sf_seg[[1]]
    plot(sf_obj["class"])
    sf_obj <- sf_obj |>
        dplyr::group_by(class) |>
        dplyr::summarise()
    plot(sf_obj["class"])
})
