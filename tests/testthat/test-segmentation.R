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
    expect_equal(nrow(samples$time_series[[1]]), 12)
})
