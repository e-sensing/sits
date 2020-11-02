context("Clouds")

test_that("Finding clouds in CBERS-4 images", {
    # Create a raster cube based on CBERS data
    data_dir <- system.file("extdata/raster/cbers", package = "sits")

    # create a raster cube file based on the information about the files
    cbers_cube <- sits_cube(name       = "022024",
                             satellite  = "CBERS-4",
                             sensor     = "AWFI",
                             resolution = "64m",
                             data_dir   = data_dir,
                             delim      = "_",
                             parse_info = c("X1", "X2", "band", "date"))

    cbers_022024_no_cld <- sits_cloud_remove(cube = cbers_cube,
                                              data_dir = tempdir(),
                                              name = "cbers_022024_no_cld")

    expect_true(all(sits_bands(cbers_022024_no_cld) %in% c("B13", "B14", "B15", "B16")))
    rast <- suppressWarnings(terra::rast(cbers_022024_no_cld$file_info[[1]]$path[1]))
    expect_true(rast@ptr$range_max[1] < 10000)
    expect_true(rast@ptr$range_min[1] > 0)
})
