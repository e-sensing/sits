context("Cube")
test_that("Reading a raster cube", {

    file <- c(system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif",
                          package = "sits"))
    raster_cube <- sits_cube(name = "Sinop-crop",
                             timeline = sits::timeline_modis_392,
                             bands = c("ndvi"),
                             satellite = "TERRA",
                             sensor = "MODIS",
                             files = file)

    # get bands names
    bands <- sits:::.sits_cube_bands(raster_cube)
    expect_true(bands %in% c("NDVI"))

    params <- sits:::.sits_raster_api_params(raster_cube$file_info[[1]]$path)
    expect_true(params$nrows == 11)
    expect_true(params$ncols == 14)
    expect_true(params$xres >= 231.5)


})

test_that("Creating a raster stack cube and selecting bands", {
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

    expect_true(all(sits_bands(cbers_cube) %in% c("B13", "B14", "B15", "B16", "CMASK")))
    rast <- suppressWarnings(terra::rast(cbers_cube$file_info[[1]]$path[1]))
    expect_true(terra::nrow(rast) == cbers_cube[1,]$nrows)
    expect_true(all(unique(cbers_cube$file_info[[1]]$date) == cbers_cube$timeline[[1]][[1]]))

    cbers_cube_B13 <- sits_select(cbers_cube, bands = "B13")
    expect_true(all(sits_bands(cbers_cube_B13) == c("B13")))
})

test_that("Creating, merging cubes from BDC",{
    testthat::skip_on_cran()
    # create a raster cube file based on the information about the files
    cbers_022024_ndvi <- sits_cube(type      = "BDC",
                                 name        = "cbers_022024_ndvi",
                                 bands       = "NDVI",
                                 tiles       = "022024",
                                 url         = "http://brazildatacube.dpi.inpe.br/stac/",
                                 collection  = "CB4_64_16D_STK-1",
                                 start_date  = "2018-09-01",
                                 end_date    = "2019-08-28")

    if (purrr::is_null(cbers_022024_ndvi))
        skip("BDC is not accessible")

    cbers_022024_evi <- sits_cube(type         = "BDC",
                                   name        = "cbers_022024_evi",
                                   bands       = "EVI",
                                   tiles       = "022024",
                                   url         = "http://brazildatacube.dpi.inpe.br/stac/",
                                   collection  = "CB4_64_16D_STK-1",
                                   start_date  = "2018-09-01",
                                   end_date    = "2019-08-28")

    cbers_merge <- sits_merge(cbers_022024_ndvi, cbers_022024_evi)
    expect_true(all(sits_bands(cbers_merge) %in% c("NDVI", "EVI")))
    expect_true(all(sits_timeline(cbers_merge) ==  sits_timeline(cbers_022024_ndvi)))
})
test_that("Cube copy", {
    data_dir <- system.file("extdata/raster/cbers", package = "sits")

    cbers_022024 <- sits_cube(type = "RASTER",
                              name = "cbers_022024",
                              satellite = "CBERS-4",
                              sensor = "AWFI",
                              resolution = "64m",
                              data_dir = data_dir,
                              parse_info = c("X1", "X2","band", "date"))

    cbers_022024_copy <- sits_cube_copy(cbers_022024, name = "cb_022024_cp",
                                        dest_dir = tempdir(),
                                        bands = "B13",
                                        srcwin = c(0, 0, 25, 25))
    expect_true(sits_bands(cbers_022024_copy) == "B13")
    expect_true(cbers_022024_copy$ncols == 25)
    expect_true(cbers_022024_copy$xmin == cbers_022024$xmin)
    expect_true(all(sits_timeline(cbers_022024_copy) == sits_timeline(cbers_022024)))
})
