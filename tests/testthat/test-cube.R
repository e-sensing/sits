context("Cube")
test_that("Reading a raster cube", {
    #skip_on_cran()
    file <- c(system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif",
                          package = "sits"))
    raster_cube <- sits_cube(type = "BRICK",
                             name = "Sinop-crop",
                             timeline = sits::timeline_modis_392,
                             bands = c("ndvi"),
                             satellite = "TERRA",
                             sensor = "MODIS",
                             files = file)


    # get cube object
    cub.obj <- sits:::.sits_cube_robj(raster_cube)
    expect_true("RasterBrick" %in% class(cub.obj))
    # get bands names
    bands <- sits:::.sits_cube_bands(raster_cube)
    expect_true(bands %in% c("ndvi"))

    params <- sits:::.sits_raster_params(sits:::.sits_cube_robj(raster_cube))
    expect_true(params$nrows == 11)
    expect_true(params$ncols == 14)
    expect_true(params$xres >= 231.5)
    expect_true(grepl("sinu", params$crs))


})
