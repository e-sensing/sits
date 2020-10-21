context("Cube")
test_that("Reading a raster cube", {
    #skip_on_cran()
    file <- c(system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif",
                          package = "sits"))
    raster_cube <- sits_cube(name = "Sinop-crop",
                             timeline = sits::timeline_modis_392,
                             bands = c("ndvi"),
                             satellite = "TERRA",
                             sensor = "MODIS",
                             files = file)


    # get cube object
    cub.obj <- suppressWarnings(terra::rast(raster_cube$file_info[[1]]$path))
    # get bands names
    bands <- sits:::.sits_cube_bands(raster_cube)
    expect_true(bands %in% c("NDVI"))

    params <- sits:::.sits_raster_params(cub.obj)
    expect_true(params$nrows == 11)
    expect_true(params$ncols == 14)
    expect_true(params$xres >= 231.5)


})

test_that("Reading a raster stack cube", {
    # Create a raster cube based on CBERS data
    data_dir <- system.file("extdata/raster/cbers", package = "sits")

    # create a raster cube file based on the information about the files
    cbers_cube <- sits_cube(name       = "022024",
                             satellite  = "CBERS-4",
                             sensor     = "AWFI",
                             resolution = "64m",
                             data_dir   = data_dir,
                             delim      = "_",
                             parse_info = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "band", "date"))

    expect_true(all(sits_bands(cbers_cube) %in% c("B13", "B14", "B15", "B16")))
    rast <- suppressWarnings(terra::rast(cbers_cube$file_info[[1]]$path[1]))
    expect_true(terra::nrow(rast) == cbers_cube[1,]$nrows)
    expect_true(all(unique(cbers_cube$file_info[[1]]$date) == cbers_cube$timeline[[1]][[1]]))
})

test_that("Reading a BDC data cube from the web", {

    # create a raster cube file based on the information about the files
    cbers_bdc_tile <- sits_cube(type       = "BDC_TILE",
                                name       = "022024",
                                satellite  = "CBERS-4",
                                sensor     = "AWFI",
                                cube       = "CB4_64_16D_STK",
                                tiles       = "022024",
                                version    = "v001",
                                data_access = "web",
                                start_date  = as.Date("2018-08-29"),
                                end_date    = as.Date("2019-08-13"))

    bands_bdc <- unique(cbers_bdc_tile$file_info[[1]]$band)
    bands_sits <- sits:::.sits_config_band_names_convert("CBERS-4", "AWFI", bands_bdc)

    expect_true(all(sits_bands(cbers_bdc_tile) %in% bands_sits))
    rast <- terra::rast(cbers_bdc_tile$file_info[[1]]$path[1])
    expect_true(terra::nrow(rast) == cbers_bdc_tile[1,]$nrows)
    expect_true(all(unique(cbers_bdc_tile$file_info[[1]]$date) == cbers_bdc_tile$timeline[[1]][[1]]))
})
