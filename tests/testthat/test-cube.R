context("Cube")
test_that("Reading a raster cube", {
    file <- c(system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif",
        package = "sits"
    ))
    raster_cube <- sits_cube(
        type = "BRICK",
        name = "Sinop-crop",
        timeline = sits::timeline_modis_392,
        bands = c("ndvi"),
        satellite = "TERRA",
        sensor = "MODIS",
        files = file
    )

    # get bands names
    bands <- sits_bands(raster_cube)
    expect_true(bands %in% c("NDVI"))

    params <- sits:::.sits_raster_api_params_file(raster_cube$file_info[[1]]$path)
    expect_true(params$nrows == 11)
    expect_true(params$ncols == 14)
    expect_true(params$xres >= 231.5)
})

test_that("Creating a raster stack cube and selecting bands", {
    # Create a raster cube based on CBERS data
    data_dir <- system.file("extdata/raster/cbers", package = "sits")

    # create a raster cube file based on the information about the files
    cbers_cube <- sits_cube(
        type = "STACK",
        name = "022024",
        satellite = "CBERS-4",
        sensor = "AWFI",
        resolution = "64m",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "X2", "band", "date")
    )

    expect_true(all(sits_bands(cbers_cube) %in%
                      c("B13", "B14", "B15", "B16", "CMASK")))
    rast <- suppressWarnings(terra::rast(cbers_cube$file_info[[1]]$path[1]))
    expect_true(terra::nrow(rast) == cbers_cube[1, ]$nrows)
    expect_true(all(unique(cbers_cube$file_info[[1]]$date) ==
                      cbers_cube$timeline[[1]][[1]])
                )

    cbers_cube_b13 <- sits_select(cbers_cube, bands = "B13")
    expect_true(all(sits_bands(cbers_cube_b13) == c("B13")))
})

test_that("Creating, merging cubes from BDC", {
    testthat::skip_on_cran()

    # Try to find the access key as an environment variable
    bdc_access_key <- Sys.getenv("BDC_ACCESS_KEY")

    if (nchar(bdc_access_key) > 0) {
      # create a raster cube file based on the information about the files
      cbers_022024_ndvi <- sits_cube(
        type = "BDC",
        name = "cbers_022024_ndvi",
        bands = "NDVI",
        tiles = "022024",
        url = "http://brazildatacube.dpi.inpe.br/stac/",
        collection = "CB4_64_16D_STK-1",
        start_date = "2018-09-01",
        end_date = "2019-08-28"
      )

      if (purrr::is_null(cbers_022024_ndvi)) {
        skip("BDC is not accessible")
      }

      cbers_022024_evi <- sits_cube(
        type = "BDC",
        name = "cbers_022024_evi",
        bands = "EVI",
        tiles = "022024",
        url = "http://brazildatacube.dpi.inpe.br/stac/",
        collection = "CB4_64_16D_STK-1",
        start_date = "2018-09-01",
        end_date = "2019-08-28"
      )

      cbers_merge <- sits_merge(cbers_022024_ndvi, cbers_022024_evi)
      expect_true(all(sits_bands(cbers_merge) %in% c("NDVI", "EVI")))
      expect_true(all(sits_timeline(cbers_merge) ==
                        sits_timeline(cbers_022024_ndvi)))
    }


})

test_that("Creating cubes from AWS", {
    testthat::skip_on_cran()
    # check "AWS_ACCESS_KEY_ID" - mandatory one per user
    aws_access_key_id <- Sys.getenv("AWS_ACCESS_KEY_ID")
    # check "AWS_SECRET_ACCESS_KEY" - mandatory one per user
    aws_secret_access_key <- Sys.getenv("AWS_SECRET_ACCESS_KEY")

    testthat::skip_if(nchar(aws_access_key_id) == 0,
                      message = "No AWS_ACCESS_KEY_ID defined in environment.")

    testthat::skip_if(nchar(aws_secret_access_key) == 0,
                      message = "No AWS_SECRET_ACCESS_KEY defined in environment.")

    s2_cube <- sits_cube(type = "S2_L2A_AWS",
                         name = "T20LKP_2018_2019",
                         satellite = "SENTINEL-2",
                         sensor = "MSI",
                         tiles = "20LKP",
                         bands = c("B08", "SCL"),
                         s2_aws_resolution = "60m",
                         start_date = as.Date("2018-07-18"),
                         end_date = as.Date("2018-07-23")
    )

    expect_true(all(sits_bands(s2_cube) %in% c("B08", "SCL")))

    file_info <- s2_cube$file_info[[1]]
    r <- terra::rast(file_info[1,]$path)

    expect_equal(s2_cube$nrows, terra::nrow(r))
    expect_equal(s2_cube$ncols, terra::ncol(r))
    expect_equal(s2_cube$xmax, terra::xmax(r))
    expect_equal(s2_cube$xmin, terra::xmin(r))

    path_images <-  paste0(tempdir(),"/images/")
    suppressWarnings(dir.create(path_images))

    gc_cube <- sits_cube(type        = "GDALCUBES",
                         cube        = s2_cube,
                         path_db     = paste0(tempdir(), "/cube.db"),
                         path_images = path_images,
                         period      = "P5D",
                         agg_method  = "median",
                         resampling  = "bilinear")

    expect_equal(s2_cube$nrows, gc_cube$nrows)
    expect_equal(s2_cube$ncols, gc_cube$ncols)
    expect_equal(s2_cube$xmax, gc_cube$xmax)
    expect_equal(s2_cube$xmin, gc_cube$xmin)

    file_info2 <- gc_cube$file_info[[1]]

    expect_equal(nrow(file_info), nrow(file_info2))

})

test_that("Cube copy", {
    data_dir <- system.file("extdata/raster/cbers", package = "sits")

    cbers_022024 <- sits_cube(
        type = "STACK",
        name = "cbers_022024",
        satellite = "CBERS-4",
        sensor = "AWFI",
        resolution = "64m",
        data_dir = data_dir,
        parse_info = c("X1", "X2", "band", "date")
    )

    cbers_022024_copy <- sits_cube_copy(cbers_022024,
        name = "cb_022024_cp",
        dest_dir = tempdir(),
        bands = "B13",
        srcwin = c(0, 0, 25, 25)
    )
    expect_true(sits_bands(cbers_022024_copy) == "B13")
    expect_true(cbers_022024_copy$ncols == 25)
    expect_true(cbers_022024_copy$xmin == cbers_022024$xmin)
    expect_true(all(sits_timeline(cbers_022024_copy) ==
                      sits_timeline(cbers_022024)))
})
