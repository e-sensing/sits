context("Cube")
test_that("Creating a SATVEG data cube", {
  testthat::skip_on_cran()
  cube_satveg <- sits_cube(type = "SATVEG", name = "terra")

  if (purrr::is_null(cube_satveg)) {
    skip("SATVEG is not accessible")
  }

  expect_true(length(cube_satveg$timeline[[1]][[1]]) > 1)
})
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

test_that("Creating cubes from BDC", {
    testthat::skip_on_cran()

    if (!sits:::.sits_config_bdc_stac_access())
        skip("BDC is not accessible")

    # Try to find the access key as an environment variable
    bdc_access_key <- Sys.getenv("BDC_ACCESS_KEY")

    if (nchar(bdc_access_key) > 0) {
      # create a raster cube file based on the information about the files
      cbers_cube <- sits_cube(
        type = "BDC",
        name = "cbers_022024_ndvi",
        bands = c("NDVI", "EVI"),
        tiles = c("022024","022023"),
        collection = "CB4_64_16D_STK-1",
        start_date = "2018-09-01",
        end_date = "2019-08-28"
      )
      expect_true(all(sits_bands(cbers_cube) %in% c("NDVI", "EVI")))
      bbox <- sits_bbox(cbers_cube)
      int_bbox <- sits:::.sits_bbox_intersect(bbox, cbers_cube[1,])
      expect_true(all(int_bbox == sits_bbox(cbers_cube[1,])))

      timeline <- sits_timeline(cbers_cube)
      expect_true(timeline[1] <= as.Date("2018-09-01"))
      expect_true(timeline[length(timeline)] <= as.Date("2019-08-28"))

      gdal_info <- suppressWarnings(
        rgdal::GDALinfo(cbers_cube[1,]$file_info[[1]]$path[1]))
      expect_true(gdal_info["rows"] == cbers_cube[1,]$nrows)

      gdal_info2 <- suppressWarnings(
        rgdal::GDALinfo(cbers_cube[2,]$file_info[[1]]$path[1]))
      expect_true(gdal_info2["rows"] == cbers_cube[2,]$nrows)
    }

})

test_that("Creating cubes from DEA", {
  testthat::skip_on_cran()

  testthat::skip_on_cran()
  # check "AWS_ACCESS_KEY_ID" - mandatory one per user
  aws_access_key_id <- Sys.getenv("AWS_ACCESS_KEY_ID")
  # check "AWS_SECRET_ACCESS_KEY" - mandatory one per user
  aws_secret_access_key <- Sys.getenv("AWS_SECRET_ACCESS_KEY")

  testthat::skip_if(nchar(aws_access_key_id) == 0,
                    message = "No AWS_ACCESS_KEY_ID defined in environment.")

  testthat::skip_if(nchar(aws_secret_access_key) == 0,
                    message = "No AWS_SECRET_ACCESS_KEY defined in environment.")

  Sys.unsetenv("AWS_DEFAULT_REGION")
  Sys.unsetenv("AWS_ENDPOINT")
  Sys.unsetenv("AWS_REQUEST_PAYER")

  dea_cube <- sits_cube(type = "DEAFRICA",
                        name = "deafrica_cube",
                        collection = "ga_s2_gm",
                        bands = c("B04", "B08"),
                        roi = c("xmin" = 17.379,
                                "ymin" = 1.1573,
                                "xmax" = 17.410,
                                "ymax" = 1.1910),
                        start_date = "2019-01-01",
                        end_date = "2019-10-28")

  expect_true(all(sits_bands(dea_cube) %in% c("B04", "B08")))

  file_info <- dea_cube$file_info[[1]]
  r <- terra::rast(file_info[1,]$path)

  expect_equal(dea_cube$nrows, terra::nrow(r))
  expect_equal(dea_cube$ncols, terra::ncol(r))
  expect_equal(dea_cube$xmax[[1]], terra::xmax(r))
  expect_equal(dea_cube$xmin[[1]], terra::xmin(r))
})

test_that("Merging cubes", {

    ndvi_file <- c(system.file("extdata/raster/mod13q1/sinop-evi-2014.tif",
                               package = "sits"
    ))

    evi_file <- c(system.file("extdata/raster/mod13q1/sinop-evi-2014.tif",
                              package = "sits"
    ))

    data("timeline_2013_2014")

    sinop_ndvi <- sits_cube(
        type = "BRICK",
        name = "sinop-2014_ndvi",
        timeline = timeline_2013_2014,
        satellite = "TERRA",
        sensor = "MODIS",
        bands = c("NDVI"),
        files = ndvi_file
    )

    sinop_evi <- sits_cube(
        type = "BRICK",
        name = "sinop-2014_evi",
        timeline = timeline_2013_2014,
        satellite = "TERRA",
        sensor = "MODIS",
        bands = c("EVI"),
        files = evi_file
    )
    cube_merge <- sits_merge(sinop_ndvi, sinop_evi)

    expect_true(all(sits_bands(cube_merge) %in% c("NDVI", "EVI")))
    expect_true(cube_merge$xmin == sinop_ndvi$xmin)
    expect_true(cube_merge$xmax == sinop_evi$xmax)
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

    Sys.unsetenv("AWS_DEFAULT_REGION")
    Sys.unsetenv("AWS_ENDPOINT")
    Sys.unsetenv("AWS_REQUEST_PAYER")

    s2_cube <- sits_cube(type = "S2_L2A_AWS",
                         name = "T20LKP_2018_2019",
                         collection = "sentinel-s2-l2a",
                         s2_resolution = "60m",
                         tiles = "20LKP",
                         bands = c("B08", "SCL"),
                         start_date = "2018-07-18",
                         end_date = "2018-07-23"
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
                         name        = "T20LKP_2018_2019_P5D",
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
test_that("Creating cubes from classified images", {
    # Create a raster cube based on bricks
    # inform the files that make up a raster probs brick with 23 time instances
    probs_file <- c(system.file("extdata/raster/mod13q1/sinop-2014_probs_2013_9_2014_8_v1.tif",
                          package = "sits"
    ))

    # inform the labels
    labels <- c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn",
                "Soy_Cotton", "Soy_Fallow", "Soy_Millet", "Soy_Sunflower")


    # create a raster cube file based on the information about the files
    probs_cube <- sits_cube(
        type = "PROBS",
        name = "Sinop-crop-probs",
        satellite = "TERRA",
        sensor  = "MODIS",
        timeline = timeline_2013_2014,
        labels = labels,
        files = probs_file
    )
    expect_equal(probs_cube$ncols, 50)
    expect_equal(sits_bands(probs_cube), "probs")
    file_info <- probs_cube$file_info[[1]]
    expect_equal(file_info$band, "probs")
    expect_equal(file_info$path, probs_file)
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

    bbox <- sits_bbox(cbers_022024)
    x_size <- bbox["xmax"] - bbox["xmin"]
    bbox["xmax"] <- bbox["xmin"] + x_size/2

    cbers_022024_copy <- sits_cube_copy(cbers_022024,
        name = "cb_022024_cp",
        dest_dir = tempdir(),
        bands = "B13",
        roi = bbox
    )
    expect_true(sits_bands(cbers_022024_copy) == "B13")
    expect_true(cbers_022024_copy$ncols == 26)
    expect_true(cbers_022024_copy$xmin == cbers_022024$xmin)
    expect_true(all(sits_timeline(cbers_022024_copy) ==
                      sits_timeline(cbers_022024)))
})

test_that("Creating a raster stack cube and renaming bands", {
    # Create a raster cube based on CBERS data
    data_dir <- system.file("extdata/raster/cbers", package = "sits")

    # create a raster cube file based on the information about the files
    cbers_cube2 <- sits_cube(
        type = "STACK",
        name = "022024",
        satellite = "CBERS-4",
        sensor = "AWFI",
        resolution = "64m",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "X2", "band", "date")
    )
    expect_true(all(sits_bands(cbers_cube2) %in%
                        c("B13", "B14", "B15", "B16", "CMASK")))
    sits_bands(cbers_cube2) <- c("BAND13", "BAND14", "BAND15", "BAND16", "CLOUD")
    expect_true(all(sits_bands(cbers_cube2) %in%
                        c("BAND13", "BAND14", "BAND15", "BAND16", "CLOUD")))

})

test_that("Creating a raster stack cube with BDC band names", {
  # Create a raster cube based on CBERS data
  data_dir <- system.file("extdata/raster/bdc", package = "sits")

  # create a raster cube file based on the information about the files
  cbers_cube_bdc <- sits_cube(
    type = "STACK",
    name = "022024",
    satellite = "CBERS-4",
    sensor = "AWFI",
    resolution = "64m",
    data_dir = data_dir,
    parse_info = c("X1", "X2", "X3", "X4", "X5", "X6", "date", "X7", "band")
  )
  expect_true(all(sits_bands(cbers_cube_bdc) %in%
                    c("B13", "B14", "B15", "B16", "CMASK")))

})

