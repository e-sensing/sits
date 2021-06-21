context("Cube")
test_that("Creating a SATVEG data cube", {
  testthat::skip_on_cran()
  cube_satveg <- sits_cube(source = "SATVEG", collection = "terra")

  if (purrr::is_null(cube_satveg)) {
    skip("SATVEG is not accessible")
  }

  expect_true(cube_satveg$ymin == -30.0)
})
test_that("Reading a raster cube", {
  data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
  raster_cube <- sits_cube(
    source = "LOCAL",
    name = "sinop-2014",
    satellite = "TERRA",
    sensor = "MODIS",
    data_dir = data_dir,
    delim = "_",
    parse_info = c("X1", "X2", "tile", "band", "date")
  )

  # get bands names
  bands <- sits_bands(raster_cube)
  expect_true(all(bands %in% c("NDVI", "EVI")))

  params <- .sits_raster_api_params_file(raster_cube$file_info[[1]]$path)
  expect_true(params$nrows == 144)
  expect_true(params$ncols == 254)
  expect_true(params$xres >= 231.5)
})

test_that("Creating a raster stack cube and selecting bands", {
  # Create a raster cube based on CBERS data
  data_dir <- system.file("extdata/raster/cbers", package = "sits")

  # create a raster cube file based on the information about the files
  cbers_cube <- sits_cube(
    source = "LOCAL",
    name = "022024",
    satellite = "CBERS-4",
    sensor = "AWFI",
    resolution = "64m",
    data_dir = data_dir,
    delim = "_",
    parse_info = c("X1", "X2", "tile", "band", "date")
  )

  expect_true(all(sits_bands(cbers_cube) %in%
                    c("B13", "B14", "B15", "B16", "CMASK")))
  rast <- .sits_raster_api_open_rast(cbers_cube$file_info[[1]]$path[[1]])
  expect_true(.sits_raster_api_nrows(rast) == cbers_cube$nrows[[1]])
  timeline <- sits_timeline(cbers_cube)
  expect_true(timeline[1] == "2018-02-02")

  cbers_cube_b13 <- sits_select(cbers_cube, bands = "B13")
  expect_true(all(sits_bands(cbers_cube_b13) == c("B13")))
})

test_that("Creating cubes from BDC", {
  testthat::skip_on_cran()

  # Try to find the access key as an environment variable
  bdc_access_key <- Sys.getenv("BDC_ACCESS_KEY")

  if (nchar(bdc_access_key) > 0) {
    # create a raster cube file based on the information about the files
    cbers_cube <- sits_cube(
      source = "BDC",
      name = "cbers_022024_ndvi",
      collection = "CB4_64_16D_STK-1",
      bands = c("NDVI", "EVI"),
      tiles = c("022024", "022023"),
      start_date = "2018-09-01",
      end_date = "2019-08-29"
    )
    if (purrr::is_null(cbers_cube)) {
      skip("BDC is not accessible")
    }
    expect_true(all(sits_bands(cbers_cube) %in% c("NDVI", "EVI")))
    bbox <- sits_bbox(cbers_cube)
    int_bbox <- sits:::.sits_bbox_intersect(bbox, cbers_cube[1, ])
    expect_true(all(int_bbox == sits_bbox(cbers_cube[1, ])))

    timeline <- sits_timeline(cbers_cube)
    expect_true(timeline[1] <= as.Date("2018-09-01"))
    expect_true(timeline[length(timeline)] <= as.Date("2019-08-29"))

    r_obj <- .sits_raster_api_open_rast(cbers_cube$file_info[[1]]$path[1])
    expect_true(terra::nrow(r_obj) == cbers_cube$nrows[[1]])
  }

})

test_that("Creating cubes from DEA", {
  testthat::skip_on_cran()
  # check "AWS_ACCESS_KEY_ID" - mandatory one per user
  aws_access_key_id <- Sys.getenv("AWS_ACCESS_KEY_ID")
  # check "AWS_SECRET_ACCESS_KEY" - mandatory one per user
  aws_secret_access_key <- Sys.getenv("AWS_SECRET_ACCESS_KEY")

  testthat::skip_if(nchar(aws_access_key_id) == 0,
                    message = "No AWS_ACCESS_KEY_ID defined in environment.")

  testthat::skip_if(nchar(aws_secret_access_key) == 0,
                    message = "No AWS_SECRET_ACCESS_KEY defined in environment.")

  dea_cube <- sits_cube(source = "DEAFRICA",
                        name = "deafrica_cube",
                        collection = "s2_l2a",
                        bands = c("B01", "B04", "B05"),
                        bbox = c("xmin" = 17.379,
                                 "ymin" = 1.1573,
                                 "xmax" = 17.410,
                                 "ymax" = 1.1910),
                        start_date = "2019-01-01",
                        end_date = "2019-10-28")

  if (purrr::is_null(dea_cube)) {
    skip("DEAFRICA is not accessible")
  }

  expect_true(all(sits_bands(dea_cube) %in% c("B01", "B04", "B05")))

  file_info <- dea_cube$file_info[[1]]
  r <- .sits_raster_api_open_rast(file_info$path[[1]])

  expect_equal(dea_cube$xmax[[1]], .sits_raster_api_xmax(r), tolerance = 1)
  expect_equal(dea_cube$xmin[[1]], .sits_raster_api_xmin(r), tolerance = 1)
})

test_that("Creating cubes from USGS", {
  testthat::skip_on_cran()
  # check "AWS_ACCESS_KEY_ID" - mandatory one per user
  aws_access_key_id <- Sys.getenv("AWS_ACCESS_KEY_ID")
  # check "AWS_SECRET_ACCESS_KEY" - mandatory one per user
  aws_secret_access_key <- Sys.getenv("AWS_SECRET_ACCESS_KEY")

  testthat::skip_if(nchar(aws_access_key_id) == 0,
                    message = "No AWS_ACCESS_KEY_ID defined in environment.")

  testthat::skip_if(nchar(aws_secret_access_key) == 0,
                    message = "No AWS_SECRET_ACCESS_KEY defined in environment.")

  usgs_cube <- sits_cube(source = "USGS",
                         name = "usgs_cube_2019",
                         collection = "landsat-c2l2-sr",
                         bands = c("B1", "B7", "FMASK4"),
                         tiles = c("140048", "140045"),
                         start_date = as.Date("2016-01-01"),
                         end_date = as.Date("2016-12-31"))

  if (purrr::is_null(usgs_cube)) {
    skip("USGS is not accessible")
  }

  expect_true(all(sits_bands(usgs_cube) %in% c("B1", "B7", "FMASK4")))

  file_info <- usgs_cube$file_info[[1]]
  r <- .sits_raster_api_open_rast(file_info$path[[1]])

  expect_equal(usgs_cube$xmax[[1]], .sits_raster_api_xmax(r), tolerance = 1)
  expect_equal(usgs_cube$xmin[[1]], .sits_raster_api_xmin(r), tolerance = 1)
})

test_that("Merging cubes", {

  data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
  ndvi_cube <- sits_cube(
    source = "LOCAL",
    name = "sinop-2014",
    satellite = "TERRA",
    sensor = "MODIS",
    bands = "NDVI",
    data_dir = data_dir,
    delim = "_",
    parse_info = c("X1", "X2", "tile", "band", "date")
  )

  evi_cube <- sits_cube(
    source = "LOCAL",
    name = "sinop-2014",
    satellite = "TERRA",
    sensor = "MODIS",
    bands = "EVI",
    data_dir = data_dir,
    delim = "_",
    parse_info = c("X1", "X2", "tile", "band", "date")
  )
  cube_merge <- sits_merge(ndvi_cube, evi_cube)

  expect_true(all(sits_bands(cube_merge) %in% c("NDVI", "EVI")))
  expect_true(cube_merge$xmin == ndvi_cube$xmin)
  expect_true(cube_merge$xmax == evi_cube$xmax)
})

# test_that("Creating cubes from AWS and regularizing them", {
#   testthat::skip_on_cran()
#   # check "AWS_ACCESS_KEY_ID" - mandatory one per user
#   aws_access_key_id <- Sys.getenv("AWS_ACCESS_KEY_ID")
#   # check "AWS_SECRET_ACCESS_KEY" - mandatory one per user
#   aws_secret_access_key <- Sys.getenv("AWS_SECRET_ACCESS_KEY")
#
#   testthat::skip_if(
#     nchar(aws_access_key_id) == 0,
#     message = "No AWS_ACCESS_KEY_ID defined in environment."
#   )
#
#   testthat::skip_if(
#     nchar(aws_secret_access_key) == 0,
#     message = "No AWS_SECRET_ACCESS_KEY defined in environment."
#   )
#
#   Sys.unsetenv("AWS_DEFAULT_REGION")
#   Sys.unsetenv("AWS_S3_ENDPOINT")
#   Sys.unsetenv("AWS_REQUEST_PAYER")
#
#   s2_cube <- sits_cube(source = "AWS",
#                        name = "T20LKP_2018_2019",
#                        collection = "sentinel-s2-l2a",
#                        s2_resolution = 60,
#                        tiles = c("20LKP"),
#                        bands = c("B08", "SCL"),
#                        start_date = "2018-07-30",
#                        end_date = "2018-08-30"
#   )
#
#   expect_true(all(sits_bands(s2_cube) %in% c("B08", "SCL")))
#
#   file_info <- s2_cube$file_info[[1]]
#   r <- sits:::.sits_raster_api_open_rast(file_info$path[[1]])
#
#   expect_equal(s2_cube$nrows[[1]], sits:::.sits_raster_api_nrows(r))
#   expect_equal(s2_cube$ncols[[1]], sits:::.sits_raster_api_ncols(r))
#   expect_equal(s2_cube$xmax[[1]], sits:::.sits_raster_api_xmax(r))
#   expect_equal(s2_cube$xmin[[1]], sits:::.sits_raster_api_xmin(r))
#
#   dir_images <-  paste0(tempdir(), "/images/")
#   if (!dir.exists(dir_images))
#     suppressWarnings(dir.create(dir_images))
#
#   gc_cube <- sits_regularize(
#     cube        = s2_cube,
#     name        = "T20LKP_2018_2019_P5D",
#     dir_images  =  dir_images,
#     period      = "P15D",
#     agg_method  = "median",
#     resampling  = "bilinear"
#   )
#
#   expect_equal(s2_cube$nrows, gc_cube$nrows)
#   expect_equal(s2_cube$ncols, gc_cube$ncols)
#   expect_equal(s2_cube$xmax, gc_cube$xmax)
#   expect_equal(s2_cube$xmin, gc_cube$xmin)
#
#   file_info2 <- gc_cube$file_info[[1]]
#
#   # expect_equal(nrow(file_info), nrow(file_info2))
#
# })
# test_that("Creating cubes from classified images", {
#   # Create a raster cube based on bricks
#   # inform the files that make up a raster probs brick with 23 time instances
#   probs_file <- c(system.file(
#     "extdata/raster/probs/sinop-2014_probs_2013_9_2014_8_v1.tif",
#     package = "sits"
#   ))
#
#   # inform the labels
#   labels <- c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn",
#               "Soy_Cotton", "Soy_Fallow", "Soy_Millet", "Soy_Sunflower")
#
#
#   # create a raster cube file based on the information about the files
#   probs_cube <- sits_cube(
#     source = "PROBS",
#     name = "Sinop-crop-probs",
#     satellite = "TERRA",
#     sensor  = "MODIS",
#     start_date = as.Date("2013-09-14"),
#     end_date = as.Date("2014-08-29"),
#     probs_labels = labels,
#     probs_files = probs_file
#   )
#   expect_equal(probs_cube$ncols, 50)
#   expect_equal(sits_bands(probs_cube), "probs")
#   file_info <- probs_cube$file_info[[1]]
#   expect_equal(file_info$band, "probs")
#   expect_equal(file_info$path, probs_file)
# })

# test_that("Cube copy", {
#   data_dir <- system.file("extdata/raster/cbers", package = "sits")
#
#   cbers_022024 <- sits_cube(
#     source = "LOCAL",
#     name = "cbers_022024",
#     satellite = "CBERS-4",
#     sensor = "AWFI",
#     resolution = "64m",
#     data_dir = data_dir,
#     parse_info = c("X1", "X2", "tile", "band", "date")
#   )
#
#   bbox <- sits_bbox(cbers_022024)
#   x_size <- bbox["xmax"] - bbox["xmin"]
#   bbox["xmax"] <- bbox["xmin"] + x_size / 2
#
#   cbers_022024_copy <- sits_cube_copy(cbers_022024,
#                                       name = "cb_022024_cp",
#                                       dest_dir = tempdir(),
#                                       bands = "B13",
#                                       roi = bbox
#   )
#   expect_true(sits_bands(cbers_022024_copy) == "B13")
#   expect_true(cbers_022024_copy$ncols == 26)
#   expect_true(cbers_022024_copy$xmin == cbers_022024$xmin)
#   expect_true(all(sits_timeline(cbers_022024_copy) ==
#                     sits_timeline(cbers_022024)))
# })

test_that("Creating a raster stack cube and renaming bands", {
  # Create a raster cube based on CBERS data
  data_dir <- system.file("extdata/raster/cbers", package = "sits")

  # create a raster cube file based on the information about the files
  cbers_cube2 <- sits_cube(
    source = "LOCAL",
    name = "022024",
    satellite = "CBERS-4",
    sensor = "AWFI",
    resolution = "64m",
    data_dir = data_dir,
    delim = "_",
    parse_info = c("X1", "X2", "tile", "band", "date")
  )
  expect_true(all(sits_bands(cbers_cube2) %in%
                    c("B13", "B14", "B15", "B16", "CMASK")))
  sits_bands(cbers_cube2) <- c("BAND13", "BAND14", "BAND15",
                               "BAND16", "CLOUD")
  expect_true(all(sits_bands(cbers_cube2) %in%
                    c("BAND13", "BAND14", "BAND15", "BAND16", "CLOUD")))

})

# test_that("Creating a raster stack cube with BDC band names", {
#   # Create a raster cube based on CBERS data
#   data_dir <- system.file("extdata/raster/bdc", package = "sits")
#
#   # create a raster cube file based on the information about the files
#   cbers_cube_bdc <- sits_cube(
#     source = "LOCAL",
#     name = "022024",
#     satellite = "CBERS-4",
#     sensor = "AWFI",
#     resolution = "64m",
#     data_dir = data_dir,
#     parse_info = c("X1", "X2", "X3", "X4", "X5", "tile", "date", "X6", "band")
#   )
#   expect_true(all(sits_bands(cbers_cube_bdc) %in%
#                     c("B13", "B14", "B15", "B16", "CMASK")))
#
# })
