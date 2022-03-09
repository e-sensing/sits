test_that("file_info functions", {
  testthat::skip_on_cran()

  cbers_cube <- sits_cube(
    source = "BDC",
    collection = "CB4_64_16D_STK-1",
    bands = c("NDVI", "EVI"),
    tiles = c("022024", "022025"),
    start_date = "2018-09-01",
    end_date = "2018-10-01"
  )

  # only works with one tile
  expect_error(.file_info(cbers_cube))

  cbers_tile <- cbers_cube[1, ]

  # file info
  expect_s3_class(.file_info(cbers_tile), "tbl_df")
  expect_equal(.file_info(cbers_tile), cbers_tile[["file_info"]][[1]])

  # tile size
  expect_equal(.file_info_nrows(cbers_tile), 6865)
  expect_equal(.file_info_ncols(cbers_tile), 10504)

  # tile paths
  expect_length(.file_info_path(cbers_tile), 1)
  expect_length(.file_info_paths(cbers_tile), 6)

  # tile resolutions
  expect_equal(.file_info_xres(cbers_tile), 63.99735, tolerance = 10e-6)
  expect_equal(.file_info_yres(cbers_tile), 64.00234, tolerance = 10e-6)

  # tile properties
  expect_length(.file_info_fids(cbers_tile), 3)
  expect_length(.file_info_timeline(cbers_tile), 3)
  expect_equal(.file_info_bands(cbers_tile), c("EVI", "NDVI"))

  # tile filters
  tile_fid <- .file_info(
    cbers_tile,
    fid = "CB4_64_16D_STK_v001_022024_2018-09-14_2018-09-29"
  )

  expect_s3_class(tile_fid, "tbl_df")
  expect_equal(nrow(tile_fid), 2)

  expect_error(
    .file_info(
      cbers_tile,
      fid = "CB4_64_16D_STK_v001_022024_2019-08-13_2019-08-28_ABC"
    )
  )

  tile_sliced_date <- .file_info(
    cbers_tile,
    start_date = "2018-08-29",
    end_date = "2018-09-14"
  )

  expect_s3_class(tile_sliced_date, "tbl_df")
  expect_equal(nrow(tile_sliced_date), 2)

  expect_error(
    .file_info(
      cbers_tile,
      start_date = "2020-07-12",
      end_date = "2021-07-28"
    )
  )

  expect_error(
    .file_info(
      cbers_tile,
      start_date = "2019-07-12",
      end_date = "2021-07-28"
    )
  )

  tile_band <- .file_info(cbers_tile, bands = "NDVI")

  expect_s3_class(tile_band, "tbl_df")
  expect_equal(nrow(tile_band), 3)

  expect_error(
    .file_info(cbers_tile, bands = "NDVIABC")
  )
})


test_that("file_info functions for result cubes", {
  testthat::skip_on_cran()

  samples_modis_2bands <- sits_select(samples_modis_4bands,
    bands = c("EVI", "NDVI")
  )

  # build an extreme gradient boosting model
  xgb_model <- sits_train(
    samples_modis_2bands,
    sits_xgboost(nrounds = 50, verbose = FALSE)
  )

  # create a data cube based on files
  data_dir <- system.file("extdata/raster/mod13q1", package = "sits")

  local_cube <- sits_cube(
    source = "BDC",
    collection = "MOD13Q1-6",
    data_dir = data_dir,
    delim = "_",
    parse_info = c("X1", "X2", "tile", "band", "date"),
    multicores = 2
  )

  # classify the data cube with xgb model
  probs_cube <- sits_classify(
    local_cube,
    xgb_model,
    output_dir = tempdir(),
    memsize = 4,
    multicores = 2
  )

  expect_error(.file_info_start_date(local_cube))
  expect_error(.file_info_end_date(local_cube))

  expect_equal(class(.file_info_start_date(probs_cube)), "Date")
  expect_equal(class(.file_info_end_date(probs_cube)), "Date")

  # timeline
  expect_error(.file_info_timeline(probs_cube))

  # tile resolutions
  expect_equal(.file_info_xres(probs_cube), 231.656, tolerance = 10e-6)
  expect_equal(.file_info_yres(probs_cube), 231.6564, tolerance = 10e-6)

  # tile properties
  expect_error(.file_info_fids(probs_cube))
  expect_equal(.file_info_bands(probs_cube), "probs")
})

test_that("file_info errors", {
  testthat::skip_on_cran()

  s2_cube <- sits_cube(
    source = "AWS",
    collection = "SENTINEL-S2-L2A-COGS",
    bands = c("B01", "B02", "CLOUD"),
    tiles = c("20LKP", "20LLP"),
    start_date = "2018-09-01",
    end_date = "2018-10-01"
  )

  # only works with one tile
  expect_error(.file_info(s2_cube))

  s2_tile <- s2_cube[1, ]

  # file info
  expect_s3_class(.file_info(s2_tile), "tbl_df")

  # raster size
  expect_error(.file_info_nrows(s2_tile))
  expect_error(.file_info_ncols(s2_tile))

  s2_tile[["file_info"]][[1]][["path"]] <- 1:18
  expect_error(.file_info_path(s2_tile))
  expect_error(.file_info_paths(s2_tile))

  # cube resolution
  s2_tile[["file_info"]][[1]][["xres"]] <- NULL
  s2_tile[["file_info"]][[1]][["yres"]] <- NULL
  expect_error(.file_info_xres(s2_tile))
  expect_error(.file_info_yres(s2_tile))

  # cube properties
  s2_tile[["file_info"]][[1]][["fid"]] <- NULL
  expect_error(.file_info_fids(s2_tile))

  s2_tile[["file_info"]][[1]][["date"]] <- NULL
  expect_error(.file_info_timeline(s2_tile))

  s2_tile[["file_info"]][[1]][["band"]] <- NULL
  expect_error(.file_info_bands(s2_tile))
})
