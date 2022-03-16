test_that("Creating a WTSS data cube", {
  testthat::skip_on_cran()

  # check "BDC_ACCESS_KEY" - mandatory one per user
  bdc_access_key <- Sys.getenv("BDC_ACCESS_KEY")

  testthat::skip_if(nchar(bdc_access_key) == 0,
                    message = "No BDC_ACCESS_KEY defined in environment."
  )

  cube_wtss <- tryCatch(
    {
      suppressMessages(
        sits_cube(
          source = "WTSS",
          collection = "MOD13Q1-6"
        )
      )
    },
    error = function(e) {
      return(NULL)
    }
  )

  testthat::skip_if(
    purrr::is_null(cube_wtss),
    message = "WTSS is not accessible"
  )

  expect_true(cube_wtss$source == "WTSS")
  expect_gt(length(sits_timeline(cube_wtss)), 1)
})

test_that("Reading a CSV file from WTSS", {
  testthat::skip_on_cran()

  # check "BDC_ACCESS_KEY" - mandatory one per user
  bdc_access_key <- Sys.getenv("BDC_ACCESS_KEY")

  testthat::skip_if(
    nchar(bdc_access_key) == 0,
    message = "No BDC_ACCESS_KEY defined in environment."
  )

  csv_file <- system.file("extdata/samples/samples_matogrosso.csv",
                          package = "sits"
  )

  cube_wtss <- tryCatch(
    {
      suppressMessages(
        sits_cube(
          source = "WTSS",
          collection = "MOD13Q1-6"
        )
      )
    },
    error = function(e) {
      return(NULL)
    })

  testthat::skip_if(
    purrr::is_null(cube_wtss),
    message = "WTSS is not accessible"
  )

  dir_samples <-  file.path(tempdir(), "samples")
  if (!dir.exists(dir_samples))
    suppressWarnings(dir.create(dir_samples))

  unlink(list.files(dir_samples, pattern = "\\.rds$", full.names = TRUE))

  points <- tryCatch({
    sits_get_data(cube_wtss, file = csv_file, output_dir = dir_samples)
    return(NULL)
  }
  )

  expect_true(all(unique(points$label) == c("Pasture", "Cerrado")))

  expect_equal(min(points$longitude), -55.0399, tolerance = 1e-5)
  expect_equal(min(points$latitude), -15.1933, tolerance = 1e-5)
  expect_equal(max(points$longitude), -46.407, tolerance = 1e-5)
  expect_equal(max(points$latitude), -10.4142, tolerance = 1e-5)

  mylabels <- sits_labels(points)

  expect_equal(nrow(dplyr::filter(points, label == "Cerrado")), 3)
  expect_equal(nrow(dplyr::filter(points, label == "Pasture")), 3)

  df_csv <- utils::read.csv(
    system.file("extdata/samples/samples_matogrosso.csv", package = "sits")
  )
  expect_true(nrow(points) == nrow(df_csv))
})

test_that("Reading a POLYGON shapefile from WTSS", {
  testthat::skip_on_cran()

  # check "BDC_ACCESS_KEY" - mandatory one per user
  bdc_access_key <- Sys.getenv("BDC_ACCESS_KEY")

  testthat::skip_if(nchar(bdc_access_key) == 0,
                    message = "No BDC_ACCESS_KEY defined in environment."
  )

  cube_wtss <- tryCatch(
    {
      suppressMessages(
        sits_cube(source = "WTSS", collection = "MOD13Q1-6")
      )
    },
    error = function(e) {
      return(NULL)
    })

  testthat::skip_if(
    purrr::is_null(cube_wtss),
    message = "WTSS is not accessible"
  )

  shp_file <- system.file(
    "extdata/shapefiles/agriculture/parcel_agriculture.shp",
    package = "sits"
  )

  dir_samples <-  file.path(tempdir(), "samples")
  if (!dir.exists(dir_samples))
    suppressWarnings(dir.create(dir_samples))

  unlink(list.files(dir_samples, pattern = "\\.rds$", full.names = TRUE))

  parcels <- tryCatch({
    sits_get_data(cube_wtss,
                  file = shp_file,
                  shp_attr = "ext_na",
                  .n_shp_pol = 3,
                  start_date = "2019-01-01",
                  end_date = "2019-06-01",
                  output_dir = dir_samples
    )
    return(NULL)
  }
  )

  testthat::skip_if(
    purrr::is_null(parcels),
    message = "WTSS is not accessible"
  )

  sf_shape <- sf::read_sf(shp_file)
  sf_shape <- sf::st_transform(sf_shape, crs = "EPSG:4326")
  bbox <- sf::st_bbox(sf_shape)
  longitudes_shp <- parcels$longitude

  expect_true(nrow(parcels) > 1)
  expect_true(all(unique(longitudes_shp) > bbox["xmin"]))
  expect_true(all(unique(longitudes_shp) < bbox["xmax"]))
  expect_true(all(parcels$label == "Soja_Algodao"))
})

test_that("Reading a POINT shapefile from WTSS", {
  testthat::skip_on_cran()

  # check "BDC_ACCESS_KEY" - mandatory one per user
  bdc_access_key <- Sys.getenv("BDC_ACCESS_KEY")

  testthat::skip_if(nchar(bdc_access_key) == 0,
                    message = "No BDC_ACCESS_KEY defined in environment."
  )

  cube_wtss <- tryCatch(
    {
      suppressMessages(
        sits_cube(source = "WTSS", collection = "MOD13Q1-6")
      )
    },
    error = function(e) {
      return(NULL)
    })

  testthat::skip_if(
    purrr::is_null(cube_wtss),
    message = "WTSS is not accessible"
  )

  shp_file <- system.file("extdata/shapefiles/cerrado/cerrado_forested.shp",
                          package = "sits"
  )

  dir_samples <-  file.path(tempdir(), "samples")
  if (!dir.exists(dir_samples))
    suppressWarnings(dir.create(dir_samples))

  unlink(list.files(dir_samples, pattern = "\\.rds$", full.names = TRUE))

  points <- tryCatch({
    sits_get_data(cube_wtss,
                  file = shp_file,
                  label = "Cerrado_Forested",
                  start_date = "2019-01-01",
                  end_date = "2019-06-01",
                  output_dir = dir_samples
    )
    return(NULL)
  }
  )
  expect_true(all(points$label == "Cerrado_Forested"))
})

test_that("Reading a point from SATVEG ", {
  testthat::skip_on_cran()

  cube_1 <- tryCatch(
    {
      sits_cube(source = "SATVEG", collection = "TERRA")
    },
    error = function(e) {
      return(NULL)
    })
  testthat::skip_if(
    purrr::is_null(cube_1),
    message = "SATVEG is not accessible for collection terra"
  )

  dir_samples <-  file.path(tempdir(), "samples")
  if (!dir.exists(dir_samples))
    suppressWarnings(dir.create(dir_samples))

  unlink(list.files(dir_samples, pattern = "\\.rds$", full.names = TRUE))

  point_terra <- sits_get_data(cube_1,
                               longitude = -55.50563,
                               latitude = -11.71557,
                               output_dir = dir_samples
  )

  testthat::skip_if(
    purrr::is_null(point_terra),
    message = "points in SATVEG for collection terra cannnot be recovered"
  )
  expect_equal(ncol(sits_time_series(point_terra)), 3)
  expect_true(max(sits_time_series(point_terra)$EVI) <= 1)

  cube_2 <- tryCatch({
    sits_cube(source = "SATVEG", collection = "AQUA")
  },
  error = function(e) {
    return(NULL)
  })
  testthat::skip_if(
    purrr::is_null(cube_2),
    message = "SATVEG is not accessible for collection aqua"
  )

  point_aqua <- sits_get_data(cube_2,
                              longitude = -55.50563,
                              latitude = -11.71557,
                              output_dir = dir_samples
  )
  testthat::skip_if(
    purrr::is_null(point_aqua),
    message = "points in SATVEG for collection aqua cannnot be recovered"
  )
  expect_equal(ncol(sits_time_series(point_aqua)), 3)
  expect_true(max(sits_time_series(point_aqua)$EVI) <= 1)

  cube_3 <- tryCatch({
    sits_cube(source = "SATVEG", collection = "COMB")
  },
  error = function(e) {
    return(NULL)
  })
  testthat::skip_if(
    purrr::is_null(cube_3),
    message = "SATVEG is not accessible for collection comb"
  )

  point_comb <- sits_get_data(cube_3,
                              longitude = -55.50563,
                              latitude = -11.71557,
                              output_dir = dir_samples
  )
  testthat::skip_if(
    purrr::is_null(point_aqua),
    message = "points in SATVEG for collection comb cannnot be recovered"
  )
  expect_equal(ncol(sits_time_series(point_comb)), 3)
  expect_true(max(sits_time_series(point_comb)$EVI) <= 1)

  expect_true(length(sits_timeline(point_comb)) >=
                length(sits_timeline(point_terra))
  )
})

test_that("Reading a CSV file from SATVEG", {
  testthat::skip_on_cran()

  csv_file <- system.file("extdata/samples/samples_matogrosso.csv",
                          package = "sits"
  )

  cube_satveg <- tryCatch({
    sits_cube(source = "SATVEG", collection = "TERRA")
    return(NULL)
  }
  )
  testthat::skip_if(
    purrr::is_null(cube_1),
    message = "SATVEG is not accessible for collection terra"
  )

  point_terra <- sits_get_data(cube_1,
                               longitude = -55.50563, latitude = -11.71557
  )
  testthat::skip_if(
    purrr::is_null(point_terra),
    message = "points in SATVEG for collection terra cannnot be recovered"
  )
  expect_equal(ncol(sits_time_series(point_terra)), 3)
  expect_true(max(sits_time_series(point_terra)$EVI) <= 1)

  cube_2 <- tryCatch(
    {
      sits_cube(source = "SATVEG", collection = "AQUA")
    },
    error = function(e) {
      return(NULL)
    }
  )
  testthat::skip_if(
    purrr::is_null(cube_2),
    message = "SATVEG is not accessible for collection aqua"
  )

  point_aqua <- sits_get_data(cube_2,
                              longitude = -55.50563, latitude = -11.71557
  )
  testthat::skip_if(
    purrr::is_null(point_aqua),
    message = "points in SATVEG for collection aqua cannnot be recovered"
  )
  expect_equal(ncol(sits_time_series(point_aqua)), 3)
  expect_true(max(sits_time_series(point_aqua)$EVI) <= 1)

  cube_3 <- tryCatch(
    {
      sits_cube(source = "SATVEG", collection = "COMB")
    },
    error = function(e) {
      return(NULL)
    }
  )
  testthat::skip_if(
    purrr::is_null(cube_3),
    message = "SATVEG is not accessible for collection comb"
  )

  point_comb <- sits_get_data(cube_3,
                              longitude = -55.50563, latitude = -11.71557
  )
  testthat::skip_if(
    purrr::is_null(point_aqua),
    message = "points in SATVEG for collection comb cannnot be recovered"
  )
  expect_equal(ncol(sits_time_series(point_comb)), 3)
  expect_true(max(sits_time_series(point_comb)$EVI) <= 1)

  expect_true(length(sits_timeline(point_comb)) >=
                length(sits_timeline(point_terra)))
})

test_that("Reading a CSV file from SATVEG", {
  testthat::skip_on_cran()
  csv_file <- system.file("extdata/samples/samples_matogrosso.csv",
                          package = "sits"
  )

  cube_satveg <- tryCatch(
    {
      sits_cube(source = "SATVEG", collection = "TERRA")
    },
    error = function(e) {
      return(NULL)
    }
  )

  testthat::skip_if(
    purrr::is_null(cube_satveg),
    message = "SATVEG is not accessible for collection terra"
  )

  dir_samples <-  file.path(tempdir(), "samples")
  if (!dir.exists(dir_samples))
    suppressWarnings(dir.create(dir_samples))

  unlink(list.files(dir_samples, pattern = "\\.rds$", full.names = TRUE))

  points <- sits_get_data(cube_satveg,
                          file = csv_file,
                          output_dir = dir_samples)

  testthat::skip_if(
    purrr::is_null(points),
    message = "points in SATVEG for csv file cannnot be recovered"
  )

  expect_true(all(unique(points$label) == c("Pasture", "Cerrado")))

  expect_equal(min(points$longitude), -55.0399, tolerance = 1e-5)
  expect_equal(min(points$latitude), -15.1933, tolerance = 1e-5)
  expect_equal(max(points$longitude), -46.407, tolerance = 1e-5)
  expect_equal(max(points$latitude), -10.4142, tolerance = 1e-5)

  mylabels <- sits_labels_summary(points)

  expect_equal(dplyr::filter(mylabels, label == "Cerrado")$count, 3)
  expect_equal(dplyr::filter(mylabels, label == "Pasture")$count, 3)

  df_csv <- utils::read.csv(
    system.file("extdata/samples/samples_matogrosso.csv", package = "sits"),
    stringsAsFactors = FALSE
  )
  expect_true(nrow(points) == nrow(df_csv))
})

test_that("Reading a POLYGON shapefile from SATVEG", {
  testthat::skip_on_cran()

  cube_satveg <- tryCatch(
    {
      sits_cube(source = "SATVEG", collection = "TERRA")
    },
    error = function(e) {
      return(NULL)
    })

  testthat::skip_if(
    purrr::is_null(cube_satveg),
    message = "SATVEG is not accessible for collection terra"
  )


  shp_file <- system.file(
    "extdata/shapefiles/agriculture/parcel_agriculture.shp", package = "sits"
  )

  dir_samples <-  file.path(tempdir(), "samples")
  if (!dir.exists(dir_samples))
    suppressWarnings(dir.create(dir_samples))

  unlink(list.files(dir_samples, pattern = "\\.rds$", full.names = TRUE))

  parcels <- sits_get_data(cube_satveg,
                           file = shp_file,
                           shp_attr = "ext_na",
                           .n_shp_pol = 3,
                           output_dir = dir_samples
  )
  testthat::skip_if(
    purrr::is_null(parcels),
    message = "points in SATVEG for shp file cannnot be recovered"
  )

  sf_shape <- sf::read_sf(shp_file)
  sf_shape <- sf::st_transform(sf_shape, crs = "EPSG:4326")
  bbox <- sf::st_bbox(sf_shape)
  longitudes_shp <- parcels$longitude

  expect_true(nrow(parcels) > 1)
  expect_true(all(unique(longitudes_shp) > bbox["xmin"]))
  expect_true(all(unique(longitudes_shp) < bbox["xmax"]))
  expect_true(all(parcels$label == "Soja_Algodao"))
})

test_that("Test reading shapefile from BDC", {
  testthat::skip_on_cran()

  # check "BDC_ACCESS_KEY" - mandatory one per user
  bdc_access_key <- Sys.getenv("BDC_ACCESS_KEY")

  testthat::skip_if(
    nchar(bdc_access_key) == 0,
    message = "No BDC_ACCESS_KEY defined in environment."
  )

  # create a raster cube file based on the information about the files
  cbers_stac_tile <- tryCatch(
    {
      sits_cube(
        source = "BDC",
        collection = "CB4_64_16D_STK-1",
        bands = c("NDVI", "EVI"),
        tiles = c("022024", "022025"),
        start_date = "2018-09-01",
        end_date = "2018-10-28"
      )
    },
    error = function(e) {
      return(NULL)
    })

  testthat::skip_if(
    purrr::is_null(cbers_stac_tile),
    message = "BDC is not accessible"
  )

  shp_path <- system.file("extdata/shapefiles/bdc-test/samples.shp",
                          package = "sits"
  )

  dir_samples <-  file.path(tempdir(), "samples")
  if (!dir.exists(dir_samples))
    suppressWarnings(dir.create(dir_samples))

  unlink(list.files(dir_samples, pattern = "\\.rds$", full.names = TRUE))

  time_series_bdc <- tryCatch({
    sits::sits_get_data(cbers_stac_tile,
                        file = shp_path,
                        output_dir = dir_samples)
    return(NULL)
  }
  )

  expect_equal(nrow(time_series_bdc), 10)
  bbox <- sits_bbox(time_series_bdc)
  expect_true(bbox["xmin"] < -46.)
  expect_true(all(sits_bands(time_series_bdc) %in% c("NDVI", "EVI")))
  ts <- time_series_bdc$time_series[[1]]
  expect_true(max(ts["EVI"]) < 1.)
})

test_that("Reading a LAT/LONG from RASTER", {
  data_dir <- system.file("extdata/raster/mod13q1", package = "sits")

  raster_cube <- tryCatch(
    {
      sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "X2", "tile", "band", "date")
      )
    },
    error = function(e) {
      return(NULL)
    })

  testthat::skip_if(
    purrr::is_null(raster_cube),
    message = "LOCAL cube was not found"
  )

  dir_samples <-  file.path(tempdir(), "samples")
  if (!dir.exists(dir_samples))
    suppressWarnings(dir.create(dir_samples))

  unlink(list.files(dir_samples, pattern = "\\.rds$", full.names = TRUE))

  point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
  point_ndvi <- sits_get_data(raster_cube,
                              longitude = -55.66738,
                              latitude = -11.76990,
                              output_dir = dir_samples
  )

  expect_equal(names(point_ndvi)[1], "longitude")
  expect_true(ncol(sits_time_series(point_ndvi)) == 3)
  expect_true(length(sits_timeline(point_ndvi)) == 23)
})

test_that("Reading a CSV file from RASTER", {
  # skip_on_cran()
  data_dir <- system.file("extdata/raster/mod13q1", package = "sits")

  raster_cube <- tryCatch(
    {
      sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "X2", "tile", "band", "date")
      )
    },
    error = function(e) {
      return(NULL)
    })
  testthat::skip_if(
    purrr::is_null(raster_cube),
    message = "LOCAL cube was not found"
  )

  csv_raster_file <- system.file("extdata/samples/samples_sinop_crop.csv",
                                 package = "sits"
  )

  dir_samples <-  file.path(tempdir(), "samples")
  if (!dir.exists(dir_samples))
    suppressWarnings(dir.create(dir_samples))

  unlink(list.files(dir_samples, pattern = "\\.rds$", full.names = TRUE))

  points <- sits_get_data(raster_cube,
                          file = csv_raster_file,
                          output_dir = dir_samples)

  df_csv <- utils::read.csv(
    system.file("extdata/samples/samples_sinop_crop.csv", package = "sits"),
    stringsAsFactors = FALSE
  )
  expect_true(nrow(points) <= nrow(df_csv))

  expect_true("Forest" %in% sits_labels(points))
  expect_equal(names(points)[1], "longitude")
  expect_equal(length(names(points)), 7)
  expect_true(ncol(sits_time_series(points)) == 3)
  expect_true(length(sits_timeline(points)) == 23)
  return(NULL)
})

