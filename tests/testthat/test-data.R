context("Data input")
test_that("Reading a point from SATVEG ", {
    testthat::skip_on_cran()
    cube_1 <- sits_cube(source = "SATVEG", collection = "terra")
    cube_2 <- sits_cube(source = "SATVEG", collection = "aqua")
    cube_3 <- sits_cube(source = "SATVEG", collection = "comb")

    if (purrr::is_null(cube_1)) {
          skip("SATVEG is not accessible")
      }

    point_terra <- sits_get_data(cube_1,
        longitude = -55.50563, latitude = -11.71557
    )

    expect_equal(ncol(sits_time_series(point_terra)), 3)
    expect_equal(sum(sits_time_series(point_terra)$EVI),
                 158.11, tolerance = 2
    )

    point_aqua <- sits_get_data(cube_2,
        longitude = -55.50563, latitude = -11.71557
    )

    expect_equal(ncol(sits_time_series(point_aqua)), 3)
    expect_equal(sum(sits_time_series(point_aqua)$EVI),
        132.3852, tolerance = 2
    )

    point_comb <- sits_get_data(cube_3,
        longitude = -55.50563, latitude = -11.71557
    )

    expect_equal(ncol(sits_time_series(point_comb)), 3)
    expect_equal(sum(sits_time_series(point_comb)$EVI),
                 290.3342, tolerance = 2
    )

    expect_true(length(sits_time_series_dates(point_comb)) >=
        length(sits_time_series_dates(point_terra))
    )
})

test_that("Reading a CSV file from SATVEG", {
    testthat::skip_on_cran()
    csv_file <- system.file("extdata/samples/samples_matogrosso.csv",
        package = "sits"
    )
    cube_satveg <- sits_cube(source = "SATVEG", collection = "terra")

    if (purrr::is_null(cube_satveg)) {
          skip("SATVEG is not accessible")
      }

    points <- sits_get_data(cube_satveg, file = csv_file)

    expect_true(all(unique(points$label) == c("Pasture", "Cerrado")))

    expect_equal(min(points$longitude), -55.0399, tolerance = 1e-5)
    expect_equal(min(points$latitude), -15.1933, tolerance = 1e-5)
    expect_equal(max(points$longitude), -46.407, tolerance = 1e-5)
    expect_equal(max(points$latitude), -10.4142, tolerance = 1e-5)

    mylabels <- sits_labels(points)

    expect_equal(dplyr::filter(mylabels, label == "Cerrado")$count, 3)
    expect_equal(dplyr::filter(mylabels, label == "Pasture")$count, 3)

    df_csv <- utils::read.csv(
      system.file("extdata/samples/samples_matogrosso.csv", package = "sits")
    )
    expect_true(nrow(points) == nrow(df_csv))
})

test_that("Reading a POLYGON shapefile from SATVEG", {
    testthat::skip_on_cran()
    cube_satveg <- sits_cube(source = "SATVEG", collection = "terra")

    if (purrr::is_null(cube_satveg)) {
          skip("SATVEG is not accessible")
      }

    shp_file <- system.file(
      "extdata/shapefiles/agriculture/parcel_agriculture.shp", package = "sits"
    )
    parcels <- sits_get_data(cube_satveg,
        file = shp_file,
        shp_attr = "ext_na",
        .n_shp_pol = 3
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

test_that("Reading a LAT/LONG from RASTER", {

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    raster_cube <- sits_cube(
        source = "LOCAL",
        name = "sinop-2014",
        satellite = "TERRA",
        sensor = "MODIS",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "X2", "band", "date")
    )


    point_ndvi <- sits_get_data(raster_cube,
        longitude = -55.66738, latitude = -11.76990
    )

    expect_equal(names(point_ndvi)[1], "longitude")
    expect_true(ncol(sits_time_series(point_ndvi)) == 3)
    expect_true(length(sits_time_series_dates(point_ndvi)) == 23)
})

test_that("Reading a CSV file from RASTER", {
    # skip_on_cran()
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    raster_cube <- sits_cube(
        source = "LOCAL",
        name = "sinop-2014",
        satellite = "TERRA",
        sensor = "MODIS",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "X2", "band", "date")
    )

    csv_raster_file <- system.file("extdata/samples/samples_sinop_crop.csv",
        package = "sits"
    )
    points <- sits_get_data(raster_cube, file = csv_raster_file)

    df_csv <- utils::read.csv(
      system.file("extdata/samples/samples_sinop_crop.csv",
        package = "sits"
    ))
    expect_true(nrow(points) <= NROW(df_csv))

    expect_true("Forest" %in% sits_labels(points))
    expect_equal(names(points)[1], "longitude")
    expect_equal(length(names(points)), 7)
    expect_true(ncol(sits_time_series(points)) == 3)
    expect_true(length(sits_time_series_dates(points)) == 23)
})

test_that("Test reading shapefile from BDC", {
    testthat::skip_on_cran()

    # check "BDC_ACCESS_KEY" - mandatory one per user
    bdc_access_key <- Sys.getenv("BDC_ACCESS_KEY")

    testthat::skip_if(nchar(bdc_access_key) == 0,
                      message = "No BDC_ACCESS_KEY defined in environment.")

    # create a raster cube file based on the information about the files
    cbers_stac_tile <- sits_cube(
        source = "BDC",
        collection = "CB4_64_16D_STK-1",
        name = "cbers_stac",
        bands = c("NDVI", "EVI"),
        tiles = c("022024", "022025"),
        url = "http://brazildatacube.dpi.inpe.br/stac/",
        start_date = "2018-09-01",
        end_date = "2019-08-28"
    )

    if (purrr::is_null(cbers_stac_tile)) {
          skip("BDC is not accessible")
      }

    shp_path <- system.file("extdata/shapefiles/bdc-test/samples.shp",
                            package = "sits"
    )

    time_series_bdc <- sits::sits_get_data(cbers_stac_tile, file = shp_path)
    expect_equal(nrow(time_series_bdc), 10)
    bbox <- sits_bbox(time_series_bdc)
    expect_true(bbox["lon_min"] < -46.)
    expect_true(all(sits_bands(time_series_bdc) %in% c("NDVI", "EVI")))
    ts <- time_series_bdc$time_series[[1]]
    expect_true(max(ts["EVI"]) < 1.)
})
