testthat::context("Data input")
testthat::test_that("Creating a WTSS coverage", {
    testthat::skip_on_cran()
    coverage_wtss <- sits_coverage(service = "WTSS-INPE", name = "MOD13Q1")
    testthat::expect_true(coverage_wtss$service == "WTSS-INPE")
})
testthat::test_that("Creating a SATVEG coverage", {
    testthat::skip_on_cran()
    coverage_satveg <- sits_coverage(service = "SATVEG",  name = "terra")
    testthat::expect_true(length(coverage_satveg$timeline[[1]][[1]]) >  1)
})

testthat::test_that("Reading a CSV file from WTSS", {
    testthat::skip_on_cran()
    csv_file <- system.file("extdata/samples/samples_matogrosso.csv", package = "sits")
    coverage_wtss <- sits_coverage(service = "WTSS-INPE", name = "MOD13Q1")
    points.tb <- sits_getdata(coverage = coverage_wtss, file = csv_file)
    df_csv <- utils::read.csv(system.file("extdata/samples/samples_matogrosso.csv", package = "sits"))
    testthat::expect_true(NROW(points.tb) == NROW(df_csv))
})

testthat::test_that("Reading a CSV file from RASTER", {
    testthat::skip_on_cran()
    files  <- c(system.file ("extdata/raster/mod13q1/sinop-crop-ndvi.tif", package = "sits"))
    raster_cov <- sits_coverage(files = files, name = "Sinop-crop",
                                timeline = timeline_modis_392, bands = c("ndvi"))
    csv_raster_file <- system.file ("extdata/samples/samples_sinop_crop.csv", package = "sits")
    points.tb <- sits_getdata (raster_cov, file = csv_raster_file)
    df_csv <- utils::read.csv(system.file("extdata/samples/samples_sinop_crop.csv", package = "sits"))
    testthat::expect_true(NROW(points.tb) == NROW(df_csv))
})

testthat::test_that("Reading a point from WTSS ",{
    testthat::skip_on_cran()
    coverage_wtss <- sits_coverage(service = "WTSS-INPE", name = "MOD13Q1")
    point.tb <- sits_getdata(coverage = coverage_wtss, longitude = -55.50563, latitude = -11.71557)
    timeline <- as.vector(point.tb$time_series[[1]]$Index)

    testthat::expect_true(point.tb$start_date == timeline[1])
    testthat::expect_true(point.tb$end_date == timeline[length(timeline)])
})

testthat::test_that("Reading a point from SATVEG ",{
    testthat::skip_on_cran()
    coverage_1 <- sits_coverage(service = "SATVEG",  name = "terra")
    coverage_2 <- sits_coverage(service = "SATVEG",  name = "aqua")
    coverage_3 <- sits_coverage(service = "SATVEG",  name = "comb")

    point_terra.tb <- sits_getdata(coverage_1, longitude = -55.50563, latitude = -11.71557)
    point_aqua.tb <- sits_getdata(coverage_2, longitude = -55.50563, latitude = -11.71557)
    point_comb.tb <- sits_getdata(coverage_3, longitude = -55.50563, latitude = -11.71557)

    testthat::expect_true(length(point_comb.tb$time_series[[1]]$Index) >=
                              length(point_terra.tb$time_series[[1]]$Index))
})

testthat::test_that("Reading a ZOO time series", {
    testthat::skip_on_cran()
    data(ts_zoo)
    data.tb <- sits_fromZOO(ts_zoo, longitude = -54.2313, latitude = -14.0482,
                            label = "Cerrado", name = "mod13q1")

    testthat::expect_true(NROW(ts_zoo) == NROW(data.tb$time_series[[1]]))
})

testthat::test_that("Reading a shapefile", {
    testthat::skip_on_cran()
    coverage_wtss <- sits_coverage(service = "WTSS-INPE", name = "MOD13Q1")
    shp_file <- system.file("extdata/shapefiles/santa_cruz_minas.shp", package = "sits")
    munic.tb <- sits_getdata(coverage = coverage_wtss, file = shp_file)

    sf_shape <- sf::read_sf(shp_file)
    bbox <- sf::st_bbox(sf_shape)
    longitudes_shp <- munic.tb$longitude

    testthat::expect_true(all(unique(longitudes_shp) > bbox["xmin"]))
    testthat::expect_true(all(unique(longitudes_shp) < bbox["xmax"]))

})

testthat::test_that("Labels and re-label",{
    testthat::skip_on_cran()
    data(prodes_226_064)
    conv.lst = list("Deforestation_2014" = "NonForest",
                    "Deforestation_2015" = "NonForest",
                    "Forest" = "Forest",
                    "Pasture" = "NonForest")
    new_data.tb <- sits_relabel(prodes_226_064, conv.lst)

    testthat::expect_true(length(sits_labels(prodes_226_064)$label) > length(sits_labels(new_data.tb)$label))
})

