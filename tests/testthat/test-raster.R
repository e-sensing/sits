testthat::test_that("Working with raster coverages", {
    testthat::skip_on_cran()
    library(rgdal)
    library(raster)
    library(sf)
    files  <- c(system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif", package = "sits"))
    data(timeline_mod13q1)
    timeline <- lubridate::as_date(timeline_mod13q1$V1)
    raster.tb <- sits_coverage(service = "RASTER",
                               name = "Sinop-crop",
                               timeline = timeline,
                               bands = c("ndvi"),
                               files = files)

    r_obj <- sits_get_raster(raster.tb, 1)
    testthat::expect_true(raster::nrow(r_obj) == raster.tb$nrows)
    testthat::expect_true(raster::xmin(r_obj) == raster.tb$xmin)

    point.tb <- sits_getdata(coverage = raster.tb, longitude = -55.55502, latitude = -11.52774)

    testthat::expect_true(length(point.tb$time_series[[1]]$Index) == length(timeline))

    raster_class.tb <- sits_classify_raster(file = "./raster-class", raster.tb, samples_MT_ndvi,
                                            ml_method = sits_svm(), blocksize = 300000, multicores = 2)

    testthat::expect_true(all(file.exists(unlist(raster_class.tb$files))))
    rc_obj <- sits_get_raster(raster_class.tb, 1)
    testthat::expect_true(raster::nrow(rc_obj) == raster_class.tb[1,]$nrows)
    testthat::expect_true(all(file.remove(unlist(raster_class.tb$files))))

})
