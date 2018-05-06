testthat::context("Raster classification")
testthat::test_that("Working with raster coverages", {
    testthat::skip_on_cran()
    library(rgdal)
    library(raster)
    library(sf)
    files  <- c(system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif", package = "sits"))
    data("timeline_modis_392")
    raster.tb <- sits_coverage(service = "RASTER",
                               name = "Sinop-crop",
                               timeline = timeline_modis_392,
                               bands = c("ndvi"),
                               files = files)

    r_obj <- unlist(raster.tb$r_objs[[1]][[1]])
    testthat::expect_true(raster::nrow(r_obj) == raster.tb$nrows)
    testthat::expect_true(raster::xmin(r_obj) == raster.tb$xmin)

    point.tb <- sits_getdata(coverage = raster.tb, longitude = -55.55502, latitude = -11.52774)

    testthat::expect_true(length(point.tb$time_series[[1]]$Index) == length(timeline_modis_392))

    svm_model <- sits_train(samples_MT_ndvi, sits_svm())

    raster_class.tb <- sits_classify_raster(file = paste0(tempdir(),"/raster-class"), raster.tb,
                                            svm_model, memsize = 4)

    testthat::expect_true(all(file.exists(unlist(raster_class.tb$files))))
    rc_obj <- raster_class.tb[1,]$r_objs[[1]]
    testthat::expect_true(raster::nrow(rc_obj) == raster_class.tb[1,]$nrows)
    testthat::expect_true(all(file.remove(unlist(raster_class.tb$files))))

})
