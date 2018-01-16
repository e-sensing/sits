library(testthat)
library(sits)
test_check("sits")

context("Validation")
testthat::test_that("Does k-fold validate and build confusion matrix", {
    data(cerrado_2classes)
    pred_ref.tb <-  sits_kfold_validate(cerrado_2classes, folds = 2)
    conf.mx <- sits_conf_matrix(pred_ref.tb)

    testthat::expect_true(NROW(pred_ref.tb) == NROW(cerrado_2classes))
    testthat::expect_true(NROW(conf.mx$table) > 1)
})

context("Classification of time series")
testthat::test_that("Classify a time series the simplest way", {
    testthat::skip_on_cran()
    data(samples_MT_ndvi)
    data(point_ndvi)
    class_ndvi.tb <-  sits_classify(point_ndvi, samples_MT_ndvi)

    testthat::expect_true(NROW(class_ndvi.tb$predicted[[1]]) == 16)
    testthat::expect_true(all(class_ndvi.tb$predicted[[1]]$class %in%
                                  sits_labels(samples_MT_ndvi)$label))
})
testthat::test_that("Classify time series with an explicit model",{
    testthat::skip_on_cran()
    data(samples_MT_ndvi)
    samples.tb <- sits_select(samples_MT_9classes,
                               bands = c("ndvi","evi"))
    model <- sits_train(samples.tb)
    data(ts_2000_2016)
    point.tb <- sits_select(ts_2000_2016, bands = c("ndvi","evi"))
    class.tb <- sits_classify_model(point.tb, samples.tb, model)

    testthat::expect_true(all(class.tb$predicted[[1]]$class %in%
                                  sits_labels(samples_MT_9classes)$label))
})

context("Clustering")
testthat::test_that("Creating a dendogram and clustering the results",{
    testthat::skip_on_cran()
    library(dtwclust)
    data(cerrado_2classes)
    dendro.obj   <- sits_dendrogram(cerrado_2classes, bands = c("ndvi", "evi"))
    clustered.tb <- sits_cluster(cerrado_2classes, dendro.obj, k = 6)
    result.mtx   <- sits_cluster_frequency(clustered.tb)
    clean.tb     <- sits_cluster_cleaner(clustered.tb, min_perc = 0.10)

    testthat::expect_true(NROW(dendro.obj@clusinfo) == NROW(cerrado_2classes))
    testthat::expect_true(NROW(clustered.tb$cluster) == NROW(dendro.obj@clusinfo))
    testthat::expect_true(NROW(result.mtx)  ==
                              (length(sits_labels(cerrado_2classes)$label) + 1))
    testthat::expect_true(all(unique(clean.tb$cluster) %in%
                                  unique(clustered.tb$cluster)))
})

context("Coverage")
testthat::test_that("Creating a WTSS coverage", {
    coverage.tb <- sits_coverage(service = "WTSS", product = "MOD13Q1",
                                 coverage = "mod13q1_512")
    testthat::expect_true(coverage.tb$service == "WTSS")
})
testthat::test_that("Creating a SATVEG coverage", {
    coverage.tb <- sits_coverage(service = "SATVEG", product = "MOD13Q1",
                                 coverage = "terra")
    testthat::expect_true(coverage.tb$service == "SATVEG")
})
testthat::test_that("Creating a raster coverage", {
    testthat::skip_on_cran()
    files  <- c(system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif", package = "sits"))
    data(timeline_mod13q1)
    timeline <- lubridate::as_date(timeline_mod13q1$V1)
    raster.tb <- sits_coverageRaster(product = "MOD13Q1", coverage = "Sinop-crop",
                                     timeline = timeline, bands = c("ndvi"),
                                     files = files)

    testthat::expect_true(raster.tb$r_obj[[1]]@nrows == raster.tb$nrows)
    testthat::expect_true(raster.tb$r_obj[[1]]@extent@xmin == raster.tb$xmin)
})

context("Data input")
testthat::test_that("Reading a CSV file from WTSS", {
    testthat::skip_on_cran()
    csv_file <- system.file("extdata/samples/samples_matogrosso.csv", package = "sits")
    points.tb <- sits_fromCSV(csv_file = csv_file, service = "WTSS", product = "MOD13Q1",
                              coverage = "mod13q1_512")
    testthat::expect_true(NROW(points.tb) > 1)
    testthat::expect_true(length(points.tb[1,]$time_series[[1]]$Index) ==
                              length(points.tb[NROW(points.tb),]$time_series[[1]]$Index))
})

testthat::test_that("Reading a point from SATVEG ",{
    testthat::skip_on_cran()
    point_terra.tb <- sits_fromSATVEG (longitude = -55.50563, latitude = -11.71557,
                                       product = "MOD13Q1", coverage = "terra")
    point_comb.tb <- sits_fromSATVEG (longitude = -55.50563, latitude = -11.71557,
                                      product = "MOD-MYD13Q1", coverage = "comb")

    testthat::expect_true(length(point_comb.tb$time_series[[1]]$Index) >=
                              length(point_terra.tb$time_series[[1]]$Index))
})

context("Filtering")
testthat::test_that("Envelope filter", {
    data(prodes_226_064)
    point_ndvi.tb <- sits_select(prodes_226_064[1,], bands = c("ndvi"))
    point_env.tb  <- sits_envelope(point_ndvi.tb)
    testthat::expect_true(all(point_env.tb$time_series[[1]][,2] >= point_ndvi.tb$time_series[[1]][,2]))
})

testthat::test_that("Cloud filter", {
    data(prodes_226_064)
    point_ndvi.tb <- sits_select(prodes_226_064[1,], bands = c("ndvi"))
    point_cld.tb  <- sits_cloud_filter(point_ndvi.tb)
    testthat::expect_true(min(point_cld.tb$time_series[[1]][,2]) >= min(point_ndvi.tb$time_series[[1]][,2]))
})

testthat::test_that("Whittaker filter", {
    data(point_ndvi)
    point_ws <- sits_whittaker(point_ndvi, lambda = 3.0)
    testthat::expect_true(NROW(point_ndvi$time_series[[1]]) == NROW(point_ws$time_series[[1]]))
})

testthat::test_that("Savitsky Golay filter", {
    data(point_ndvi)
    point_sg <- sits_sgolay(point_ndvi)
    testthat::expect_true(NROW(point_ndvi$time_series[[1]]) == NROW(point_sg$time_series[[1]]))
})
