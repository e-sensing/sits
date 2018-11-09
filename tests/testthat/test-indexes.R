context("Config")

test_that("NDWI", {
    data (samples_MT_9classes)
    ndwi.tb <- sits_ndwi(samples_MT_9classes)

    expect_true("ndwi" %in% names(ndwi.tb$time_series[[1]]))
})

test_that("SAVI", {
    data (samples_MT_9classes)
    savi.tb <- sits_savi(samples_MT_9classes)

    expect_true("savi" %in% names(savi.tb$time_series[[1]]))
})

test_that("Tasseled", {
    data (samples_MT_9classes)
    ts.tb <- sits_tasseled_cap(samples_MT_9classes, satellite = "MODIS")

    expect_true("tcb" %in% names(ts.tb$time_series[[1]]))
    expect_true("tcg" %in% names(ts.tb$time_series[[1]]))
    expect_true("tcw" %in% names(ts.tb$time_series[[1]]))
})
