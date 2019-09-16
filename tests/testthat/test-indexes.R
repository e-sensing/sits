context("Config")

test_that("NDWI", {
    ndwi.tb <- sits_ndwi(samples_mt_4bands)

    expect_true("ndwi" %in% names(ndwi.tb$time_series[[1]]))
})

test_that("SAVI", {
    data (samples_mt_6bands)
    savi.tb <- sits_savi(samples_mt_6bands)

    expect_true("savi" %in% names(savi.tb$time_series[[1]]))
})

test_that("Tasseled", {
    data(samples_mt_6bands)
    ts.tb <- sits_tasseled_cap(samples_mt_6bands)

    expect_true("tcb" %in% names(ts.tb$time_series[[1]]))
    expect_true("tcg" %in% names(ts.tb$time_series[[1]]))
    expect_true("tcw" %in% names(ts.tb$time_series[[1]]))
})
