test_that("Whittaker filter", {
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    point_ws <- sits_filter(point_ndvi, filter = sits_whittaker(lambda = 3.0))

    expect_true(length(sits_timeline(point_ndvi))
    == length(sits_timeline(point_ws)))
})

test_that("Savitzky-Golay filter", {
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    point_sg <- sits_filter(point_ndvi, filter = sits_sgolay())

    expect_true(length(sits_timeline(point_ndvi)) ==
        length(sits_timeline(point_sg)))

    expect_error(sits_sgolay(point_ndvi, length = 6))
    expect_error(sits_sgolay(point_ndvi, order = 5, length = 5))
})
