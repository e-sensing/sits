test_that("Whittaker filter", {
    # skip_on_cran()
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    point_ws <- sits_apply(point_ndvi,
                           NDVI = sits_whittaker(NDVI, lambda = 3.0))

    expect_true(length(sits_timeline(point_ndvi))
    == length(sits_timeline(point_ws)))
})

test_that("Savitsky Golay filter", {

    # skip_on_cran()
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    point_sg <- sits_apply(point_ndvi, NDVI = sits_sgolay(NDVI))

    expect_true(length(sits_timeline(point_ndvi)) ==
        length(sits_timeline(point_sg)))
})
