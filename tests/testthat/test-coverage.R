context("Coverage")
test_that("Bbox", {
    expect_equal(sum(sits:::.sits_get_bbox("RASTER", NULL, raster::raster())), 0)
})
