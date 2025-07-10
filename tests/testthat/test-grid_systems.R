test_that("MGRS to ROI", {
    roi <- sits_mgrs_to_roi("22LBL")
    expect_equal(roi[["xmin"]], -53.77298, tolerance = 0.001)
    tile <- sits_roi_to_mgrs(roi)
    tile <- dplyr::filter(tile, .data[["coverage_percentage"]] == 100.0)
    expect_equal(tile[["tile_id"]], "22LBL")
})
