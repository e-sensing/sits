test_that("MGRS to ROI", {
    roi <- sits_tiles_to_roi("22LBL", grid_system = "MGRS")
    expect_equal(roi[["lon_min"]], -53.77298, tolerance = 0.001)

    tile <- sits_roi_to_tiles(roi, grid_system = "MGRS")
    tile <- dplyr::filter(tile, .data[["coverage_percentage"]] == 100.0)
    expect_equal(tile[["tile_id"]], "22LBL")
})
