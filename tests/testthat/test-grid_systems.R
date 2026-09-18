test_that("MGRS to ROI", {
    roi <- sits_tiles_to_roi("22LBL", grid_system = "MGRS")
    expect_equal(roi[["lon_min"]], -53.77298, tolerance = 0.001)

    tile <- sits_roi_to_tiles(roi, grid_system = "MGRS")
    tile <- dplyr::filter(tile, .data[["coverage_percentage"]] == 100.0)
    expect_equal(tile[["tile_id"]], "22LBL")
})

test_that("Grid systems report their CRS scope", {
    expect_true(sits:::.grid_has_unique_crs("BDC_LG_V2"))
    expect_true(sits:::.grid_has_unique_crs("BDC_MD_V2"))
    expect_true(sits:::.grid_has_unique_crs("BDC_SM_V2"))
    expect_false(sits:::.grid_has_unique_crs("MGRS"))
    # internal grid systems must be requested explicitly
    expect_false(sits:::.grid_has_unique_crs("ALPHAEARTH", private = TRUE))
    expect_error(sits:::.grid_has_unique_crs("ALPHAEARTH"))
})

test_that("Internal grid systems are not exposed to users", {
    roi <- c(
        lon_min = -46.2, lat_min = -23.4,
        lon_max = -46.1, lat_max = -23.3
    )
    # the grid selector must fail
    expect_error(.grid_filter_tiles("ALPHAEARTH", roi, NULL))
    expect_error(sits_roi_to_tiles(roi, grid_system = "ALPHAEARTH"))
    expect_error(sits_tiles_to_roi("23S-2-100", grid_system = "ALPHAEARTH"))
})
