test_that("Find MGRS tiles with bbox", {
    roi1 <- c(
        lon_min = -64.03732099,
        lat_min = -9.64467633,
        lon_max = -63.88698997,
        lat_max = -9.38935222
    )

    tiles <- sits_find_tiles(roi1)
    expect_s3_class(tiles, "sf")
    expect_equal(
        colnames(tiles), c("tile_id", "coverage_percentage", "geom")
    )
    expect_equal(tiles[["tile_id"]], c("20LLQ", "20LMQ"))
})

test_that("Find MGRS tiles with multipolygon", {
    roi1 <- c(
        lon_min = -64.03732099,
        lat_min = -9.64467633,
        lon_max = -63.88698997,
        lat_max = -9.38935222
    )
    roi1 <- .roi_as_sf(roi1)
    roi2 <- c(
        lon_min = -65.04532648,
        lat_min = -9.68396664,
        lon_max = -64.93041845,
        lat_max = -9.57234169
    )
    roi2 <- .roi_as_sf(roi2)
    polys <- dplyr::bind_rows(list(roi1, roi2))
    multi_poly <- sf::st_cast(polys, "MULTIPOLYGON")

    tiles <- sits_find_tiles(multi_poly)
    expect_s3_class(tiles, "sf")
    expect_equal(
        colnames(tiles), c("tile_id", "coverage_percentage", "geom")
    )
    expect_equal(tiles[["tile_id"]], c("20LKQ", "20LLQ", "20LMQ"))
})

test_that("Find MGRS tiles with points", {
    pt1 <- sf::st_point(c(-64.3887, -10.4357))
    pt <- sf::st_sfc(pt1, crs = 4326)
    tiles <- sits_find_tiles(pt)
    expect_s3_class(tiles, "sf")
    # We apply a small buffer in the point, turning it into a polygon.
    expect_equal(
        colnames(tiles), c("tile_id", "coverage_percentage", "geom")
    )
    expect_equal(tiles[["tile_id"]], c("20LLP"))

    pt2 <- sf::st_point(c(-63.4497, -12.1725))
    pts <- sf::st_sfc(pt1, pt2, crs = 4326)
    tiles <- sits_find_tiles(pts)
    expect_s3_class(tiles, "sf")
    expect_equal(
        colnames(tiles), c("tile_id", "geom")
    )
    expect_equal(tiles[["tile_id"]], c("20LLP", "20LMM"))
})

test_that("Find BDC tiles with bbox", {
    roi <- c(
        lon_min = -71.66605459,
        lat_min = -8.65079126,
        lon_max = -71.58148249,
        lat_max = -8.56555523
    )

    tiles <- sits_find_tiles(roi, grid_system = "BDC_SM_V2")
    expect_s3_class(tiles, "sf")
    # We apply a small buffer in the point, turning it into a polygon.
    expect_equal(
        colnames(tiles), c("tile_id", "coverage_percentage", "geom")
    )
    expect_equal(tiles[["tile_id"]], c("004015"))
})

test_that("Find BDC tiles with points", {
    pt1 <- sf::st_point(c(-64.3887, -10.4357))
    pt <- sf::st_sfc(pt1, crs = 4326)
    tiles <- sits_find_tiles(pt, grid_system = "BDC_SM_V2")
    expect_s3_class(tiles, "sf")
    # We apply a small buffer in the point, turning it into a polygon.
    expect_equal(
        colnames(tiles), c("tile_id", "coverage_percentage", "geom")
    )
    expect_equal(tiles[["tile_id"]], c("011017"))

    pt2 <- sf::st_point(c(-63.4497, -12.1725))
    pts <- sf::st_sfc(pt1, pt2, crs = 4326)
    tiles <- sits_find_tiles(pts, grid_system = "BDC_SM_V2")
    expect_s3_class(tiles, "sf")
    expect_equal(
        colnames(tiles), c("tile_id", "geom")
    )
    expect_equal(tiles[["tile_id"]], c("011017", "012018"))
})
