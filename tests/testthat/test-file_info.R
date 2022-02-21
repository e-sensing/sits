test_that("file_info functions", {

    testthat::skip_on_cran()

    cbers_cube <- sits_cube(
        source = "BDC",
        collection = "CB4_64_16D_STK-1",
        bands = c("NDVI", "EVI"),
        tiles = c("022024", "022025"),
        start_date = "2018-09-01",
        end_date = "2019-08-28"
    )

    # only works with one tile
    expect_error(.file_info(cbers_cube))

    cbers_tile <- cbers_cube[1, ]

    # file info
    expect_s3_class(.file_info(cbers_tile), "tbl_df")
    expect_equal(.file_info(cbers_tile), cbers_tile[["file_info"]][[1]])

    # tile size
    expect_equal(.file_info_nrows(cbers_tile), 6865)
    expect_equal(.file_info_ncols(cbers_tile), 10504)

    # tile paths
    expect_length(.file_info_path(cbers_tile), 1)
    expect_length(.file_info_paths(cbers_tile), 46)

    # tile resolutions
    expect_equal(.file_info_xres(cbers_tile), 63.99735, tolerance = 10e-6)
    expect_equal(.file_info_yres(cbers_tile), 64.00234, tolerance = 10e-6)

    # tile properties
    expect_length(.file_info_fids(cbers_tile), 23)
    expect_length(.file_info_timeline(cbers_tile), 23)
    #expect_length(.file_info_start_date(cbers_tile))
})

test_that("file_info errors", {

    testthat::skip_on_cran()

    s2_cube <- sits_cube(
        source = "AWS",
        collection = "SENTINEL-S2-L2A-COGS",
        bands = c("B01", "B02", "CLOUD"),
        tiles = c("20LKP", "20LLP"),
        start_date = "2018-09-01",
        end_date = "2018-10-01"
    )

    # only works with one tile
    expect_error(.file_info(s2_cube))

    s2_tile <- s2_cube[1, ]

    # file info
    expect_s3_class(.file_info(s2_tile), "tbl_df")

    # raster size
    expect_error(.file_info_nrows(s2_tile))
    expect_error(.file_info_ncols(s2_tile))
})
