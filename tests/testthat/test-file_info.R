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

    # file info nrows
    expect_equal(.file_info_nrows(cbers_tile), 6865)

    # file info nrows


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

    # file info nrows
    expect_error(.file_info_nrows(s2_tile))

    # file info ncols
    expect_error(.file_info_ncols(s2_tile))

    expect_error(.file_info_ncols(s2_tile))


})
