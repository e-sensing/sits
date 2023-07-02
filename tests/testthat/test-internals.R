test_that("Period functions check", {
    expect_error(.period_check("P16"))
    expect_equal(object = .period_val("P16D"), expected = 16)
    expect_equal(object = .period_val("P2M"), expected = 2)
    expect_equal(object = .period_val("P1Y"), expected = 1)
    expect_equal(object = .period_unit("P16D"), expected = "day")
    expect_equal(object = .period_unit("P2M"), expected = "month")
    expect_equal(object = .period_unit("P1Y"), expected = "year")
})

test_that("Tests functions", {
    expect_equal(class(sits_run_tests()), "logical")
    expect_equal(class(sits_run_examples()), "logical")
})

test_that("Timeline tests", {
    s2_cube <- tryCatch(
        {
            sits_cube(
                source = "MPC",
                collection = "sentinel-2-l2a",
                tiles = "20LKP",
                bands = c("B05", "B8A", "CLOUD"),
                start_date = "2019-07-18",
                end_date = "2019-08-30",
                progress = FALSE
            )
        },
        error = function(e) {
            return(NULL)
        }
    )

    testthat::skip_if(
        purrr::is_null(s2_cube),
        "MPC is not accessible"
    )

    tla <- .cube_timeline_acquisition(
        s2_cube,
        origin = as.Date("2019-07-18"),
        period = "P16D"
    )
    expect_s3_class(tla, "tbl_df")
    expect_equal(colnames(tla), c("from_date", "to_date", "20LKP"))
})
