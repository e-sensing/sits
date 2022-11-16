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
