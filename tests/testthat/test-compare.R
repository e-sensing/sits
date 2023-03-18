test_that("is equal tests", {
    expect_error(.is_eq(1, 1, tolerance = -1))

    expect_true(.is_eq(1, 1, tolerance = 0))

    expect_false(.is_eq(1, 2, tolerance = 0))

    expect_true(.is_eq(1, 2, tolerance = 1))

    expect_true(all(.is_eq(runif(10), 1, tolerance = 1)))

    expect_false(all(.is_eq(runif(100), 1, tolerance = 0.1)))

    # tests with float values
    expect_false(.is_eq(-1.5, -1.4, tolerance = 0.1))

    expect_true(.is_eq(1.2 - 1.1, 0.1, tolerance = 1e-8))
})

test_that("is less than tests", {
    expect_error(.is_lt(1, 1, tolerance = -1))

    expect_true(.is_lt(1, 2, tolerance = 0))

    expect_true(.is_lt(1, 2, tolerance = 0.5))

    expect_false(.is_lt(1, 2, tolerance = 1))

    expect_true(all(.is_lt(runif(10), 2, tolerance = 1)))

    expect_false(all(.is_lt(runif(100), 1, tolerance = 0.1)))

    # tests with float values
    expect_true(.is_lt(-1.5, -1.4, tolerance = 0.1))

    expect_true(1.2 - 1.1 < 0.1)

    expect_false(.is_lt(1.2 - 1.1, 0.1, tolerance = 1e-8))

    expect_false(.is_lt(1.5 - 1.4, 0.1, tolerance = 1e-8))
})

test_that("is greater than tests", {
    expect_error(.is_gt(1, 1, tolerance = -1))

    expect_true(.is_gt(2, 1, tolerance = 0))

    expect_true(.is_gt(2, 1, tolerance = 0.5))

    expect_false(.is_gt(2, 1, tolerance = 1))

    expect_true(all(.is_gt(2, runif(10), tolerance = 1)))

    expect_false(all(.is_gt(1, runif(100), tolerance = 0.1)))

    # tests with float values
    expect_true(.is_gt(-1.4, -1.5, tolerance = 0.1))

    expect_true(1.5 - 1.4 > 0.1)

    expect_false(.is_gt(1.5 - 1.4, 0.1, tolerance = 1e-8))

    expect_true(.is_gt(1.5 - 1.4, 0.1, tolerance = 0))

    expect_false(.is_gt(1.2 - 1.1, 0.1, tolerance = 1e-8))
})
