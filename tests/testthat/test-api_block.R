test_that("API block", {

    x <- list(a = 0, z = 0)
    y <- .block(x)
    expect_null(y)
    expect_false(.has_block(y))

    .col(x) <- 1
    .row(x) <- 2
    .ncols(x) <- 3
    .nrows(x) <- 4
    expect_true(.has_block(x))
    expect_equal(.row(x), 2)
    expect_equal(.col(x), 1)
    expect_equal(.ncols(x), 3)
    expect_equal(.nrows(x), 4)

    z <- list(a = 0, col = 1, row = 2, ncols = 3, nrows = 4, z = 0)
    expect_true(.has_block(z))

    s0 <- .block_size(z, overlap = 0)
    expect_equal(s0, 12)

    s2 <- .block_size(z, overlap = 2)
    expect_equal(s2, 56)

    expect_equal(.col(z), 1)



})
