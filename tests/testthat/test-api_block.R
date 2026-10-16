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
test_that("the chunks of a draw are whole file blocks of a fixed side", {
    image <- c(ncols = 8192L, nrows = 8192L)
    # tiled file, 256 x 256: four blocks on each side
    expect_identical(
        sits:::.jobs_draw_block(c(nrows = 256L, ncols = 256L), image),
        c(ncols = 1024L, nrows = 1024L)
    )
    # striped file, 2 rows of 4096: one block wide, 512 blocks high
    expect_identical(
        sits:::.jobs_draw_block(
            c(nrows = 2L, ncols = 4096L), c(ncols = 4096L, nrows = 4096L)
        ),
        c(ncols = 4096L, nrows = 1024L)
    )
    # a block larger than the side is kept
    expect_identical(
        sits:::.jobs_draw_block(c(nrows = 2048L, ncols = 2048L), image),
        c(ncols = 2048L, nrows = 2048L)
    )
    # the chunk does not exceed the image
    expect_identical(
        sits:::.jobs_draw_block(
            c(nrows = 256L, ncols = 256L), c(ncols = 600L, nrows = 300L)
        ),
        c(ncols = 600L, nrows = 300L)
    )
})
