context("Color")

test_that("Colors", {
    expect_equal(sits:::.sits_color_name("BluePurple"), "BuPu")

    expect_equal(sits:::.sits_max_colors("BuPu"), 19)

    nc <- sits:::.sits_max_colors()
    expect_equal(length(nc), 35)
    expect_equal(nc$Spectral, 10)
})
