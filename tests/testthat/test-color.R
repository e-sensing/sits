context("Color")

test_that("Color name",{
    expect_equal(sits:::.sits_color_name("BluePurple"), "BuPu")
})

test_that("Color name",{
    expect_equal(sits:::.sits_max_colors("BuPu"), 19)
})
