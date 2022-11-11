test_that("sits colors", {
    expect_error(sits_colors("Green"))
    expect_length(sits_colors("Greens"), 32)
})

test_that("sits color show", {
    expect_error(sits_colors_show("Green"))
    expect_length(sits_colors_show("Greens"), 32)
})
