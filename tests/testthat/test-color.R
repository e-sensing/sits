test_that("sits colors", {
    expect_error(sits_colors("Green"))
    expect_length(sits_colors("Greens"), 32)
})

test_that("sits color show", {
    expect_error(sits_colors_show("Green"))
    expect_length(sits_colors_show("Greens"), 32)
})

test_that("sits color names", {
    color_names <- sits_color_names()
    expect_length(color_names, 35)
})
test_that("colors_get", {
    labels <- c("Forest", "Agriculture", "Pasture")
    colors <- suppressWarnings(sits:::.colors_get(labels))
    expect_length(colors, 3)
    expect_equal(colors[["Forest"]], "#7DB0DDFF")
})
