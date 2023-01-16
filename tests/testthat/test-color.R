test_that("sits colors", {
    expect_true("Greens" %in% sits_color_names())
    expect_error(sits_colors("Green"))
    expect_length(sits_colors("Greens"), 32)
})

test_that("sits color show", {
    expect_error(sits_colors_show("Green"))
    expect_length(sits_colors_show("Greens"), 32)
})

test_that("plot colors", {
    data_dir <- system.file("extdata/raster/classif", package = "sits")
    ro_class <- sits_cube(
        source = "MPC",
        collection = "SENTINEL-2-L2A",
        data_dir = data_dir,
        parse_info = c("X1", "X2", "tile", "start_date", "end_date",
                       "band", "version"),
        bands = "class",
        labels = c("ClearCut_Fire", "ClearCut_BareSoil",
                   "ClearCut_Veg", "Forest")
    )
    msg_plot1 <- tryCatch({
        plot(ro_class)
        NULL
    }, warning = function(x) x)

    expect_true(grepl(pattern = "most labels", x = msg_plot1))
    sits_labels(ro_class) <- c("ClearCut_Burn", "ClearCut_BareSoil",
                               "Highly_Degraded", "FlorestA")
    msg_plot2 <- tryCatch({
        plot(ro_class)
        NULL
    }, warning = function(x) x)
    expect_true(grepl(pattern = "some labels", x = msg_plot2))
})

test_that("colors_get", {
    labels <- c("Forest", "Agriculture", "Pasture")
    colors <- suppressWarnings(sits:::.colors_get(labels))
    expect_length(colors, 3)
    expect_equal(colors[["Forest"]], "#7DB0DDFF")
})
