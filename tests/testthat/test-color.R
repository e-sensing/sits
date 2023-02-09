test_that("sits colors", {
    g <- sits_colors_show()
    expect_equal(g$labels$xmin, "x + 0.05")
    expect_equal(g$labels$label, "name")

    color_tb <- sits_colors()
    expect_equal(color_tb[1, ]$name, "Evergreen_Broadleaf_Forest")
    expect_equal(unname(color_tb[1, ]$color), "#1E8449")
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
        labels = c("ClearCut_Burned", "ClearCut_BareSoil",
                   "ClearCut_Vegetation", "Forest")
    )
    msg_plot1 <- tryCatch({
        plot(ro_class)
        NULL
    }, warning = function(x) x)

    expect_true(grepl(pattern = "labels missing", x = msg_plot1))
    sits_labels(ro_class) <- c("ClearCut_Burn", "ClearCut_BareSoil",
                               "Highly_Degraded", "Forest")
    msg_plot2 <- tryCatch({
        plot(ro_class)
        NULL
    }, warning = function(x) x)
    expect_true(purrr::is_null(msg_plot2))
})

test_that("colors_get", {
    labels <- c("Forest", "Cropland", "Pasture")
    colors <- suppressWarnings(sits:::.colors_get(labels))
    expect_length(colors, 3)
    expect_equal(colors[["Forest"]], "#1E8449")
})
