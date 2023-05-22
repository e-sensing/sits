test_that("sits colors", {
    g <- sits_colors_show()
    expect_equal(g$labels$xmin, "x + 0.05")
    expect_equal(g$labels$label, "name")

    color_tb <- sits_colors()
    expect_equal(color_tb[1, ]$name, "Evergreen_Broadleaf_Forest")
    expect_equal(unname(color_tb[1, ]$color), "#1E8449")

    color_val <- sits_color_value("Evergreen_Broadleaf_Forest")
    expect_equal(color_val, "#1E8449")

    color_tb[1,]$name = "Tropical_Forest"
    new_tb <- sits_colors_set(color_tb)
    expect_equal(new_tb[1, ]$name, "Tropical_Forest")
    sits_colors_reset()
    old_tb <- sits_colors()
    expect_equal(old_tb[1, ]$name, "Evergreen_Broadleaf_Forest")
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
        labels = c("ClearCut_Burn", "ClearCut_Soil",
                   "ClearCut_Veg", "Forest")
    )
    p <- plot(ro_class)
    expect_equal(p$tm_shape$line.center, "midpoint")
    expect_equal(p$tm_layout$legend.bg.color, "white")
    expect_equal(unname(p$tm_raster$labels),
                 c("ClearCut_Burn", "ClearCut_Soil","ClearCut_Veg", "Forest"))
})

test_that("colors_get", {
    labels <- c("Forest", "Cropland", "Pasture")
    colors <- suppressWarnings(sits:::.colors_get(labels))
    expect_length(colors, 3)
    expect_equal(colors[["Forest"]], "#1E8449")
})
