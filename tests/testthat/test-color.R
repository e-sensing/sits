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
        labels = c("ClearCut_Burned", "ClearCut_BareSoil",
                   "ClearCut_Veg", "Forest")
    )
    msg_plot1 <- tryCatch({
        plot(ro_class)
        NULL
    }, warning = function(x) x)

    expect_true(grepl(pattern = "missing colors", x = msg_plot1))
    sits_labels(ro_class) <- c("ClearCut_Burn", "ClearCut_Soil",
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

    labels2 <- c("Forest", "Cropland", "Pastagem")
    colors2 <- suppressWarnings(sits:::.colors_get(labels2))
    expect_equal(colors2[["Pastagem"]], "#584B9FFF")

    labels3 <- c("Forest", "Cropland", "Pastagem", "Soja")
    leg3 <- c("Pastagem" = "azure", "Forest" = "green")
    colors3 <- suppressWarnings(sits:::.colors_get(labels3, legend = leg3))
    expect_equal(colors3[["Soja"]], "#584B9FFF")
    expect_equal(colors3[["Pastagem"]], "azure")
    expect_equal(colors3[["Forest"]], "green")
})
