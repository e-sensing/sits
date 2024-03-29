test_that("sits colors", {
    g <- sits_colors_show("PRODES")
    expect_equal(g$labels$xmin, "x + 0.05")

    color_tb <- sits_colors()
    expect_equal(color_tb[1, ]$name, "Evergreen_Broadleaf_Forest")
    expect_equal(unname(color_tb[1, ]$color), "#518940")

    color_tb[1, ]$name <- "Tropical_Forest"
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
        parse_info = c(
            "X1", "X2", "tile", "start_date", "end_date",
            "band", "version"
        ),
        bands = "class",
        labels = c(
            "1" = "Clear_Cut_Burned_Area", "2" = "Clear_Cut_Bare_Soil",
            "3" = "Clear_Cut_Vegetation", "4" = "Forest"
        ),
        progress = FALSE
    )
    p <- plot(ro_class)
    expect_equal(p$tm_shape$line.center, "midpoint")
    expect_equal(p$tm_layout$legend.bg.color, "white")
    expect_equal(
        unname(p$tm_raster$labels),
        c("Clear_Cut_Burned_Area", "Clear_Cut_Bare_Soil",
          "Clear_Cut_Vegetation", "Forest")
    )
})

test_that("colors_get", {
    labels <- c("Forest", "Cropland", "Pasture")
    colors <- suppressWarnings(sits:::.colors_get(labels,
        legend = NULL,
        palette = "Spectral",
        rev = TRUE
    ))
    expect_length(colors, 3)
    expect_equal(colors[["Forest"]], "#518940")
})

test_that("legend", {
    def_legend <- c(
        "Forest" = "forestgreen", "Cerrado" = "lightgreen",
        "Pasture" = "bisque2", "SoyCorn" = "sienna"
    )

    labels <- c("Forest", "Cerrado", "Pasture", "SoyCorn")

    colors <- suppressWarnings(sits:::.colors_get(labels,
        legend = def_legend,
        palette = "Spectral",
        rev = TRUE
    ))
    expect_true(all(names(colors) %in% labels))

    def_legend_2 <- c(
        "Forest" = "forestgreen", "Cerrado" = "lightgreen",
        "Pasture" = "bisque2"
    )

    expect_warning({
        expect_warning({
            sits:::.colors_get(labels,
                legend = def_legend_2,
                palette = "Spectral", rev = TRUE
            )
        })
    })
})
