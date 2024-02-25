test_that("sits colors", {
    g <- sits_colors_show("PRODES")
    expect_equal(g$labels$xmin, "x + 0.05")
    sits_colors_reset()

    color_tb <- sits_colors()
    expect_equal(color_tb[1, ]$name, "Evergreen_Broadleaf_Forest")
    expect_equal(unname(color_tb[1, ]$color), "#1E8449")

    us_nlcd <- tibble::tibble(name = character(), color = character())
    us_nlcd <- us_nlcd |>
        tibble::add_row(name = "Urban_Built_Up", color = "#85929E") |>
        tibble::add_row(name = "Agricultural_Land", color = "#F0B27A") |>
        tibble::add_row(name = "Rangeland", color = "#F1C40F") |>
        tibble::add_row(name = "Forest_Land", color = "#27AE60") |>
        tibble::add_row(name = "Water", color = "#2980B9") |>
        tibble::add_row(name = "Wetland", color = "#D4E6F1") |>
        tibble::add_row(name = "Barren_Land", color = "#FDEBD0") |>
        tibble::add_row(name = "Tundra", color = "#EBDEF0") |>
        tibble::add_row(name = "Snow_and_Ice", color = "#F7F9F9")

    # Load the color table into `sits`
    new_color_tb <- sits_colors_set(colors = us_nlcd, legend = "US_NLCD")

    us_nlcd_color_1 <- dplyr::filter(new_color_tb, name == "Urban_Built_Up")
    expect_equal(us_nlcd_color_1$color, "#85929E")

    mycolors <- tibble::tibble(name = character(), color = character())
    mycolors <- mycolors |>
        tibble::add_row(name = "Savannas", color = "#F8C471") |>
        tibble::add_row(name = "Grasslands", color = "#ABEBC6")

    # set the user-defined colors
    new_color_tb2 <- sits_colors_set(colors = mycolors)
    new_color_grass <- dplyr::filter(new_color_tb2, name == "Grasslands")
    expect_equal(new_color_grass$color, "#ABEBC6")
    sits_colors_reset()
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
    expect_equal(colors[["Forest"]], "#1E8449")
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
