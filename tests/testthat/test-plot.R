#---- Utilitary functions ----


#' Save the image resulting from running the given code.
#'
#' @param code   R code.
#' @param wight  An integer. The image's width.
#' @param height An integer. The image's height.
#' @return       A character. The path to the image.
save_png <- function(code, width = 400, height = 400) {
    path <- tempfile(fileext = ".png")
    png(path, width = width, height = height)
    on.exit(dev.off())
    code
    path
}


#' Run a test on code that produces an image.
#'
#' @param name A character. A name for the resulting image.
#' @param code R code that produces an image.
expect_snapshot_plot <- function(name, code) {
  skip_if_not_installed("ggplot2", "3.3.0")
  skip_on_os("windows")

  name <- paste0(name, ".png")

  announce_snapshot_file(name = name)

  path <- save_png(code)
  expect_snapshot_file(path, name, cran = FALSE)
}



#----Test ----


test_that("Plot of sits tibble", {
    path <- save_png(
        plot(cerrado_2classes[1:20, ])
    )
    expect_snapshot_plot(
        "plot_sits_tibble",
        plot(cerrado_2classes[1:20, ])
    )
})


test_that("Plot of sits patterns", {
    path <- save_png(
        plot(sits_patterns(cerrado_2classes))
    )
    expect_snapshot_plot(
        "plot_sits_patterns",
        plot(sits_patterns(cerrado_2classes))
    )
})


test_that("Plot of sits predictions", {
    samples_mt_ndvi <- sits_select(samples_modis_4bands, bands = "NDVI")
    model_svm <- sits_train(samples_mt_ndvi, ml_method = sits_svm())
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    class_ndvi.tb <- sits_classify(point_ndvi, model_svm)

    path <- save_png(
        plot(sits_patterns(cerrado_2classes))
    )
    expect_snapshot_plot(
        "plot_sits_predictions",
        plot(class_ndvi.tb)
    )
})


test_that("Plot of sits SOM", {
    set.seed(123)
    samples_mt_2bands <- sits_select(samples_modis_4bands,
                                     bands = c("NDVI", "EVI"))
    som_map <- sits_som_map(samples_mt_2bands)
    suppressMessages(
        cluster_overall <- sits_som_evaluate_cluster(som_map)
    )

    path <- save_png(
        plot(som_map, type = "codes")
    )
    expect_snapshot_plot(
        "plot_sits_som_codes",
        {
            plot(som_map, type = "codes")
        }
    )

    path <- save_png(
        plot(som_map, type = "mapping")
    )
    expect_snapshot_plot(
        "plot_sits_som_mapping",
        {
            plot(som_map, type = "mapping")
        }
    )

    path <- save_png(
        plot(cluster_overall)
    )
    expect_snapshot_plot(
        "plot_sits_som_cluster_overall",
        {
            plot(cluster_overall)
        }
    )
})
