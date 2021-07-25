context("Plot")
# verifies if proto package is installed
if (!requireNamespace("proto", quietly = TRUE)) {
    stop("proto required for this function to work.
             Please install it.", call. = FALSE)
    library(proto)
}
library(proto)
test_that("Plot Time Series and Images", {
    data("cerrado_2classes")

    cerrado_ndvi <- sits_select(cerrado_2classes, "NDVI")

    jpeg(filename = paste0(tempdir(), "/plot.jpg"))

    p <- plot(cerrado_ndvi[1, ])
    expect_equal(p$labels$title, "location (-14.05, -54.23) - Cerrado")

    cerrado_ndvi_1class <- dplyr::filter(cerrado_2classes, label == "Cerrado")
    p1 <- plot(cerrado_ndvi_1class)
    expect_equal(
        p1$labels$title,
        "Samples (400) for class Cerrado in band = NDVI"
    )

    p2 <- plot(sits_patterns(cerrado_2classes))
    expect_equal(p2$guides$colour$title, "Bands")
    expect_equal(p2$theme$legend.position, "bottom")

    samples_mt_ndvi <- sits_select(samples_mt_6bands, bands = "NDVI")
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    rfor_model <- sits_train(samples_mt_ndvi, ml_method = sits_rfor())
    point_class <- sits_classify(point_ndvi, rfor_model)
    p3 <- plot(point_class)
    expect_equal(p3$labels$y, "Value")
    expect_equal(p3$labels$x, "Time")
    expect_equal(p3$theme$legend.position, "bottom")

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "LOCAL",
        name = "sinop-2014",
        satellite = "TERRA",
        sensor = "MODIS",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "X2", "tile", "band", "date")
    )
    v_stack <- sits_view(sinop, red = "NDVI", blue = "NDVI", green = "NDVI")
    expect_equal(sits:::.raster_nrows(v_stack@object[[1]]), 144)
    expect_equal(v_stack@map[[1]]$options$maxZoom, 52)

    sinop_probs <- suppressMessages(
        sits_classify(
            sinop,
            ml_model = rfor_model,
            memsize = 1,
            multicores = 1,
            output_dir = tempdir()
        )
    )
    p_probs <- plot(sinop_probs)
    expect_equal(p_probs$adj, 0.5)
    expect_equal(p_probs$lend, "round")

    sinop_labels <- sits_label_classification(sinop_probs,
                                              output_dir = tempdir())

    p4 <- plot(sinop_labels, title = "Classified image")
    expect_equal(p4$labels$title, "Classified image")
    expect_equal(p4$layers[[1]]$geom_params$hjust, 0.5)
    expect_true(p4$layers[[1]]$inherit.aes)

    v4 <- sits_view(sinop_labels, time = 1)
    expect_equal(sits:::.raster_nrows(v4@object[[1]]), 144)
    expect_equal(v4@map$x$options$maxZoom, 52)
    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(sinop_labels$file_info[[1]]$path))))

    invisible(dev.off())
    invisible(file.remove(paste0(tempdir(), "/plot.jpg")))
})

test_that("Plot Stack Images", {
    # Create a raster cube based on CBERS data
    data_dir <- system.file("extdata/raster/cbers", package = "sits")

    # create a raster cube file based on the information about the files
    cbers_cube <- sits_cube(
        source = "LOCAL",
        name = "022024",
        satellite = "CBERS-4",
        sensor = "AWFI",
        resolution = "64m",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "X2", "tile", "band", "date")
    )
    v_cbers <- sits_view(cbers_cube, red = "B15", green = "B16", blue = "B13")
    expect_equal(sits:::.raster_nrows(v_cbers@object[[1]]), 50)
    expect_equal(v_cbers@map[[1]]$options$maxZoom, 52)
})


test_that("Dendrogram Plot", {
    # verifies if imager package is installed
    if (!requireNamespace("imager", quietly = TRUE)) {
        stop("imager required for this function to work.
             Please install it.", call. = FALSE)
    }
    cluster_obj <- sits:::.sits_cluster_dendrogram(cerrado_2classes,
        bands = c("NDVI", "EVI")
    )
    cut.vec <- sits:::.sits_cluster_dendro_bestcut(
        cerrado_2classes,
        cluster_obj
    )

    file_plot <- paste0(tempdir(), "/dendro.jpg")
    file_ref <- system.file("extdata/plot/dendro_ref.jpg", package = "sits")

    jpeg(filename = file_plot)

    sits:::.sits_plot_dendrogram(
        cerrado_2classes,
        cluster_obj,
        cut.vec["height"]
    )
    invisible(dev.off())

    img <- imager::load.image(file_plot)
    img_ref <- imager::load.image(file_ref)
    expect_true(all(dim(img) == dim(img_ref)))

    mean_diff <- mean(imager::grayscale(img)) - mean(imager::grayscale(img_ref))

    expect_true(abs(mean_diff) < 0.20)

    expect_true(file.remove(file_plot))
})

test_that("SOM map plot", {
    som_map <-
        suppressWarnings(sits_som_map(
            cerrado_2classes,
            grid_xdim = 5,
            grid_ydim = 5
        ))


    file_plot <- paste0(tempdir(), "/som_map_ref.jpg")
    file_ref <- system.file("extdata/plot/som_map_ref.jpg", package = "sits")

    jpeg(filename = file_plot)
    plot(som_map)
    invisible(dev.off())

    img <- imager::load.image(file_plot)
    img_ref <- imager::load.image(file_ref)

    expect_true(all(dim(img) == dim(img_ref)))

    mean_diff <- mean(imager::grayscale(img)) - mean(imager::grayscale(img_ref))

    expect_true(abs(mean_diff) < 0.20)

    expect_true(file.remove(file_plot))
})

test_that("SOM evaluate cluster plot", {
    som_map <-
        suppressWarnings(sits_som_map(
            cerrado_2classes,
            grid_xdim = 5,
            grid_ydim = 5
        ))

    cluster_purity_tb <- sits_som_evaluate_cluster(som_map)

    file_plot <- paste0(tempdir(), "/evaluate_cluster.jpg")
    file_ref <- system.file("extdata/plot/evaluate_cluster.jpg",
                            package = "sits")

    jpeg(filename = file_plot)
    plot(cluster_purity_tb)
    invisible(dev.off())

    img <- imager::load.image(file_plot)
    img_ref <- imager::load.image(file_ref)

    expect_true(all(dim(img) == dim(img_ref)))

    mean_diff <- mean(imager::grayscale(img)) - mean(imager::grayscale(img_ref))

    expect_true(abs(mean_diff) < 0.20)

    expect_true(file.remove(file_plot))
})
