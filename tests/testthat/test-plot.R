context("Plot")
# verifies if proto package is installed
if (!requireNamespace("proto", quietly = TRUE)) {
    stop("proto required for this function to work.
             Please install it.", call. = FALSE)
}
library(proto)
test_that("Plot Time Series and Classification", {
    data("cerrado_2classes")

    cerrado_ndvi <- sits_select(cerrado_2classes, "NDVI")

    plot(cerrado_ndvi[1,])

    cerrado_ndvi_1class <- dplyr::filter(cerrado_2classes, label == "Cerrado")
    plot(cerrado_ndvi_1class)

    plot(sits_patterns(cerrado_2classes))

    p <- plot(cerrado_2classes[1,], colors = "Dark2")
    expect_equal(p$labels$title, "location (-14.05, -54.23) - Cerrado")

    p1 <- plot(cerrado_ndvi, colors = "Dark2")
    expect_equal(p1$labels$title,
                 "Samples (400) for class Cerrado in band = NDVI")

    p2 <- plot(cerrado_2classes)
    expect_equal(p2$labels$y, "value")
    expect_equal(p2$labels$x, "Index")

    samples_mt_ndvi <- sits_select(samples_mt_6bands, bands = "NDVI")
    data(point_ndvi)
    rfor_model    <- sits_train(samples_mt_ndvi, ml_method = sits_rfor())
    class_ndvi.tb <-  sits_classify(point_ndvi, rfor_model)
    p3 <- plot(class_ndvi.tb)
    expect_equal(p3$labels$y, "Value")
    expect_equal(p3$labels$x, "Time")
    expect_equal(p3$theme$legend.position, "bottom")

    files  <- c(system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif",
                            package = "sits"))
    data(timeline_modis_392)
    sinop <- sits_cube(name  = "Sinop-crop",
                       satellite = "TERRA",
                       sensor    = "MODIS",
                       timeline = timeline_modis_392,
                       bands = "ndvi",
                       files = files)

    sinop_probs <- sits_classify(sinop, ml_model = rfor_model,
                                 memsize = 1, multicores = 1)
    sinop_labels <- sits_label_classification(sinop_probs)
    p4 <- plot(sinop_labels, time = 15)

    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(sinop_labels$file_info[[1]]$path))))

})
test_that("Plot Time Series with NA", {
    sql_file <- system.file("/extdata/cloud_data/s2_cloud_data.sql", package = "sits")
    conn <- sits_db_connect(sql_file)
    cloud_data <- sits_db_read(conn, "s2_cloud_data")
    p1 <- sits:::.sits_ggplot_series(cloud_data[1,])

    expect_equal(p1$labels$title, "location (-10.3, -65.74) - NoClass")

})
test_that("Dendogram Plot", {
    # verifies if imager package is installed
    if (!requireNamespace("imager", quietly = TRUE)) {
        stop("imager required for this function to work.
             Please install it.", call. = FALSE)
    }
    cluster.obj <- sits:::.sits_cluster_dendrogram(cerrado_2classes,
                                                  bands = c("NDVI", "EVI"))
    cut.vec <- sits:::.sits_cluster_dendro_bestcut(cerrado_2classes,
                                                   cluster.obj)

    file_plot <- "./dendro.jpg"
    file_ref <- system.file("extdata/plot/dendro_ref.jpg", package = "sits")

    jpeg(filename = file_plot)

    sits:::.sits_plot_dendrogram(cerrado_2classes,
                                       cluster.obj,
                                       cut.vec["height"])
    dev.off()

    img     <- imager::load.image(file_plot)
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
            grid_ydim = 5,
            alpha = 1,
            distance = "euclidean",
            iterations = 4
        ))

    plot(som_map, type = "mapping")
    plot(som_map)
    file_plot <- "./som_map_ref.jpg"
    file_ref  <- system.file("extdata/plot/som_map_ref.jpg", package = "sits")
    jpeg(filename = file_plot)
    sits:::.sits_plot_som_map(som_map, type = "codes", whatmap = 1)
    dev.off()

    img     <- imager::load.image(file_plot)
    img_ref <- imager::load.image(file_ref)

    expect_true(all(dim(img) == dim(img_ref)))

    mean_diff <- mean(imager::grayscale(img)) - mean(imager::grayscale(img_ref))

    expect_true(abs(mean_diff) < 0.20)

    expect_true(file.remove(file_plot))


})
