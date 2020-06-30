context("Plot")
# verifies if proto package is installed
if (!requireNamespace("proto", quietly = TRUE)) {
    stop("proto required for this function to work.
             Please install it.", call. = FALSE)
}
library(proto)
test_that("Plot Time Series and Classification", {
    data("cerrado_2classes")
    p <- sits:::.sits_plot_allyears(cerrado_2classes[1,], colors = "Dark2")
    expect_equal(p$labels$title, "location (-14.0482, -54.2313) - Cerrado")

    p1 <- sits:::.sits_plot_together(cerrado_2classes, colors = "Dark2")
    expect_equal(p1$labels$title,
                 "Samples (400) for class Cerrado in band = ndvi")

    p2 <- sits:::.sits_plot_patterns(cerrado_2classes)
    expect_equal(p2$labels$y, "Value")
    expect_equal(p2$labels$x, "Time")

    samples_mt_ndvi <- sits_select_bands(samples_mt_6bands, ndvi)
    data(point_ndvi)
    rfor_model    <- sits_train(samples_mt_ndvi, ml_method = sits_rfor())
    class_ndvi.tb <-  sits_classify(point_ndvi, rfor_model)
    p3 <- sits:::.sits_plot_classification (class_ndvi.tb)
    expect_equal(p3$labels$y, "Value")
    expect_equal(p3$labels$x, "Time")
    expect_equal(p3$theme$legend.position, "bottom")

    files  <- c(system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif",
                            package = "sits"))
    data(timeline_modis_392)
    sinop <- sits_cube(type = "BRICK",
                       name  = "Sinop-crop",
                       satellite = "TERRA",
                       sensor    = "MODIS",
                       timeline = timeline_modis_392,
                       bands = "ndvi",
                       files = files)

    sinop_probs <- sits_classify(sinop, ml_model = rfor_model,
                                 memsize = 1, multicores = 1)
    sinop_labels <- sits_label_classification(sinop_probs)
    p4 <- sits:::.sits_plot_raster(sinop_labels, time = 1,
                                   title = "SINOP class 2000-2001")
    expect_equal(p4$labels$title,"SINOP class 2000-2001")
    expect_equal(p4$labels$x,"x")
    expect_equal(p4$labels$fill,"factor(class)")
    expect_equal(p4$data[1,"class"], 3)
    expect_equal(p4$data[154,"class"], 3)

    expect_true(all(file.remove(unlist(sinop_probs$files))))
    expect_true(all(file.remove(unlist(sinop_labels$files))))

})
test_that("Dendogram Plot", {
    # verifies if imager package is installed
    if (!requireNamespace("imager", quietly = TRUE)) {
        stop("imager required for this function to work.
             Please install it.", call. = FALSE)
    }
    cluster.obj <- sits:::.sits_cluster_dendrogram(cerrado_2classes,
                                                  bands = c("ndvi", "evi"))
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

    expect_true(all(imager::imrow(imager::R(img),385) ==
                        imager::imrow(imager::R(img_ref),385)))

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
