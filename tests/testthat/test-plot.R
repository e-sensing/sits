# # verifies if proto package is installed
# if (!requireNamespace("proto", quietly = TRUE)) {
#     stop("proto required for this function to work.
#              Please install it.", call. = FALSE)
#     library(proto)
# }
# library(proto)
test_that("Plot Time Series and Images", {

    cerrado_ndvi <- sits_select(cerrado_2classes, "NDVI")

    p <- plot(cerrado_ndvi[1, ])
    expect_equal(p$labels$title, "location (-14.05, -54.23) - Cerrado")

    cerrado_ndvi_1class <- dplyr::filter(cerrado_ndvi, label == "Cerrado")
    p1 <- plot(cerrado_ndvi_1class)
    expect_equal(
        p1$labels$title,
        "Samples (400) for class Cerrado in band = NDVI"
    )

    p2 <- plot(sits_patterns(cerrado_2classes))
    expect_equal(p2$guides$colour$title, "Bands")
    expect_equal(p2$theme$legend.position, "bottom")

    samples_mt_ndvi <- sits_select(samples_modis_4bands, bands = "NDVI")
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    rfor_model <- sits_train(samples_mt_ndvi, ml_method = sits_rfor())
    point_class <- sits_classify(point_ndvi, rfor_model)
    p3 <- plot(point_class)
    expect_equal(p3$labels$y, "Value")
    expect_equal(p3$labels$x, "Time")
    expect_equal(p3$theme$legend.position, "bottom")

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        parse_info = c("X1", "X2", "tile", "band", "date")
    )
    r_obj <- plot(sinop, red = "EVI", blue = "EVI", green = "NDVI")

    expect_equal(terra::nlyr(r_obj), 3)
    expect_equal(terra::ncol(r_obj), 254)

    r_obj <- plot(sinop, band = "EVI")

    expect_equal(terra::nlyr(r_obj), 1)
    expect_equal(terra::ncol(r_obj), 254)

    sinop_probs <- suppressMessages(
        sits_classify(
            sinop,
            ml_model = rfor_model,
            memsize = 2,
            multicores = 1,
            output_dir = tempdir()
        )
    )
    p_probs <- plot(sinop_probs)
    expect_equal(p_probs$adj, 0.5)
    expect_equal(p_probs$lend, "round")

    p_probs <- plot(sinop_probs, labels = "Forest")
    expect_equal(p_probs$adj, 0.5)
    expect_equal(p_probs$lend, "round")

    sinop_uncert <- sits_uncertainty(sinop_probs,
                                     output_dir = tempdir())

    p_uncert <- plot(sinop_uncert, n_breaks = 11, breaks = "pretty")

    expect_equal(p_probs$adj, 0.5)
    expect_equal(p_probs$bg, "white")
    expect_equal(p_probs$lty, "solid")

    sinop_labels <- sits_label_classification(sinop_probs,
                                              output_dir = tempdir())



    p4 <- plot(sinop_labels, title = "Classified image")
    expect_equal(p4$labels$title, "Classified image")
    expect_equal(p4$layers[[1]]$geom_params$hjust, 0.5)
    expect_true(p4$layers[[1]]$inherit.aes)

    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(sinop_labels$file_info[[1]]$path))))
})

test_that("Dendrogram Plot", {
    cluster_obj <- sits:::.sits_cluster_dendrogram(cerrado_2classes,
                                                   bands = c("NDVI", "EVI")
    )
    cut.vec <- sits:::.sits_cluster_dendro_bestcut(
        cerrado_2classes,
        cluster_obj
    )

    dend <- sits:::.sits_plot_dendrogram(
        data = cerrado_2classes,
        cluster = cluster_obj,
        cutree_height = cut.vec["height"],
        palette = "RdYlGn"
    )
    expect_equal(class(dend), "dendrogram")
})

test_that("SOM map plot", {
    set.seed(1234)
    som_map <-
        suppressWarnings(sits_som_map(
            cerrado_2classes,
            grid_xdim = 5,
            grid_ydim = 5
        ))

    p <- plot(som_map)
    expect_true(all(names(p$rect) %in% c("w", "h", "left", "top")))

})

test_that("SOM evaluate cluster plot", {
    set.seed(1234)
    som_map <-
        suppressWarnings(sits_som_map(
            cerrado_2classes,
            grid_xdim = 5,
            grid_ydim = 5
        ))

    cluster_purity_tb <- sits_som_evaluate_cluster(som_map)

    p <- plot(cluster_purity_tb)
    expect_equal(p$labels$title, "Confusion by cluster")
    expect_equal(p$labels$y, "Percentage of mixture")
})

