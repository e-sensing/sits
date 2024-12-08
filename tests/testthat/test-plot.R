test_that("Plot Time Series and Images", {
    set.seed(290356)
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
    expect_equal(p2$theme$legend.position, "bottom")

    p3 <- cerrado_2classes |>
        sits_patterns() |>
        sits_select(bands = "EVI") |>
        plot()
    expect_equal(as.Date(p3$data$Time[1]), as.Date("2000-09-13"))
    expect_equal(p3$data$Pattern[1], "Cerrado")
    expect_equal(p3$data$name[1], "EVI")

    p4 <- cerrado_2classes |>
        sits_patterns() |>
        plot(bands = "NDVI")
    expect_equal(as.Date(p4$data$Time[1]), as.Date("2000-09-13"))
    expect_equal(p4$data$Pattern[1], "Cerrado")
    expect_equal(p4$data$name[1], "NDVI")

    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    set.seed(290356)
    rfor_model <- sits_train(samples_modis_ndvi, ml_method = sits_rfor())
    point_class <- sits_classify(point_ndvi, rfor_model, progress = FALSE)
    p3 <- plot(point_class)
    expect_equal(p3[[1]]$labels$y, "Value")
    expect_equal(p3[[1]]$labels$x, "Time")
    expect_equal(p3[[1]]$theme$legend.position, "bottom")

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    p <- plot(sinop, band = "NDVI", palette = "RdYlGn")
    rast_ndvi <- p[[1]]$shp
    expect_equal(nrow(rast_ndvi), 147)

    p_rgb <- plot(sinop, red = "NDVI", green = "NDVI", blue = "NDVI")
    rast_rgb <- p_rgb[[1]]$shp
    expect_true("SpatRaster" %in% class(rast_rgb))

    sinop_probs <- suppressMessages(
        sits_classify(
            sinop,
            ml_model = rfor_model,
            memsize = 2,
            multicores = 2,
            output_dir = tempdir(),
            progress = FALSE
        )
    )
    p_probs <- plot(sinop_probs)
    rast_probs <- p_probs[[1]]$shp
    expect_equal(terra::nlyr(rast_probs), 4)

    p_probs_f <- plot(sinop_probs, labels = "Forest")
    rast_probs_f <- p_probs_f[[1]]$shp
    expect_equal(terra::nlyr(rast_probs_f), 1)

    sinop_uncert <- sits_uncertainty(sinop_probs,
        output_dir = tempdir()
    )
    p_uncert <- plot(sinop_uncert, palette = "Reds", rev = FALSE)
    rast_uncert <- p_uncert[[1]]$shp
    expect_equal(terra::nlyr(rast_uncert), 1)

    sinop_labels <- sits_label_classification(
        sinop_probs,
        output_dir = tempdir(),
        progress = FALSE
    )
    p_class <- plot(sinop_labels)
    rast_class <- p_class[[1]]$shp
    expect_true("stars" %in% class(rast_class))
})

test_that("Plot Accuracy", {
    set.seed(290356)
    # show accuracy for a set of samples
    train_data <- sits_sample(samples_modis_ndvi, frac = 0.5)
    test_data  <- sits_sample(samples_modis_ndvi, frac = 0.5)
    # compute a random forest model
    rfor_model <- sits_train(train_data, sits_rfor())
    # classify training points
    points_class <- sits_classify(test_data, rfor_model, progress = FALSE)
    # calculate accuracy
    acc <- sits_accuracy(points_class)
    # plot accuracy
    p_acc <- plot(acc)
    expect_equal(p_acc$labels$title, "Confusion matrix")

})

test_that("Plot Models", {
    set.seed(290356)
    rfor_model <- sits_train(samples_modis_ndvi, ml_method = sits_rfor())
    p_model <- plot(rfor_model)
    expect_equal(p_model$labels$title, "Distribution of minimal depth and its mean")
})

test_that("Dendrogram Plot", {
    samples <- sits_cluster_dendro(cerrado_2classes,
        bands = c("NDVI", "EVI"))
    cluster <- .cluster_dendrogram(
        samples = samples,
        bands = c("NDVI", "EVI")
    )
    best_cut <- .cluster_dendro_bestcut(samples, cluster)

    dend <- plot(samples,
        cluster = cluster,
        cutree_height = best_cut["height"],
        palette = "RdYlGn"
    )
    expect_true("dendrogram" %in% class(dend))
})
test_that("Plot torch model", {
    set.seed(290356)
    model <- sits_train(
        samples_modis_ndvi,
        sits_mlp(
            layers = c(128, 128),
            dropout_rates = c(0.5, 0.4),
            epochs = 50
        )
    )
    p_torch <- plot(model)
    expect_equal(p_torch$labels$x, "epoch")
    expect_equal(p_torch$labels$y, "value")
})

test_that("SOM map plot", {
    set.seed(1234)
    som_map <- suppressWarnings(sits_som_map(
            cerrado_2classes,
            grid_xdim = 5,
            grid_ydim = 5
        ))

    p_som_map <- plot(som_map)
    expect_true(any("Cerrado" %in% p_som_map$som_properties$neuron_label))
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

    p_purity <- plot(cluster_purity_tb)
    expect_equal(p_purity$labels$title,
                 "Confusion by cluster")
})
