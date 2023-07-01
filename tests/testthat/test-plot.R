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

    p3 <- cerrado_2classes |>
        sits_patterns() |>
        sits_select(bands = "EVI") |>
        plot()
    expect_equal(as.Date(p3$data$Time[1]), as.Date("2000-09-13"))
    expect_equal(p3$data$Pattern[1], "Cerrado")
    expect_equal(p3$data$name[1], "EVI")
    expect_equal(p3$guides$colour$title, "Bands")

    p4 <- cerrado_2classes |>
        sits_patterns() |>
        plot(bands = "NDVI")
    expect_equal(as.Date(p4$data$Time[1]), as.Date("2000-09-13"))
    expect_equal(p4$data$Pattern[1], "Cerrado")
    expect_equal(p4$data$name[1], "NDVI")
    expect_equal(p4$guides$colour$title, "Bands")

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
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        progress = FALSE
    )
    p <- plot(sinop, band = "NDVI", palette = "RdYlGn", rev = TRUE)
    expect_equal(p$tm_shape$shp_name, "stars_obj")
    expect_equal(p$tm_raster$palette, "RdYlGn")
    expect_equal(p$tm_grid$grid.projection, 4326)

    tmap_options = list("tmap_legend_title_size" = 1.0,
                        "tmap_legend_text_size" = 0.7,
                        "tmap_max_cells" = 1e+06,
                        "tmap_graticules_labels_size" = 0.7,
                        "tmap_legend_bg_color" = "white",
                        "tmap_legend_bg_alpha" = 0.6)

    p_rgb <- plot(sinop, red = "NDVI", green = "NDVI", blue = "NDVI",
                  tmap_options = tmap_options)

    expect_equal(p_rgb$tm_shape$shp_name, "rgb_st")
    expect_equal(p_rgb$tm_grid$grid.projection, 4326)

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
    expect_equal(p_probs$tm_raster$palette, "YlGnBu")
    expect_equal(length(p_probs$tm_raster$title), 4)
    expect_equal(p_probs$tm_layout$legend.bg.color, "white")

    p_probs_f <- plot(sinop_probs, labels = "Forest")
    expect_equal(p_probs_f$tm_raster$palette, "YlGnBu")
    expect_equal(length(p_probs_f$tm_raster$title), 1)
    expect_equal(p_probs_f$tm_layout$legend.bg.color, "white")

    sinop_uncert <- sits_uncertainty(sinop_probs,
        output_dir = tempdir()
    )

    p_uncert <- plot(sinop_uncert, palette = "Reds", rev = FALSE)

    expect_equal(p_uncert$tm_raster$palette, "Reds")
    expect_equal(length(p_uncert$tm_raster$title), 1)
    expect_equal(p_uncert$tm_layout$legend.bg.color, "white")

    sinop_labels <- sits_label_classification(
        sinop_probs,
        output_dir = tempdir(),
        progress = FALSE
    )

    p4 <- plot(sinop_labels, title = "Classified image")
    expect_equal(p4$tm_grid$grid.projection, 4326)
    expect_equal(p4$tm_raster$n, 5)
    expect_true(p4$tm_shape$check_shape)

    # segment the image
    segments <- sits_segment(
        cube = sinop,
        tile = "012010",
        bands = "NDVI",
        date = sits_timeline(sinop)[1],
        seg_fn = sits_supercells(step = 20)
    )
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
    #'     # get the average value per segment
    samples_seg <- sits_get_data(
        cube = sinop,
        samples = segments,
        progress = FALSE
    )
    #'     # classify the segments
    seg_class <- sits_classify(
        data = samples_seg,
        ml_model = rfor_model,
        progress = FALSE,
    )
    #'     # add a column to the segments by class
    sf_seg <- sits_join_segments(
        data = seg_class,
        segments = segments
    )
    p5 <- plot(sinop, band = "NDVI", segments = sf_seg, palette = "RdYlGn")
    expect_equal(p5$tm_raster$palette, "RdYlGn")
    expect_equal(p5$tm_shape$shp_name, "stars_obj")


    p6 <- plot(sinop, red = "NDVI", green = "NDVI", blue = "NDVI",
               segments = sf_seg, seg_color = "red")

    expect_equal(p6$tm_shape$shp_name, "rgb_st")
    expect_equal(p6$tm_borders$col, "red")
    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(sinop_labels$file_info[[1]]$path))))
})

test_that("Plot Accuracy", {
    # show accuracy for a set of samples
    train_data <- sits_sample(samples_modis_ndvi, n = 200)
    test_data <- sits_sample(samples_modis_ndvi, n = 200)
    # compute a random forest model
    rfor_model <- sits_train(train_data, sits_rfor())
    # classify training points
    points_class <- sits_classify(test_data, rfor_model, progress = FALSE)
    # calculate accuracy
    acc <- sits_accuracy(points_class)
    # plot accuracy
    p <- plot(acc)
    expect_equal(p$labels$title, "Confusion matrix")
    expect_equal(p$labels$x, "Class")
    expect_equal(p$labels$y, "Agreement with reference")
    expect_equal(p$theme$line$colour, "black")
})

test_that("Plot Models", {
    set.seed(290356)
    rfor_model <- sits_train(samples_modis_ndvi, ml_method = sits_rfor())
    p_model <- plot(rfor_model)
    expect_true(all(p_model$data$variable %in% c(
        "NDVI1", "NDVI2", "NDVI3",
        "NDVI4", "NDVI5", "NDVI6",
        "NDVI7", "NDVI8", "NDVI9",
        "NDVI10", "NDVI11", "NDVI12"
    )))
    expect_true(all(p_model$data$minimal_depth[1:2] %in% c(0, 1)))

    xgb_model <- sits_train(samples_modis_ndvi, ml_method = sits_xgboost())
    p_xgb <- plot(xgb_model)
    expect_equal(p_xgb$x$config$engine, "dot")
    expect_false(p_xgb$sizingPolicy$browser$fill)
    expect_false(p_xgb$sizingPolicy$browser$external)
})

test_that("Dendrogram Plot", {
    samples <- sits_cluster_dendro(cerrado_2classes,
        bands = c("NDVI", "EVI"),
        .plot = FALSE
    )
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
    expect_equal(class(dend), "dendrogram")
})

test_that("Plot torch model", {
    model <- sits_train(
        samples_modis_ndvi,
        sits_mlp(
            layers = c(128, 128),
            dropout_rates = c(0.5, 0.4),
            epochs = 50
        )
    )
    pk <- plot(model)
    expect_true(length(pk$layers) == 2)
    expect_true(pk$labels$colour == "data")
    expect_true(pk$labels$x == "epoch")
    expect_true(pk$labels$y == "value")
})

test_that("Plot series with NA", {
    cerrado_ndvi <- cerrado_2classes |>
        sits_select(bands = "NDVI") |>
        dplyr::filter(label == "Cerrado")
    cerrado_ndvi_1 <- cerrado_ndvi[1, ]
    ts <- cerrado_ndvi_1$time_series[[1]]
    ts[1, 2] <- NA
    ts[10, 2] <- NA
    cerrado_ndvi_1$time_series[[1]] <- ts
    pna <- suppressWarnings(plot(cerrado_ndvi_1))
    expect_true(pna$labels$x == "Index")
    expect_true(pna$labels$y == "value")
})

test_that("SOM map plot", {
    set.seed(1234)
    som_map <-
        suppressWarnings(sits_som_map(
            cerrado_2classes,
            grid_xdim = 5,
            grid_ydim = 5
        ))

    p <- suppressWarnings(plot(som_map))
    expect_true(all(names(p$rect) %in% c("w", "h", "left", "top")))

    pc <- plot(som_map, type = "mapping")
    expect_true(all(names(pc$rect) %in% c("w", "h", "left", "top")))
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
