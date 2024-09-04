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
    vdiffr::expect_doppelganger("NDVI_RdYlGn", p)

    p_rgb <- plot(sinop, red = "NDVI", green = "NDVI", blue = "NDVI")
    vdiffr::expect_doppelganger("NDVI_rgb", p_rgb)

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
    vdiffr::expect_doppelganger("NDVI_probs", p_probs)

    p_probs_f <- plot(sinop_probs, labels = "Forest")
    vdiffr::expect_doppelganger("NDVI_probs_f", p_probs_f)

    sinop_uncert <- sits_uncertainty(sinop_probs,
        output_dir = tempdir()
    )
    p_uncert <- plot(sinop_uncert, palette = "Reds", rev = FALSE)
    vdiffr::expect_doppelganger("NDVI_uncert", p_uncert)

    sinop_labels <- sits_label_classification(
        sinop_probs,
        output_dir = tempdir(),
        progress = FALSE
    )
    p4 <- plot(sinop_labels)
    vdiffr::expect_doppelganger("NDVI_labels", p4)
})

test_that("Plot class cube from STAC", {
    world_cover <- .try(
        {
            sits_cube(
                source     = "TERRASCOPE",
                collection = "WORLD-COVER-2021",
                bands      = "CLASS",
                roi        = c("lon_min" = -62.7,
                               "lon_max" = -62.5,
                               "lat_min" = -8.83 ,
                               "lat_max" = -8.70
                ),
                progress   = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(purrr::is_null(world_cover),
                      message = "TERRASCOPE is not accessible"
    )
    p_world_cover <- plot(world_cover)
    vdiffr::expect_doppelganger("World_Cover", p_world_cover)
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
    vdiffr::expect_doppelganger("accuracy_point", p_acc)
})

test_that("Plot Models", {
    set.seed(290356)
    rfor_model <- sits_train(samples_modis_ndvi, ml_method = sits_rfor())
    p_model <- plot(rfor_model)
    vdiffr::expect_doppelganger("rfor_model", p_model)
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
    vdiffr::expect_doppelganger("dend", dend)
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
    vdiffr::expect_doppelganger("p_torch", p_torch)
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
    suppressWarnings(vdiffr::expect_doppelganger("plot_NA", pna))
})

test_that("SOM map plot", {
    set.seed(1234)
    som_map <-
        suppressWarnings(sits_som_map(
            cerrado_2classes,
            grid_xdim = 5,
            grid_ydim = 5
        ))

    p_som_map <- suppressWarnings(plot(som_map))
    vdiffr::expect_doppelganger("plot_som_map", p_som_map)
    p_som_map_2 <- plot(som_map, type = "mapping")
    vdiffr::expect_doppelganger("plot_som_map_2", p_som_map_2)
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
    vdiffr::expect_doppelganger("plot_cluster_purity", p_purity)
})
