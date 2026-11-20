test_that("Creating clustering using Self-organizing Maps", {
    expect_equal(formals(sits_som_map)[["distance"]], "euclidean")
    set.seed(2903)

    doc_mode <- Sys.getenv("SITS_DOCUMENTATION_MODE")
    Sys.setenv("SITS_DOCUMENTATION_MODE" = "FALSE")
    expect_warning(som_map <- sits_som_map(
        samples_modis_ndvi,
        grid_xdim = 4,
        grid_ydim = 4,
        distance  = "euclidean"
    ))
    Sys.setenv("SITS_DOCUMENTATION_MODE" = doc_mode)
    expect_true(all(colnames(som_map$labelled_neurons) %in%
        c(
            "id_neuron", "label_samples", "count",
            "prior_prob", "post_prob"
        )))

    expect_true(som_map$labelled_neurons[1, ]$prior_prob >= 0)
    expect_true(som_map$labelled_neurons[1, ]$post_prob >= 0)
    expect_true(all(unique(som_map$labelled_neurons$id_neuron) %in% 1:16))

    cleaned_samples <- sits_som_clean_samples(som_map)
    expect_true("eval" %in% names(cleaned_samples))
    expect_true("post_prob" %in% names(cleaned_samples))
    expect_true(all(cleaned_samples$eval %in% c("clean", "analyze", "remove")))

    expect_true(cleaned_samples[1, ]$post_prob > 0)

    cluster_purity <- suppressMessages(sits_som_evaluate_cluster(som_map))

    expect_true(cluster_purity[1, ]$mixture_percentage > 60)
    expect_true(cluster_purity[2, ]$mixture_percentage < 40)
    expect_error(sits_som_clean_samples(samples_modis_ndvi))
    expect_error(sits_som_evaluate_samples(samples_modis_ndvi))

    doc_mode <- Sys.getenv("SITS_DOCUMENTATION_MODE")
    Sys.setenv("SITS_DOCUMENTATION_MODE" = "FALSE")
    expect_warning(
        expect_warning(
            som_map <- sits_som_map(
                samples_modis_ndvi,
                grid_xdim = 4,
                grid_ydim = 4,
                distance  = "dtw"
            ),
            regexp = "deprecated"
        ),
        regexp = "recommended"
    )
    Sys.setenv("SITS_DOCUMENTATION_MODE" = doc_mode)

    expect_true(all(colnames(som_map$labelled_neurons) %in%
        c(
            "id_neuron", "label_samples", "count",
            "prior_prob", "post_prob"
        )))
    # test remove samples
    samples_pasture_clean <- sits_som_remove_samples(som_map,
                                                     cluster_purity,
                                                     "Pasture",
                                                     "Cerrado")
    sum <- summary(samples_modis_ndvi)
    sum1 <- summary(samples_pasture_clean)
    n_cerrado <- sum[1,2]
    n_cerrado_1 <- sum1[1,2]
    expect_true(n_cerrado_1[["count"]] <= n_cerrado[["count"]])
    n_pasture <- sum[3,2]
    n_pasture_1 <- sum1[3,2]
    expect_true(n_pasture[["count"]] == n_pasture_1[["count"]])

})
test_that("SOM map legend keeps labels aligned with colors", {
    # "Cotton" and "Fallow_Cotton" share the same default color,
    # which used to shift the labels of the SOM map legend
    koh <- list(
        som_properties = list(
            neuron_label = c(
                "Cotton", "Fallow_Cotton", "Dunes",
                "Cotton", "Dunes", "Fallow_Cotton"
            )
        )
    )
    koh <- .som_paint_neurons(koh)
    leg <- .som_legend(koh)

    expect_equal(leg[["label"]], c("Cotton", "Fallow_Cotton", "Dunes"))
    neuron_colors <- stats::setNames(
        koh[["som_properties"]][["paint_map"]],
        koh[["som_properties"]][["neuron_label"]]
    )
    expect_equal(leg[["color"]], unname(neuron_colors[leg[["label"]]]))
    expect_equal(leg[["color"]][[1L]], leg[["color"]][[2L]])
    expect_false(leg[["color"]][[3L]] == leg[["color"]][[1L]])

    # "Millet_Cotton" has its own color, distinct from "Fallow_Cotton"
    cotton_colors <- .colors_get(c("Fallow_Cotton", "Millet_Cotton"))
    expect_false(cotton_colors[["Fallow_Cotton"]] ==
        cotton_colors[["Millet_Cotton"]])
})
