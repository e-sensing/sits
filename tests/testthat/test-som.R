test_that("Creating clustering using Self-organizing Maps", {
    set.seed(2903)

    expect_warning(som_map <- sits_som_map(
        samples_modis_ndvi,
        grid_xdim = 4,
        grid_ydim = 4,
        distance  = "euclidean"
    ))

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

    expect_warning(som_map <- sits_som_map(
        samples_modis_ndvi,
        grid_xdim = 4,
        grid_ydim = 4,
        distance  = "dtw"
    ))
    expect_true(all(colnames(som_map$labelled_neurons) %in%
        c(
            "id_neuron", "label_samples", "count",
            "prior_prob", "post_prob"
        )))
})
