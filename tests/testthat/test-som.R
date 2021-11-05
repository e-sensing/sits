test_that("Creating clustering using Self-organizing Maps", {
    # skip_on_cran()
    data("cerrado_2classes")
    set.seed(2903)
    som_map <- sits_som_map(
            samples_modis_4bands,
            grid_xdim = 10,
            grid_ydim = 10)

    expect_true(all(colnames(som_map$labelled_neurons) %in%
        c("id_neuron", "label_samples", "count", "prior_prob", "post_prob")))

    expect_true(som_map$labelled_neurons[1, ]$prior_prob >= 0)
    expect_true(som_map$labelled_neurons[1, ]$post_prob >= 0)
    expect_true(all(unique(som_map$labelled_neurons$id_neuron) %in% 1:100))

    cleaned_samples <- sits_som_clean_samples(som_map)
    expect_true("eval" %in% names(cleaned_samples))
    expect_true("post_prob" %in% names(cleaned_samples))
    expect_true(all(cleaned_samples$eval %in% c("clean", "analyze", "remove")))

    expect_true(cleaned_samples[1, ]$post_prob > 0)

    cluster_purity <- suppressMessages(sits_som_evaluate_cluster(som_map))

    expect_true(cluster_purity[1, ]$mixture_percentage > 90.0)
    expect_true(cluster_purity[2, ]$mixture_percentage < 5.0)

})
