test_that("Creating clustering using Self-organizing Maps", {
    set.seed(2903)

    doc_mode <- Sys.setenv("SITS_DOCUMENTATION_MODE")
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

    doc_mode <- Sys.setenv("SITS_DOCUMENTATION_MODE")
    Sys.setenv("SITS_DOCUMENTATION_MODE" = "FALSE")
    expect_warning(som_map <- sits_som_map(
        samples_modis_ndvi,
        grid_xdim = 4,
        grid_ydim = 4,
        distance  = "dtw"
    ))
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
