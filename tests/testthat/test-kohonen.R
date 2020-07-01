context("Evaluate samples using SOM")
test_that("Creating clustering using Self-organizing Maps", {
    #skip_on_cran()
    data("cerrado_2classes")
    new_samples <-
        suppressWarnings(sits_cluster_som(
            cerrado_2classes,
            grid_xdim = 5,
            grid_ydim = 5,
            alpha = 1,
            distance = "euclidean",
            iterations = 2))

    expect_true("conditional_prob" %in% names(new_samples$cleaned_samples.tb))
    expect_true("posterior_prob" %in% names(new_samples$cleaned_samples.tb))

    som_map <-
        suppressWarnings(sits_som_map(
            cerrado_2classes,
            grid_xdim = 5,
            grid_ydim = 5,
            alpha = 1,
            distance = "euclidean",
            iterations = 4
        ))

    expect_equal(length(names(som_map$som_properties)), 17)

    plot(som_map)

    cleaned_samples <- sits_som_clean_samples(som_map, samples_analysis = FALSE)
    expect_true("conditional_prob" %in% names(cleaned_samples))
    expect_true("posterior_prob" %in% names(cleaned_samples))

    cleaned_samples <- sits_som_clean_samples(som_map, samples_analysis = TRUE)
    expect_true("conditional_prob" %in% names(cleaned_samples$make_analysis.tb))
    expect_true("posterior_prob" %in% names(cleaned_samples$make_analysis.tb))

    cluster_overall <- sits_som_evaluate_cluster(som_map)
    expect_equal(length(names(cluster_overall$confusion_matrix)), 6)

    sits_som_plot_clusters(cluster_overall, "Confusion between the sample classes")

})

test_that("SOM with a small map and neighborhood cluster", {

    som_map2 <-
        suppressWarnings(sits_som_map(
            cerrado_2classes,
            grid_xdim = 2,
            grid_ydim = 2,
            alpha = 1,
            distance = "euclidean",
            iterations = 4
        ))
    expect_true(all(som_map2$statistics_samples$samples$neuron_label %in% c("Cerrado", "Pasture")))

    neigh <- sits:::.sits_som_cluster_neighbourhood(som_map2)

    expect_true(all(neigh[1,"percentage_n"] > 0.90))

})
