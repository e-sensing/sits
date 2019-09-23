context("Evaluate samples using SOM")
test_that("Creating clustering using Self-organizing Maps", {
    #skip_on_cran()
    data("cerrado_2classes")
    new_samples <-
        sits_cluster_som(
            cerrado_2classes,
            grid_xdim = 5,
            grid_ydim = 5,
            alpha = 1,
            distance = "euclidean",
            iterations = 2
        )
    expect_true("probability" %in% names(new_samples))

    som_map <-
        sits_som_map(
            cerrado_2classes,
            grid_xdim = 5,
            grid_ydim = 5,
            alpha = 1,
            distance = "euclidean",
            iterations = 4
        )

    expect_equal(length(names(som_map$som_properties)), 17)

    plot(som_map)

    cleaned_samples <- sits_som_clean_samples(som_map)
    expect_true("probability" %in% names(cleaned_samples))

    cluster_overall <- sits_som_evaluate_cluster(som_map)
    expect_equal(length(names(cluster_overall$confusion_matrix)), 6)

    sits_som_plot_clusters(cluster_overall, "Confusion by cluster")

    subgroups <- sits_som_evaluate_subgroups(som_map)
    expect_true("samples_subgroups.tb" %in% names(subgroups))
    expect_true("som_properties" %in% names(subgroups))

})
