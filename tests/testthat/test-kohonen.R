context("Evaluate samples using SOM")
test_that("Creating clustering using Self-organizing Maps", {
    #skip_on_cran()
    data("cerrado_2classes")
    som_cluster.tb <-
        sits_cluster_som(
            cerrado_2classes,
            grid_xdim = 5,
            grid_ydim = 5,
            rlen = 10,
            alpha = 1,
            distance = "euclidean",
            mode =  "online",
            iterations = 2
        )
    expect_equal(length(names(som_cluster.tb$som_properties)), 17)

    sits_plot_som(som_cluster.tb)

    new_samples.tb <- sits_clean_samples_som(som_cluster.tb)
    expect_true("probability" %in% names(new_samples.tb))

    cluster_overall <- sits_evaluate_cluster(som_cluster.tb)
    expect_equal(length(names(cluster_overall$confusion_matrix)), 6)

    sits_plot_cluster_info(cluster_overall, "Confusion by cluster")

    subgroups <- sits_evaluate_som_subgroups(som_cluster.tb)
    expect_true("samples_subgroups.tb" %in% names(subgroups))
    expect_true("som_properties" %in% names(subgroups))

})
