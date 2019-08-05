context("Evaluate samples")
test_that("Creating clustering using Self-organizing Maps", {
    #skip_on_cran()
    data("cerrado_2classes")

    koh <-
        sits_kohonen(
            cerrado_2classes,
            grid_xdim = 5,
            grid_ydim = 5,
            rlen = 20,
            distance = "euclidean"
        )
    expect_true(NROW(cerrado_2classes) == NROW(koh$info_samples))
    expect_equal(length(names(koh$kohonen_obj)), 15)
    expect_true("cluster" %in% names(koh$info_samples))

    sits_plot_kohonen(koh)

    confusion_by_cluster <- sits_evaluate_cluster(koh$info_samples)
    expect_equal(length(names(confusion_by_cluster$confusion_matrix)), 6)

    sits_plot_cluster_info(confusion_by_cluster)

    subgroups <- sits_subgroup(koh)
    neurons_subgroup <- subgroups$neurons_subgroup.lst

    expect_true("label_subgroup" %in% names(subgroups$samples_subgroup.tb))

    evaluate_samples <- sits_evaluate_samples(
        cerrado_2classes,
        grid_xdim = 5,
        grid_ydim = 5,
        rlen = 20,
        distance = "euclidean",
        iterations = 1
    )
    expect_true("metrics_by_samples" %in% names(evaluate_samples))
    expect_true("samples.tb" %in% names(evaluate_samples))

})
