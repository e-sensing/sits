context("Evaluate samples")
test_that("Creating clustering using Self-organizing Maps", {
    #skip_on_cran()
    data("samples_mt_9classes")
    data.tb <- samples_mt_9classes[1:500,]

    koh <-
        sits_kohonen(
            data.tb,
            grid_xdim = 5,
            grid_ydim = 5,
            rlen = 20,
            distance = "euclidean"
        )
    expect_true(NROW(data.tb) == NROW(koh$info_samples))
    expect_equal(length(names(koh$kohonen_obj)), 15)
    expect_true("cluster" %in% names(koh$info_samples))

    sits_plot_kohonen(koh)

    confusion_by_cluster <- sits_evaluate_cluster(koh$info_samples)
    expect_equal(length(names(confusion_by_cluster$confusion_matrix)), 6)

    sits_plot_cluster_info(confusion_by_cluster)

    subgroups <- sits_subgroup(koh)
    neurons_subgroup <- subgroups$neurons_subgroup.lst

    expect_true("label_subgroup" %in% names(subgroups$samples_subgroup.tb))

    sits_plot_subgroups(neurons_subgroup)
    expect_true(all(file.remove(list.files(path = ".", pattern = ".png", full.names = TRUE))))

    evaluate_samples <- sits_evaluate_samples(
        data.tb,
        grid_xdim = 5,
        grid_ydim = 5,
        rlen = 20,
        distance = "euclidean",
        iterations = 1
    )
    expect_true("metrics_by_samples" %in% names(evaluate_samples))
    expect_true("samples.tb" %in% names(evaluate_samples))

})
