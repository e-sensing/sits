context("Evaluate samples")
test_that("Creating clustering using Self-organizing Maps", {

    library(kohonenDTW)
    #skip_on_cran()
    data("samples_MT_9classes")
    data.tb <- samples_MT_9classes[1:500,]

    time_series.ts <- sits_values(data.tb, format = "bands_cases_dates")
    expect_equal(length(names(time_series.ts)), (dim(data.tb$time_series[[1]])[2] - 1))

    koh <-
        sits_kohonen(
            data.tb,
            time_series.ts,
            grid_xdim = 5,
            grid_ydim = 5,
            rlen = 20,
            dist.fcts = "euclidean"
        )
    expect_true(NROW(data.tb) == NROW(koh$info_samples))
    expect_equal(length(names(koh$kohonen_obj)), 15)
    expect_true("neuron_label" %in% names(koh$info_samples))

    sits_plot_kohonen(koh)

    confusion_by_cluster <- sits_metrics_by_cluster(koh$info_samples)
    expect_equal(length(names(confusion_by_cluster$confusion_matrix)), 6)

    subgroups <- sits_subgroup(koh)
    neurons_subgroup <- subgroups$neurons_subgroup.lst

    expect_true("label_subgroup" %in% names(subgroups$samples_subgroup.tb))

    sits_plot_subgroups(neurons_subgroup)
    expect_true(all(file.remove(list.files(path = ".", pattern = ".png", full.names = TRUE))))

    evaluate_samples <- sits_evaluate_samples(
        samples_MT_9classes,
        time_series.ts,
        grid_xdim = 25,
        grid_ydim = 25,
        rlen = 100,
        distance = "euclidean",
        iterations = 1
    )
    expect_true("metrics_by_samples" %in% names(evaluate_samples))
    expect_true("samples.tb" %in% names(evaluate_samples))

})
