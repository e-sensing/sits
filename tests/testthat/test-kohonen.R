context("Evaluate samples")
test_that("Creating clustering using Self organizing Maps",{
    #skip_on_cran()
    data.tb <- data("samples_MT_9classes")
    data.tb <- samples_MT_9classes

    time_series.ts <- sits_values(samples_MT_9classes, format = "bands_cases_dates")
    expect_equal(length(names(time_series.ts)), (dim(samples_MT_9classes$time_series[[1]])[2] - 1))

    koh <-
        sits_kohonen(
            samples_MT_9classes,
            time_series.ts,
            grid_xdim = 25,
            grid_ydim = 25,
            rlen = 100,
            dist.fcts = "euclidean"
        )
    expect_true(NROW(data.tb) == NROW(koh$info_samples))
    expect_equal(length(names(koh$kohonen_obj)), 15)
    expect_true("neuron_label" %in% names(koh$info_samples))

    confusion_by_cluster <- sits_metrics_by_cluster(koh$info_samples)
    expect_equal(length(names(confusion_by_cluster$confusion_matrix)), 6)

    subgroups <- sits_subgroup(koh)
    expect_true("label_subgroup" %in% names(subgroups$samples_subgroup.tb))

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
