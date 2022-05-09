test_that("Tuning - random search", {
    tuned <- sits_tuning(
        samples_modis_4bands,
        ml_method = sits_tempcnn(),
        params = list(
            optimizer = torchopt::optim_yogi,
            opt_hparams = list(
                lr = beta(0.3, 5)
            )
        ),
        trials = 4,
        multicores = 4,
        progress = FALSE
    )

    accuracy <- tuned$accuracy
    kappa <- tuned$kappa
    lr <- unlist(tuned$opt_hparams)

    expect_true(max(accuracy) > 0.6)
    expect_true(max(kappa) > 0.4)
    expect_true(max(lr) <= 1.89)
})
