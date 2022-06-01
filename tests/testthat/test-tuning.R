test_that("Tuning - random search", {

    set.seed(123)
    torch::torch_manual_seed(1234)

    tuned <- sits_tuning(
        samples_modis_4bands,
        ml_method = sits_tempcnn(),
        params = sits_tuning_hparams(
            optimizer = torch::optim_adam,
            opt_hparams = list(
                lr = choice(0.01, 0.05, 0.001, 0.0005)
            )
        ),
        trials = 4,
        multicores = 4,
        progress = FALSE
    )

    accuracy <- tuned$accuracy
    kappa <- tuned$kappa

    expect_true(max(accuracy) > 0.7)
    expect_true(max(kappa) > 0.7)
})
