test_that("Tuning - random search", {

    samples <- sits_select(samples_modis_4bands, bands = "NDVI")

    tuned <- sits_tuning(
        samples,
        ml_method = sits_tempcnn(),
        params = list(
            optimizer = torchopt::optim_adamw,
            opt_hparams = list(
                lr = beta(0.3, 5)
            )
        ),
        trials = 4,
        multicores = 4,
        progress = FALSE)

    accuracy <- tuned$tuning$accuracy
    kappa <- tuned$tuning$accuracy
    best_lr <- tuned$tuning$params[[1]]$opt_hparams

    expect_true(max(accuracy) > 0.8)
    expect_true(max(kappa) > 0.8)
    expect_true(best_lr < 0.1)
})
