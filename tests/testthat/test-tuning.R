test_that("Tuning - random search", {

    Sys.setenv("OMP_NUM_THREADS" = 1)

    set.seed(123)
    torch::torch_manual_seed(1234)

    tuned_choice <- sits_tuning(
        samples_modis_ndvi,
        ml_method = sits_tempcnn(),
        params = sits_tuning_hparams(
            epochs = 10,
            optimizer = torch::optim_adam,
            opt_hparams = list(
                lr = choice(0.01, 0.05, 0.001, 0.0005)
            )
        ),
        trials = 2,
        multicores = 2,
        progress = FALSE
    )
    expect_true(max(tuned_choice$accuracy) > 0.5)
    expect_true(max(tuned_choice$kappa) > 0.5)

    tuned_beta <- sits_tuning(
        samples_modis_ndvi,
        ml_method = sits_tempcnn(),
        params = sits_tuning_hparams(
            epochs = 10,
            optimizer = torch::optim_adam,
            opt_hparams = list(
                lr = beta(0.2, 4)
            )
        ),
        trials = 2,
        multicores = 2,
        progress = FALSE
    )

    expect_true(max(tuned_beta$accuracy) > 0.5)
    expect_true(max(tuned_beta$kappa) > 0.5)

    tuned_unif <- sits_tuning(
        samples_modis_ndvi,
        ml_method = sits_tempcnn(),
        params = sits_tuning_hparams(
            epochs = 10,
            optimizer = torch::optim_adam,
            opt_hparams = list(
                lr = uniform(min = 0.01, max = 0.02)
            )
        ),
        trials = 2,
        multicores = 1,
        progress = FALSE
    )

    expect_true(max(tuned_unif$accuracy) > 0.5)
    expect_true(max(tuned_unif$kappa) > 0.5)

    tuned_norm <- sits_tuning(
        samples_modis_ndvi,
        ml_method = sits_tempcnn(),
        params = sits_tuning_hparams(
            epochs = 10,
            optimizer = torch::optim_adam,
            opt_hparams = list(
                lr = normal(mean = 0.01, sd = 0.002)
            )
        ),
        trials = 2,
        multicores = 1,
        progress = FALSE
    )

    expect_true(max(tuned_norm$accuracy) > 0.5)
    expect_true(max(tuned_norm$kappa) > 0.5)

    tuned_lnorm <- sits_tuning(
        samples_modis_ndvi,
        ml_method = sits_tempcnn(),
        params = sits_tuning_hparams(
            epochs = 10,
            optimizer = torch::optim_adam,
            opt_hparams = list(
                lr = lognormal(meanlog = -4.5, sdlog = 1)
            )
        ),
        trials = 2,
        multicores = 1,
        progress = FALSE
    )

    expect_true(max(tuned_lnorm$accuracy) > 0.5)
    expect_true(max(tuned_lnorm$kappa) > 0.5)

    tuned_lunif <- sits_tuning(
        samples_modis_ndvi,
        ml_method = sits_tempcnn(),
        params = sits_tuning_hparams(
            epochs = 10,
            optimizer = torch::optim_adam,
            opt_hparams = list(
                lr = loguniform(minlog = -5, maxlog = -4)
            )
        ),
        trials = 2,
        multicores = 1,
        progress = FALSE
    )

    expect_true(max(tuned_lunif$accuracy) > 0.5)
    expect_true(max(tuned_lunif$kappa) > 0.5)

})
