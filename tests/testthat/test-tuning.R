test_that("Tuning - grid search", {

})

test_that("Tuning - random search", {

    testthat::skip_on_cran()

    samples <- sits_select(samples_modis_4bands, bands = "NDVI")

    tuned <- sits_tuning_random(
        samples,
        ml_method = sits_tempcnn(),
        params = list(
            optimizer = torchopt::optim_adamw,
            opt_hparams = list(
                lr = uniform(0, 1)
            )
        ),
        trials = 4,
        multicores = 1,
        progress = FALSE)
})
