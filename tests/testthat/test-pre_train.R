test_that("Pre-training - samples fewer than the batch size", {
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")
    samples <- samples_modis_ndvi[seq(1L, nrow(samples_modis_ndvi), 4L), ]
    methods <- list(
        sits_ssl_mae, sits_ssl_lejepa, sits_ssl_vicreg,
        sits_ssl_barlow_twins, sits_barlow_twins, sits_contrastive_learning
    )
    # no batch would be trained, so each method must stop
    purrr::walk(methods, function(method) {
        expect_error(
            sits_pre_train(
                samples,
                rl_method = method(
                    batch_size = 512L, epochs = 1L, verbose = FALSE
                )
            ),
            "smaller than batch_size"
        )
    })
})
