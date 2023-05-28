test_that("predictors",{

    pred <- sits_predictors(samples_modis_ndvi)
    expect_equal(nrow(pred), nrow(samples_modis_ndvi))
    expect_equal(ncol(pred), length(sits_timeline(samples_modis_ndvi)) + 2)
    expect_true(all(sits_labels(samples_modis_ndvi) %in% unique(pred$label)))

    features <- sits_pred_features(pred)
    expect_equal(ncol(features), length(sits_timeline(samples_modis_ndvi)))

    ref  <- sits_pred_references(pred)
    expect_true(all(sits_labels(samples_modis_ndvi) %in% unique(ref)))
    expect_equal(nrow(pred), length(ref))

    stats <- sits_stats(samples_modis_ndvi)
    expect_equal(length(stats), 2)
    expect_equal(length(stats[[1]]), length(sits_timeline(samples_modis_ndvi)))

    pred_norm <- sits_pred_normalize(pred, stats)
    expect_equal(nrow(pred_norm), nrow(samples_modis_ndvi))
    expect_equal(ncol(pred_norm), length(sits_timeline(samples_modis_ndvi)) + 2)
    expect_true(all(sits_labels(samples_modis_ndvi) %in%
                        unique(pred_norm$label)))
    frac <- 1/6
    pred_frac <- sits_pred_sample(pred, frac)
    expect_equal(nrow(pred_frac), 201)

})
