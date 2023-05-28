test_that("Classify with random forest - single core and multicore", {
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor(num_trees = 40))

    expect_type(rfor_model, "closure")
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    class_ndvi <- sits_classify(
        data = point_ndvi,
        ml_model = rfor_model,
        progress = FALSE
    )

    expect_true(nrow(class_ndvi$predicted[[1]]) == 17)
    expect_true(all(class_ndvi$predicted[[1]]$class %in%
        sits_labels(samples_modis_ndvi)))
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    class_ndvi <- sits_classify(
        data = point_ndvi,
        ml_model = rfor_model,
        multicores = 2,
        progress = FALSE
    )

    expect_true(nrow(class_ndvi$predicted[[1]]) == 17)
    expect_true(all(class_ndvi$predicted[[1]]$class %in%
        sits_labels(samples_modis_ndvi)))
})

test_that("Classify a set of time series with svm + filter", {


    # single core
    samples_filt <- sits_apply(cerrado_2classes,
                               NDVI = sits_sgolay(NDVI),
                               EVI = sits_sgolay(EVI),
    )

    svm_model <- sits_train(samples_filt, sits_svm())

    class1 <- sits_classify(cerrado_2classes,
        ml_model = svm_model,
        filter_fn = sits_sgolay(),
        multicores = 2,
        progress = FALSE,
    )

    expect_true(class1$predicted[[1]]$class %in%
        sits_labels(cerrado_2classes))
})

test_that("Classify error bands 1", {

    model <- sits_train(samples_modis_ndvi, sits_svm())
    point <- sits_select(point_mt_6bands, "EVI")

    expect_error(
        sits_classify(
            data = point,
            ml_model = model
        )
    )
})
