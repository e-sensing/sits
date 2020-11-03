context("Accuracy")
test_that("XLS", {
    data(cerrado_2classes)
    pred_ref.tb <- sits_kfold_validate(cerrado_2classes, folds = 2, ml_method = sits_rfor(num_trees = 100))
    invisible(capture.output(conf.mx <- sits_conf_matrix(pred_ref.tb)))
    results <- list()
    conf.mx$name <- "confusion_matrix"
    results[[length(results) + 1]] <- conf.mx
    xls_file <- paste0(tempdir(),"/confusion_matrix.xlsx")
    sits_to_xlsx(results, file = xls_file)

    expect_true(file.remove(xls_file))
})

test_that("Accuracy - more than 2 classes", {
    data("samples_mt_4bands")
    samples <- sits_select(samples_mt_4bands, bands = c("NDVI", "EVI"))
    pred_ref.tb <- sits_kfold_validate(samples, folds = 2, ml_method = sits_rfor(num_trees = 100))
    invisible(capture.output(conf.mx <- sits_conf_matrix(pred_ref.tb)))

    expect_true(conf.mx$overall["Accuracy"] > 0.90)
    expect_true(conf.mx$overall["Kappa"] > 0.90)

    conv.lst <- list(Soy_Corn = "Cropland",
                    Soy_Cotton  = "Cropland",
                    Soy_Fallow  = "Cropland",
                    Soy_Millet  = "Cropland",
                    Soy_Sunflower  = "Cropland",
                    Fallow_Cotton  = "Cropland")
    invisible(capture.output(conf2.mx <- sits_conf_matrix(pred_ref.tb, conv.lst = conv.lst)))

    expect_true(conf2.mx$overall["Accuracy"] > 0.95)
    expect_true(conf2.mx$overall["Kappa"] > 0.95)
})
test_that("Accuracy areas", {
    samples_mt_2bands <- sits_select(samples_mt_4bands, bands = c("NDVI", "EVI"))

    samples_mt_2bands <- dplyr::filter(samples_mt_2bands, label %in%
                                c("Forest", "Pasture", "Soy_Corn"))
    rfor_model <- sits_train(samples_mt_2bands, sits_rfor(num_trees = 200))

    ndvi_file <- c(system.file("extdata/raster/mod13q1/sinop-ndvi-2014.tif",
                               package = "sits"))

    evi_file <- c(system.file("extdata/raster/mod13q1/sinop-evi-2014.tif",
                              package = "sits"))

    data("timeline_2013_2014")

    sinop_2014 <- sits_cube(name = "sinop-2014",
                            timeline = timeline_2013_2014,
                            satellite = "TERRA",
                            sensor = "MODIS",
                            bands = c("ndvi", "evi"),
                            files = c(ndvi_file, evi_file))

    sinop_2014_probs <- sits_classify(sinop_2014,
                                      rfor_model,
                                      output_dir = tempdir(),
                                      memsize = 4,
                                      multicores = 2)


    expect_true(all(file.exists(unlist(sinop_2014_probs$file_info[[1]]$path))))
    tc_obj <- suppressWarnings(terra::rast(sinop_2014_probs$file_info[[1]]$path[1]))
    expect_true(terra::nrow(tc_obj) == sinop_2014_probs$nrows)

    sinop_2014_label <- sits_label_classification(sinop_2014_probs,
                                                  output_dir = tempdir(),
                                                  smoothing = "bayesian")

    ground_truth <- system.file("extdata/samples/samples_sinop_crop.csv", package = "sits")
    as <- suppressWarnings(sits_accuracy(sinop_2014_label, ground_truth))

    expect_true(as.numeric(as$accuracy$user["Forest"]) > 0.8)
    expect_true(as.numeric(as$accuracy$producer["Pasture"]) > 0.8)


})

