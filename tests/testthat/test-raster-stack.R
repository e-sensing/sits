context("Raster classification")

test_that("test stack classification", {
    if (!requireNamespace("inSitu", quietly = TRUE)) {
        if (!requireNamespace("devtools", quietly = TRUE))
            install.packages("devtools")
        devtools::install_github("e-sensing/inSitu")
    }
    # load the inSitu library
    library(inSitu)
    # load the samples
    data("cbers_samples_022024")
    # obtain the timeline
    timeline <- sits_timeline(cbers_samples_022024)
    # get the start and end dates
    start_date <- as.Date(timeline[1])
    end_date   <- as.Date(timeline[length(timeline)])
    # set up the bands
    bands <- c("NDVI", "EVI")
    # define the local directory to load the images
    local_dir <- system.file("extdata/CBERS/CB4_64_16D_STK/022024", package = "inSitu")

    # define the local CBERS data cube
    cbers_cube <- sits_cube(name = "cbers_022024",
                            satellite = "CBERS-4",
                            sensor = "AWFI",
                            resolution = "64m",
                            data_dir = local_dir,
                            parse_info = c("X1", "X2", "X3", "X4", "X5", "date", "X7", "band"))
    file_info <- cbers_cube$file_info[[1]]
    expect_equal(nrow(file_info), 46)
    expect_true(all(unique(file_info$band) %in% c("NDVI", "EVI")))
    expect_true(all(sits_timeline(cbers_cube) %in% sits_timeline(cbers_samples_022024)))

    # select the ndvi and evi bands
    cbers_samples_2bands <- sits_select(cbers_samples_022024, bands = c("NDVI", "EVI"))
    # train a random forest model
    rfor_model <- sits_train(cbers_samples_2bands, sits_rfor())

    # classify the data (remember to set the appropriate memory size)
    cbers_probs <- sits_classify(cbers_cube, rfor_model, memsize = 12, maxcores = 4)

    expect_true(all(sits_labels(cbers_probs) %in% sits_labels(cbers_samples_2bands)$label))
    expect_true(all(sits_timeline(cbers_probs) %in% sits_timeline(cbers_samples_022024)))

    cbers_label <- sits_label_classification(cbers_probs, smoothing = "bayesian")

    expect_true(all(sits_labels(cbers_label) %in% sits_labels(cbers_samples_2bands)$label))
    expect_true(all(sits_timeline(cbers_label) %in% sits_timeline(cbers_samples_022024)))

    # region of interest
    roi <- c("lon_min" = -46.40548810, "lon_max" = -46.11029403,
             "lat_min" = -13.06915262, "lat_max" = -12.88174865)

    # classify the data (remember to set the appropriate memory size)
    cbers_probs_roi <- sits_classify(cbers_cube, rfor_model, roi = roi, memsize = 12, maxcores = 4)

    expect_true(cbers_probs_roi$xmin >= cbers_probs$xmin)
    expect_true(cbers_probs_roi$xmax <= cbers_probs$xmax)

    expect_true(cbers_probs_roi$ymin >= cbers_probs$ymin)
    expect_true(cbers_probs_roi$ymax <= cbers_probs$ymax)

    cbers_label_roi <- sits_label_classification(cbers_probs_roi, smoothing = "bayesian")

    expect_true(cbers_label_roi$xmin >= cbers_probs$xmin)
    expect_true(cbers_label_roi$xmax <= cbers_probs$xmax)

    expect_true(cbers_label_roi$ymin >= cbers_probs$ymin)
    expect_true(cbers_label_roi$ymax <= cbers_probs$ymax)

    expect_true(all(file.remove(unlist(cbers_probs$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(cbers_label$file_info[[1]]$path))))
})
