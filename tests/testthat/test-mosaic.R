test_that("One-year, multicores mosaic", {
    # create a random forest model
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
    # create a data cube from local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    output_dir <- paste0(tempdir(), "/mosaic")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
    # classify a data cube
    probs_cube <- sits_classify(
        data = sinop,
        ml_model = rfor_model,
        output_dir = output_dir,
        progress = FALSE
    )
    # smooth the probability cube using Bayesian statistics
    bayes_cube <- sits_smooth(probs_cube, output_dir = output_dir)
    # label the probability cube
    label_cube <- sits_label_classification(
        bayes_cube,
        output_dir = output_dir,
        progress = FALSE
    )
    # create roi
    roi <- sf::st_sfc(
        sf::st_polygon(
            list(rbind(
                c(-55.64768, -11.68649),
                c(-55.69654, -11.66455),
                c(-55.62973, -11.61519),
                c(-55.64768, -11.68649)
            ))
        ),
        crs = 4326
    )
    # crop and reproject original cube
    suppressWarnings({
        mosaic_cube <- sits_mosaic(
            cube = sinop,
            roi = roi,
            output_dir = output_dir,
            version = "v1",
            multicores = 1,
            progress = FALSE
        )
    })
    expect_equal(mosaic_cube[["tile"]], "MOSAIC")
    expect_equal(nrow(mosaic_cube), 1)
    bbox_mos <- sits_bbox(mosaic_cube, as_crs = 4326)
    bbox_roi <- sf::st_bbox(roi)
    expect_true(
        bbox_mos[["xmin"]] <= bbox_roi[["xmin"]] &&
            bbox_mos[["xmax"]] >= bbox_roi[["xmax"]] &&
            bbox_mos[["ymin"]] <= bbox_roi[["ymin"]] &&
            bbox_mos[["ymax"]] >= bbox_roi[["ymax"]]
    )

    # crop and reproject classified image
    suppressWarnings({
        mosaic_class <- sits_mosaic(
            cube = label_cube,
            roi = roi,
            crs = 4326,
            output_dir = output_dir,
            version = "v1",
            multicores = 1,
            progress = FALSE
        )
    })

    expect_equal(mosaic_class[["tile"]], "MOSAIC")
    expect_equal(nrow(mosaic_class), 1)
    bbox_cube <- sits_bbox(mosaic_class)
    bbox_roi <- sf::st_bbox(roi)
    expect_equal(bbox_cube[["xmin"]], bbox_roi[["xmin"]], tolerance = 0.01)
    expect_equal(bbox_cube[["ymin"]], bbox_roi[["ymin"]], tolerance = 0.01)
    expect_equal(bbox_cube[["xmax"]], bbox_roi[["xmax"]], tolerance = 0.01)
    expect_equal(bbox_cube[["ymax"]], bbox_roi[["ymax"]], tolerance = 0.01)
    # resume feature
    mosaic_class <- sits_mosaic(
        cube = label_cube,
        roi = roi,
        crs = 4326,
        output_dir = output_dir,
        version = "v1",
        progress = FALSE
    )
    expect_equal(mosaic_class[["tile"]], "MOSAIC")

    # create new roi
    roi2 <- sf::st_sfc(
        sf::st_polygon(
            list(rbind(
                c(-55.91563676, -11.92443997),
                c(-55.02414662, -11.92443997),
                c(-55.02414662, -11.38658587),
                c(-55.91563676, -11.38658587),
                c(-55.91563676, -11.92443997)
            ))
        ),
        crs = 4326
    )

    # reproject classified image
    mosaic_class2 <- sits_mosaic(
        cube = label_cube,
        roi = roi2,
        crs = 4326,
        output_dir = output_dir,
        version = "v2",
        progress = FALSE
    )

    expect_equal(mosaic_class2[["tile"]], "MOSAIC")
    expect_equal(nrow(mosaic_class2), 1)
    bbox_cube <- sits_bbox(mosaic_class2)
    bbox_roi <- sf::st_bbox(roi2)
    expect_equal(bbox_cube[["xmin"]], bbox_roi[["xmin"]], tolerance = 0.01)
    expect_equal(bbox_cube[["ymin"]], bbox_roi[["ymin"]], tolerance = 0.01)
    expect_equal(bbox_cube[["xmax"]], bbox_roi[["xmax"]], tolerance = 0.01)
    expect_equal(bbox_cube[["ymax"]], bbox_roi[["ymax"]], tolerance = 0.01)
    uncert_cube <- sits_uncertainty(probs_cube, output_dir = output_dir)
    mosaic_uncert <- sits_mosaic(
        cube = uncert_cube,
        roi = roi,
        crs = 4326,
        output_dir = output_dir,
        version = "v3",
        progress = FALSE
    )

    expect_equal(mosaic_uncert[["tile"]], "MOSAIC")
    expect_equal(nrow(mosaic_uncert), 1)
    bbox_cube <- sits_bbox(mosaic_uncert)
    bbox_roi <- sf::st_bbox(roi)
    expect_equal(bbox_cube[["xmin"]], bbox_roi[["xmin"]], tolerance = 0.01)
    expect_equal(bbox_cube[["ymin"]], bbox_roi[["ymin"]], tolerance = 0.01)
    expect_equal(bbox_cube[["xmax"]], bbox_roi[["xmax"]], tolerance = 0.01)
    expect_equal(bbox_cube[["ymax"]], bbox_roi[["ymax"]], tolerance = 0.01)

    unlink(probs_cube$file_info[[1]]$path)
    unlink(bayes_cube$file_info[[1]]$path)
    unlink(label_cube$file_info[[1]]$path)
    unlink(mosaic_cube$file_info[[1]]$path)
    unlink(mosaic_class$file_info[[1]]$path)
    unlink(mosaic_class2$file_info[[1]]$path)
    unlink(mosaic_uncert$file_info[[1]]$path)
    unlink(uncert_cube$file_info[[1]]$path)
})

test_that("One-year, multicores mosaic with class cube from STAC", {
    # prepare output dir
    output_dir <- paste0(tempdir(), "/mosaic")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
    # create roi
    roi <- sf::st_sfc(
        sf::st_polygon(
            list(rbind(
                c(-55.64768, -11.68649),
                c(-55.69654, -11.66455),
                c(-55.62973, -11.61519),
                c(-55.64768, -11.68649)
            ))
        ),
        crs = 4326
    )
    roi <- sf::st_transform(roi, 3857)
    # load class cube
    label_cube <- sits_cube(
        source     = "TERRASCOPE",
        collection = "WORLD-COVER-2021",
        roi        = roi,
        progress   = FALSE
    )
    # crop and reproject classified image
    suppressWarnings({
        mosaic_class <- sits_mosaic(
            cube = label_cube,
            roi = roi,
            crs = 3857,
            output_dir = output_dir,
            version = "v1",
            multicores = 1,
            progress = FALSE
        )
    })

    expect_equal(mosaic_class[["tile"]], "MOSAIC")
    expect_equal(nrow(mosaic_class), 1)
    bbox_cube <- sits_bbox(mosaic_class)
    bbox_roi <- sf::st_bbox(roi)
    expect_equal(bbox_cube[["xmin"]], bbox_roi[["xmin"]], tolerance = 0.01)
    expect_equal(bbox_cube[["ymin"]], bbox_roi[["ymin"]], tolerance = 0.01)
    expect_equal(bbox_cube[["xmax"]], bbox_roi[["xmax"]], tolerance = 0.01)
    expect_equal(bbox_cube[["ymax"]], bbox_roi[["ymax"]], tolerance = 0.01)
    # resume feature
    mosaic_class <- sits_mosaic(
        cube = label_cube,
        roi = roi,
        crs = 4326,
        output_dir = output_dir,
        version = "v1",
        progress = FALSE
    )
    expect_equal(mosaic_class[["tile"]], "MOSAIC")

    unlink(label_cube$file_info[[1]]$path)
    unlink(mosaic_class$file_info[[1]]$path)
})
