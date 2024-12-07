test_that("Smoothing with exclusion mask", {
    # preparation - create cube
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    raster_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        tiles = "012010",
        bands = "NDVI",
        start_date = "2013-09-14",
        end_date = "2014-08-29",
        multicores = 2,
        progress = FALSE
    )
    # preparation - create directory to save NA
    data_dir <- paste0(tempdir(), "/smooth-exclusion-mask-na")
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
    # preparation - create exclusion mask
    exclusion_mask <- sf::st_as_sfc(
        x = sf::st_bbox(c(
            xmin = -6057482,
            ymin = -1290723,
            xmax = -6055209,
            ymax = -1288406
        ),
            crs = .cube_crs(raster_cube)
        )
    )
    # preparation - calculate centroid of the exclusion mask
    exclusion_mask_centroid <- sf::st_centroid(exclusion_mask)
    # preparation - create a random forest model
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor(num_trees = 40))
    # test classification with NA
    probs_map <- suppressWarnings(
        sits_classify(
            data = raster_cube,
            ml_model = rfor_model,
            output_dir = data_dir,
            progress = FALSE
        )
    )
    # smoth with exclusion mask
    smooth_map <- sits_smooth(
        cube = probs_map,
        exclusion_mask = exclusion_mask,
        output_dir = data_dir,
        multicores = 2
    )
    # testing original data (no na)
    probs_map_rst <- terra::rast(probs_map[["file_info"]][[1]][["path"]])
    probs_map_value <- terra::extract(
        x = probs_map_rst,
        y = terra::vect(exclusion_mask_centroid)
    )
    expect_false(any(is.na(probs_map_value)))
    # testing smooth data (with na)
    smooth_map_rst <- terra::rast(smooth_map[["file_info"]][[1]][["path"]])
    smooth_map_value <- terra::extract(
        x = smooth_map_rst,
        y = terra::vect(exclusion_mask_centroid)
    )
    expect_true(any(is.na(smooth_map_value)))
    # remove test files
    unlink(data_dir)
})
