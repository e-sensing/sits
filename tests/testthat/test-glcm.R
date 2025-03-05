test_that("Testing glcm generation", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    # Create a MODIS cube
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir
    )
    dir_images <- paste0(tempdir(), "/images/")
    if (!dir.exists(dir_images)) {
        suppressWarnings(dir.create(dir_images))
    }
    unlink(list.files(dir_images,
                      pattern = "\\.tif$",
                      full.names = TRUE
    ))
    feature <- sits_select(cube, bands = "NDVI", dates = "2013-09-14")
    # Compute the NDVI variance
    texture <- sits_glcm(
        cube = feature,
        NDVIVAR = glcm_variance(NDVI),
        window_size = 5,
        output_dir = dir_images
    )

    # Test NDVIVAR
    expect_true(all(sits_bands(texture) %in% c("NDVI", "NDVIVAR")))

    timeline <- sits_timeline(texture)
    expect_true(timeline == "2013-09-14")

    file_info_ndvivar <- .fi(texture) |> .fi_filter_bands(bands = "NDVIVAR")
    ndvivar_band_1 <- .raster_open_rast(file_info_ndvivar$path[[1]])
    rast_freq <-  .raster_freq(ndvivar_band_1)
    expect_true(mean(a[,"value"]) > 7000)

    unlink(dir_images, recursive = TRUE)
})
