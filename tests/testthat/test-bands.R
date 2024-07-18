test_that("band rename", {
    bands <- sits_bands(point_mt_6bands)
    point_mt_6bands <- .band_rename(point_mt_6bands,
                            c("SWIR", "BLUE", "NIR08", "RED2", "EVI2", "NDVI2"))
    new_bands <- sits_bands(point_mt_6bands)
    expect_true(all(new_bands %in% c("SWIR", "BLUE", "NIR08",
                                     "RED2", "EVI2", "NDVI2")))
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    bands_cube <- sits_bands(sinop)
    sinop <- .band_rename(sinop, "NDVI2")
    new_band <- sits_bands(sinop)
    expect_equal(new_band, "NDVI2")

    sp <- sinop
    class(sinop) <- "data.frame"
    bands_cube <- sits_bands(sinop)
    expect_equal(bands_cube, "NDVI2")
})
