test_that("Utils", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        progress = FALSE
    )
    fi <- sinop$file_info[[1]]
    expect_error(length(unique(.by(fi, col = "banda", .fi_timeline))))

})
