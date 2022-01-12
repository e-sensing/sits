test_that("One-year, multi-core classification in parallel", {

    s2_cube <- sits_cube(
        source = "MSPC",
        collection = "SENTINEL-2-L2A",
        tiles = c("20LKP"),
        bands = c("B8A", "B11", "CLOUD"),
        start_date = "2018-07-12",
        end_date = "2019-07-28"
    )
    dir_images <-  paste0(tempdir(), "/images/")

    if (!dir.exists(dir_images))
        suppressWarnings(dir.create(dir_images))

    gc_cube <- sits_regularize(
        cube        = s2_cube,
        output_dir  = dir_images,
        res         = 40,
        period      = "P16D",
        multicores = 4)

})


