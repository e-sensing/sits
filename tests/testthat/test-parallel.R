test_that("One-year, multi-core classification in parallel", {
    testthat::skip_on_cran()

    l8_cube <- tryCatch(
        {
            sits_cube(
                source = "BDC",
                collection = "LC8_30_16D_STK-1",
                tiles = c("038047"),
                bands = c("NDVI", "EVI"),
                start_date = "2018-07-12",
                end_date = "2019-07-28"
            )
        },
        error = function(e) {
            return(NULL)
        }
    )

    testthat::skip_if(purrr::is_null(l8_cube),
        message = "BDC is not accessible"
    )

    rfor_model <- sits_train(samples_l8_rondonia_2bands, sits_rfor())

    roi <- c(
        "lon_min" = -65.2313, "lat_min" = -10.5411,
        "lon_max" = -64.6915, "lat_max" = -10.3122
    )

    dir_images <- paste0(tempdir(), "/images/")
    if (!dir.exists(dir_images)) {
        suppressWarnings(dir.create(dir_images))
    }
    unlink(list.files(dir_images,
        pattern = "\\.tif$",
        full.names = TRUE
    ))


    l8_probs <- sits_classify(l8_cube,
        rfor_model,
        roi = roi,
        memsize = 8,
        multicores = 2,
        output_dir = dir_images
    )


    r_obj <- sits:::.raster_open_rast(.file_info_path(l8_probs))

    expect_true(l8_probs[["xmin"]] > l8_cube[["xmin"]])
    expect_true(l8_probs[["xmax"]] < l8_cube[["xmax"]])

    expect_true(
        sits:::.raster_nrows(r_obj) < sits:::.cube_size(l8_cube)[["nrows"]]
    )

    expect_equal(
        sits:::.raster_nrows(r_obj),
        sits:::.cube_size(l8_probs)[["nrows"]]
    )

    max_lyr2 <- max(sits:::.raster_get_values(r_obj)[, 2])
    expect_true(max_lyr2 <= 10000)

    max_lyr3 <- max(sits:::.raster_get_values(r_obj)[, 3])
    expect_true(max_lyr3 <= 10000)

    min_lyr3 <- min(sits:::.raster_get_values(r_obj)[, 3])
    expect_true(min_lyr3 >= 0)
})
