test_that("EVI generation", {
    testthat::skip_on_cran()

    s2_cube <- tryCatch({
        sits_cube(source = "MSPC",
                  collection = "sentinel-2-l2a",
                  tiles = "20LKP",
                  bands = c("B05", "B8A", "CLOUD"),
                  start_date = as.Date("2019-07-18"),
                  end_date = as.Date("2019-08-30")
        )},
        error = function(e){
            return(NULL)
        })

    testthat::skip_if(purrr::is_null(s2_cube),
                      "MSPC is not accessible")

    dir_images <-  paste0(tempdir(), "/images/")
    if (!dir.exists(dir_images))
        suppressWarnings(dir.create(dir_images))

    gc_cube <- sits_regularize(
        cube        = s2_cube,
        output_dir  = dir_images,
        res         = 160,
        period      = "P1M",
        multicores = 4)

    gc_cube_new <- sits_apply(gc_cube,
                              EVI2 = 2.5*(B8A - B05)/(B8A + 2.4*B05 + 1),
                              output_dir = dir_images)

    expect_true(all(sits_bands(gc_cube_new) %in% c("EVI2", "B05", "B8A")))

    timeline <- sits_timeline(gc_cube_new)
    start_date <- timeline[1]
    end_date <- timeline[length(timeline)]

    expect_true(start_date == as.Date("2019-07-01"))
    expect_true(end_date == as.Date("2019-08-01"))

    file_info_b05 <- .file_info(gc_cube_new, band = "B05")
    b05_band_1 <- terra::rast(file_info_b05$path[[1]])

    file_info_b8a <- .file_info(gc_cube_new, band = "B8A")
    b8a_band_1 <- terra::rast(file_info_b8a$path[[1]])

    file_info_evi2 <- .file_info(gc_cube_new, band = "EVI2")
    evi2_band_1 <- terra::rast(file_info_evi2$path[[1]])

    b05_100 <- as.numeric(b05_band_1[100]/10000)
    b8a_100 <- as.numeric(b8a_band_1[100]/10000)
    evi2_100 <- as.numeric(evi2_band_1[100]/10000)

    evi2_calc_100 <- 2.5*(b8a_100 - b05_100)/(b8a_100 + 2.4*b05_100 + 1)
    expect_equal(evi2_100, evi2_calc_100, tolerance = 0.001)

    b05_150 <- as.numeric(b05_band_1[150]/10000)
    b8a_150 <- as.numeric(b8a_band_1[150]/10000)
    evi2_150 <- as.numeric(evi2_band_1[150]/10000)

    evi2_calc_150 <- 2.5*(b8a_150 - b05_150)/(b8a_150 + 2.4*b05_150 + 1)
    expect_equal(evi2_150, evi2_calc_150, tolerance = 0.001)

    bbox_cube <- sits_bbox(gc_cube_new, wgs84 = TRUE)
    lats <- runif(10, min = bbox_cube["ymin"], max = bbox_cube["ymax"])
    longs <- runif(10, min = bbox_cube["xmin"], max = bbox_cube["xmax"])

    timeline <- sits_timeline(gc_cube_new)
    start_date <- timeline[1]
    end_date <- timeline[length(timeline)]

    csv.tb <- purrr::map2_dfr(lats, longs, function(lat, long){
        tibble::tibble(
            longitude  = long,
            latitude   = lat,
            start_date = start_date,
            end_date   = end_date,
            label = "NoClass"
        )
    })
    csv_file <- paste0(tempdir(),"/csv_gc_cube.csv")
    write.csv(csv.tb, file = csv_file)

    evi_tibble <- sits_get_data(gc_cube_new, file = csv_file)
    evi_tibble_2 <- sits_apply(evi_tibble, EVI2_NEW = 2.5*(B8A - B05)/(B8A + 2.4*B05 + 1))

    values_evi2 <- sits_time_series(evi_tibble_2)$EVI2
    values_evi2_new <- sits_time_series(evi_tibble_2)$EVI2_NEW
    expect_equal(values_evi2, values_evi2_new, tolerance = 0.001)

})
