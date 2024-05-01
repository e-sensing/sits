test_that("Access to SwissDataCube", {
    roi <- c(
        lon_min = 7.54, lat_min = 46.73,
        lon_max = 7.65, lat_max = 46.77
    )
    s2_cube_sdc <- .try(
        {
            sits_cube(
                source = "SDC",
                collection = "S2_L2A_10M_SWISS",
                roi = roi,
                bands = c("B08"),
                start_date = as.Date("2018-07-18"),
                end_date = as.Date("2018-08-23"),
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(purrr::is_null(s2_cube_sdc), "SDC is not accessible")
})
