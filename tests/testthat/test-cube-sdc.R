test_that("Access to SwissDataCube with roi", {
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
    expect_true(nrow(s2_cube_sdc) == 1)
    expect_true(s2_cube_sdc[["tile"]] == "32TLS")
})

test_that("Access to SwissDataCube with tiles", {
    s2_cube_sdc <- .try(
        {
            sits_cube(
                source = "SDC",
                collection = "S2_L2A_10M_SWISS",
                tiles = "32TLS",
                bands = c("B08"),
                start_date = as.Date("2018-07-18"),
                end_date = as.Date("2018-08-23"),
                progress = FALSE,
                limit = 1
            )
        },
        .default = NULL
    )
    testthat::skip_if(purrr::is_null(s2_cube_sdc), "SDC is not accessible")
    expect_true(nrow(s2_cube_sdc) == 1)
    expect_true(s2_cube_sdc["tile"] == "32TLS")
})

test_that("Access to SwissDataCube with tiles and roi", {
    roi <- c(
        lon_min = 7.54, lat_min = 46.73,
        lon_max = 7.65, lat_max = 46.77
    )
    expect_error(
        sits_cube(
            source = "SDC",
            collection = "S2_L2A_10M_SWISS",
            roi = roi,
            tiles = "32TLS",
            bands = c("B08"),
            start_date = as.Date("2018-07-18"),
            end_date = as.Date("2018-08-23"),
            progress = FALSE
        )
    )
})
