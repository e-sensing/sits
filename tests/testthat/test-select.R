test_that("Selecting from samples", {
    # ---- Selecting samples by bands ----
    bands <- c("BLUE", "NIR")
    samples_selected <- sits_select(
       point_mt_6bands, bands = bands
    )
    expect_equal(
        object = sits_bands(samples_selected),
        expected = bands
    )
    expect_s3_class(
        object = samples_selected,
        class = "sits"
    )
    expect_error(
        sits_select(
            point_mt_6bands, bands = "NBR"
        )
    )

    # ---- Selecting samples by start_date and end_date ----
    # Selecting dates with start_date and end_date
    samples_selected <- sits_select(
        samples_modis_ndvi,
        start_date = "2010-01-01",
        end_date = "2015-05-25"
    )
    expect_gte(
        object = .ts_min_date(.ts(samples_selected)),
        expected = as.Date("2010-01-01")
    )
    expect_lte(
        object = .ts_max_date(.ts(samples_selected)),
        expected = as.Date("2015-05-25")
    )
    expect_s3_class(
        object = samples_selected,
        class = "sits"
    )
    # Not in range
    expect_error(
        sits_select(
            samples_modis_ndvi,
            start_date = "2022-01-01",
            end_date = "2023-05-25"
        )
    )
    expect_error(
        sits_select(
            samples_modis_ndvi,
            start_date = "1990-01-01",
            end_date = "1991-05-25"
        )
    )
    samples_selected <- sits_select(
        samples_modis_ndvi,
        start_date = "2015-05-25",
        end_date = "2015-05-25"
    )
    expect_equal(
        object = unique(.ts_index(.ts(samples_selected))),
        expected = as.Date("2015-05-25")
    )

    # ---- Selecting samples by dates ----
    samples_selected <- sits_select(
        samples_modis_ndvi,
        dates = c("2015-05-25")
    )
    expect_equal(
        object = unique(.ts_index(.ts(samples_selected))),
        expected = as.Date("2015-05-25")
    )

    samples_selected <- sits_select(
        samples_modis_ndvi,
        dates = c("2013-09-14", "2015-05-25")
    )
    expect_equal(
        object = unique(.ts_index(.ts(samples_selected))),
        expected = as.Date(c("2013-09-14", "2015-05-25"))
    )
    # One invalid date
    expect_error(
        sits_select(
            samples_modis_ndvi,
            dates = c("2022-09-14", "2015-05-25")
        )
    )
    expect_error(
        sits_select(
            samples_modis_ndvi,
            dates = c("2022-09-14", "2020-05-25")
        )
    )

    # ---- Selecting samples by labels ----
    samples_selected <- sits_select(
        samples_modis_ndvi,
        labels = "Cerrado"
    )
    expect_equal(
        object = sits_labels(samples_selected),
        expected = "Cerrado"
    )
    samples_selected <- sits_select(
        samples_modis_ndvi,
        labels = c("Cerrado", "Forest")
    )
    expect_equal(
        object = sits_labels(samples_selected),
        expected =  c("Cerrado", "Forest")
    )
    expect_error(
        sits_select(
            samples_modis_ndvi,
            labels = c("Cerrados", "Forest")
        )
    )

})

test_that("Selecting from cube", {
    cube <- .try(
        {
            sits_cube(
                source = "MPC",
                collection = "SENTINEL-2-L2A",
                tiles = c("20LKP", "20LLP"),
                start_date = "2020-01-01",
                end_date = "2022-01-01",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(cube), message = "MPC is not accessible")

    # ---- Selecting by bands ----
    bands <- c("B02", "B08", "B12", "CLOUD")
    selected_cube <- sits_select(
        cube, bands = bands
    )
    expect_equal(
        object = sits_bands(selected_cube),
        expected = bands
    )
    expect_error(
        sits_select(
            cube, bands = "NDVI"
        )
    )

    # ---- Selecting by start_date and end_date ----
    selected_cube <- sits_select(
        cube, start_date = "2021-01-01", end_date = "2021-12-31"
    )
    expect_gte(
        object = min(.as_date(.dissolve(.cube_timeline(selected_cube)))),
        expected = .as_date("2021-01-01")
    )

    expect_lte(
        object = max(.as_date(.dissolve(.cube_timeline(selected_cube)))),
        expected = .as_date("2021-12-31")
    )
    expect_error(
        sits_select(
            cube, start_date = "2024-01-01", end_date = "2024-12-31"
        )
    )
    # ---- Selecting by dates ----
    selected_cube <- sits_select(
        cube, dates = c("2021-09-15", "2021-11-09")
    )
    expect_equal(
        object = .as_date(.dissolve(.cube_timeline(selected_cube))),
        expected = .as_date(c("2021-09-15", "2021-11-09"))
    )
    expect_error(
        sits_select(
            cube, dates = c("2018-01-01")
        )
    )
    # ---- Selecting by tiles ----
    selected_cube <- sits_select(
        cube, tiles = "20LKP"
    )
    expect_equal(
        object = selected_cube$tile,
        expected = "20LKP"
    )
    expect_equal(
        object = nrow(sits_select(cube, tiles = c("22LKP"))),
        expected = 0
    )
})
