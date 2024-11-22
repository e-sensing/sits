test_that("sits_merge - same sensor, same bands, same tiles, compatible timeline", {
    s2a_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "ga_s2am_ard_3",
                bands = c("BLUE"),
                tiles = c("53HQE","53HPE"),
                start_date = "2019-01-01",
                end_date = "2019-07-10",
                progress = FALSE
            )
        },
        .default = NULL
    )

    s2b_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "GA_S2BM_ARD_3",
                bands = c("BLUE"),
                tiles = c("53HQE","53HPE"),
                start_date = "2019-01-01",
                end_date = "2019-07-10",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(c(s2a_cube, s2b_cube)),
                      message = "DEAustralia is not accessible"
    )

    sentinel_cube <- sits_merge(s2a_cube, s2b_cube)

    expect_true(all(sits_bands(sentinel_cube) %in% c("BLUE")))
    expect_equal(nrow(sentinel_cube), 2)

})

test_that("sits_merge - same sensor, different bands, same tiles, compatible timeline", {

})

test_that("sits_merge - different sensor, different bands, same tiles, compatible timeline", {

})

test_that("sits_merge - different sensor, same bands, same tiles, compatible timeline", {

})

test_that("sits_merge - different sensor, same bands, different tiles, compatible timeline", {

})

test_that("sits_merge - different sensor, different bands, different tiles, compatible timeline", {

})

test_that("sits_merge - different sensor, different bands, different tiles, different timeline", {

})

test_that("sits_merge - same sensor, same bands, same tiles, different timeline", {
    s2a_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "ga_s2am_ard_3",
                bands = c("BLUE"),
                tiles = c("53HQE"),
                start_date = "2019-01-01",
                end_date = "2019-02-01",
                progress = FALSE
            )
        },
        .default = NULL
    )

    s2b_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "GA_S2BM_ARD_3",
                bands = c("BLUE"),
                tiles = c("53HQE"),
                start_date = "2019-01-01",
                end_date = "2019-02-10",
                progress = FALSE
            )
        },
        .default = NULL
    )

    expect_error(
        .cube_is_regular(sits_merge(s2a_cube, s2b_cube))
    )
})

