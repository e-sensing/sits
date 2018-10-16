context("Space Time Operations")

test_that("All",{
    reproj <- sits:::.sits_latlong_to_proj(-10, -20, 4326)

    expect_equal(reproj[1], -10)
    expect_equal(reproj[2], -20)

    reproj <- sits:::.sits_proj_to_latlong(-10, -20, 4326)

    expect_equal(reproj[1], -10)
    expect_equal(reproj[2], -20)

    wtss_coverage <- sits_coverage(service = "WTSS-INPE", name = "MOD13Q1")

    #.sits_convert_resolution(wtss_coverage)
})

