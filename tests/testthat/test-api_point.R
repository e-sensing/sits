test_that("API point", {
    x <- c(longitude = -55, latitude = -10.)

    expect_true(.lon(x) == x[["longitude"]])
    expect_true(.lat(x) == x[["latitude"]])

    .lon(x) <- -50
    expect_true(.lon(x) == -50)
    .lat(x) <- -4
    expect_true(.lat(x) == -4)

    expect_true(.has_point(x))
    expect_false(.is_point(x))

    p <- .point(x)

    expect_true(.is_point(p))

    sf_p <- .point_as_sf(p)
    expect_true(sf::st_geometry_type(sf_p) == "POINT")
})
