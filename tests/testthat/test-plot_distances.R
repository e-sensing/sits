test_that("geo_distances", {
    mt_shp <- system.file("extdata/shapefiles/mato_grosso/mt.shp",
        package = "sits"
    )
    mt_sf <- sf::read_sf(mt_shp)

    distances <- sits_geo_dist(samples = samples_modis_ndvi, roi = mt_sf)

    expect_true("geo_distances" %in% class(distances))
    ss <- as.vector(dplyr::filter(
        distances,
        type == "sample-to-sample"
    )$distance)
    expect_true(mean(ss) > 5000.)
    p <- plot(distances)
    expect_equal(p$labels$title, "Distribution of Nearest Neighbor Distances")
})
