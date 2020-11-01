context("Space Time Operations")

test_that("All", {
    reproj <- sits:::.sits_latlong_to_proj(-10, -20, 4326)

    expect_equal(as.numeric(reproj[1,1]), -10)
    expect_equal(as.numeric(reproj[1,2]), -20)

    reproj <- sits:::.sits_proj_to_latlong(-10, -20, 4326)

    expect_equal(as.numeric(reproj[1,1]), -10)
    expect_equal(as.numeric(reproj[1,2]), -20)
})

test_that("Inside", {
    ndvi_file <- c(system.file("extdata/raster/mod13q1/sinop-ndvi-2014.tif",
                               package = "sits"))

    evi_file <- c(system.file("extdata/raster/mod13q1/sinop-evi-2014.tif",
                              package = "sits"))

    data("timeline_2013_2014")

    sinop_2014 <- sits_cube(name = "sinop-2014",
                            timeline = timeline_2013_2014,
                            satellite = "TERRA",
                            sensor = "MODIS",
                            bands = c("ndvi", "evi"),
                            files = c(ndvi_file, evi_file))

    point <- data.frame(X = 1, Y = 1)

    expect_false(sits:::.sits_raster_xy_inside(point, sinop_2014))

    point <- data.frame(X = -6058762, Y = -1308378)

    expect_true(sits:::.sits_raster_xy_inside(point, sinop_2014))
})
test_that("Time Series Dates", {
    times <- sits_time_series_dates(cerrado_2classes)
    expect_true(length(times) == 23)

})

