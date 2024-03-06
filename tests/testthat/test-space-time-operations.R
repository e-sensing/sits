test_that("All", {
    reproj <- .proj_from_latlong(-10, -20, crs = 4326)

    expect_equal(as.numeric(reproj[1, 1]), -10)
    expect_equal(as.numeric(reproj[1, 2]), -20)

    reproj <- .proj_to_latlong(-10, -20, 4326)

    expect_equal(as.numeric(reproj[1, 1]), -10)
    expect_equal(as.numeric(reproj[1, 2]), -20)
})
test_that("Time Series Dates", {
    times <- sits_timeline(cerrado_2classes)
    expect_true(length(times) == 23)

    cerrado_tb <- cerrado_2classes
    class(cerrado_tb) <- "tbl_df"
    times2 <- sits_timeline(cerrado_2classes)
    expect_true(length(times2) == 23)
})
test_that("Timeline format", {
    expect_equal(.timeline_format(date = "2000-10-30"), as.Date("2000-10-30"))
    expect_equal(.timeline_format(date = "2000-10"), as.Date("2000-10-01"))
    expect_equal(.timeline_format(date = "2000"), as.Date("2000-01-01"))
    expect_equal(.timeline_format(date = "20001030"), as.Date("2000-10-30"))
})
test_that("Timeline date", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    raster_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        multicores = 2,
        progress = FALSE
    )
    timeline <- sits_timeline(raster_cube)
    expect_false(.timeline_valid_date(as.Date("2000-09-12"), timeline))
    expect_true(.timeline_valid_date(as.Date("2013-09-12"), timeline))
    expect_true(.timeline_valid_date(as.Date("2014-09-12"), timeline))
    expect_equal(timeline, .timeline_during(timeline))

})
