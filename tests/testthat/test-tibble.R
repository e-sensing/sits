context("Tibble")

test_that("Apply",{
    point2 <- sits_apply(point_ndvi, fun = function(x) {(x - min(x)) / (max(x) - min(x))})

    expect_equal(sum(point2$time_series[[1]]$ndvi), 219.068, tolerance = 0.01)
})

test_that("Bands",{
    bands <- sits_bands(samples_MT_9classes)

    expect_equal(length(bands), 6)
    expect_equal(bands[1], "mir")
    expect_equal(bands[6], "ndvi")
})

test_that("Break",{
    #skip_on_cran()
    points.tb <- sits_break(point_ndvi, timeline_modis_392, "2000-08-28", "2016-08-12")

    expect_equal(dim(points.tb)[1], 16)
    expect_equal(dim(points.tb)[2], 7)
})

test_that("Dates",{
    dates <- sits_dates(point_MT_6bands)

    expect_equal(length(dates), 412)
    expect_equal(dates[1], lubridate::ymd("2000-02-18"))
    expect_equal(dates[412], lubridate::ymd("2018-01-01"))
})

test_that("Merge", {
    data(point_ndvi)
    point_ws.tb <- sits_whittaker(point_ndvi, lambda = 3.0)
    result <- sits_merge(point_ndvi, point_ws.tb)

    expect_equal(dim(result$time_series[[1]])[1], 392)
    expect_equal(dim(result$time_series[[1]])[2], 3)
})

test_that("Mutate", {
    savi.tb <- sits_mutate(samples_MT_9classes, savi = (1.5*(nir - red)/(nir + red + 0.5)))

    expect_equal(sum(savi.tb$time_series[[1]]$savi), 9.0234, tolerance = 0.001)
})

test_that("Rename",{
    ndvi1.tb <- sits_rename(point_ndvi, names = "veg_index")
    expect_equal(sits_bands(ndvi1.tb), "veg_index")
})

test_that("Transmute", {
    data.tb <- sits_sample(cerrado_2classes, n = 10)

    expect_equal(sits_labels(cerrado_2classes)$label, sits_labels(data.tb)$label)
    expect_equal(dim(data.tb)[1], 20)
})


test_that("Values", {
    values <- sits_values(cerrado_2classes[1:2,], format = "bands_dates_cases")

    expect_equal(names(values), sits_bands(cerrado_2classes))

    expect_equal(sum(values$ndvi[, "ndvi"]), 13.6291, tolerance = 0.001)
})

test_that("Values", {
    data (samples_MT_9classes)
    savi.tb <- sits_transmute (samples_MT_9classes, savi = (1.5*(nir - red)/(nir + red + 0.5)))

    expect_equal(names(savi.tb$time_series[[1]])[2], "savi")
})

test_that("Select",{
    bands <- sits_bands(samples_MT_9classes)

    samplesMir <- sits_select(samples_MT_9classes, bands = c("mir"))

    expect_equal(length(sits_bands(samplesMir)), 1)

    samplesPasture <- samples_MT_9classes %>% dplyr::filter(label == "Pasture")

    expect_equal(dim(samplesPasture)[1], 370)
})
