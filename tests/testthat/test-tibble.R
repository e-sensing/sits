test_that("Align dates", {
    data("samples_modis_4bands")
    timeline <- sits_timeline(point_mt_6bands)
    start_date <- lubridate::as_date("2001-08-01")
    end_date <- lubridate::as_date("2002-07-31")

    ref_dates <- timeline[timeline > start_date]
    ref_dates <- ref_dates[ref_dates < end_date]

    new_data <- sits:::.sits_tibble_align_dates(samples_modis_4bands, ref_dates)

    ts_dates <- sits_timeline(new_data)

    expect_true(start_date <= lubridate::as_date(ts_dates[1]))
    expect_true(end_date >= lubridate::as_date(ts_dates[length(ts_dates)]))
})

test_that("Apply", {
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    point2 <- sits_apply(point_ndvi,
                         NDVI = (NDVI - min(NDVI)) / (max(NDVI) - min(NDVI))
    )

    expect_equal(sum((sits_time_series(point2))$NDVI_norm),
                 216.6617,
                 tolerance = 0.1
    )
})

test_that("Bands", {
    samples_mt_ndvi <- sits_select(samples_modis_4bands, bands = "NDVI")
    bands <- sits_bands(samples_mt_ndvi)

    expect_equal(length(bands), 1)
    expect_equal(bands[1], "NDVI")
})

test_that("Bbox", {
    bbox <- sits_bbox(samples_modis_4bands)
    expect_true(all(names(bbox_ll) %in%
                        c("lon_min", "lat_min", "lon_max", "lat_max")))
    expect_true(bbox["lon_min"] < -60.0)


})
test_that("Merge", {
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    point_evi <- sits_select(point_mt_6bands, bands = "EVI")
    result <- sits_merge(point_ndvi, point_evi)

    expect_true(length(sits_timeline(result)) == 412)
    expect_true(ncol(sits_time_series(result)) == 3)
})

test_that("Prune", {
    data("cerrado_2classes")
    new_data <- cerrado_2classes[1:3, ]
    ts_1 <- sits_time_series(new_data[1, ])
    ts_2 <- ts_1[1:10, ]
    new_data[1, ]$time_series[[1]] <- ts_2

    pruned_data <- suppressMessages(sits:::.sits_tibble_prune(new_data))
    expect_true(nrow(pruned_data) == 2)
})

test_that("Sample", {
    data(cerrado_2classes)

    data <- sits_sample(cerrado_2classes, n = 10)
    expect_true(nrow(data) == 20)

    data <- sits_sample(cerrado_2classes, frac = 0.1)
    expect_true(nrow(dplyr::filter(data, label == "Cerrado")) == 40)
    expect_true(nrow(dplyr::filter(data, label == "Pasture")) == 35)
})

test_that("Select", {
    samples_mt_ndvi <- sits_select(samples_modis_4bands, bands = "NDVI")
    expect_equal(length(sits_bands(samples_mt_ndvi)), 1)

    samples_pasture <- samples_mt_ndvi %>% dplyr::filter(label == "Pasture")

    expect_equal(dim(samples_pasture)[1], 344)
})

test_that("Sample", {
    data <- sits_sample(cerrado_2classes, n = 10)

    expect_equal(sits_labels(cerrado_2classes), sits_labels(data))
    expect_equal(dim(data)[1], 20)
})


test_that("Values", {
    values <- sits_values(cerrado_2classes[1:2, ], format = "bands_dates_cases")

    expect_equal(names(values), sits_bands(cerrado_2classes))

    expect_equal(sum(values$NDVI[, "NDVI"]), 13.6291, tolerance = 0.001)
})

test_that("Ops Compute", {
    ndwi <- sits:::.sits_ops_compute(samples_modis_4bands,
                              NDWI = (1.5) * (NIR - MIR) / (NIR + MIR))

    expect_true("NDWI" %in% names(sits_time_series(ndwi)))
})
