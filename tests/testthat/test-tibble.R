context("Tibble")

test_that("Align dates", {
    data("samples_mt_4bands")
    data("timeline_2000_2017")
    timeline <- lubridate::as_date(timeline_2000_2017)
    start_date <- lubridate::as_date("2001-08-01")
    end_date <- lubridate::as_date("2002-07-31")

    ref_dates <- timeline[timeline > start_date]
    ref_dates <- ref_dates[ref_dates < end_date]

    new_data <- sits:::.sits_align_dates(samples_mt_4bands, ref_dates)

    ts_dates <- sits_time_series_dates(new_data)

    expect_true(start_date <= lubridate::as_date(ts_dates[1]))
    expect_true(end_date >= lubridate::as_date(ts_dates[length(ts_dates)]))
})

test_that("Apply", {
    point2 <- sits_apply(point_ndvi,
        fun = function(x) {
            (x - min(x)) / (max(x) - min(x))
        }
    )

    expect_equal(sum((sits_time_series(point2))$NDVI),
        219.068,
        tolerance = 0.01
    )
})

test_that("Bands", {
    samples_mt_ndvi <- sits_select(samples_mt_4bands, bands = "NDVI")
    bands <- sits_bands(samples_mt_ndvi)

    expect_equal(length(bands), 1)
    expect_equal(bands[1], "NDVI")
})

test_that("Merge", {
    data(point_ndvi)
    point_ws <- sits_whittaker(point_ndvi, lambda = 3.0)
    result <- sits_merge(point_ndvi, point_ws)

    expect_true(length(sits_time_series_dates(result)) == 392)
    expect_true(ncol(sits_time_series(result)) == 3)
})

test_that("Mutate", {
    savi <- sits_mutate_bands(samples_mt_6bands,
        SAVI = (1.5 * (NIR - RED) / (NIR + RED + 0.5))
    )

    expect_equal(sum(sits_time_series(savi)$SAVI),
        5.980619,
        tolerance = 0.001
    )
})

test_that("Prune", {
    data("cerrado_2classes")
    new_data <- cerrado_2classes[1:3, ]
    ts_1 <- sits_time_series(new_data[1, ])
    ts_2 <- ts_1[1:10, ]
    new_data[1, ]$time_series[[1]] <- ts_2

    pruned_data <- suppressMessages(sits:::.sits_prune(new_data))
    expect_true(nrow(pruned_data) == 2)
})

test_that("Rename", {
    point_new <- point_ndvi
    sits_bands(point_new) <- "VEGINDEX"
    expect_equal(sits_bands(point_new), "VEGINDEX")
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
    samples_mt_ndvi <- sits_select(samples_mt_4bands, bands = "NDVI")
    expect_equal(length(sits_bands(samples_mt_ndvi)), 1)

    samples_pasture <- samples_mt_ndvi %>% dplyr::filter(label == "Pasture")

    expect_equal(dim(samples_pasture)[1], 344)
})

test_that("Transmute", {
    data <- sits_sample(cerrado_2classes, n = 10)

    expect_equal(sits_labels(cerrado_2classes)$label, sits_labels(data)$label)
    expect_equal(dim(data)[1], 20)
})


test_that("Values", {
    values <- sits_values(cerrado_2classes[1:2, ], format = "bands_dates_cases")

    expect_equal(names(values), sits_bands(cerrado_2classes))

    expect_equal(sum(values$NDVI[, "NDVI"]), 13.6291, tolerance = 0.001)
})

test_that("Values", {
    data(samples_mt_6bands)
    savi <- sits:::.sits_transmute_bands(samples_mt_6bands,
        SAVI = (1.5 * (NIR - RED) / (NIR + RED + 0.5))
    )

    expect_true("SAVI" %in% names(sits_time_series(savi)))
})
