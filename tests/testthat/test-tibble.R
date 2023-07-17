test_that("Align dates", {
    timeline <- sits_timeline(point_mt_6bands)
    start_date <- lubridate::as_date("2001-08-01")
    end_date <- lubridate::as_date("2002-07-31")

    ref_dates <- timeline[timeline > start_date]
    ref_dates <- ref_dates[ref_dates < end_date]

    new_data <- .tibble_align_dates(samples_modis_ndvi, ref_dates)

    ts_dates <- sits_timeline(new_data)

    expect_true(start_date <= lubridate::as_date(ts_dates[1]))
    expect_true(end_date >= lubridate::as_date(ts_dates[length(ts_dates)]))
})

test_that("Apply", {
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    point2 <- sits_apply(point_ndvi,
        NDVI_norm = (NDVI - min(NDVI)) /
            (max(NDVI) - min(NDVI))
    )

    expect_equal(sum((.tibble_time_series(point2))$NDVI_norm),
        101.5388,
        tolerance = 0.1
    )
})

test_that("Bands", {
    bands <- sits_bands(samples_modis_ndvi)

    expect_equal(length(bands), 1)
    expect_equal(bands[1], "NDVI")

    sits_bands(samples_modis_ndvi) <- "EVI"
    new_bands <- sits_bands(samples_modis_ndvi)
    expect_equal(new_bands[1], "EVI")
    sits_bands(samples_modis_ndvi) <- "NDVI"
    bands <- sits_bands(samples_modis_ndvi)
    expect_equal(bands[1], "NDVI")

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        progress = FALSE
    )
    sits_bands(cube) <- "EVI"
    new_bands <- sits_bands(cube)
    expect_equal(new_bands[1], "EVI")
    sits_bands(cube) <- "NDVI"
    bands <- sits_bands(cube)
    expect_equal(bands[1], "NDVI")
})

test_that("Dates", {
    selected_samples1 <- sits_select(
        samples_modis_ndvi,
        start_date = "2006-11-17", end_date = "2007-07-28"
    )
    expect_equal(
        min(.ts_start_date(.ts(selected_samples1))), as.Date("2006-11-17")
    )
    expect_equal(
        max(.ts_end_date(.ts(selected_samples1))), as.Date("2007-07-28")
    )

    selected_samples2 <- sits_select(
        samples_modis_ndvi,
        start_date = "2006-11-17"
    )
    expect_equal(
        min(.ts_start_date(.ts(selected_samples2))), as.Date("2006-11-17")
    )
    expect_equal(
        max(.ts_end_date(.ts(selected_samples2))), as.Date("2016-08-28")
    )

    selected_samples3 <- sits_select(
        samples_modis_ndvi,
        end_date = "2010-09-14"
    )
    expect_equal(
        min(.ts_start_date(.ts(selected_samples3))), as.Date("2000-09-13")
    )
    expect_equal(
        max(.ts_end_date(.ts(selected_samples3))), as.Date("2010-09-14")
    )

    expect_error(object = {
        sits_select(
            samples_modis_ndvi,
            start_date = "2020-01-01",
            end_date = "2021-09-14"
        )
    })
})

test_that("Bbox", {
    bbox <- sits_bbox(samples_modis_ndvi)
    expect_true(all(names(bbox) %in%
        c("xmin", "ymin", "xmax", "ymax", "crs")))
    expect_true(bbox["xmin"] < -60.0)
})

test_that("Merge", {
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    point_evi <- sits_select(point_mt_6bands, bands = "EVI")
    result <- sits_merge(point_ndvi, point_evi)

    expect_true(length(sits_timeline(result)) == 204)
    expect_true(ncol(.tibble_time_series(result)) == 3)

    result2 <- sits_merge(point_ndvi, point_ndvi)
    expect_true(all(sits_bands(result2) %in% c("NDVI.1", "NDVI.2")))
})

test_that("Prune", {
    data("cerrado_2classes")
    new_data <- cerrado_2classes[1:3, ]
    ts_1 <- .tibble_time_series(new_data[1, ])
    ts_2 <- ts_1[1:10, ]
    new_data$time_series[[1]] <- ts_2

    pruned_data <- suppressMessages(.tibble_prune(new_data))
    expect_true(nrow(pruned_data) == 2)
})

test_that("Select", {
    expect_equal(length(sits_bands(samples_modis_ndvi)), 1)
    samples_pasture <- samples_modis_ndvi |> dplyr::filter(label == "Pasture")
    expect_equal(dim(samples_pasture)[1], 344)
})

test_that("Sample", {
    data <- sits_sample(cerrado_2classes, n = 10)
    expect_equal(sits_labels(cerrado_2classes), sits_labels(data))
    expect_equal(dim(data)[1], 20)
})

test_that("Values", {
    values <- .values_ts(cerrado_2classes[1:2, ], format = "bands_dates_cases")

    expect_equal(names(values), sits_bands(cerrado_2classes))

    expect_equal(sum(values$NDVI[, "NDVI"]), 13.6291, tolerance = 0.001)
})

test_that("Apply", {
    samples_ndwi <- sits_apply(point_mt_6bands,
        NDWI = (1.5) * (NIR - MIR) / (NIR + MIR)
    )

    expect_true("NDWI" %in% sits_bands(samples_ndwi))
})

test_that("samples_as_sf works (point)", {
    samples_tb <- cerrado_2classes
    samples_sf <- sits_as_sf(samples_tb)

    expect_true(inherits(samples_sf, "sf"))
    expect_equal(
        as.character(unique(sf::st_geometry_type(samples_sf))),
        "POINT"
    )
})

test_that("samples_as_sf works (polygon)", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        progress = FALSE
    )
    cube_sf <- sits_as_sf(cube)

    expect_true(inherits(cube_sf, "sf"))
    expect_equal(
        as.character(unique(sf::st_geometry_type(cube_sf))),
        "POLYGON"
    )
})

test_that("ts errors", {
    point <- point_mt_6bands[, 1:5]
    expect_error(.ts(point))
    expect_error({
        .ts(point) <- 2.0
    })
    expect_error(.ts_values(point_mt_6bands, bands = "B08"))
})
