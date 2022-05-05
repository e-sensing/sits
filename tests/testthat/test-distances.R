test_that("compute distances in time series for indices", {

    bands_samples <- sits_bands(samples_modis_4bands)
    n_timeline <- length(sits_timeline(samples_modis_4bands))

    sample_distances <- .sits_distances(samples_modis_4bands)

    testthat::expect_equal(
        class(sample_distances),
        c("data.table", "data.frame")
    )

    testthat::expect_equal(
        sample_distances[10, "NDVI2"][[1]],
        0.3842
    )

    testthat::expect_equal(
        sample_distances[290, "MIR14"][[1]],
        0.0859
    )

    testthat::expect_equal(
        sample_distances[102, "reference"][[1]],
        "Pasture"
    )

    testthat::expect_equal(
        sample_distances[500, "original_row"][[1]],
        500
    )
})

test_that("compute distances in time series for bands and indices", {

    rename_cols <- function(ts) {

        dplyr::rename(ts,
                      "B01" = .data[["MIR"]],
                      "B02" = .data[["NDVI"]],
                      "B03" = .data[["NIR"]])
    }

    ts <- .sits_fast_apply(samples_modis_4bands, "time_series", rename_cols)

    sample_distances <- .sits_distances(ts)

    testthat::expect_equal(
        class(sample_distances),
        c("data.table", "data.frame")
    )

    testthat::expect_equal(
        sample_distances[10, "B022"][[1]],
        0.3842
    )

    testthat::expect_equal(
        sample_distances[290, "B0114"][[1]],
        0.0859
    )

    testthat::expect_equal(
        sample_distances[102, "reference"][[1]],
        "Pasture"
    )

    testthat::expect_equal(
        sample_distances[500, "original_row"][[1]],
        500
    )
})
