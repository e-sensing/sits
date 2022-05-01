test_that("compute distances in time series for indices", {

    bands_samples <- sits_bands(samples_modis_4bands)
    n_timeline <- length(sits_timeline(samples_modis_4bands))
    bands_distances_names <- unlist(
        purrr::map(bands_samples, paste0, seq_len(n_timeline))
    )

    samples_distances <- .sits_distances(samples_modis_4bands)

    testthat::expect_equal(
        colnames(samples_distances),
        c("original_row", "reference", bands_distances_names)
    )

    testthat::expect_equal(
        class(samples_distances),
        c("data.table", "data.frame")
    )

    testthat::expect_equal(
        samples_distances[10, "NDVI2"][[1]],
        0.3842
    )

    testthat::expect_equal(
        samples_distances[290, "MIR14"][[1]],
        0.0859
    )

    testthat::expect_equal(
        samples_distances[102, "reference"][[1]],
        "Pasture"
    )

    testthat::expect_equal(
        samples_distances[500, "original_row"][[1]],
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

    bands_samples <- sits_bands(ts)
    n_timeline <- length(sits_timeline(ts))
    bands_distances_names <- unlist(
        purrr::map(bands_samples, paste0, seq_len(n_timeline))
    )

    samples_distances <- .sits_distances(ts)

    testthat::expect_equal(
        colnames(samples_distances),
        c("original_row", "reference", bands_distances_names)
    )

    testthat::expect_equal(
        class(samples_distances),
        c("data.table", "data.frame")
    )

    testthat::expect_equal(
        samples_distances[10, "B022"][[1]],
        0.3842
    )

    testthat::expect_equal(
        samples_distances[290, "B0114"][[1]],
        0.0859
    )

    testthat::expect_equal(
        samples_distances[102, "reference"][[1]],
        "Pasture"
    )

    testthat::expect_equal(
        samples_distances[500, "original_row"][[1]],
        500
    )
})

test_that("compute distances in time series with two times", {

    filter_rows <- function(ts) ts[1:2,]
    ts <- .sits_fast_apply(samples_modis_4bands, "time_series", filter_rows)
    ts <- sits_select(ts, "NDVI")

    bands_samples <- sits_bands(ts)
    n_timeline <- length(sits_timeline(ts))
    bands_distances_names <- unlist(
        purrr::map(bands_samples, paste0, seq_len(n_timeline))
    )

    samples_distances <- .sits_distances(ts)

    testthat::expect_equal(
        colnames(samples_distances),
        c("original_row", "reference", bands_distances_names)
    )

    testthat::expect_equal(
        class(samples_distances),
        c("data.table", "data.frame")
    )
})
