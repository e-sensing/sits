test_that("Export and import samples in parquet format", {
    skip_if_not_installed("arrow")
    skip_if_not_installed("jsonlite")

    parquet_file <- paste0(tempdir(), "/samples.parquet")
    on.exit(unlink(parquet_file), add = TRUE)

    sits_to_parquet(samples_modis_ndvi, file = parquet_file)
    expect_true(file.exists(parquet_file))

    samples <- sits_from_parquet(parquet_file)
    expect_equal(class(samples), class(samples_modis_ndvi))
    expect_equal(colnames(samples), colnames(samples_modis_ndvi))
    expect_equal(nrow(samples), nrow(samples_modis_ndvi))
    expect_equal(sits_bands(samples), sits_bands(samples_modis_ndvi))
    expect_equal(sits_timeline(samples), sits_timeline(samples_modis_ndvi))
    expect_equal(
        as.data.frame(samples[["time_series"]][[1L]]),
        as.data.frame(samples_modis_ndvi[["time_series"]][[1L]])
    )
    # the round trip must survive a further use of the samples
    expect_no_error(sits_predictors(samples))
})

test_that("Parquet round trip keeps the class of sample variants", {
    skip_if_not_installed("arrow")
    skip_if_not_installed("jsonlite")

    parquet_file <- paste0(tempdir(), "/variants.parquet")
    on.exit(unlink(parquet_file), add = TRUE)

    # a variant with an extra scalar column and no class of its own
    folds <- samples_modis_ndvi
    folds[["folds"]] <- rep(seq_len(2L), length.out = nrow(folds))
    sits_to_parquet(folds, file = parquet_file)
    expect_equal(colnames(sits_from_parquet(parquet_file)), colnames(folds))

    # a variant carrying its own class, structurally identical to sits
    patterns <- sits_patterns(samples_modis_ndvi)
    sits_to_parquet(patterns, file = parquet_file)
    expect_equal(class(sits_from_parquet(parquet_file)), class(patterns))
})

test_that("Parquet round trip handles samples without time series", {
    skip_if_not_installed("arrow")
    skip_if_not_installed("jsonlite")

    parquet_file <- paste0(tempdir(), "/no_series.parquet")
    on.exit(unlink(parquet_file), add = TRUE)

    csv_file <- system.file("extdata/samples/samples_amazonia.csv",
        package = "sits"
    )
    samples <- .csv_get_samples(csv_file, crs = "EPSG:4326")
    sits_to_parquet(samples, file = parquet_file)

    back <- sits_from_parquet(parquet_file)
    expect_equal(class(back), class(samples))
    expect_equal(colnames(back), colnames(samples))
    expect_false("time_series" %in% colnames(back))
})

test_that("Parquet round trip handles an empty time series", {
    skip_if_not_installed("arrow")
    skip_if_not_installed("jsonlite")

    parquet_file <- paste0(tempdir(), "/empty_series.parquet")
    on.exit(unlink(parquet_file), add = TRUE)

    samples <- samples_modis_ndvi[seq_len(3L), ]
    samples[["time_series"]][[2L]] <- samples[["time_series"]][[2L]][0L, ]
    sits_to_parquet(samples, file = parquet_file)

    back <- sits_from_parquet(parquet_file)
    expect_equal(nrow(back), 3L)
    expect_equal(nrow(back[["time_series"]][[2L]]), 0L)
    expect_equal(nrow(back[["time_series"]][[1L]]), 12L)
})

test_that("Parquet file is readable as a point layer", {
    skip_if_not_installed("arrow")
    skip_if_not_installed("jsonlite")

    parquet_file <- paste0(tempdir(), "/geo.parquet")
    on.exit(unlink(parquet_file), add = TRUE)

    sits_to_parquet(samples_modis_ndvi, file = parquet_file)
    metadata <- arrow::ParquetFileReader$create(parquet_file)$GetSchema()
    expect_true(all(c("sits", "geo") %in% names(metadata$metadata)))

    geo <- jsonlite::fromJSON(metadata$metadata[["geo"]])
    expect_equal(geo[["primary_column"]], "geometry")
    expect_equal(geo[["columns"]][["geometry"]][["encoding"]], "WKB")
})

test_that("Parquet errors are informative", {
    skip_if_not_installed("arrow")
    skip_if_not_installed("jsonlite")

    expect_error(sits_to_parquet(samples_modis_ndvi, file = "x.csv"))
    expect_error(sits_from_parquet("does_not_exist.parquet"))

    # a file without the sits block comes back flat, with a warning
    plain_file <- paste0(tempdir(), "/plain.parquet")
    on.exit(unlink(plain_file), add = TRUE)
    arrow::write_parquet(tibble::tibble(a = seq_len(3L)), plain_file)
    expect_warning(flat <- sits_from_parquet(plain_file))
    expect_equal(colnames(flat), "a")
})
