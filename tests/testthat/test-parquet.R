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
    # the written file must still be usable a further use of the samples
    expect_no_error(sits_predictors(samples))
})

test_that("Parquet keeps the class of sample variants", {
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

test_that("Parquet handles samples without time series", {
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

test_that("Parquet handles an empty time series", {
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

    # a file without the sits block is refused when it is not a sample set
    plain_file <- paste0(tempdir(), "/plain.parquet")
    on.exit(unlink(plain_file), add = TRUE)
    arrow::write_parquet(tibble::tibble(a = seq_len(3L)), plain_file)
    expect_error(suppressWarnings(sits_from_parquet(plain_file)))
})

test_that("Class is inferred when the file has no sits metadata", {
    skip_if_not_installed("arrow")
    skip_if_not_installed("jsonlite")

    # writes a file and strips the metadata blocks from it
    strip <- function(data) {
        written <- paste0(tempdir(), "/with_block.parquet")
        stripped <- paste0(tempdir(), "/no_block.parquet")
        sits_to_parquet(data, written)
        table <- arrow::arrow_table(arrow::read_parquet(written))
        table$metadata <- NULL
        arrow::write_parquet(table, stripped)
        unlink(written)
        stripped
    }

    samples <- samples_modis_ndvi
    expect_warning(back <- sits_from_parquet(strip(samples)))
    expect_equal(class(back), class(samples))
    expect_equal(nrow(back), nrow(samples))
    expect_equal(nrow(back[["time_series"]][[1L]]), 12L)

    # a class of its own, resolved by a column
    cluster <- sits_cluster_dendro(samples, bands = "NDVI")
    expect_warning(back <- sits_from_parquet(strip(cluster)))
    expect_s3_class(back, "sits_cluster")

    # patterns, resolved by having no location and more than one date
    patterns <- sits_patterns(samples)
    expect_warning(back <- sits_from_parquet(strip(patterns)))
    expect_s3_class(back, "patterns")

    # samples without a series keep their class and gain no series
    csv_file <- system.file("extdata/samples/samples_amazonia.csv",
        package = "sits"
    )
    no_series <- .csv_get_samples(csv_file, crs = "EPSG:4326")
    expect_warning(back <- sits_from_parquet(strip(no_series)))
    expect_equal(class(back), class(no_series))
    expect_false("time_series" %in% colnames(back))
})

test_that("A file that is not a sample set is refused", {
    skip_if_not_installed("arrow")
    skip_if_not_installed("jsonlite")

    other <- paste0(tempdir(), "/other.parquet")
    on.exit(unlink(other), add = TRUE)
    arrow::write_parquet(tibble::tibble(a = seq_len(3L)), other)

    expect_error(suppressWarnings(sits_from_parquet(other)))
})

test_that("Predicted with several intervals is written and read back", {
    skip_if_not_installed("arrow")
    skip_if_not_installed("jsonlite")

    parquet_file <- paste0(tempdir(), "/predicted.parquet")
    on.exit(unlink(parquet_file), add = TRUE)

    # a long series against a short model timeline yields one prediction per
    # window, and that cardinality is unrelated to the number of dates
    model <- sits_train(samples_modis_ndvi, sits_rfor(num_trees = 50L))
    long <- sits_select(point_mt_6bands, bands = "NDVI")
    classified <- sits_classify(long, model, multicores = 1L)
    expect_gt(nrow(classified[["predicted"]][[1L]]), 1L)

    sits_to_parquet(classified, file = parquet_file)
    back <- sits_from_parquet(parquet_file)

    expect_equal(class(back), class(classified))
    expect_equal(
        as.data.frame(back[["predicted"]][[1L]]),
        as.data.frame(classified[["predicted"]][[1L]])
    )
})

test_that("Samples can be read from parquet by sits_get_data", {
    skip_if_not_installed("arrow")
    skip_if_not_installed("jsonlite")

    parquet_file <- paste0(tempdir(), "/points.parquet")
    on.exit(unlink(parquet_file), add = TRUE)

    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = system.file("extdata/raster/mod13q1", package = "sits"),
        progress = FALSE
    )
    bbox <- sits_bbox(cube, as_crs = "EPSG:4326")
    timeline <- sits_timeline(cube)
    set.seed(7L)
    points <- tibble::tibble(
        longitude = runif(20L, bbox[["xmin"]] + 0.02, bbox[["xmax"]] - 0.02),
        latitude = runif(20L, bbox[["ymin"]] + 0.02, bbox[["ymax"]] - 0.02),
        start_date = as.Date(timeline[[1L]]),
        end_date = as.Date(timeline[[length(timeline)]]),
        label = rep(c("A", "B"), length.out = 20L)
    )
    class(points) <- c("sits", class(points))
    sits_to_parquet(points, file = parquet_file)

    samples <- sits_get_data(cube, samples = parquet_file,
        multicores = 1L, progress = FALSE
    )
    expect_s3_class(samples, "sits")
    expect_gt(nrow(samples), 0L)
    expect_equal(nrow(samples[["time_series"]][[1L]]), length(timeline))

    # a long table is refused: each row would be taken as its own sample and
    # the same point would be extracted once per date. It takes a file with a
    # series to produce one, since the index column is the signal
    series_file <- paste0(tempdir(), "/series.parquet")
    on.exit(unlink(series_file), add = TRUE)
    sits_to_parquet(samples_modis_ndvi, file = series_file)
    long <- arrow::read_parquet(series_file)
    expect_true("Index" %in% colnames(long))
    expect_error(
        sits_get_data(cube, samples = long, multicores = 1L, progress = FALSE)
    )
})

test_that("The footer alone carries the schema and the sits block", {
    skip_if_not_installed("arrow")
    skip_if_not_installed("jsonlite")

    parquet_file <- paste0(tempdir(), "/footer.parquet")
    on.exit(unlink(parquet_file), add = TRUE)
    sits_to_parquet(samples_modis_ndvi, file = parquet_file)

    full <- arrow::ParquetFileReader$create(parquet_file)

    # the stub the remote path builds, from the tail of the local file
    size <- file.size(parquet_file)
    con <- file(parquet_file, "rb")
    on.exit(close(con), add = TRUE)
    seek(con, size - 8L)
    len <- readBin(con, "integer", size = 4L, endian = "little")
    seek(con, size - len - 8L)
    tail <- readBin(con, "raw", n = len + 8L)
    stub <- paste0(tempdir(), "/footer_stub.parquet")
    on.exit(unlink(stub), add = TRUE)
    writeBin(c(charToRaw("PAR1"), tail), stub)

    # a stub of a few KB answers what a reader of the whole file answers
    expect_lt(file.size(stub), file.size(parquet_file))
    from_stub <- arrow::ParquetFileReader$create(stub)
    expect_equal(from_stub$GetSchema()$names, full$GetSchema()$names)
    expect_equal(from_stub$num_rows, full$num_rows)
    expect_equal(
        .parquet_read_block(from_stub), .parquet_read_block(full)
    )

    # the check runs on the schema, so it never reads a row
    expect_no_error(
        .parquet_check_block(from_stub, .parquet_read_block(from_stub))
    )
    expect_error(.parquet_check_block(from_stub, list(key = "absent")))
})

test_that("Samples are read from an URL", {
    skip_on_cran()
    skip_if_not_installed("arrow")
    skip_if_not_installed("jsonlite")

    url <- paste0(
        "https://huggingface.co/datasets/gilbertocamara/samples_cerrado/",
        "resolve/main/samples_cerrado_2017_2024_pretrain.parquet"
    )
    source <- tryCatch(
        .parquet_check(.parquet_source(url)),
        error = function(e) NULL
    )
    skip_if(purrr::is_null(source), "Hugging Face is not accessible")

    # the size comes from the HEAD, before anything is transferred
    expect_gt(attr(source, "size"), 0.0)

    # only the footer travels, so the file is checked without its data
    footer <- .parquet_footer(source)
    on.exit(.parquet_close(source, footer), add = TRUE)
    expect_lt(file.size(footer), attr(source, "size"))

    reader <- arrow::ParquetFileReader$create(footer)
    block <- .parquet_read_block(reader)
    expect_equal(unlist(block[["class"]])[[1L]], "sits")
    expect_no_error(.parquet_check_block(reader, block))
    expect_gt(reader$num_rows, 0L)

    .parquet_close(source, footer)
    expect_false(file.exists(footer))
})

test_that("An URL that is not a parquet sample set is refused", {
    skip_on_cran()
    skip_if_not_installed("arrow")

    # the extension is checked before any request
    expect_error(sits_from_parquet("https://example.org/samples.csv"))

    # a server that does not answer with byte ranges is refused
    reachable <- tryCatch(
        .head_request("https://example.org/"),
        error = function(e) NULL
    )
    skip_if(purrr::is_null(reachable), "example.org is not accessible")
    expect_error(sits_from_parquet("https://example.org/samples.parquet"))
})

test_that("A footer larger than the file is refused", {
    skip_on_cran()
    skip_if_not_installed("arrow")

    url <- paste0(
        "https://huggingface.co/datasets/gilbertocamara/samples_cerrado/",
        "resolve/main/samples_cerrado_2017_2024_pretrain.parquet"
    )
    footer <- tryCatch(
        .parquet_remote_footer(url, 219218781.0),
        error = function(e) NULL
    )
    skip_if(purrr::is_null(footer), "Hugging Face is not accessible")
    on.exit(unlink(footer), add = TRUE)
    expect_lt(file.size(footer), 219218781.0)

    # a declared size that cannot hold the footer stops the second request
    expect_error(.parquet_remote_footer(url, 1000.0))
})

test_that("A byte count reaches the range header in full", {
    # a large count in scientific notation would be an invalid range
    expect_equal(
        paste0("bytes=-", format(2.0e9, scientific = FALSE, trim = TRUE)),
        "bytes=-2000000000"
    )
})

test_that("A download that fails names the timeout", {
    skip_on_cran()
    skip_if_not_installed("arrow")

    source <- .set_class(
        "https://nope.invalid/x.parquet", "parquet_http", "character"
    )
    expect_error(
        suppressWarnings(.parquet_read(source)),
        regexp = "timeout"
    )
})
