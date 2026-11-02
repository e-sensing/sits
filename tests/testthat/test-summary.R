test_that("sits summary", {
    sum <- summary(samples_modis_ndvi)
    expect_equal(sum$label, c("Cerrado", "Forest", "Pasture", "Soy_Corn"))
    expect_equal(sum$count, c(379, 131, 344, 364))
    sum1 <- suppressWarnings(sits_labels_summary(samples_modis_ndvi))
    expect_equal(sum1$label, c("Cerrado", "Forest", "Pasture", "Soy_Corn"))
    expect_equal(sum1$count, c(379, 131, 344, 364))
})

test_that("summary cube", {
    # create a data cube from local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    sum <- capture.output(summary(cube))
    expect_true(grepl("MODIS", sum[1]))
    expect_true(grepl("Median", sum[4]))
})

test_that("summary sits accuracy", {
    data(cerrado_2classes)
    # split training and test data
    train_data <- sits_sample(cerrado_2classes, frac = 0.5)
    test_data <- sits_sample(cerrado_2classes, frac = 0.5)
    # train a random forest model
    rfor_model <- sits_train(train_data, sits_rfor())
    # classify test data
    points_class <- sits_classify(
        data = test_data,
        ml_model = rfor_model,
        progress = FALSE
    )
    # measure accuracy
    acc <- sits_accuracy(points_class)
    sum <- capture.output(summary(acc))
    expect_true(grepl("Accuracy", sum[2]))
    expect_true(grepl("Kappa", sum[4]))
})

test_that("summary sits area accuracy", {
    # create a data cube from local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    sum_cube <- capture.output(suppressWarnings(summary(cube)))
    expect_true(grepl("TERRA", sum_cube[1]))

    # create a random forest model
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
    # classify a data cube
    probs_cube <- sits_classify(
        data = cube,
        ml_model = rfor_model,
        output_dir = tempdir(),
        progress = FALSE
    )
    sum_probs <- capture.output(suppressWarnings(summary(probs_cube)))
    expect_true(any(grepl("Min", sum_probs)))

    # get the variance cube
    variance_cube <- sits_variance(
        probs_cube,
        output_dir = tempdir(),
        progress = FALSE
    )
    sum_var <- capture.output(suppressWarnings(summary(variance_cube)))
    expect_true(any(grepl("80%", sum_var)))

    # label the probability cube
    label_cube <- sits_label_classification(
        probs_cube,
        output_dir = tempdir(),
        progress = FALSE
    )
    sum_label <- capture.output(suppressWarnings(summary(label_cube)))
    expect_true(any(grepl("area", sum_label)))

    # obtain the ground truth for accuracy assessment
    ground_truth <- system.file("extdata/samples/samples_sinop_crop.csv",
        package = "sits"
    )
    # make accuracy assessment
    as <- sits_accuracy(label_cube, validation = ground_truth)
    sum_as <- capture.output(summary(as))
    expect_true(grepl("Accuracy", sum_as[2]))
    expect_true(grepl("Mapped", sum_as[11]))
    expect_true(grepl("Cerrado", sum_as[13]))
})

test_that("summary BDC cube", {
    tiles <- c("007004", "007005")
    start_date <- "2022-05-01"
    end_date <- "2022-08-29"
    bands <- c("NDVI", "EVI", "B13", "B14", "B15", "B16", "CLOUD")
    # create a raster cube file from BDC
    cbers_cube_8d <- .try(
        {
            sits_cube(
                source = "BDC",
                collection = "CBERS-WFI-8D",
                tiles = tiles,
                start_date = start_date,
                end_date = end_date,
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(purrr::is_null(cbers_cube_8d),
        message = "BDC cube CBERS-WFI-8D is not accessible"
    )
    sum2 <- capture.output(summary(cbers_cube_8d, tile = "007004"))
    expect_true(grepl("007004", sum2[4]))
    expect_true(grepl("007004", sum2[48]))
})
test_that("summary of a variance cube does not depend on multicores", {
    # three labels of random variance, 4096 x 4096 pixels in blocks of 256
    data_dir <- file.path(tempdir(), "variance_random")
    dir.create(data_dir, showWarnings = FALSE)
    set.seed(21L)
    r <- terra::rast(
        nrows = 4096L, ncols = 4096L, nlyrs = 3L, xmin = 0, xmax = 122880,
        ymin = 0, ymax = 122880, crs = "EPSG:32723"
    )
    terra::values(r) <- sample.int(10000L, terra::ncell(r) * 3L, TRUE)
    terra::writeRaster(
        r,
        file.path(
            data_dir,
            "LANDSAT_OLI_000000_2020-01-01_2020-12-31_variance_v1.tif"
        ),
        datatype = "INT2U", overwrite = TRUE,
        gdal = c("TILED=YES", "BLOCKXSIZE=256", "BLOCKYSIZE=256")
    )
    cube <- sits_cube(
        source = "BDC", collection = "LANDSAT-OLI-16D", data_dir = data_dir,
        bands = "variance", version = "v1",
        labels = c("1" = "A", "2" = "B", "3" = "C"),
        parse_info = c("X1", "X2", "tile", "start_date", "end_date",
                       "band", "version"),
        progress = FALSE
    )
    run <- function(multicores, memsize) {
        set.seed(5L)
        suppressMessages(summary(
            cube, sample_size = 10000L, multicores = multicores,
            memsize = memsize
        ))
    }
    a <- run(multicores = 2L, memsize = 1L)
    expect_identical(run(multicores = 2L, memsize = 1L), a)
    expect_identical(run(multicores = 1L, memsize = 1L), a)
    expect_identical(run(multicores = 1L, memsize = 4L), a)
    unlink(data_dir, recursive = TRUE)
})
test_that("summary of a variance cube with seed leaves the global state", {
    # three labels of random variance, 512 x 512 pixels in blocks of 256
    data_dir <- file.path(tempdir(), "variance_seed")
    dir.create(data_dir, showWarnings = FALSE)
    set.seed(23L)
    r <- terra::rast(
        nrows = 512L, ncols = 512L, nlyrs = 3L, xmin = 0, xmax = 15360,
        ymin = 0, ymax = 15360, crs = "EPSG:32723"
    )
    terra::values(r) <- sample.int(10000L, terra::ncell(r) * 3L, TRUE)
    terra::writeRaster(
        r,
        file.path(
            data_dir,
            "LANDSAT_OLI_000000_2020-01-01_2020-12-31_variance_v1.tif"
        ),
        datatype = "INT2U", overwrite = TRUE,
        gdal = c("TILED=YES", "BLOCKXSIZE=256", "BLOCKYSIZE=256")
    )
    cube <- sits_cube(
        source = "BDC", collection = "LANDSAT-OLI-16D", data_dir = data_dir,
        bands = "variance", version = "v1",
        labels = c("1" = "A", "2" = "B", "3" = "C"),
        parse_info = c("X1", "X2", "tile", "start_date", "end_date",
                       "band", "version"),
        progress = FALSE
    )
    # summary has `...`, so an ignored seed shows as a changed result
    run <- function(seed) {
        suppressMessages(summary(
            cube, sample_size = 1000L, multicores = 1L, memsize = 1L,
            seed = seed
        ))
    }
    set.seed(1L, kind = "Mersenne-Twister")
    before <- get(".Random.seed", envir = globalenv())
    kind <- RNGkind()
    a <- run(seed = 10L)
    expect_identical(get(".Random.seed", envir = globalenv()), before)
    expect_identical(RNGkind(), kind)
    set.seed(2L)
    expect_identical(run(seed = 10L), a)
    expect_false(identical(run(seed = 11L), a))
    unlink(data_dir, recursive = TRUE)
})
