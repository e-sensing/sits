test_that("Sample", {
    data(cerrado_2classes)

    data <- sits_sample(cerrado_2classes, frac = 0.1)
    expect_true(nrow(dplyr::filter(data, label == "Cerrado")) == 40)
    expect_true(nrow(dplyr::filter(data, label == "Pasture")) == 34)

    data2 <- sits_sample(cerrado_2classes, frac = 1.3, oversample = TRUE)
    expect_true(nrow(data2) > nrow(cerrado_2classes))
})

test_that("Sample reduce imbalance", {
    # print the labels summary for a sample set
    sum_ori_samples <- summary(samples_modis_ndvi)
    # reduce the sample imbalance
    new_samples <- sits_reduce_imbalance(samples_modis_ndvi,
        n_samples_over = 200, n_samples_under = 200,
        multicores = 1
    )
    # print the labels summary for the rebalanced set
    sum_new_samples <- summary(new_samples)
    expect_true(nrow(new_samples) < nrow(samples_modis_ndvi))
    expect_true(sd(sum_new_samples[["count"]]) < sd(sum_ori_samples[["count"]]))
})

test_that("Sampling design", {
    # create a random forest model
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
    # create a data cube from local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    # classify a data cube
    probs_cube <- sits_classify(
        data = cube, ml_model = rfor_model, output_dir = tempdir(),
        progress = FALSE
    )
    # label the probability cube
    label_cube <- sits_label_classification(
        probs_cube,
        output_dir = tempdir(),
        progress = FALSE
    )
    # estimated UA for classes
    expected_ua <- c(
        Cerrado = 0.75, Forest = 0.9,
        Pasture = 0.8, Soy_Corn = 0.8
    )
    sampling_design <- sits_sampling_design(label_cube, expected_ua,
        alloc_options = c(100)
    )

    expect_true(all(c(
        "prop", "expected_ua", "std_dev", "equal",
        "alloc_100", "alloc_prop"
    )
    %in% colnames(sampling_design)))

    # select samples
    shp_file <- paste0(tempdir(), "/strata.shp")
    overhead <- 1.2
    samples <- sits_stratified_sampling(
        cube = label_cube,
        sampling_design = sampling_design,
        overhead = overhead,
        alloc = "alloc_prop",
        shp_file = shp_file,
        progress = FALSE
    )
    expect_true(file.exists(shp_file))

    sd <- sampling_design[, 5]
    expect_equal(sum(ceiling(sd * overhead)), nrow(samples), tolerance = 10)

    sf_shp <- sf::st_read(shp_file)
    expect_true(all(sf::st_geometry_type(sf_shp) == "POINT"))
})

test_that("Sampling design from vector cube", {
    # create a random forest model
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
    # create a data cube from local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    # create output dir
    output_dir <- paste0(tempdir(), "/seg_sample")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
    # Segment the cube
    segs_cube <- sits_segment(
        cube = cube,
        output_dir = output_dir,
        multicores = 1,
        memsize = 24,
        progress = FALSE,
        version = "vt"
    )
    probs_vector_cube <- sits_classify(
        data = segs_cube,
        ml_model = rfor_model,
        output_dir = output_dir
    )
    # label the probability cube
    label_vec_cube <- sits_label_classification(
        probs_vector_cube,
        output_dir = output_dir,
        label_method = "mean",
        progress = FALSE
    )
    # estimated UA for classes
    expected_ua <- c(
        Cerrado = 0.75, Forest = 0.9,
        Pasture = 0.8, Soy_Corn = 0.8
    )
    sampling_design <- sits_sampling_design(label_vec_cube, expected_ua,
                                            alloc_options = c(100)
    )

    expect_true(all(c(
        "prop", "expected_ua", "std_dev", "equal",
        "alloc_100", "alloc_prop"
    )
    %in% colnames(sampling_design)))

    # select samples
    shp_file <- paste0(tempdir(), "/strata.shp")
    overhead <- 1.2
    samples <- sits_stratified_sampling(
        cube = label_vec_cube,
        sampling_design = sampling_design,
        overhead = overhead,
        alloc = "alloc_prop",
        shp_file = shp_file,
        progress = FALSE
    )
    expect_true(file.exists(shp_file))

    sd <- unlist(sampling_design[, 5], use.names = FALSE)
    expect_equal(sum(ceiling(sd * overhead)), nrow(samples), tolerance = 10)

    sf_shp <- sf::st_read(shp_file)
    expect_true(all(sf::st_geometry_type(sf_shp) == "POINT"))
})
test_that("Sampling design with class cube from STAC", {
    # define roi
    roi <- c(
        "lon_min" = -55.80259, "lon_max" = -55.19900,
        "lat_min" = -11.80208, "lat_max" = -11.49583
    )
    # load cube from stac
    class_cube <- .try(
        {
            sits_cube(
                source     = "TERRASCOPE",
                collection = "WORLD-COVER-2021",
                roi        = roi,
                progress   = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(purrr::is_null(class_cube),
        message = "TERRASCOPE is not accessible"
    )
    # download data
    class_cube <- sits_cube_copy(
        cube       = class_cube,
        roi        = roi,
        output_dir = tempdir(),
        multicores = 2,
        progress   = FALSE
    )
    # create sampling design
    sampling_design <- sits_sampling_design(class_cube)

    expect_true(all(c(
        "prop", "expected_ua", "std_dev", "equal",
        "alloc_100", "alloc_75", "alloc_50", "alloc_prop"
    )
    %in% colnames(sampling_design)))

    # select samples
    shp_file <- paste0(tempdir(), "/strata.shp")
    overhead <- 1.2
    samples <- sits_stratified_sampling(
        cube = class_cube,
        sampling_design = sampling_design,
        overhead = overhead,
        alloc = "alloc_prop",
        shp_file = shp_file,
        progress = FALSE
    )
    expect_true(file.exists(shp_file))

    sd <- unlist(sampling_design[, 5], use.names = FALSE)
    expect_equal(sum(ceiling(sd * overhead)), nrow(samples), tolerance = 10)

    sf_shp <- sf::st_read(shp_file)
    expect_true(all(sf::st_geometry_type(sf_shp) == "POINT"))

    unlink(class_cube$file_info[[1]]$path)
})
test_that("samples_transform",{
    samples_modis_3857 <- .samples_transform(
        samples_modis_ndvi,
        crs = "EPSG:4236",
        as_crs = "EPSG:3857"
    )
    colnames(samples_modis_3857) <- c("X", "Y",
                                      "start_date", "end_date",
                                      "label", "cube", "time_series")
    xmax <- max(samples_modis_3857[["X"]])
    ymax <- max(samples_modis_3857[["Y"]])
    xmin <- min(samples_modis_3857[["X"]])
    ymin <- min(samples_modis_3857[["Y"]])

    x1 <- samples_modis_3857[1, "X"]
    y1 <- samples_modis_3857[1, "Y"]
    expect_true(x1 <= xmax)
    expect_true(y1 <= ymax)
    expect_true(x1 >= xmin)
    expect_true(y1 >= ymin)

})
test_that("stratified sampling does not repeat the draw of each chunk", {
    # one class everywhere, 4096 x 4096 pixels in blocks of 256
    data_dir <- file.path(tempdir(), "strata_uniform")
    dir.create(data_dir, showWarnings = FALSE)
    r <- terra::rast(
        nrows = 4096L, ncols = 4096L, xmin = 0, xmax = 122880,
        ymin = 0, ymax = 122880, crs = "EPSG:32723", vals = 1L
    )
    terra::writeRaster(
        r,
        file.path(
            data_dir,
            "LANDSAT_OLI_000000_2020-01-01_2020-12-31_class_v1.tif"
        ),
        datatype = "INT1U", overwrite = TRUE,
        gdal = c("TILED=YES", "BLOCKXSIZE=256", "BLOCKYSIZE=256")
    )
    cube <- sits_cube(
        source = "BDC", collection = "LANDSAT-OLI-16D", data_dir = data_dir,
        bands = "class", version = "v1", labels = c("1" = "A", "2" = "B"),
        parse_info = c("X1", "X2", "tile", "start_date", "end_date",
                       "band", "version"),
        progress = FALSE
    )
    draw <- function(multicores, memsize, seed) {
        set.seed(seed)
        p <- sits_stratified_sampling(
            cube, samples_per_class = 1000L, multicores = multicores,
            memsize = memsize, progress = FALSE
        )
        xy <- sf::st_coordinates(sf::st_transform(p, 32723))
        rc <- terra::rowColFromCell(r, terra::cellFromXY(r, xy)) - 1L
        rc[order(rc[, 1L], rc[, 2L]), , drop = FALSE]
    }
    # offset of a point inside its 256 x 256 block
    repeats <- function(rc) {
        sum(duplicated(paste(rc[, 1L] %% 256L, rc[, 2L] %% 256L)))
    }
    a <- draw(multicores = 2L, memsize = 1L, seed = 1L)
    expect_identical(nrow(a), 1000L)
    # oracle: uniform draws of as many cells with base R
    set.seed(2L)
    uniform <- replicate(50L, {
        k <- sample.int(4096L * 4096L, 1000L) - 1L
        repeats(cbind(k %/% 4096L, k %% 4096L))
    })
    expect_lte(repeats(a), 2L * max(uniform))
    # the chunks of the draw do not depend on multicores and memsize
    expect_identical(draw(multicores = 1L, memsize = 1L, seed = 1L), a)
    expect_identical(draw(multicores = 1L, memsize = 2L, seed = 1L), a)
    expect_false(identical(draw(multicores = 2L, memsize = 1L, seed = 2L), a))
    unlink(data_dir, recursive = TRUE)
})
