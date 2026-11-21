test_that("Testing texture generation", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    # Create a MODIS cube
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    feature <- sits_select(cube, bands = "NDVI", dates = "2013-09-14")
    dir_images <- paste0(tempdir(), "/images/")
    if (!dir.exists(dir_images)) {
        suppressWarnings(dir.create(dir_images))
    }
    unlink(list.files(dir_images,
        pattern = "\\.tif$",
        full.names = TRUE
    ))
    # Compute the NDVI variance
    class(feature) <- "data.frame"
    texture <- sits_texture(
        cube = feature,
        NDVIVAR = glcm_variance(NDVI),
        window_size = 5,
        multicores = 1,
        output_dir = dir_images
    )
    expect_true(all(sits_bands(texture) %in% c("NDVI", "NDVIVAR")))

    # Compute the NDVI mean
    texture <- sits_texture(
        cube = feature,
        NDVIMEAN = glcm_mean(NDVI),
        window_size = 5,
        multicores = 1,
        output_dir = dir_images
    )
    expect_true(all(sits_bands(texture) %in% c("NDVI", "NDVIMEAN")))

    # Compute the NDVI contrast
    texture <- sits_texture(
        cube = feature,
        NDVICONTRAST = glcm_contrast(NDVI),
        window_size = 5,
        multicores = 1,
        output_dir = dir_images
    )
    expect_true(all(sits_bands(texture) %in% c("NDVI", "NDVICONTRAST")))

    # Compute the NDVI dissimilarity
    texture <- sits_texture(
        cube = feature,
        NDVIDISSIMILARITY = glcm_dissimilarity(NDVI),
        window_size = 5,
        multicores = 1,
        output_dir = dir_images
    )
    expect_true(all(sits_bands(texture) %in% c("NDVI", "NDVIDISSIMILARITY")))

    # Compute the NDVI homogeneity
    texture <- sits_texture(
        cube = feature,
        NDVIHOMOGEINEITY = glcm_homogeneity(NDVI),
        window_size = 5,
        multicores = 1,
        output_dir = dir_images
    )
    expect_true(all(sits_bands(texture) %in% c("NDVI", "NDVIHOMOGEINEITY")))

    # Compute the NDVI energy
    texture <- sits_texture(
        cube = feature,
        NDVIENERGY = glcm_energy(NDVI),
        window_size = 5,
        multicores = 1,
        output_dir = dir_images
    )
    expect_true(all(sits_bands(texture) %in% c("NDVI", "NDVIENERGY")))

    # Compute the NDVI asm
    texture <- sits_texture(
        cube = feature,
        NDVIASM = glcm_asm(NDVI),
        window_size = 5,
        multicores = 1,
        output_dir = dir_images
    )
    expect_true(all(sits_bands(texture) %in% c("NDVI", "NDVIASM")))

    # Compute the NDVI std
    texture <- sits_texture(
        cube = feature,
        NDVISTD = glcm_std(NDVI),
        window_size = 5,
        multicores = 1,
        output_dir = dir_images
    )
    expect_true(all(sits_bands(texture) %in% c("NDVI", "NDVISTD")))

    # Compute the NDVI correlation
    texture <- sits_texture(
        cube = feature,
        NDVICORRELATION = glcm_correlation(NDVI),
        window_size = 5,
        multicores = 1,
        output_dir = dir_images
    )
    expect_true(all(sits_bands(texture) %in% c("NDVI", "NDVICORRELATION")))

    unlink(dir_images, recursive = TRUE)
})
test_that("Texture - four angles", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    out_dir <- file.path(tempdir(), "texture-four-angles")
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    texture <- sits_texture(
        cube = cube,
        NDVICONTRAST = glcm_contrast(NDVI),
        window_size = 3,
        angles = c(0, pi / 4, pi / 2, 3 * pi / 4),
        multicores = 1,
        output_dir = out_dir,
        progress = FALSE
    )
    expect_true("NDVICONTRAST" %in% sits_bands(texture))
    unlink(out_dir, recursive = TRUE)
})
test_that("Texture - logs the start and end of each step", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    out_dir <- file.path(tempdir(), "texture-debug-log")
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    .debug(flag = TRUE, output_dir = out_dir)
    on.exit(.debug(flag = FALSE), add = TRUE)
    sits_texture(
        cube = cube,
        NDVIVAR = glcm_variance(NDVI),
        window_size = 3,
        multicores = 1,
        output_dir = out_dir,
        progress = FALSE
    )
    events <- utils::read.csv(
        .file_log_name(out_dir),
        strip.white = TRUE
    )[["event"]]
    steps <- c(
        "call", "tile", "block_data_read", "block_kernel",
        "block_write", "tile_merge"
    )
    purrr::walk(steps, function(step) {
        n_start <- sum(events == paste0("start_", step))
        expect_gt(n_start, 0L)
        expect_equal(sum(events == paste0("end_", step)), n_start)
    })
    unlink(out_dir, recursive = TRUE)
})
