test_that("Suggested samples have low confidence, high entropy", {
    # Get uncertaintly cube.
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    set.seed(123)
    rfor_model <- sits_train(samples_modis_ndvi,
        ml_method = sits_rfor()
    )
    output_dir <- paste0(tempdir(), "/al")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
    probs_cube <- sits_classify(
        cube,
        ml_model = rfor_model,
        output_dir = output_dir,
        memsize = 4,
        multicores = 2,
        progress = FALSE
    )
    uncert_cube <- sits_uncertainty(
        probs_cube,
        type = "least",
        output_dir = output_dir,
        progress = FALSE
    )

    # Get sample suggestions.
    samples_df <- suppressWarnings(sits_uncertainty_sampling(
        uncert_cube,
        min_uncert = 0.3,
        n = 100,
        sampling_window = 10,
        multicores = 2,
        memsize = 2
    ))

    expect_true(nrow(samples_df) <= 100)
    expect_true(all(colnames(samples_df) %in% c(
        "longitude", "latitude", "uncertainty",
        "start_date", "end_date",
        "label"
    )))
    expect_true(all(samples_df[["label"]] == "NoClass"))
    expect_true(all(samples_df[["uncertainty"]] >= 0.3))

    unlink(probs_cube$file_info[[1]]$path)
    unlink(uncert_cube$file_info[[1]]$path)
})

test_that("max_uncert parameter works correctly", {
    # Get uncertainty cube.
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    set.seed(123)
    rfor_model <- sits_train(samples_modis_ndvi,
        ml_method = sits_rfor()
    )
    output_dir <- paste0(tempdir(), "/al_max_uncert")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
    probs_cube <- sits_classify(
        cube,
        ml_model = rfor_model,
        output_dir = output_dir,
        memsize = 4,
        multicores = 2,
        progress = FALSE
    )
    uncert_cube <- sits_uncertainty(
        probs_cube,
        type = "least",
        output_dir = output_dir,
        progress = FALSE
    )

    # Test with max_uncert limit
    samples_df <- suppressWarnings(sits_uncertainty_sampling(
        uncert_cube,
        min_uncert = 0.3,
        max_uncert = 0.7,  # Upper bound
        n = 100,
        sampling_window = 10,
        multicores = 2,
        memsize = 2
    ))

    expect_true(nrow(samples_df) <= 100)
    expect_true(all(colnames(samples_df) %in% c(
        "longitude", "latitude", "uncertainty",
        "start_date", "end_date",
        "label"
    )))
    expect_true(all(samples_df[["label"]] == "NoClass"))
    expect_true(all(samples_df[["uncertainty"]] >= 0.3))
    expect_true(all(samples_df[["uncertainty"]] <= 0.7))

    unlink(probs_cube$file_info[[1]]$path)
    unlink(uncert_cube$file_info[[1]]$path)
})

test_that("Increased samples have high confidence, low entropy", {
    # Get uncertaintly cube.
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    out_dir <- tempdir()
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    rfor_model <- sits_train(samples_modis_ndvi,
        ml_method = sits_rfor()
    )
    output_dir <- paste0(tempdir(), "/al_2")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
    probs_cube <- sits_classify(
        cube,
        ml_model = rfor_model,
        output_dir = output_dir,
        memsize = 4, multicores = 2,
        progress = FALSE
    )
    # Get sample suggestions based on high confidence
    samples_df <- suppressWarnings(
        sits_confidence_sampling(
            probs_cube = probs_cube,
            n = 20,
            min_margin = 0.5,
            sampling_window = 10,
            multicores = 2,
            memsize = 2
        )
    )
    doc_mode <- Sys.getenv("SITS_DOCUMENTATION_MODE")
    Sys.setenv("SITS_DOCUMENTATION_MODE" = "FALSE")
    expect_warning(
        sits_confidence_sampling(
            probs_cube = probs_cube,
            n = 60,
            min_margin = 0.5,
            sampling_window = 10,
            multicores = 2,
            memsize = 2
        )
    )
    Sys.setenv("SITS_DOCUMENTATION_MODE" = doc_mode)
    labels <- sits_labels(probs_cube)

    samples_count <- dplyr::count(samples_df, .data[["label"]])
    expect_true(
        nrow(dplyr::filter(samples_count, .data[["n"]] <= 20)) == 4
    )

    expect_true(all(colnames(samples_df) %in% c(
        "longitude", "latitude",
        "start_date", "end_date",
        "label", "confidence"
    )))
    unlink(probs_cube$file_info[[1]]$path)
})

test_that("uncertainty sampling returns correct values at sample coordinates", {
    # Test that the bug with 0-based vs 1-based indexing is fixed
    # Create a small raster with known values
    r <- terra::rast(
        nrows = 100,
        ncols = 100,
        xmin = -6073798,
        xmax = -6014726,
        ymin = -1312333,
        ymax = -1278280,
        crs = "EPSG:6933"
    )
    terra::values(r) <- 1:10000

    # Save the raster
    temp_dir <- tempdir()
    raster_path <- file.path(temp_dir, "test_raster.tif")
    terra::writeRaster(r, raster_path, overwrite = TRUE)

    # Create uncertainty cube
    uncert_cube <- tibble::tibble(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        satellite = "TERRA",
        sensor = "MODIS",
        tile = "test_tile",
        xmin = terra::xmin(r),
        xmax = terra::xmax(r),
        ymin = terra::ymin(r),
        ymax = terra::ymax(r),
        crs = terra::crs(r),
        file_info = list(tibble::tibble(
            band = "entropy",
            start_date = as.Date("2023-01-01"),
            end_date = as.Date("2023-12-31"),
            ncols = terra::ncol(r),
            nrows = terra::nrow(r),
            xres = terra::xres(r),
            yres = terra::yres(r),
            xmin = terra::xmin(r),
            xmax = terra::xmax(r),
            ymin = terra::ymin(r),
            ymax = terra::ymax(r),
            path = raster_path
        )),
        labels = list(c("1", "2", "3", "4"))
    )
    class(uncert_cube) <- c("uncertainty_cube", "derived_cube", "raster_cube",
                             "tbl_df", "tbl", "data.frame")

    # Get samples
    samples_df <- sits_uncertainty_sampling(
        uncert_cube,
        n = 5,
        min_uncert = 0.0,
        max_uncert = 1.0,
        progress = FALSE
    )

    # Verify that uncertainty values match the raster values at the sample coordinates
    all_match <- all(sapply(1:nrow(samples_df), function(i) {
        lon <- samples_df$longitude[i]
        lat <- samples_df$latitude[i]
        uncert <- samples_df$uncertainty[i]

        # Convert lat/lon (WGS84) to raster CRS (EPSG:6933)
        pts_wgs84 <- terra::vect(matrix(c(lon, lat), ncol = 2), type = "points", crs = "EPSG:4326")
        pts_proj <- terra::project(pts_wgs84, terra::crs(r))
        coords <- terra::geom(pts_proj)[, c("x", "y")]

        # Get cell from coordinates
        cell_idx <- terra::cellFromXY(r, matrix(coords, ncol = 2))

        # Get raster value at that cell
        raster_value <- terra::values(r)[cell_idx]

        # Expected uncertainty = raster_value / 10000
        expected_uncert <- raster_value / 10000

        # Check if they match (with small tolerance for floating point)
        abs(uncert - expected_uncert) < 0.001
    }))

    expect_true(all_match)

    # Clean up
    unlink(raster_path)
})
