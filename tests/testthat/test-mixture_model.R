test_that("Mixture model tests", {
    # Create a sentinel-2 cube
    s2_cube <- .try({
        sits_cube(
            source = "AWS",
            collection = "SENTINEL-2-L2A",
            tiles = "20LKP",
            bands = c("B02", "B03", "B04", "B8A", "B11", "B12", "CLOUD"),
            start_date = "2019-07-01",
            end_date = "2019-07-30",
            progress = FALSE
        )
    },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(s2_cube),
        message = "AWS is not accessible"
    )
    # Delete files before check
    unlink(list.files(path = tempdir(), pattern = "\\.jp2$", full.names = TRUE))
    unlink(list.files(path = tempdir(), pattern = "\\.tif$", full.names = TRUE))

    # Cube regularization for 16 days and 320 meters
    doc_mode <- Sys.getenv("SITS_DOCUMENTATION_MODE")
    Sys.setenv("SITS_DOCUMENTATION_MODE" = "FALSE")
    expect_warning({
        reg_cube <- sits_regularize(
            cube = s2_cube,
            period = "P16D",
            roi = c(
                lon_min = -65.54870165,
                lat_min = -10.63479162,
                lon_max = -65.07629670,
                lat_max = -10.36046639
            ),
            res = 320,
            multicores = 2,
            output_dir = tempdir(),
            progress = FALSE
        )
    })
    Sys.setenv("SITS_DOCUMENTATION_MODE" = doc_mode)
    # Create the endmembers tibble for cube
    em <- tibble::tribble(
        ~type, ~B02, ~B03, ~B04, ~B8A, ~B11, ~B12,
        "forest", 0.02, 0.0352, 0.0189, 0.28, 0.134, 0.0546,
        "land", 0.04, 0.065, 0.07, 0.36, 0.35, 0.18,
        "water", 0.07, 0.11, 0.14, 0.085, 0.004, 0.0026
    )

    # Generate the mixture model
    mm_rmse <- sits_mixture_model(
        data = reg_cube,
        endmembers = em,
        memsize = 2,
        multicores = 2,
        output_dir = tempdir(),
        rmse_band = TRUE,
        progress = FALSE
    )

    frac_bands <- sits_bands(mm_rmse)

    expect_true(all(c("FOREST", "LAND", "WATER", "RMSE") %in% frac_bands))
    expect_true("raster_cube" %in% class(mm_rmse))
    expect_true(all(sits_timeline(reg_cube) %in% sits_timeline(mm_rmse)))
    expect_true(all(reg_cube[["tiles"]] == mm_rmse[["tiles"]]))

    rast <- .raster_open_rast(mm_rmse$file_info[[1]]$path[[2]])
    expect_true(.raster_nrows(rast) == .tile_nrows(reg_cube))

    # test errors in mixture model
    reg_cube2 <- reg_cube
    class(reg_cube2) <- "derived_cube"
    expect_error(sits_mixture_model(
        data = reg_cube2,
        endmembers = em,
        memsize = 2,
        multicores = 2,
        output_dir = tempdir(),
        rmse_band = TRUE,
        progress = FALSE
    ))

    # Read endmembers from CSV
    write.csv(em, file = paste0(tempdir(), "/mmodel.csv"), row.names = FALSE)
    csv_file <- paste0(tempdir(), "/mmodel.csv")

    reg_cube3 <- reg_cube
    class(reg_cube3) <- "data.frame"
    mm_rmse_csv <- sits_mixture_model(
        data = reg_cube3,
        endmembers = csv_file,
        memsize = 2,
        multicores = 2,
        output_dir = tempdir(),
        rmse_band = TRUE,
        progress = FALSE
    )

    frac_bands <- sits_bands(mm_rmse_csv)

    expect_true(all(c("FOREST", "LAND", "WATER") %in% frac_bands))
    expect_true("raster_cube" %in% class(mm_rmse_csv))
    expect_true(all(sits_timeline(reg_cube) %in% sits_timeline(mm_rmse_csv)))
    expect_true(all(reg_cube[["tiles"]] == mm_rmse_csv[["tiles"]]))
    expect_true(all(file.exists(unlist(mm_rmse_csv$file_info[[1]]$path))))

    rast <- .raster_open_rast(mm_rmse_csv$file_info[[1]]$path[[2]])

    expect_true(.raster_nrows(rast) == .tile_nrows(reg_cube))

    samples <- tibble::tibble(
        longitude = c(-65.39246320, -65.21814581, -65.11511198),
        latitude = c(-10.38223059, -10.43160677, -10.50638970),
        label = c("WATER", "LAND", "FOREST"),
        start_date = "2019-07-03",
        end_date = "2019-07-19"
    )

    ts_bands <- sits_get_data(
        cube = reg_cube,
        samples = samples,
        multicores = 2,
        output_dir = tempdir(),
        progress = FALSE
    )

    ts_em <- sits_mixture_model(
        data = ts_bands,
        endmembers = em,
        multicores = 1,
        rmse_band = TRUE,
        progress = FALSE
    )
    ts_labels <- sits_labels(ts_em)

    frac_labels <- vapply(ts_labels, function(ts_label) {
        ts_filt <- dplyr::filter(ts_em, .data[["label"]] == !!ts_label)
        all(ts_filt[["time_series"]][[1]][[ts_label]] > 0.8)
    }, logical(1))

    expect_true(all(frac_labels))

    ts_em_bands <- sits_get_data(
        cube = mm_rmse_csv,
        samples = samples,
        multicores = 2,
        output_dir = tempdir(),
        progress = FALSE
    )
    expect_equal(
        dplyr::bind_rows(
            ts_em_bands$time_series
        )[, c("FOREST", "LAND", "WATER")],
        dplyr::bind_rows(
            ts_em$time_series
        )[, c("FOREST", "LAND", "WATER")],
        tolerance = 0.01
    )
    unlink(list.files(tempdir(), full.names = TRUE))
})
