test_that("Mixture model tests", {

    # Create a sentinel-2 cube
    s2_cube <- sits_cube(
        source = "AWS",
        collection = "SENTINEL-S2-L2A-COGS",
        tiles = "20LKP",
        bands = c("B02", "B03", "B04", "B8A", "B11", "B12", "CLOUD"),
        start_date = "2019-07-01",
        end_date = "2019-07-30"
    )

    # Delete files before check
    unlink(list.files(path = tempdir(), pattern = "\\.jp2$", full.names = T))
    unlink(list.files(path = tempdir(), pattern = "\\.tif$", full.names = T))

    # Cube regularization for 16 days and 320 meters
    reg_cube <- sits_regularize(
        cube = s2_cube,
        period = "P16D",
        roi = c(lon_min = -65.54870165,
                lat_min = -10.63479162,
                lon_max = -65.07629670,
                lat_max = -10.36046639),
        res = 320,
        multicores = 2,
        output_dir = tempdir()
    )

    # Create the endmembers tibble
    em <- tibble::tribble(
        ~type, ~B02, ~B03, ~B04, ~B8A, ~B11, ~B12,
        "forest",  200,  352,  189, 2800, 1340,  546,
        "land",  400,  650,  700, 3600, 3500, 1800,
        "water",  700, 1100, 1400,  850,   40,   26,
    )

    # Generate the mixture model
    mm_rmse <- sits_mixture_model(
        cube = reg_cube,
        endmembers = em,
        memsize = 2,
        multicores = 1,
        output_dir = tempdir(),
        rmse_band = TRUE
    )

    frac_bands <- sits_bands(mm_rmse)

    expect_true(all(c("FOREST", "LAND", "WATER", "RMSE") %in% frac_bands))
    expect_true("raster_cube" %in% class(mm_rmse))
    expect_true(all(sits_timeline(reg_cube) %in% sits_timeline(mm_rmse)))
    expect_true(all(reg_cube[["tiles"]] == mm_rmse[["tiles"]]))
    expect_true(all(file.exists(unlist(mm_rmse$file_info[[1]]$path))))

    r_obj <- .raster_open_rast(mm_rmse$file_info[[1]]$path[[2]])

    expect_true(.raster_nrows(r_obj) == .cube_size(reg_cube)[["nrows"]])

    # Generate the mixture model
    mm <- sits_mixture_model(
        cube = reg_cube,
        endmembers = em,
        memsize = 2,
        multicores = 2,
        output_dir = tempdir(),
        rmse_band = FALSE
    )

    frac_bands <- sits_bands(mm)

    expect_true(all(c("FOREST", "LAND", "WATER") %in% frac_bands))
    expect_true("raster_cube" %in% class(mm))
    expect_true(all(sits_timeline(reg_cube) %in% sits_timeline(mm)))
    expect_true(all(reg_cube[["tiles"]] == mm_rmse[["tiles"]]))
    expect_true(all(file.exists(unlist(mm$file_info[[1]]$path))))

    r_obj <- .raster_open_rast(mm$file_info[[1]]$path[[2]])

    expect_true(.raster_nrows(r_obj) == .cube_size(reg_cube)[["nrows"]])
    unlink(list.files(tempdir(), full.names = TRUE))

})
