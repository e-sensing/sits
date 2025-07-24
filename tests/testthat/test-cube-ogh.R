test_that("Access to global Landsat data from OpenGeoHub", {
    roi <- c(
        lon_min = 7.54, lat_min = 46.73,
        lon_max = 7.65, lat_max = 46.77
    )
    l8_cube_ogh <- .try(
        {
            sits_cube(
                source = "OGH",
                collection = "LANDSAT-GLAD-2M",
                bands = c("BLUE"),
                roi = roi,
                start_date = as.Date("2018-01-01"),
                end_date = as.Date("2018-12-01"),
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(purrr::is_null(l8_cube_ogh), "OpenGeoHub is not accessible")

    # Assert cube properties
    expect_true(nrow(l8_cube_ogh) == 1)
    expect_true(length(sits_timeline(l8_cube_ogh)) == 6)
    expect_true(l8_cube_ogh[["tile"]] == "NoTilingSystem")
    expect_equal(sits_bands(l8_cube_ogh), c("BLUE"))

    l8_cube_ogh_2 <- .try(
        {
            sits_cube(
                source = "OGH",
                collection = "LANDSAT-GLAD-2M",
                bands = c("BLUE", "GREEN", "RED"),
                roi = roi,
                start_date = as.Date("2005-01-01"),
                end_date = as.Date("2005-06-01"),
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(purrr::is_null(l8_cube_ogh_2), "OpenGeoHub is not accessible")

    # Get cube timeline
    cube_timeline <- sits_timeline(l8_cube_ogh_2)

    # Assert cube properties
    expect_equal(sits_bands(l8_cube_ogh_2), c("BLUE", "GREEN", "RED"))
    expect_true(length(cube_timeline) == 3)
    expect_true(
        all("2005-01-01" <= cube_timeline) && all(cube_timeline <= "2005-05-01")
    )
})

test_that("Copy global Landsat data from OpenGeoHub", {
    # Define roi
    roi <- sits_tiles_to_roi("22LBL")

    # Try to load cube
    l8_cube_ogh <- .try(
        {
            sits_cube(
                source = "OGH",
                collection = "LANDSAT-GLAD-2M",
                bands = c("BLUE"),
                roi = roi,
                crs = "EPSG:4326",
                start_date = as.Date("2018-01-01"),
                end_date = as.Date("2018-03-01"),
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(l8_cube_ogh), "OpenGeoHub is not accessible")

    # Create output directory
    dir_images <- paste0(tempdir(), "/ogh_copy/")
    if (!dir.exists(dir_images)) {
        suppressWarnings(dir.create(dir_images))
    }

    # Regularize
    l8_cube_ogh_copy <- sits_cube_copy(
        cube = l8_cube_ogh,
        output_dir = dir_images,
        multicores = 2,
        progress = FALSE,
        roi = roi
    )

    expect_true(
        length(sits_timeline(l8_cube_ogh_copy)) == 1
    )
    expect_true(
        length(l8_cube_ogh_copy[["tile"]]) == 1
    )
    expect_true(
        l8_cube_ogh_copy[["tile"]] == "NoTilingSystem"
    )
    expect_equal(
        sits_bands(l8_cube_ogh_copy), c("BLUE")
    )

    # Delete previous directory
    unlink(dir_images, recursive = TRUE)
})

test_that("Regularize global Landsat data from OpenGeoHub", {
    # Define roi
    roi <- sits_tiles_to_roi("22LBL")

    # Try to load cube
    l8_cube_ogh <- .try(
        {
            sits_cube(
                source = "OGH",
                collection = "LANDSAT-GLAD-2M",
                bands = c("BLUE", "SWIR2"),
                roi = roi,
                crs = "EPSG:4326",
                start_date = as.Date("2018-01-01"),
                end_date = as.Date("2018-05-01"),
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(l8_cube_ogh), "OpenGeoHub is not accessible")

    # Create output directory
    dir_images <- paste0(tempdir(), "/ogh_regularization/")
    if (!dir.exists(dir_images)) {
        suppressWarnings(dir.create(dir_images))
    }

    # Regularize
    l8_cube_ogh_reg <- suppressWarnings(sits_regularize(
        cube = l8_cube_ogh,
        output_dir = dir_images,
        res = 240,
        period = "P2M",
        multicores = 2,
        progress = FALSE,
        tiles = "22LBL"
    ))

    expect_true(
        length(sits_timeline(l8_cube_ogh_reg)) == 2
    )
    expect_true(
        length(l8_cube_ogh_reg[["tile"]]) == 1
    )
    expect_equal(
        sits_bands(l8_cube_ogh_reg), c("BLUE", "SWIR2")
    )

    # Delete previous directory
    unlink(dir_images, recursive = TRUE)
})
