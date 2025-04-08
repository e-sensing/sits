test_that("Creating Sentinel cubes from AWS", {
    s2_cube <- .try(
        {
            sits_cube(
                source = "AWS",
                collection = "SENTINEL-2-L2A",
                tiles = c("20LKP", "20LLP"),
                bands = c("B05", "CLOUD"),
                start_date = as.Date("2018-07-18"),
                end_date = as.Date("2018-08-23"),
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(
        purrr::is_null(s2_cube),
        "AWS is not accessible"
    )
    expect_true(all(sits_bands(s2_cube) %in% c("B05", "CLOUD")))
    r <- .raster_open_rast(.tile_path(s2_cube))
    expect_equal(s2_cube$xmax[[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(s2_cube$xmin[[1]], .raster_xmin(r), tolerance = 1)
    s2_cube_s2a <- .try(
        {
            sits_cube(
                source = "AWS",
                collection = "SENTINEL-2-L2A",
                tiles = "20LKP",
                bands = c("B05", "CLOUD"),
                start_date = as.Date("2018-07-18"),
                end_date = as.Date("2018-08-23"),
                progress = FALSE,
                platform = "SENTINEL-2A"
            )
        },
        .default = NULL
    )
    n_images_1 <- nrow(s2_cube$file_info[[1]])
    n_images_2 <- nrow(s2_cube_s2a$file_info[[1]])
    expect_true(n_images_2 < n_images_1)
})

test_that("Creating LANDSAT cubes from AWS with ROI", {
    roi <- c(
        lon_min = -47.50, lat_min = -15.80,
        lon_max = -47.30, lat_max = -15.50026
    )
    l8_cube_aws <- .try(
        {
            sits_cube(
                source = "AWS",
                collection = "LANDSAT-C2-L2",
                roi = roi,
                bands = c("NIR08", "CLOUD"),
                start_date = as.Date("2022-07-18"),
                end_date = as.Date("2022-08-23"),
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(purrr::is_null(l8_cube_aws), "AWS is not accessible")
    expect_true(all(sits_bands(l8_cube_aws) %in% c("NIR08", "CLOUD")))
    expect_equal(nrow(l8_cube_aws), 1)
    bbox_cube <- sits_bbox(l8_cube_aws, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(l8_cube_aws), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    rast <- .raster_open_rast(l8_cube_aws$file_info[[1]]$path[1])
    tile_nrows <- .tile_nrows(l8_cube_aws)[[1]]
    expect_true(.raster_nrows(rast) == tile_nrows)

    l8_cube_aws_l8 <- .try(
        {
            sits_cube(
                source = "AWS",
                collection = "LANDSAT-C2-L2",
                roi = roi,
                bands = c("NIR08", "CLOUD"),
                start_date = as.Date("2022-07-18"),
                end_date = as.Date("2022-08-23"),
                progress = FALSE,
                platform = "LANDSAT-8"
            )
        },
        .default = NULL
    )
    num_files_1 <- nrow(l8_cube_aws$file_info[[1]])
    num_files_2 <- nrow(l8_cube_aws_l8$file_info[[1]])
    expect_true(num_files_2 < num_files_1)
})
