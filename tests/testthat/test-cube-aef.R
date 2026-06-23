test_that("AlphaEarth grid system", {
    # define ROI
    roi <- c(
        lon_min = -47.9,
        lat_min = -15.9,
        lon_max = -47.8,
        lat_max = -15.8
    )

    # ROI -> tiles
    tiles <- sits_roi_to_tiles(roi, grid_system = "ALPHAEARTH")

    expect_s3_class(tiles, "sf")
    expect_true("23S-2-100" %in% tiles[["tile_id"]])

    # tiles -> ROI
    bbox <- sits_tiles_to_roi(tiles[["tile_id"]], grid_system = "ALPHAEARTH")

    expect_true(all(c("xmin", "ymin", "xmax", "ymax") %in% names(bbox)))
    expect_true(bbox[["xmin"]] <= roi[["lon_min"]])
    expect_true(bbox[["xmax"]] >= roi[["lon_max"]])
})

test_that("Creating an AlphaEarth cube", {
    # define cube
    cube <- .try(
        {
            sits_cube(
                source     = "GOOGLE",
                collection = "ALPHAEARTH",
                roi = c(
                    lon_min = -47.9, lat_min = -15.9,
                    lon_max = -47.8, lat_max = -15.8
                ),
                start_date = "2021",
                end_date   = "2022",
                bands      = c("A00", "A04"),
                progress   = FALSE
            )
        },
        .default = NULL
    )

    # skip if cube is null
    testthat::skip_if(purrr::is_null(cube),
        message = "AlphaEarth is not accessible"
    )

    # check cube
    expect_s3_class(cube, "alphaearth_cube")
    expect_true(all(sits_bands(cube) %in% c("A00", "A04")))
    expect_equal(as.character(sits_timeline(cube)),
        c("2021-01-01", "2022-01-01")
    )

    # get file_info
    fi <- .fi(cube)

    # check file_info
    expect_equal(nrow(fi), 4L)
    expect_true(all(fi[["xres"]] == 10 & fi[["yres"]] == 10))
    expect_true(all(fi[["nrows"]] == 8192L & fi[["ncols"]] == 8192L))

    # the path must open and have one band
    rast <- .raster_open_rast(fi[["path"]][[1L]])

    expect_equal(.raster_nlayers(rast), 1L)
    expect_equal(.raster_nrows(rast), .tile_nrows(cube)[[1L]])
})
