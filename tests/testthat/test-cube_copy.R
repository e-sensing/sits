test_that("Copy local cube works", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")

    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        multicores = 2,
        progress = FALSE
    )

    cube_local <- sits_cube_copy(
        cube = cube,
        output_dir = tempdir(),
        progress = FALSE
    )

    # Comparing tiles
    expect_equal(nrow(cube), nrow(cube_local))
    bbox_tile <- sits_bbox(cube, TRUE)
    bbox_cube <- sits_bbox(cube_local, TRUE)
    # Comparing bounding boxes
    expect_equal(bbox_tile[["xmin"]], bbox_cube[["xmin"]])
    expect_equal(bbox_tile[["ymin"]], bbox_cube[["ymin"]])
    expect_equal(bbox_tile[["xmax"]], bbox_cube[["xmax"]])
    expect_equal(bbox_tile[["ymax"]], bbox_cube[["ymax"]])
    # Comparing classes
    expect_equal(class(cube), class(cube_local))
    # Comparing timelines
    expect_equal(sits_timeline(cube), sits_timeline(cube_local))
    # Comparing X resolution
    expect_equal(
        cube[["file_info"]][[1]][["xres"]][[1]],
        cube_local[["file_info"]][[1]][["xres"]][[1]]
    )
    # Comparing Y resolution
    expect_equal(
        cube[["file_info"]][[1]][["yres"]][[1]],
        cube_local[["file_info"]][[1]][["yres"]][[1]]
    )
    files <- cube_local$file_info[[1]]$path
    unlink(files)

    cube_local_roi_tr <- sits_cube_copy(
        cube = cube,
        output_dir = tempdir(),
        roi = c(
            lon_min = -55.62248575,
            lat_min = -11.62017052,
            lon_max = -55.60154307,
            lat_max = -11.60790603
        ),
        res = 464,
        multicores = 2,
        progress = FALSE
    )

    # Comparing bounding boxes
    bbox_roi_tr <- sits_bbox(cube_local_roi_tr, TRUE)
    expect_lt(bbox_tile[["xmin"]], bbox_roi_tr[["xmin"]])
    expect_lt(bbox_tile[["ymin"]], bbox_roi_tr[["ymin"]])
    expect_gt(bbox_tile[["xmax"]], bbox_roi_tr[["xmax"]])
    expect_gt(bbox_tile[["ymax"]], bbox_roi_tr[["ymax"]])
    # Comparing classes
    expect_equal(class(cube), class(cube_local_roi_tr))
    # Comparing timelines
    expect_equal(sits_timeline(cube), sits_timeline(cube_local_roi_tr))
    # Comparing X resolution
    expect_lt(
        cube[["file_info"]][[1]][["xres"]][[1]],
        cube_local_roi_tr[["file_info"]][[1]][["xres"]][[1]]
    )
    # Comparing Y resolution
    expect_lt(
        cube[["file_info"]][[1]][["yres"]][[1]],
        cube_local_roi_tr[["file_info"]][[1]][["yres"]][[1]]
    )
    expect_equal(cube_local_roi_tr[["file_info"]][[1]][["xres"]][[1]], 464)
    expect_equal(cube_local_roi_tr[["file_info"]][[1]][["yres"]][[1]], 464)
    files <- cube_local_roi_tr$file_info[[1]]$path
    unlink(files)
})

test_that("Copy remote cube works (full region)", {
    # Create directory
    data_dir <- paste0(tempdir(), "/remote_copy")
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
    # ROI
    roi <- c("lon_min" = -40.76319703, "lat_min" = -4.36079723,
             "lon_max" = -40.67849202, "lat_max" = -4.29126327)
    # Data cube
    cube_s2 <- sits_cube(
        source = "AWS",
        collection = "SENTINEL-2-L2A",
        bands = c("B02", "B8A"),
        roi = roi,
        start_date = "2024-09-15",
        end_date = "2024-09-25",
        progress = FALSE
    )
    # Copy
    cube_s2_local <- sits_cube_copy(
        cube = cube_s2,
        output_dir = data_dir,
        multicores = 2,
        progress = FALSE
    )

    # Tiles
    expect_equal(nrow(cube_s2_local), 2)
    expect_true(all(cube_s2_local[["tile"]] %in% c("24MUA", "24MTA")))

    # Files
    expect_equal(nrow(dplyr::bind_rows(cube_s2_local[["file_info"]])), 8)

    # Extent
    expect_equal(cube_s2[["xmin"]], cube_s2_local[["xmin"]])
    expect_equal(cube_s2[["xmax"]], cube_s2_local[["xmax"]])
    expect_equal(cube_s2[["ymin"]], cube_s2_local[["ymin"]])
    expect_equal(cube_s2[["ymax"]], cube_s2_local[["ymax"]])

    # Delete files
    unlink(data_dir, recursive = TRUE)
})

test_that("Copy remote cube works (full region with resampling)", {
    # Create directory
    data_dir <- paste0(tempdir(), "/remote_copy")
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
    # ROI
    roi <- c("lon_min" = -40.76319703, "lat_min" = -4.36079723,
             "lon_max" = -40.67849202, "lat_max" = -4.29126327)
    # Data cube
    cube_s2 <- sits_cube(
        source = "AWS",
        collection = "SENTINEL-2-L2A",
        bands = c("B02", "B8A"),
        roi = roi,
        start_date = "2024-09-15",
        end_date = "2024-09-25",
        progress = FALSE
    )

    cube_s2_local <- sits_cube_copy(
        cube = cube_s2,
        output_dir = data_dir,
        res = 540,
        multicores = 2,
        progress = FALSE
    )

    # Tiles
    expect_equal(nrow(cube_s2_local), 2)
    expect_true(all(cube_s2_local[["tile"]] %in% c("24MUA", "24MTA")))

    # Files
    expect_equal(nrow(dplyr::bind_rows(cube_s2_local[["file_info"]])), 8)

    # Extent
    expect_equal(cube_s2[["xmin"]], cube_s2_local[["xmin"]])
    expect_equal(cube_s2[["xmax"]], cube_s2_local[["xmax"]])
    expect_equal(cube_s2[["ymin"]], cube_s2_local[["ymin"]])
    expect_equal(cube_s2[["ymax"]], cube_s2_local[["ymax"]])

    # Delete files
    unlink(data_dir, recursive = TRUE)
})

test_that("Copy remote cube works (specific region with resampling)", {
    # Create directory
    data_dir <- paste0(tempdir(), "/remote_copy")
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
    # ROI
    roi <- c("lon_min" = -40.76319703, "lat_min" = -4.36079723,
             "lon_max" = -40.67849202, "lat_max" = -4.29126327)
    # Data cube
    cube_s2 <- sits_cube(
        source = "AWS",
        collection = "SENTINEL-2-L2A",
        bands = c("B02", "B8A"),
        roi = roi,
        start_date = "2024-09-15",
        end_date = "2024-09-25",
        progress = FALSE
    )
    #  roi without res
    expect_error({
        sits_cube_copy(
            cube = cube_s2,
            output_dir = data_dir,
            multicores = 2,
            roi = roi,
            progress = FALSE
        )
    })
    # Copy with roi + res
    cube_s2_local <- sits_cube_copy(
        cube = cube_s2,
        output_dir = data_dir,
        multicores = 2,
        roi = roi,
        res = 540,
        progress = FALSE
    )
    # Spatial extent
    expect_true(sf::st_within(
        sf::st_union(sits_as_sf(cube_s2_local)),
        sf::st_union(sits_as_sf(cube_s2)),
        sparse = FALSE
    ))

    # Files
    expect_equal(nrow(dplyr::bind_rows(cube_s2_local[["file_info"]])), 8)

    # Spatial resolution
    cube_files <- dplyr::bind_rows(cube_s2_local[["file_info"]])

    expect_equal(unique(cube_files[["xres"]]), 540)
    expect_equal(unique(cube_files[["yres"]]), 540)

    unlink(data_dir, recursive = TRUE)
})

test_that("Copy invalid files", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")

    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        multicores = 2,
        progress = FALSE
    )

    # Editing cube with invalid files
    # (skipping the first line to bypass the cube check and simulate a
    # cube containing invalid files)
    .fi(cube) <- .fi(cube) |>
                    dplyr::mutate(
                        path = ifelse(
                            dplyr::row_number() > 1,
                            paste0(path, "_invalid-file"),
                            path
                        )
                    )


    cube_local <- sits_cube_copy(
        cube = cube,
        output_dir = tempdir(),
        progress = FALSE
    )

    expect_equal(nrow(cube_local), 1)
    expect_equal(length(sits_timeline(cube_local)), 1)

    # Clean
    files <- cube_local[["file_info"]][[1]][["path"]]
    unlink(files)
})
