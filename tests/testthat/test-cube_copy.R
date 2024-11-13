test_that("Downloading entire images from local cubes", {
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
