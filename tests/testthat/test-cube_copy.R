test_that("Downloading and cropping cubes from BDC", {
    cbers_cube <- tryCatch(
        {
            sits_cube(
                source = "BDC",
                collection = "CBERS-WFI-16D",
                tiles = c("007004", "007005"),
                bands = c("B15", "CLOUD"),
                start_date = "2018-01-01",
                end_date = "2018-01-12",
                progress = FALSE
            )
        },
        error = function(e) {
            return(NULL)
        }
    )
    testthat::skip_if(
        purrr::is_null(cbers_cube),
        "BDC is not accessible"
    )
    roi_xy <- c(
        xmin = 5800000,
        xmax = 5900000,
        ymin = 9600000,
        ymax = 9700000
    )

    cube_local_roi <- sits_cube_copy(
        cube = cbers_cube,
        output_dir = tempdir(),
        roi = roi_xy,
        multicores = 1,
        progress = FALSE
    )
    # Recovery
    Sys.setenv("SITS_DOCUMENTATION_MODE" = "FALSE")
    expect_message(
        sits_cube_copy(
            cube = cbers_cube,
            output_dir = tempdir(),
            roi = roi_xy,
            multicores = 1,
            progress = FALSE
        )
    )
    # Comparing tiles
    expect_true(nrow(cbers_cube) >= nrow(cube_local_roi))
    bbox_tile <- sits_bbox(cbers_cube)
    bbox_crop <- sits_bbox(cube_local_roi)
    # Comparing bounding boxes
    expect_lt(bbox_tile[["xmin"]], bbox_crop[["xmin"]])
    expect_lt(bbox_tile[["ymin"]], bbox_crop[["ymin"]])
    expect_gt(bbox_tile[["xmax"]], bbox_crop[["xmax"]])
    expect_gt(bbox_tile[["ymax"]], bbox_crop[["ymax"]])
    # Comparing classes
    expect_equal(class(cbers_cube), class(cube_local_roi))
    # Comparing timelines
    expect_equal(sits_timeline(cbers_cube), sits_timeline(cube_local_roi))
    # Comparing X resolution
    expect_equal(
        cbers_cube[["file_info"]][[1]][["xres"]][[1]],
        cube_local_roi[["file_info"]][[1]][["xres"]][[1]]
    )
    # Comparing Y resolution
    expect_equal(
        cbers_cube[["file_info"]][[1]][["yres"]][[1]],
        cube_local_roi[["file_info"]][[1]][["yres"]][[1]]
    )
    files <- cube_local_roi$file_info[[1]]$path
    unlink(files)

    roi_ll <- .roi_as_sf(roi_xy,
        default_crs = cbers_cube$crs[[1]],
        as_crs = 4326
    )

    cube_local_roi_ll <- sits_cube_copy(
        cube = cbers_cube,
        output_dir = tempdir(),
        roi = roi_ll,
        multicores = 1,
        progress = FALSE
    )
    # Comparing tiles
    expect_true(nrow(cbers_cube) >= nrow(cube_local_roi_ll))
    bbox_tile <- sits_bbox(cbers_cube)
    bbox_crop <- sits_bbox(cube_local_roi_ll)
    # Comparing bounding boxes
    expect_lt(bbox_tile[["xmin"]], bbox_crop[["xmin"]])
    expect_lt(bbox_tile[["ymin"]], bbox_crop[["ymin"]])
    expect_gt(bbox_tile[["xmax"]], bbox_crop[["xmax"]])
    expect_gt(bbox_tile[["ymax"]], bbox_crop[["ymax"]])
    # Comparing classes
    expect_equal(class(cbers_cube), class(cube_local_roi_ll))
    # Comparing timelines
    expect_equal(sits_timeline(cbers_cube), sits_timeline(cube_local_roi_ll))
    # Comparing X resolution
    expect_equal(
        cbers_cube[["file_info"]][[1]][["xres"]][[1]],
        cube_local_roi_ll[["file_info"]][[1]][["xres"]][[1]]
    )
    # Comparing Y resolution
    expect_equal(
        cbers_cube[["file_info"]][[1]][["yres"]][[1]],
        cube_local_roi_ll[["file_info"]][[1]][["yres"]][[1]]
    )
    files <- cube_local_roi_ll$file_info[[1]]$path
    unlink(files)

    bbox_ll <- .bbox(roi_ll)

    roi_ll_bbox <- c(
        "lon_min" = bbox_ll[["xmin"]],
        "lon_max" = bbox_ll[["xmax"]],
        "lat_min" = bbox_ll[["ymin"]],
        "lat_max" = bbox_ll[["ymax"]]
    )

    cube_local_roi_tr <- sits_cube_copy(
        cube = cbers_cube,
        output_dir = tempdir(),
        res = 128,
        roi = roi_ll_bbox,
        multicores = 1,
        progress = FALSE
    )

    # Comparing tiles
    expect_true(nrow(cbers_cube) >= nrow(cube_local_roi_tr))
    # Comparing bounding boxes
    bbox_roi_tr <- sits_bbox(cube_local_roi_tr)
    expect_lt(bbox_tile[["xmin"]], bbox_roi_tr[["xmin"]])
    expect_lt(bbox_tile[["ymin"]], bbox_roi_tr[["ymin"]])
    expect_gt(bbox_tile[["xmax"]], bbox_roi_tr[["xmax"]])
    expect_gt(bbox_tile[["ymax"]], bbox_roi_tr[["ymax"]])
    # Comparing classes
    expect_equal(class(cbers_cube), class(cube_local_roi_tr))
    # Comparing timelines
    expect_equal(sits_timeline(cbers_cube), sits_timeline(cube_local_roi_tr))
    # Comparing X resolution
    expect_lt(
        cbers_cube[["file_info"]][[1]][["xres"]][[1]],
        cube_local_roi_tr[["file_info"]][[1]][["xres"]][[1]]
    )
    # Comparing Y resolution
    expect_lt(
        cbers_cube[["file_info"]][[1]][["yres"]][[1]],
        cube_local_roi_tr[["file_info"]][[1]][["yres"]][[1]]
    )
    files <- cube_local_roi_tr$file_info[[1]]$path
    unlink(files)
})

test_that("Downloading entire images from local cubes", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")

    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
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
