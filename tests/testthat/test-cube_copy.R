test_that("Copy local cube works", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")

    cube <- .try(
        sits_cube(
            source = "BDC",
            collection = "MOD13Q1-6.1",
            data_dir = data_dir,
            multicores = 2,
            progress = FALSE
        ),
        default = NULL
    )
    testthat::skip_if(
        purrr::is_null(cube),
        "BDC is not accessible"
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
# ROI
roi <- sits_tiles_to_roi("007003", "BDC_LG_V2")
# Data cube
cube_modis <- .try(
    {
        sits_cube(
            source = "BDC",
            collection = "MOD13Q1-6.1",
            bands = c("NDVI", "EVI"),
            roi = roi,
            start_date = "2024-09-15",
            end_date = "2024-09-30",
            progress = FALSE
        )
    },
    .default = NULL
)
test_that("Copy remote cube works (full region)", {
    testthat::skip_if(
        purrr::is_null(cube_modis),
        "BDC is not accessible"
    )
    # Create directory
    data_dir <- paste0(tempdir(), "/remote_copy")
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

    # Copy
    cube_modis_local <- sits_cube_copy(
        cube = cube_modis,
        output_dir = data_dir,
        multicores = 1,
        progress = FALSE
    )

    # Tiles
    expect_equal(nrow(cube_modis_local), 1)
    expect_true(all(cube_modis_local[["tile"]] %in% c("013009")))

    # Files
    expect_equal(nrow(dplyr::bind_rows(cube_modis_local[["file_info"]])), 4)

    # Delete files
    unlink(data_dir, recursive = TRUE)
})

test_that("Copy remote cube works (full region with resampling)", {
    testthat::skip_if(
        purrr::is_null(cube_modis),
        "BDC is not accessible"
    )
    # Create directory
    data_dir <- paste0(tempdir(), "/remote_copy")
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

    cube_modis_local <- sits_cube_copy(
        cube = cube_modis,
        output_dir = data_dir,
        res = 540,
        multicores = 2,
        progress = FALSE
    )

    # Tiles
    expect_equal(nrow(cube_modis_local), 1)

    # Files
    expect_equal(nrow(dplyr::bind_rows(cube_modis_local[["file_info"]])), 4)

    # Extent
    expect_equal(cube_modis[["xmin"]], cube_modis_local[["xmin"]])
    expect_equal(cube_modis[["xmax"]], cube_modis_local[["xmax"]])
    expect_equal(cube_modis[["ymin"]], cube_modis_local[["ymin"]])
    expect_equal(cube_modis[["ymax"]], cube_modis_local[["ymax"]])

    # Delete files
    unlink(data_dir, recursive = TRUE)
})

test_that("Copy remote cube works (specific region with resampling)", {
    testthat::skip_if(
        purrr::is_null(cube_modis),
        "BDC is not accessible"
    )
    # Create directory
    data_dir1 <- paste0(tempdir(), "/remote_copy_1")
    data_dir2 <- paste0(tempdir(), "/remote_copy_2")
    dir.create(data_dir1, recursive = TRUE, showWarnings = FALSE)
    dir.create(data_dir2, recursive = TRUE, showWarnings = FALSE)
    # ROI
    roi <- c(
        "lon_min" = -40.76319703, "lat_min" = -4.36079723,
        "lon_max" = -40.67849202, "lat_max" = -4.29126327
    )
    # Data cube

    #  roi without res
    cube_modis_local_nores <- sits_cube_copy(
        cube = cube_modis,
        output_dir = data_dir1,
        multicores = 2,
        roi = roi,
        progress = FALSE
    )
    # Copy with roi + res
    cube_modis_local <- sits_cube_copy(
        cube = cube_modis,
        output_dir = data_dir2,
        multicores = 2,
        roi = roi,
        res = 540,
        progress = FALSE
    )
    # Spatial extent
    expect_true(sf::st_within(
        sf::st_union(sits_as_sf(cube_modis_local)),
        sf::st_union(sits_as_sf(cube_modis)),
        sparse = FALSE
    ))

    # Files
    expect_equal(nrow(dplyr::bind_rows(cube_modis_local[["file_info"]])), 4)

    # Spatial resolution
    cube_files <- dplyr::bind_rows(cube_modis_local[["file_info"]])

    expect_equal(unique(cube_files[["xres"]]), 540)
    expect_equal(unique(cube_files[["yres"]]), 540)

    unlink(data_dir1, recursive = TRUE)
    unlink(data_dir2, recursive = TRUE)
})

test_that("Copy local class cube works (naming and resampling)", {
    # Open a local classification (class/derived) cube
    data_dir <- system.file("extdata/raster/classif", package = "sits")
    ro_class <- sits_cube(
        source = "MPC",
        collection = "SENTINEL-2-L2A",
        data_dir = data_dir,
        parse_info = c(
            "X1", "X2", "tile", "start_date", "end_date",
            "band", "version"
        ),
        bands = "class",
        labels = c(
            "1" = "ClearCut_Fire", "2" = "ClearCut_Soil",
            "3" = "ClearCut_Veg", "4" = "Forest"
        ),
        progress = FALSE
    )

    # Copy the class cube reducing the resolution
    out_dir <- file.path(tempdir(), "class_copy")
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    ro_class_local <- sits_cube_copy(
        cube = ro_class,
        res = 100,
        output_dir = out_dir,
        multicores = 1,
        progress = FALSE
    )

    # The copy must remain a class cube with the same labels
    expect_s3_class(ro_class_local, "class_cube")
    expect_equal(sits_labels(ro_class), sits_labels(ro_class_local))

    # The output must follow the derived naming scheme (end date + version)
    # so it can be read back as a results cube. The input version (v1) is
    # preserved in the copy
    out_files <- list.files(out_dir, pattern = "\\.tif$")
    expect_true(all(grepl("_class_v1\\.tif$", out_files)))

    # Resolution was updated
    fi <- dplyr::bind_rows(ro_class_local[["file_info"]])
    expect_equal(unique(fi[["xres"]]), 100)
    expect_equal(unique(fi[["yres"]]), 100)

    # The copy can be read back as a valid class cube
    ro_reread <- sits_cube(
        source = "MPC",
        collection = "SENTINEL-2-L2A",
        data_dir = out_dir,
        bands = "class",
        labels = sits_labels(ro_class),
        version = "v1",
        progress = FALSE
    )

    expect_s3_class(ro_reread, "class_cube")
    expect_equal(nrow(ro_reread), nrow(ro_class))

    unlink(out_dir, recursive = TRUE)
})

