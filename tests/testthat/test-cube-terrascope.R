test_that("Creating WORLD-COVER-2021 cubes from TERRASCOPE", {
    class_cube <- .try(
        {
            sits_cube(
                source = "TERRASCOPE",
                collection = "WORLD-COVER-2021",
                roi = c(
                    lon_min = 137.15991,
                    lon_max = 138.18467,
                    lat_min = -33.85777,
                    lat_max = -32.56690
                ),
                progress = FALSE,
                crs = "EPSG:4326"
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(class_cube),
        message = "TERRASCOPE is not accessible"
    )

    expect_true(all(sits_bands(class_cube) %in% c("class")))
    expect_equal(nrow(class_cube), 4)
    bbox_cube <- sits_bbox(class_cube, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(class_cube), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    rast <- .raster_open_rast(class_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(class_cube)
    expect_true(.raster_nrows(rast) == cube_nrows)
})

test_that("Creating WORLD-CEREAL-2021 cubes from TERRASCOPE",{

    # get roi for an MGRS tile
    roi_22LBL <- sits_tiles_to_roi("22LBL", grid_system = "MGRS")

    # retrieve the world cereal map for the chosen roi
    world_cereal_2021 <- .try(
        {
            sits_cube(
                source = "TERRASCOPE",
                collection = "WORLD-CEREAL-2021",
                roi = roi_22LBL,
                progress = FALSE,
                crs = "EPSG:4326"
            )
        },
        .default = NULL
    )
    testthat::skip_if(purrr::is_null(world_cereal_2021),
                      message = "TERRASCOPE is not accessible")

    # cut the 3 x 3 degree grid to match the MGRS tile 22LBL
    world_cereal_2021_20LBL <- sits_cube_copy(
        cube = world_cereal_2021,
        roi = roi_22LBL,
        multicores = 6,
        output_dir = tempdir(),
        crs = "EPSG:4326"
    )
    sumwc <- suppressWarnings(summary(world_cereal_2021_20LBL))

    expect_true(all(sumwc[["class"]] %in% c("Non_Cropland", "Cropland")))
    expect_true(all(sumwc[["value"]] %in% c("0", "100")))
})
