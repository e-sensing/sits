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
                progress = FALSE
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
    bbox_22LBL <- sits_mgrs_to_roi("22LBL")

    # retrieve the world cereal map for the chosen roi
    world_cereal_2021 <- sits_cube(
        source = "TERRASCOPE",
        collection = "WORLD-CEREAL-2021",
        roi = bbox_22LBL
    )

    # cut the 3 x 3 degree grid to match the MGRS tile 22LBL
    world_cereal_2021_20LBL <- sits_cube_copy(
        cube = world_cereal_2021,
        roi = bbox_22LBL,
        multicores = 6,
        output_dir = tempdir()
    )
    roi_wc <- sits_bbox(world_cereal_2021_20LBL)[,1:4]
    roi_20LBL <- .bbox(bbox_22LBL, as_crs = "EPSG:4326")[,1:4]
    expect_equal(roi_wc[["xmin"]], roi_20LBL[["xmin"]], tolerance = 0.001)
    sumwc <- summary(world_cereal_2021_20LBL)
    expect_true(all(sumwc[["class"]] %in% c("Non_Cropland", "Cropland")))
    expect_true(all(sumwc[["value"]] %in% c("0", "100")))
})
