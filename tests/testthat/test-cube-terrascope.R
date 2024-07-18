test_that("Creating WORLD-COVER-2021 cubes from TERRASCOPE", {
    class_cube <- .try(
        {
            sits_cube(
                source = "TERRASCOPE",
                collection = "WORLD-COVER-2021",
                roi   = c(
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
    r_obj <- .raster_open_rast(class_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(class_cube)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
})
