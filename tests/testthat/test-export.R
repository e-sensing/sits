test_that("sf", {
    # Export a sits tibble to sf
    sf_obj <- sits_as_sf(samples_modis_ndvi)
    # Display the sf object
    expect_equal(
        colnames(sf_obj),
        c("crs", "geometry", "longitude", "latitude",
          "start_date", "end_date", "label", "cube", "time_series")
    )
    expect_true(all(sf::st_geometry_type(sf_obj) == "POINT"))

    # create a data cube from a local set of TIFF files
    # this is a cube with 23 instances and one band ("NDVI")
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir
    )
    sf_cube_obj <- sits_as_sf(cube)
    expect_true(all(c("source", "collection", "satellite", "sensor",
                      "tile", "xmin", "xmax", "ymin", "ymax",
                      "crs", "geometry") %in% colnames(sf_cube_obj)))

    # create output dir
    output_dir <- paste0(tempdir(), "/seg")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
    # Segment the cube
    segments <- sits_segment(
        cube = cube,
        output_dir = output_dir,
        multicores = 1,
        memsize = 6,
        progress = FALSE,
        version = "vt"
    )
    sf_cube_vec_obj <- sits_as_sf(segments)
    expect_true(all(c("source", "collection", "satellite", "sensor",
                      "tile", "xmin", "xmax", "ymin", "ymax",
                      "crs", "geometry") %in% colnames(sf_cube_vec_obj)))

})
test_that("stars", {
    # create a data cube from a local set of TIFF files
    # this is a cube with 23 instances and one band ("NDVI")
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir
    )
    stars_object <- sits_as_stars(cube)
    expect_equal(names(stars_object), "TERRA_MODIS_012010_NDVI_2013-09-14.jp2")
})

test_that("terra", {
    # create a data cube from a local set of TIFF files
    # this is a cube with 23 instances and one band ("NDVI")
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir
    )
    terra_object <- sits_as_terra(cube, date = "2013-09-14")
    expect_true("SpatRaster" %in% class(terra_object))
})
test_that("csv", {
    # save to memory
    csv_samples <- sits_timeseries_to_csv(samples_modis_ndvi)
    expect_equal(ncol(csv_samples), 18)
    expect_true(all(c("NDVI1", "NDVI3", "NDVI10") %in%
                        colnames(csv_samples)))
    csv_file <- file.path(tempdir(), "modis_samples.csv")
    sits_timeseries_to_csv(samples_modis_ndvi, csv_file)
    expect_true(file.exists(csv_file))
})
