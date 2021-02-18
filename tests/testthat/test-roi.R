test_that("One-year, multicore classification with ROI", {
    samples_2bands <- sits_select(samples_mt_4bands, bands = c("NDVI", "EVI"))

    svm_model <- sits_train(samples_2bands, sits_svm())

    ndvi_file <- c(system.file("extdata/raster/mod13q1/sinop-evi-2014.tif",
                               package = "sits"
    ))

    evi_file <- c(system.file("extdata/raster/mod13q1/sinop-evi-2014.tif",
                              package = "sits"
    ))

    data("timeline_2013_2014")

    sinop <- sits_cube(
        type = "BRICK",
        name = "sinop-2014",
        timeline = timeline_2013_2014,
        satellite = "TERRA",
        sensor = "MODIS",
        bands = c("ndvi", "evi"),
        files = c(ndvi_file, evi_file)
    )

    bbox <- sits_bbox(sinop)
    bbox["xmax"] <- (bbox["xmax"] - bbox["xmin"]) / 2 + bbox["xmin"]
    bbox["ymax"] <- (bbox["ymax"] - bbox["ymin"]) / 2 + bbox["ymin"]

    sinop_probs <- suppressMessages(
        sits_classify(sinop,
                      svm_model,
                      output_dir = tempdir(),
                      roi = bbox,
                      memsize = 4, multicores = 2
        )
    )
    expect_true(all(file.exists(unlist(sinop_probs$file_info[[1]]$path))))
    rc_obj <- suppressWarnings(terra::rast(sinop_probs$file_info[[1]]$path[1]))
    expect_true(terra::nrow(rc_obj) == sinop_probs$nrows)

    bbox_p <- sits_bbox(sinop_probs)
    expect_lte(bbox["xmax"], bbox_p["xmax"])
    expect_lte(bbox["xmin"], bbox_p["xmin"])
    expect_lte(bbox["ymax"], bbox_p["ymax"])
    expect_lte(bbox["ymin"], bbox_p["ymin"])

    max_lyr2 <- max(terra::values(rc_obj)[, 2])
    expect_true(max_lyr2 < 1000)

    max_lyr3 <- max(terra::values(rc_obj)[, 3])
    expect_true(max_lyr3 > 8000)

    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
})
test_that("Functions that work with ROI",{

    ndvi_file <- c(system.file("extdata/raster/mod13q1/sinop-evi-2014.tif",
                               package = "sits"
    ))
    evi_file <- c(system.file("extdata/raster/mod13q1/sinop-evi-2014.tif",
                              package = "sits"
    ))

    cube <- sits_cube(
        type = "BRICK",
        name = "sinop-2014",
        timeline = timeline_2013_2014,
        satellite = "TERRA",
        sensor = "MODIS",
        bands = c("ndvi", "evi"),
        files = c(ndvi_file, evi_file)
    )
    # create a roi
    roi <- sits_bbox(cube)
    roi["xmax"] <- (roi["xmax"] - roi["xmin"]) / 2 + roi["xmin"]
    roi["ymax"] <- (roi["ymax"] - roi["ymin"]) / 2 + roi["ymin"]

    # retrieve the bounding box for this ROI
    bbox_1 <- sits:::.sits_roi_bbox(roi, cube)

    expect_true(length(sits:::.sits_bbox_intersect(bbox_1, cube)) == 4)

    # read a set of lat long coordinates
    csv_file <- system.file("extdata/samples/samples_sinop_crop.csv",
                            package = "sits")
    sf_obj <- csv_file %>%
        read.csv() %>%
        tibble::as_tibble() %>%
        dplyr::select(longitude, latitude) %>%
        sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

    # read a bbox as an sf object
    bbox_2 <- sits:::.sits_roi_bbox(sf_obj, cube)
    expect_true(length(sits:::.sits_bbox_intersect(bbox_2, cube)) == 4)

    # extract the bounding box from a set of lat/long points
    sf_bbox <- sf::st_bbox(sf_obj)
    names(sf_bbox) <- c("lon_min", "lat_min", "lon_max", "lat_max")
    class(sf_bbox) <- c("vector")
    bbox_3 <- sits:::.sits_roi_bbox(sf_bbox, cube)

    expect_true(length(sits:::.sits_bbox_intersect(bbox_3, cube)) == 4)
})

test_that("Internal functions in ROI",{

    ndvi_file <- c(system.file("extdata/raster/mod13q1/sinop-evi-2014.tif",
                               package = "sits"
    ))
    evi_file <- c(system.file("extdata/raster/mod13q1/sinop-evi-2014.tif",
                              package = "sits"
    ))

    cube <- sits_cube(
        type = "BRICK",
        name = "sinop-2014",
        timeline = timeline_2013_2014,
        satellite = "TERRA",
        sensor = "MODIS",
        bands = c("ndvi", "evi"),
        files = c(ndvi_file, evi_file)
    )

    # create a roi
    roi <- sits_bbox(cube)
    x_size <- as.numeric(roi["xmax"] - roi["xmin"])
    y_size <- as.numeric(roi["ymax"] - roi["ymin"])

    roi["xmax"] <- roi["xmax"] - 2*x_size
    roi["xmin"] <- roi["xmin"] - 2*x_size
    expect_null(sits:::.sits_bbox_intersect(roi, cube))

    bbox <- sits_bbox(cube)
    bbox["xmax"] <- bbox["xmax"] + x_size
    bbox["xmin"] <- bbox["xmin"] - x_size
    bbox["ymax"] <- bbox["ymax"] + x_size
    bbox["ymin"] <- bbox["ymin"] - x_size

    int_bbox <- sits:::.sits_bbox_intersect(bbox, cube)
    expect_true(all(int_bbox == sits_bbox(cube)))

    bb <- sits_bbox(cube)
    bb["xmin"] <- bb["xmin"] + x_size/4
    bb["ymin"] <- bb["ymin"] + x_size/4

    si <- sits:::.sits_sub_image_from_bbox(bb, cube)
    expect_true(si["first_row"] == 1)
    expect_true(si["first_col"] == 13)
    expect_true(si["nrows"] == 38)
    expect_true(si["ncols"] == 38)
})
