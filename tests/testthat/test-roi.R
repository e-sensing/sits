test_that("One-year, multicore classification with ROI", {
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor(num_trees = 30))

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )

    bbox <- .bbox(sinop)
    roi <- bbox
    roi[["xmax"]] <- (bbox[["xmax"]] - bbox[["xmin"]]) / 2 + bbox[["xmin"]]
    roi[["ymax"]] <- (bbox[["ymax"]] - bbox[["ymin"]]) / 2 + bbox[["ymin"]]

    expect_error(.bbox_type(sinop$crs))
    expect_warning(.bbox_from_tbl(samples_modis_ndvi))

    bbox_samples <- sits_bbox(samples_modis_ndvi)
    sinop_probs <- .try(
        {
            sits_classify(
                data = sinop,
                ml_model = rfor_model,
                output_dir = tempdir(),
                roi = roi,
                memsize = 4,
                multicores = 2,
                progress = FALSE,
                version = "version_roi"
            )
        },
        .default = NULL
    )

    if (purrr::is_null(sinop_probs)) {
        skip("Unable to allocated multicores")
    }
    expect_true(all(file.exists(unlist(sinop_probs$file_info[[1]]$path))))
    rc_obj <- .raster_open_rast(sinop_probs$file_info[[1]]$path[[1]])


    bbox_p <- sits_bbox(sinop_probs)
    expect_lte(bbox_p[["xmax"]], bbox[["xmax"]])
    expect_equal(bbox[["xmin"]], bbox_p[["xmin"]])
    expect_lte(bbox_p[["ymax"]], bbox[["ymax"]])
    expect_equal(bbox[["ymin"]], bbox_p[["ymin"]])

    max_lyr2 <- max(.raster_get_values(rc_obj)[, 2])
    expect_true(max_lyr2 <= 10000)

    max_lyr3 <- max(.raster_get_values(rc_obj)[, 3])
    expect_true(max_lyr3 > 7000)

    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
})

test_that("Bbox in WGS 84", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )

    bbox <- sits_bbox(sinop, as_crs = "EPSG:4326")
    expect_true(all(names(bbox) %in% c("xmin", "ymin", "xmax", "ymax", "crs")))
})

test_that("bbox as sf", {
    # create a raster cube
    s2_cube_s2a <- .try(
        {
            sits_cube(
                source = "AWS",
                collection = "SENTINEL-2-L2A",
                tiles = c("20LKP", "21LTF"),
                bands = c("B05"),
                start_date = as.Date("2018-07-18"),
                end_date = as.Date("2018-08-23"),
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(purrr::is_null(s2_cube_s2a),
        message = "MPC is not accessible"
    )
    expect_warning(sits_bbox(s2_cube_s2a))
})

test_that("Functions that work with ROI", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    # create a roi
    roi <- sits_bbox(cube)
    roi[["xmax"]] <- (roi[["xmax"]] - roi[["xmin"]]) / 2 + roi[["xmin"]]
    roi[["ymax"]] <- (roi[["ymax"]] - roi[["ymin"]]) / 2 + roi[["ymin"]]

    # retrieve the bounding box for this ROI
    bbox_1 <- .bbox(roi, as_crs = .cube_crs(cube))

    expect_true(.is_bbox(.bbox_intersection(bbox_1, .cube_bbox(cube))))

    # read a set of lat long coordinates
    csv_file <- system.file("extdata/samples/samples_sinop_crop.csv",
        package = "sits"
    )
    sf_obj <- csv_file |>
        read.csv(stringsAsFactors = FALSE) |>
        tibble::as_tibble() |>
        dplyr::select(longitude, latitude) |>
        sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

    # read a bbox as an sf object
    bbox_2 <- .bbox(sf_obj, as_crs = .cube_crs(cube))
    expect_true(.is_bbox(.bbox_intersection(bbox_2, .cube_bbox(cube))))

    # extract the bounding box from a set of lat/long points
    sf_bbox <- sf::st_bbox(sf_obj)
    names(sf_bbox) <- c("lon_min", "lat_min", "lon_max", "lat_max")
    class(sf_bbox) <- c("numeric")
    bbox_3 <- .bbox(.roi_as_sf(sf_bbox, as_crs = .cube_crs(cube)))

    expect_true(.is_bbox(.bbox_intersection(bbox_3, .cube_bbox(cube))))
})

test_that("Internal functions in ROI", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    # create a roi
    roi <- sits_bbox(cube)
    x_size <- as.numeric(roi[["xmax"]] - roi[["xmin"]])
    y_size <- as.numeric(roi[["ymax"]] - roi[["ymin"]])

    roi_2size <- roi

    roi_2size["xmax"] <- roi[["xmax"]] - 2 * x_size
    roi_2size["xmin"] <- roi[["xmin"]] - 2 * x_size
    expect_null(.bbox_intersection(.bbox(roi_2size), .bbox(cube)))

    bbox <- sits_bbox(cube)
    bbox[["xmax"]] <- bbox[["xmax"]] + x_size
    bbox[["xmin"]] <- bbox[["xmin"]] - x_size
    bbox[["ymax"]] <- bbox[["ymax"]] + x_size
    bbox[["ymin"]] <- bbox[["ymin"]] - x_size

    int_bbox <- .bbox_intersection(.bbox(bbox), .bbox(cube))
    expect_true(all(int_bbox == sits_bbox(cube)))

    bb <- sits_bbox(cube)
    bb[["xmin"]] <- bb[["xmin"]] + x_size / 4
    bb[["ymin"]] <- bb[["ymin"]] + x_size / 4

    si <- .raster_sub_image_from_bbox(bb, cube)
    expect_equal(si[["col"]], 64)
    expect_equal(si[["row"]], 1)
    expect_equal(si[["ncols"]], 192)
    expect_equal(si[["nrows"]], 84)
})
