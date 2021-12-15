test_that("One-year, multicore classification with ROI", {
    samples_2bands <- sits_select(samples_modis_4bands,
                                  bands = c("NDVI", "EVI"))

    svm_model <- sits_train(samples_2bands, sits_svm())

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "X2", "tile", "band", "date")
    )

    bbox <- sits_bbox(sinop)
    bbox["xmax"] <- (bbox["xmax"] - bbox["xmin"]) / 2 + bbox["xmin"]
    bbox["ymax"] <- (bbox["ymax"] - bbox["ymin"]) / 2 + bbox["ymin"]

    sinop_probs <- tryCatch({
        suppressMessages(
            sits_classify(
                data = sinop,
                ml_model = svm_model,
                output_dir = tempdir(),
                roi = bbox,
                memsize = 4, multicores = 2
            )
        )
    },
    error = function(e) {
        return(NULL)
    })

    if (purrr::is_null(sinop_probs)) {
        skip("Unable to allocated multicores")
    }
    expect_true(all(file.exists(unlist(sinop_probs$file_info[[1]]$path))))
    rc_obj <- .raster_open_rast(sinop_probs$file_info[[1]]$path[[1]])
    # expect_true(.raster_nrows(rc_obj) == sinop_probs$nrows)

    bbox_p <- sits_bbox(sinop_probs)
    expect_lte(bbox["xmax"], bbox_p["xmax"])
    expect_lte(bbox["xmin"], bbox_p["xmin"])
    expect_lte(bbox["ymax"], bbox_p["ymax"])
    expect_lte(bbox["ymin"], bbox_p["ymin"])

    max_lyr2 <- max(.raster_get_values(rc_obj)[, 2])
    expect_true(max_lyr2 <= 10000)

    max_lyr3 <- max(.raster_get_values(rc_obj)[, 3])
    expect_true(max_lyr3 > 7000)

    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
})
test_that("Functions that work with ROI", {

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "X2", "tile", "band", "date")
    )
    # create a roi
    roi <- sits_bbox(cube)
    roi["xmax"] <- (roi["xmax"] - roi["xmin"]) / 2 + roi["xmin"]
    roi["ymax"] <- (roi["ymax"] - roi["ymin"]) / 2 + roi["ymin"]

    # retrieve the bounding box for this ROI
    bbox_1 <- .sits_roi_bbox(roi, cube)

    expect_true(length(.sits_bbox_intersect(bbox_1, cube)) == 4)

    # read a set of lat long coordinates
    csv_file <- system.file("extdata/samples/samples_sinop_crop.csv",
                            package = "sits")
    sf_obj <- csv_file %>%
        read.csv(stringsAsFactors = FALSE) %>%
        tibble::as_tibble() %>%
        dplyr::select(longitude, latitude) %>%
        sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

    # read a bbox as an sf object
    bbox_2 <- .sits_roi_bbox(sf_obj, cube)
    expect_true(length(.sits_bbox_intersect(bbox_2, cube)) == 4)

    # extract the bounding box from a set of lat/long points
    sf_bbox <- sf::st_bbox(sf_obj)
    names(sf_bbox) <- c("lon_min", "lat_min", "lon_max", "lat_max")
    class(sf_bbox) <- c("vector")
    bbox_3 <- .sits_roi_bbox(sf_bbox, cube)

    expect_true(length(.sits_bbox_intersect(bbox_3, cube)) == 4)
})

test_that("Internal functions in ROI", {

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "X2", "tile", "band", "date")
    )
    # create a roi
    roi <- sits_bbox(cube)
    x_size <- as.numeric(roi["xmax"] - roi["xmin"])
    y_size <- as.numeric(roi["ymax"] - roi["ymin"])

    roi["xmax"] <- roi["xmax"] - 2 * x_size
    roi["xmin"] <- roi["xmin"] - 2 * x_size
    expect_null(.sits_bbox_intersect(roi, cube))

    bbox <- sits_bbox(cube)
    bbox["xmax"] <- bbox["xmax"] + x_size
    bbox["xmin"] <- bbox["xmin"] - x_size
    bbox["ymax"] <- bbox["ymax"] + x_size
    bbox["ymin"] <- bbox["ymin"] - x_size

    int_bbox <- .sits_bbox_intersect(bbox, cube)
    expect_true(all(int_bbox == sits_bbox(cube)))

    bb <- sits_bbox(cube)
    bb["xmin"] <- bb["xmin"] + x_size / 4
    bb["ymin"] <- bb["ymin"] + x_size / 4

    si <- .sits_raster_sub_image_from_bbox(bb, cube)
    expect_true(si["first_row"] == 1)
    expect_true(si["first_col"] == 64)
    expect_true(si["nrows"] == 81)
    expect_true(si["ncols"] == 191)
})
