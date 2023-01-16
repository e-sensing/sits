test_that("Reading a LAT/LONG from RASTER", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    raster_cube <- tryCatch(
        {
            sits_cube(
                source = "BDC",
                collection = "MOD13Q1-6",
                data_dir = data_dir,
                delim = "_",
                parse_info = c("X1", "tile", "band", "date")
            )
        },
        error = function(e) {
            return(NULL)
        }
    )

    testthat::skip_if(purrr::is_null(raster_cube),
                      message = "LOCAL cube was not found"
    )

    samples <- tibble::tibble(longitude = -55.66738, latitude = -11.76990)

    point_ndvi <- sits_get_data(raster_cube, samples)

    expect_equal(names(point_ndvi)[1], "longitude")
    expect_true(ncol(.tibble_time_series(point_ndvi)) == 2)
    expect_true(length(sits_timeline(point_ndvi)) == 23)
})

test_that("Reading a LAT/LONG from RASTER with crs parameter", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    raster_cube <- tryCatch(
        {
            sits_cube(
                source = "BDC",
                collection = "MOD13Q1-6",
                data_dir = data_dir,
                delim = "_",
                parse_info = c("X1", "tile", "band", "date")
            )
        },
        error = function(e) {
            return(NULL)
        }
    )

    testthat::skip_if(purrr::is_null(raster_cube),
                      message = "LOCAL cube was not found"
    )

    samples <- tibble::tibble(longitude = 4821005, latitude = 10025310)

    point_ndvi <- sits_get_data(
        cube = raster_cube,
        samples = samples,
        crs = "+proj=aea +lat_0=-12 +lon_0=-54 +lat_1=-2 +lat_2=-22 +x_0=5000000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs "
    )

    expect_equal(names(point_ndvi)[1], "longitude")
    expect_true(ncol(.tibble_time_series(point_ndvi)) == 2)
    expect_true(length(sits_timeline(point_ndvi)) == 23)
})

test_that("Reading a CSV file from RASTER", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    raster_cube <- tryCatch(
        {
            sits_cube(
                source = "BDC",
                collection = "MOD13Q1-6",
                data_dir = data_dir,
                delim = "_",
                parse_info = c("X1", "tile", "band", "date")
            )
        },
        error = function(e) {
            return(NULL)
        }
    )

    testthat::skip_if(purrr::is_null(raster_cube),
                      message = "LOCAL cube was not found"
    )

    csv_raster_file <- system.file("extdata/samples/samples_sinop_crop.csv",
                                   package = "sits"
    )
    points_poly <- sits_get_data(raster_cube,
                                 samples = csv_raster_file,
                                 output_dir = tempdir()
    )

    df_csv <- utils::read.csv(
        system.file("extdata/samples/samples_sinop_crop.csv", package = "sits"),
        stringsAsFactors = FALSE
    )
    expect_true(nrow(points_poly) <= nrow(df_csv))

    expect_true("Forest" %in% sits_labels(points_poly))
    expect_equal(names(points_poly)[1], "longitude")
    expect_equal(length(names(points_poly)), 7)
    expect_true(ncol(.tibble_time_series(points_poly)) == 2)
    expect_true(length(sits_timeline(points_poly)) == 23)

    points_df <- sits_get_data(raster_cube,
                               samples = df_csv,
                               output_dir = tempdir()
    )

    expect_true("Forest" %in% sits_labels(points_df))
    expect_equal(names(points_df)[1], "longitude")
    expect_equal(length(names(points_df)), 7)
    expect_true(ncol(.tibble_time_series(points_df)) == 2)
    expect_true(length(sits_timeline(points_df)) == 23)
})

test_that("Reading a CSV file from RASTER with crs parameter", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    raster_cube <- tryCatch(
        {
            sits_cube(
                source = "BDC",
                collection = "MOD13Q1-6",
                data_dir = data_dir,
                delim = "_",
                parse_info = c("X1", "tile", "band", "date")
            )
        },
        error = function(e) {
            return(NULL)
        }
    )

    testthat::skip_if(purrr::is_null(raster_cube),
                      message = "LOCAL cube was not found"
    )

    df_csv <- utils::read.csv(
        system.file("extdata/samples/samples_sinop_crop.csv", package = "sits"),
        stringsAsFactors = FALSE
    )

    class(df_csv) <- c("sits", class(df_csv))
    df_sf <- sits_as_sf(data = df_csv, crs = 4326)
    df_sf_reproj <- sf::st_transform(
        x = df_sf,
        crs = "+proj=aea +lat_0=-12 +lon_0=-54 +lat_1=-2 +lat_2=-22 +x_0=5000000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs "
    )

    df_sf_reproj[, c("longitude", "latitude")] <-
        sf::st_coordinates(df_sf_reproj)
    path_samples_repr <- tempfile(fileext = ".csv")
    utils::write.csv(
        x = sf::st_drop_geometry(df_sf_reproj),
        file = path_samples_repr
    )

    points_df <- sits_get_data(
        raster_cube,
        samples = path_samples_repr,
        output_dir = tempdir(),
        crs = "+proj=aea +lat_0=-12 +lon_0=-54 +lat_1=-2 +lat_2=-22 +x_0=5000000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs "
    )

    expect_true("Forest" %in% sits_labels(points_df))
    expect_equal(names(points_df)[1], "longitude")
    expect_equal(length(names(points_df)), 7)
    expect_true(ncol(.tibble_time_series(points_df)) == 2)
    expect_true(length(sits_timeline(points_df)) == 23)
})

test_that("Reading a SHP file from RASTER", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    raster_cube <- tryCatch(
        {
            sits_cube(
                source = "BDC",
                collection = "MOD13Q1-6",
                data_dir = data_dir,
                delim = "_",
                parse_info = c("X1", "tile", "band", "date")
            )
        },
        error = function(e) {
            return(NULL)
        }
    )

    testthat::skip_if(purrr::is_null(raster_cube),
                      message = "LOCAL cube was not found"
    )
    polygons_sf <- rbind(
        sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(c(
            xmin = -55.62471702, xmax = -55.57293653,
            ymin = -11.63300767, ymax = -11.60607152), crs = 4326
        ))),
        sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(c(
            xmin = -55.29847023, xmax = -55.26194177,
            ymin = -11.56743498, ymax = -11.55169416), crs = 4326
        ))),
        sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(c(
            xmin = -55.55720906, xmax = -55.54030539,
            ymin = -11.75144257, ymax = -11.74521358), crs = 4326
        )))
    )

    polygons_sf[["id"]] <- seq(1, 3)
    polygons_sf[["label"]] <- c("a", "b", "c")
    polygons_bbox <- sf::st_bbox(polygons_sf)

    points_poly <- sits_get_data(raster_cube,
                                 samples = polygons_sf,
                                 output_dir = tempdir()
    )

    cube_timeline <- sits_timeline(raster_cube)
    expect_equal(object = nrow(points_poly), expected = 90)
    expect_equal(object = unique(points_poly[["start_date"]]),
                 expected = as.Date(cube_timeline[1]))
    expect_equal(object = unique(points_poly[["end_date"]]),
                 expected = as.Date(cube_timeline[length(cube_timeline)]))

    points_poly_in_bbox <- dplyr::filter(
        points_poly,
        .data[["longitude"]] >= polygons_bbox[["xmin"]],
        .data[["longitude"]] <= polygons_bbox[["xmax"]],
        .data[["latitude"]] >= polygons_bbox[["ymin"]],
        .data[["latitude"]] <= polygons_bbox[["ymax"]],
    )

    expect_true(nrow(points_poly_in_bbox) == nrow(points_poly))

    temp_shp <- sf::st_write(
        obj = polygons_sf,
        dsn = tempfile(fileext = ".shp"),
        quiet = TRUE
    )
    points_shp <- sits_get_data(raster_cube,
                                samples = temp_shp,
                                output_dir = tempdir()
    )
    expect_equal(object = nrow(points_shp), expected = 90)
    expect_equal(object = unique(points_shp[["start_date"]]),
                 expected = as.Date(cube_timeline[1]))
    expect_equal(object = unique(points_shp[["end_date"]]),
                 expected = as.Date(cube_timeline[length(cube_timeline)]))

    points_shp_in_bbox <- dplyr::filter(
        points_shp,
        .data[["longitude"]] >= polygons_bbox[["xmin"]],
        .data[["longitude"]] <= polygons_bbox[["xmax"]],
        .data[["latitude"]] >= polygons_bbox[["ymin"]],
        .data[["latitude"]] <= polygons_bbox[["ymax"]],
    )

    expect_true(nrow(points_shp_in_bbox) == nrow(points_shp))

    expect_error(
        sits_get_data(raster_cube,
                      samples = temp_shp,
                      pol_avg = TRUE,
                      output_dir = tempdir()
        )
    )
    expect_error(
        sits_get_data(raster_cube,
                      samples = temp_shp,
                      pol_avg = TRUE,
                      pol_id = "iddddddd",
                      output_dir = tempdir()
        )
    )

    points_shp_avg <- sits_get_data(raster_cube,
                                    samples = temp_shp,
                                    pol_avg = TRUE,
                                    pol_id = "id",
                                    output_dir = tempdir()
    )

    expect_equal(object = nrow(points_shp_avg), expected = 3)
    expect_equal(
        object = sits_labels(points_shp_avg),
        expected = c("a", "b", "c")
    )

    temp_shp_no_label <- dplyr::select(temp_shp, -"label")
    points_shp_no_label <- sits_get_data(raster_cube,
                                    samples = temp_shp_no_label,
                                    pol_avg = TRUE,
                                    pol_id = "id",
                                    output_dir = tempdir()
    )

    expect_equal(object = nrow(points_shp_no_label), expected = 3)
    expect_equal(
        object = sits_labels(points_shp_no_label),
        expected = "NoClass"
    )

    temp_shp_label_attr <- dplyr::rename(temp_shp, label_2 = "label")
    points_shp_label_attr <- sits_get_data(raster_cube,
                                           samples = temp_shp_label_attr,
                                           pol_avg = TRUE,
                                           pol_id = "id",
                                           label_attr = "label_2",
                                           output_dir = tempdir()
    )

    expect_equal(object = nrow(points_shp_label_attr), expected = 3)
    expect_equal(
        object = sits_labels(points_shp_label_attr),
        expected = c("a", "b", "c")
    )


    expect_error(
        sits_get_data(raster_cube,
                      samples = temp_shp,
                      label_attr = "labelddddsssaaa",
                      output_dir = tempdir()
        )
    )
    points_shp_label <- sits_get_data(raster_cube,
                                      samples = temp_shp,
                                      label_attr = "label",
                                      output_dir = tempdir()
    )
    expect_equal(
        object = sits_labels(points_shp_label),
        expected = c("a", "b", "c")
    )
})

test_that("Test reading shapefile from BDC", {
    # check "BDC_ACCESS_KEY" - mandatory one per user
    bdc_access_key <- Sys.getenv("BDC_ACCESS_KEY")

    testthat::skip_if(nchar(bdc_access_key) == 0,
                      message = "No BDC_ACCESS_KEY defined in environment."
    )

    # create a raster cube file based on the information about the files
    cbers_stac_tile <- tryCatch(
        {
            sits_cube(
                source = "BDC",
                collection = "CB4_64_16D_STK-1",
                bands = c("NDVI", "EVI"),
                tiles = c("022024", "022025"),
                start_date = "2018-09-01",
                end_date = "2018-10-28"
            )
        },
        error = function(e) {
            return(NULL)
        }
    )

    testthat::skip_if(purrr::is_null(cbers_stac_tile),
                      message = "BDC is not accessible"
    )

    shp_path <- system.file("extdata/shapefiles/bdc-test/samples.shp",
                            package = "sits"
    )

    time_series_bdc <- sits::sits_get_data(cbers_stac_tile,
                                           samples = shp_path,
                                           output_dir = tempdir()
    )

    if (purrr::is_null(time_series_bdc)) {
        skip("BDC not accessible")
    }

    expect_equal(nrow(time_series_bdc), 10)

    bbox <- sits_bbox(time_series_bdc)
    expect_true(bbox["xmin"] < -46.)
    expect_true(all(sits_bands(time_series_bdc) %in% c("NDVI", "EVI")))

    ts <- time_series_bdc$time_series[[1]]
    expect_true(max(ts["EVI"]) < 1.)

    sf_object <- sf::st_read(shp_path, quiet = TRUE)

    time_series_sf <- sits::sits_get_data(cbers_stac_tile,
                                          samples = sf_object,
                                          output_dir = tempdir()
    )

    if (purrr::is_null(time_series_sf)) {
        skip("BDC not accessible")
    }

    expect_equal(nrow(time_series_sf), 10)

    bbox <- sits_bbox(time_series_sf)
    expect_true(bbox["xmin"] < -46.)
    expect_true(all(sits_bands(time_series_sf) %in% c("NDVI", "EVI")))

    ts <- time_series_sf$time_series[[1]]
    expect_true(max(ts["EVI"]) < 1.)
})

test_that("Reading metadata from CSV file", {
    csv_file <- paste0(tempdir(), "/cerrado_2classes.csv")
    sits_to_csv(cerrado_2classes, file = csv_file)
    csv <- read.csv(csv_file)
    expect_true(nrow(csv) == 746)
    expect_true(all(names(csv) %in% c(
        "id", "longitude", "latitude",
        "start_date", "end_date", "label"
    )))
})

test_that("Reading data from Classified data", {
    # create a random forest model
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
    # create a data cube from local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "tile", "band", "date")
    )
    output_dir <- tempdir()
    # classify a data cube
    probs_cube <- sits_classify(
        data = cube,
        ml_model = rfor_model,
        output_dir = output_dir
    )
    # smooth the probability cube using Bayesian statistics
    bayes_cube <- sits_smooth(probs_cube, output_dir = output_dir)
    # smooth the probability cube using variace
    var_cube <- sits_smooth_variance(probs_cube, output_dir = output_dir)
    # label the probability cube
    label_cube <- sits_label_classification(bayes_cube, output_dir = output_dir)

    # Using CSV
    csv_raster_file <- system.file("extdata/samples/samples_sinop_crop.csv",
                                   package = "sits"
    )
    points_poly <- sits_get_data(label_cube,
                                 samples = csv_raster_file,
                                 output_dir = tempdir()
    )
    expect_equal(
        nrow(points_poly), nrow(read.csv(csv_raster_file))
    )

    expect_equal(
        colnames(points_poly), c("longitude", "latitude",
                                 "start_date", "end_date",
                                 "label", "cube", "predicted")
    )
    # Using lat/long
    samples <- tibble::tibble(longitude = -55.66738, latitude = -11.76990)

    point_ndvi <- sits_get_data(label_cube, samples)
    expect_equal(nrow(point_ndvi), 1)

    expect_equal(
        colnames(point_ndvi), c("longitude", "latitude",
                                 "start_date", "end_date",
                                 "label", "cube", "predicted")
    )
    # Using shp
    polygons_sf <- rbind(
        sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(c(
            xmin = -55.62471702, xmax = -55.57293653,
            ymin = -11.63300767, ymax = -11.60607152), crs = 4326
        ))),
        sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(c(
            xmin = -55.29847023, xmax = -55.26194177,
            ymin = -11.56743498, ymax = -11.55169416), crs = 4326
        ))),
        sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(c(
            xmin = -55.55720906, xmax = -55.54030539,
            ymin = -11.75144257, ymax = -11.74521358), crs = 4326
        )))
    )

    polygons_sf[["id"]] <- seq(1, 3)
    polygons_sf[["label"]] <- c("a", "b", "c")
    polygons_bbox <- sf::st_bbox(polygons_sf)

    points_poly <- sits_get_data(label_cube,
                                 samples = polygons_sf,
                                 output_dir = tempdir()
    )
    expect_equal(nrow(points_poly), 90)

    expect_equal(
        colnames(points_poly), c("longitude", "latitude",
                                 "start_date", "end_date",
                                 "label", "cube", "predicted")
    )
    unlink(probs_cube$file_info[[1]]$path)
    unlink(bayes_cube$file_info[[1]]$path)
    unlink(label_cube$file_info[[1]]$path)
})
