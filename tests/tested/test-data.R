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
    expect_true(length(sits_timeline(point_ndvi)) == 12)
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
        crs = "+proj=aea
        +lat_0=-12 +lon_0=-54
        +lat_1=-2 +lat_2=-22
        +x_0=5000000 +y_0=10000000
        +ellps=GRS80 +units=m +no_defs "
    )

    expect_equal(names(point_ndvi)[1], "longitude")
    expect_true(ncol(.tibble_time_series(point_ndvi)) == 2)
    expect_true(length(sits_timeline(point_ndvi)) == 12)
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
    expect_true(length(sits_timeline(points_poly)) == 12)

    points_df <- sits_get_data(raster_cube,
                               samples = df_csv,
                               output_dir = tempdir()
    )

    expect_true("Forest" %in% sits_labels(points_df))
    expect_equal(names(points_df)[1], "longitude")
    expect_equal(length(names(points_df)), 7)
    expect_true(ncol(.tibble_time_series(points_df)) == 2)
    expect_true(length(sits_timeline(points_df)) == 12)
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
        crs = "+proj=aea
        +lat_0=-12 +lon_0=-54
        +lat_1=-2 +lat_2=-22
        +x_0=5000000 +y_0=10000000
        +ellps=GRS80 +units=m +no_defs "
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
        crs = "+proj=aea
        +lat_0=-12 +lon_0=-54
        +lat_1=-2 +lat_2=-22
        +x_0=5000000 +y_0=10000000
        +ellps=GRS80 +units=m +no_defs "
    )

    expect_true("Forest" %in% sits_labels(points_df))
    expect_equal(names(points_df)[1], "longitude")
    expect_equal(length(names(points_df)), 7)
    expect_true(ncol(.tibble_time_series(points_df)) == 2)
    expect_true(length(sits_timeline(points_df)) == 12)
})

test_that("Reading a SHP file from BDC", {
    # check "BDC_ACCESS_KEY" - mandatory one per user
    bdc_access_key <- Sys.getenv("BDC_ACCESS_KEY")
    testthat::skip_if(nchar(bdc_access_key) == 0,
                      message = "No BDC_ACCESS_KEY defined in environment."
    )

    shp_file <- system.file(
        "extdata/shapefiles/mato_grosso/mt.shp",
        package = "sits"
    )
    sf_mt <- sf::read_sf(shp_file)

    # create a raster cube file based on the information about the files
    modis_cube <- .try({
        sits_cube(
            source = "BDC",
            collection = "MOD13Q1-6",
            bands = c("NDVI", "EVI"),
            roi = sf_mt,
            start_date = "2018-09-01",
            end_date = "2019-08-29"
        )
    },
    .default = NULL)

    testthat::skip_if(purrr::is_null(modis_cube),
                      message = "BDC is not accessible"
    )
    points_poly <- sits_get_data(modis_cube,
                                 samples = sf_mt,
                                 n_sam_pol = 5,
                                 output_dir = tempdir()
    )

    cube_timeline <- sits_timeline(modis_cube)
    expect_equal(object = nrow(points_poly), expected = 5)
    expect_equal(object = unique(points_poly[["start_date"]]),
                 expected = as.Date(cube_timeline[1]))
    expect_equal(object = unique(points_poly[["end_date"]]),
                 expected = as.Date(cube_timeline[length(cube_timeline)]))

    polygons_bbox <- .bbox(sf_mt)

    points_poly_in_bbox <- dplyr::filter(
        points_poly,
        .data[["longitude"]] >= polygons_bbox[["xmin"]],
        .data[["longitude"]] <= polygons_bbox[["xmax"]],
        .data[["latitude"]] >= polygons_bbox[["ymin"]],
        .data[["latitude"]] <= polygons_bbox[["ymax"]],
    )

    expect_true(nrow(points_poly_in_bbox) == nrow(points_poly))

    points_shp <- sits_get_data(modis_cube,
                                samples = shp_file,
                                n_sam_pol = 5,
                                output_dir = tempdir()
    )
    expect_equal(object = nrow(points_shp), expected = 5)
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
        sits_get_data(modis_cube,
                      samples = shp_file,
                      pol_avg = TRUE,
                      output_dir = tempdir()
        )
    )
    expect_error(
        sits_get_data(modis_cube,
                      samples = shp_file,
                      pol_avg = TRUE,
                      pol_id = "iddddddd",
                      output_dir = tempdir()
        )
    )

    points_shp_avg <- sits_get_data(modis_cube,
                                    samples = shp_file,
                                    n_sam_pol = 5,
                                    label_attr = "NM_ESTADO",
                                    pol_avg = TRUE,
                                    pol_id = "CD_GEOCUF",
                                    output_dir = tempdir()
    )

    expect_equal(object = nrow(points_shp_avg), expected = 1)
    expect_equal(
        object = sits_labels(points_shp_avg),
        expected = "MATO GROSSO"
    )

    points_shp_no_label <- sits_get_data(modis_cube,
                                         samples = shp_file,
                                         n_sam_pol = 5,
                                         pol_avg = TRUE,
                                         pol_id = "CD_GEOCUF",
                                         output_dir = tempdir()
    )

    expect_equal(object = nrow(points_shp_no_label), expected = 1)
    expect_equal(
        object = sits_labels(points_shp_no_label),
        expected = "NoClass"
    )

    expect_error(
        sits_get_data(raster_cube,
                      samples = temp_shp,
                      label_attr = "labelddddsssaaa",
                      output_dir = tempdir()
        )
    )
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
    unlink(probs_cube$file_info[[1]]$path)
    unlink(bayes_cube$file_info[[1]]$path)
    unlink(label_cube$file_info[[1]]$path)
})
