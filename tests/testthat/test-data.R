test_that("Reading a LAT/LONG from RASTER", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    raster_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir
    )

    samples <- tibble::tibble(longitude = -55.66738, latitude = -11.76990)

    point_ndvi <- sits_get_data(raster_cube, samples)

    expect_equal(names(point_ndvi)[1], "longitude")
    expect_true(ncol(.tibble_time_series(point_ndvi)) == 2)
    expect_true(length(sits_timeline(point_ndvi)) == 12)
})

test_that("Reading a LAT/LONG from RASTER with crs parameter", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    raster_cube <-  sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir
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
    raster_cube <-  sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir
    )

    csv_raster_file <- system.file("extdata/samples/samples_sinop_crop.csv",
                                   package = "sits"
    )
    points_poly <- sits_get_data(raster_cube,
                                 samples = csv_raster_file
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
                               samples = df_csv
    )

    expect_true("Forest" %in% sits_labels(points_df))
    expect_equal(names(points_df)[1], "longitude")
    expect_equal(length(names(points_df)), 7)
    expect_true(ncol(.tibble_time_series(points_df)) == 2)
    expect_true(length(sits_timeline(points_df)) == 12)
})

test_that("Reading a CSV file from RASTER with crs parameter", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    raster_cube <-  sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir
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

    # read the data as SF

    points_df_sf <- sits_get_data(
        raster_cube,
        samples = path_samples_repr,
        crs = "+proj=aea
        +lat_0=-12 +lon_0=-54
        +lat_1=-2 +lat_2=-22
        +x_0=5000000 +y_0=10000000
        +ellps=GRS80 +units=m +no_defs "
    )
})

test_that("Retrieving points from BDC using POLYGON shapefiles", {
    # check "BDC_ACCESS_KEY" - mandatory one per user
    bdc_access_key <- Sys.getenv("BDC_ACCESS_KEY")
    testthat::skip_if(nchar(bdc_access_key) == 0,
                      message = "No BDC_ACCESS_KEY defined in environment."
    )
    # read the shape file for Mato Grosso
    shp_file <- system.file(
        "extdata/shapefiles/mato_grosso/mt.shp",
        package = "sits"
    )
    sf_mt <- sf::read_sf(shp_file)

    # create a raster cube covering for the Mato Grosso state
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
    # get the timeline
    cube_timeline <- sits_timeline(modis_cube)

    # Retrieve points based on a POLYGON shapefile
    points_shp <- sits_get_data(modis_cube,
                                samples = shp_file,
                                n_sam_pol = 5
    )
    expect_equal(object = nrow(points_shp), expected = 5)
    expect_equal(object = unique(points_shp[["start_date"]]),
                 expected = as.Date(cube_timeline[1]))
    expect_equal(object = unique(points_shp[["end_date"]]),
                 expected = as.Date(cube_timeline[length(cube_timeline)]))

    # test bounding box
    polygons_bbox <- .bbox(sf_mt)
    points_shp_in_bbox <- dplyr::filter(
        points_shp,
        .data[["longitude"]] >= polygons_bbox[["xmin"]],
        .data[["longitude"]] <= polygons_bbox[["xmax"]],
        .data[["latitude"]] >= polygons_bbox[["ymin"]],
        .data[["latitude"]] <= polygons_bbox[["ymax"]],
    )
    expect_true(nrow(points_shp_in_bbox) == nrow(points_shp))

    # test for errors in get_data syntax
    expect_error(
        sits_get_data(modis_cube,
                      samples = shp_file,
                      pol_avg = TRUE
        )
    )
    # test for errors in get_data syntax
    expect_error(
        sits_get_data(modis_cube,
                      samples = shp_file,
                      pol_avg = TRUE,
                      pol_id = "iddddddd"
        )
    )
    # retrieve labelled points from BDC cube
    points_shp_avg <- sits_get_data(modis_cube,
                                    samples = shp_file,
                                    n_sam_pol = 5,
                                    label_attr = "NM_ESTADO",
                                    pol_avg = TRUE,
                                    pol_id = "CD_GEOCUF"
    )

    expect_equal(object = nrow(points_shp_avg), expected = 1)
    expect_equal(
        object = sits_labels(points_shp_avg),
        expected = "MATO GROSSO"
    )
    # retrieve points from BDC cube with no label
    points_shp_no_label <- sits_get_data(modis_cube,
                                         samples = shp_file,
                                         n_sam_pol = 5,
                                         pol_avg = TRUE,
                                         pol_id = "CD_GEOCUF"
    )

    expect_equal(object = nrow(points_shp_no_label), expected = 1)
    expect_equal(
        object = sits_labels(points_shp_no_label),
        expected = "NoClass"
    )
    # test for errors in get_data syntax
    expect_error(
        sits_get_data(raster_cube,
                      samples = temp_shp,
                      label_attr = "labelddddsssaaa"
        )
    )
})
test_that("Retrieving points from BDC using POINT shapefiles", {
    # check "BDC_ACCESS_KEY" - mandatory one per user
    bdc_access_key <- Sys.getenv("BDC_ACCESS_KEY")
    testthat::skip_if(nchar(bdc_access_key) == 0,
                      message = "No BDC_ACCESS_KEY defined in environment."
    )
    shp_file <- system.file(
        "extdata/shapefiles/cerrado/cerrado_forested.shp",
        package = "sits"
    )
    sf_cf <- sf::read_sf(shp_file)

    sf_roi <- sf::st_bbox(sf_cf)
    sf_roi["crs"] <- 4326

    # create a raster cube file based on the information about the files
    modis_cube <- .try({
        sits_cube(
            source = "BDC",
            collection = "MOD13Q1-6",
            bands = c("NDVI", "EVI"),
            roi = sf_roi,
            start_date = "2018-09-01",
            end_date = "2019-08-29"
        )
    },
    .default = NULL)
    testthat::skip_if(purrr::is_null(modis_cube),
                      message = "BDC is not accessible"
    )
    tf <- paste0(tempdir(), "/cerrado_forested.shp")
    sf::st_write(sf_cf[1:5,], dsn = tf)
    points_cf <- sits_get_data(modis_cube,
                               samples = tf,
                               label = "Woodland"
    )
    cube_timeline <- sits_timeline(modis_cube)
    expect_equal(object = nrow(points_cf), expected = 5)
    expect_equal(object = unique(points_cf[["start_date"]]),
                 expected = as.Date(cube_timeline[1]))
    expect_equal(object = unique(points_cf[["end_date"]]),
                 expected = as.Date(cube_timeline[length(cube_timeline)]))

    points_bbox <- .bbox(sf_cf)

    points_in_bbox <- dplyr::filter(
        points_cf,
        .data[["longitude"]] >= points_bbox[["xmin"]],
        .data[["longitude"]] <= points_bbox[["xmax"]],
        .data[["latitude"]] >= points_bbox[["ymin"]],
        .data[["latitude"]] <= points_bbox[["ymax"]],
    )

})
test_that("Retrieving points from BDC using sits tibble", {
    # check "BDC_ACCESS_KEY" - mandatory one per user
    bdc_access_key <- Sys.getenv("BDC_ACCESS_KEY")
    testthat::skip_if(nchar(bdc_access_key) == 0,
                      message = "No BDC_ACCESS_KEY defined in environment."
    )
    cube_bbox <- sits_bbox(cerrado_2classes)
    # create a raster cube file based on the bbox of the sits tibble
    modis_cube <- .try({
        sits_cube(
            source = "BDC",
            collection = "MOD13Q1-6",
            bands = c("NDVI", "EVI"),
            roi = cube_bbox,
            start_date = "2018-09-01",
            end_date = "2019-08-29"
        )
    },
    .default = NULL)
    testthat::skip_if(purrr::is_null(modis_cube),
                      message = "BDC is not accessible"
    )
    # create a sits_tibble to retrieve the data
    # first select unique locations
    cerrado_pts <- dplyr::distinct(
        cerrado_2classes,
        .data[["longitude"]],
        .data[["latitude"]],
        .data[["label"]]
    )
    input_tb <- cerrado_pts[1:5,]
    input_tb$start_date = as.Date("2018-09-01")
    input_tb$end_date = as.Date("2019-08-29")
    points_tb <- sits_get_data(modis_cube,
                               samples = input_tb
    )
    cube_timeline <- sits_timeline(modis_cube)
    expect_equal(object = nrow(points_tb), expected = 5)
    expect_equal(object = unique(points_tb[["start_date"]]),
                 expected = as.Date(cube_timeline[1]))
    expect_equal(object = unique(points_tb[["end_date"]]),
                 expected = as.Date(cube_timeline[length(cube_timeline)]))
})

test_that("Retrieving points from BDC using sf objects", {
    # check "BDC_ACCESS_KEY" - mandatory one per user
    bdc_access_key <- Sys.getenv("BDC_ACCESS_KEY")
    testthat::skip_if(nchar(bdc_access_key) == 0,
                      message = "No BDC_ACCESS_KEY defined in environment."
    )
    shp_file <- system.file(
        "extdata/shapefiles/cerrado/cerrado_forested.shp",
        package = "sits"
    )
    sf_cf <- sf::read_sf(shp_file)

    sf_roi <- sf::st_bbox(sf_cf)
    sf_roi["crs"] <- 4326

    # create a raster cube file based on the bbox of the sf object
    modis_cube <- .try({
        sits_cube(
            source = "BDC",
            collection = "MOD13Q1-6",
            bands = c("NDVI", "EVI"),
            roi = sf_roi,
            start_date = "2018-09-01",
            end_date = "2019-08-29"
        )
    },
    .default = NULL)

    testthat::skip_if(purrr::is_null(modis_cube),
                      message = "BDC is not accessible"
    )
    points_cf <- sits_get_data(modis_cube,
                               samples = sf_cf[1:5, ],
                               label = "Woodland"
    )

    cube_timeline <- sits_timeline(modis_cube)
    expect_equal(object = nrow(points_cf), expected = 5)
    expect_equal(object = unique(points_cf[["start_date"]]),
                 expected = as.Date(cube_timeline[1]))
    expect_equal(object = unique(points_cf[["end_date"]]),
                 expected = as.Date(cube_timeline[length(cube_timeline)]))

    points_bbox <- .bbox(sf_cf)

    points_in_bbox <- dplyr::filter(
        points_cf,
        .data[["longitude"]] >= points_bbox[["xmin"]],
        .data[["longitude"]] <= points_bbox[["xmax"]],
        .data[["latitude"]] >= points_bbox[["ymin"]],
        .data[["latitude"]] <= points_bbox[["ymax"]],
    )

    expect_true(nrow(points_in_bbox) == nrow(points_cf))

    # read the shape file for Mato Grosso
    shp_file <- system.file(
        "extdata/shapefiles/mato_grosso/mt.shp",
        package = "sits"
    )
    sf_mt <- sf::read_sf(shp_file)

    # create a raster cube covering for the Mato Grosso state
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
    # obtain a set of points based on an SF POLYGOn geometry
    points_poly <- sits_get_data(modis_cube,
                                 samples = sf_mt,
                                 n_sam_pol = 5
    )

    cube_timeline <- sits_timeline(modis_cube)
    expect_equal(object = nrow(points_poly), expected = 5)
    expect_equal(object = unique(points_poly[["start_date"]]),
                 expected = as.Date(cube_timeline[1]))
    expect_equal(object = unique(points_poly[["end_date"]]),
                 expected = as.Date(cube_timeline[length(cube_timeline)]))

    # test bounding box
    polygons_bbox <- .bbox(sf_mt)

    points_poly_in_bbox <- dplyr::filter(
        points_poly,
        .data[["longitude"]] >= polygons_bbox[["xmin"]],
        .data[["longitude"]] <= polygons_bbox[["xmax"]],
        .data[["latitude"]] >= polygons_bbox[["ymin"]],
        .data[["latitude"]] <= polygons_bbox[["ymax"]],
    )

    expect_true(nrow(points_poly_in_bbox) == nrow(points_poly))
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

test_that("Working with shapefile ", {
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
        data_dir = data_dir
    )
    output_dir <- paste0(tempdir(), "/al_1")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
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
                                 samples = csv_raster_file
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
