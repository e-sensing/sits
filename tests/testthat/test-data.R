test_that("Reading a LAT/LONG from RASTER", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    raster_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )

    samples <- tibble::tibble(longitude = -55.66738, latitude = -11.76990)

    point_ndvi <- sits_get_data(raster_cube, samples, progress = FALSE)

    expect_equal(names(point_ndvi)[1], "longitude")
    expect_true(ncol(.tibble_time_series(point_ndvi)) == 2)
    expect_true(length(sits_timeline(point_ndvi)) == 12)
    expect_true(
        all(
            c("sits", "tbl_df", "tbl", "data.frame") %in% class(point_ndvi)
        )
    )

    samples2 <- tibble::tibble(longitude = -55.66738, latitude = 11.76990)
    expect_warning(
        sits_get_data(raster_cube, samples2, progress = FALSE)
    )
})

test_that("Reading a CSV file from RASTER", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    raster_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )

    csv_raster_file <- system.file("extdata/samples/samples_sinop_crop.csv",
                                   package = "sits"
    )
    points_poly <- sits_get_data(
        raster_cube,
        samples = csv_raster_file,
        progress = FALSE
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
    expect_true(
        all(
            c("sits", "tbl_df", "tbl", "data.frame") %in% class(points_poly)
        )
    )

    Sys.setenv("SITS_SAMPLES_CACHE_DIR" = tempdir())

    points_df <- sits_get_data(
        raster_cube,
        samples = df_csv,
        multicores = 1,
        progress = FALSE
    )

    expect_true("Forest" %in% sits_labels(points_df))
    expect_equal(names(points_df)[1], "longitude")
    expect_equal(length(names(points_df)), 7)
    expect_true(ncol(.tibble_time_series(points_df)) == 2)
    expect_true(length(sits_timeline(points_df)) == 12)
    expect_true(
        all(
            c("sits", "tbl_df", "tbl", "data.frame") %in% class(points_df)
        )
    )

    Sys.unsetenv("SITS_SAMPLES_CACHE_DIR")
})

test_that("Retrieving points from BDC using POLYGON shapefiles", {
    # read the shape file for Mato Grosso
    shp_file <- system.file(
        "extdata/shapefiles/mato_grosso/mt.shp",
        package = "sits"
    )
    sf_mt <- sf::read_sf(shp_file)

    # create a raster cube covering for the Mato Grosso state
    modis_cube <- .try(
        {
            sits_cube(
                source = "MPC",
                collection = "MOD13Q1-6.1",
                bands = c("NDVI", "EVI"),
                roi = sf_mt,
                start_date = "2019-10-01",
                end_date = "2019-12-30",
                multicores = 1,
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(purrr::is_null(modis_cube),
                      message = "BDC is not accessible"
    )
    # get the timeline
    cube_timeline <- sits_timeline(modis_cube)
    # Retrieve points based on a POLYGON shapefile
    points_shp <- suppressMessages(sits_get_data(
        modis_cube,
        samples = shp_file,
        n_sam_pol = 5,
        progress = FALSE,
        multicores = 1
    ))
    expect_equal(object = nrow(points_shp), expected = 5)
    expect_equal(
        object = unique(points_shp[["start_date"]]),
        expected = as.Date(cube_timeline[1])
    )
    expect_equal(
        object = unique(points_shp[["end_date"]]),
        expected = as.Date(cube_timeline[length(cube_timeline)])
    )
    expect_true(
        all(
            c("sits", "tbl_df", "tbl", "data.frame") %in% class(points_shp)
        )
    )

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


    # retrieve labelled points from BDC cube
    points_shp_avg <- suppressMessages(sits_get_data(modis_cube,
                                                     samples = shp_file,
                                                     n_sam_pol = 5,
                                                     label_attr = "NM_ESTADO",
                                                     pol_avg = TRUE,
                                                     progress = FALSE
    ))

    expect_equal(object = nrow(points_shp_avg), expected = 1)
    expect_equal(
        object = sits_labels(points_shp_avg),
        expected = "MATO GROSSO"
    )
    # retrieve points from BDC cube with no label
    points_shp_no_label <- suppressMessages(sits_get_data(modis_cube,
                                                          samples = shp_file,
                                                          n_sam_pol = 5,
                                                          pol_avg = TRUE,
                                                          progress = FALSE
    ))

    expect_equal(object = nrow(points_shp_no_label), expected = 1)
    expect_equal(
        object = sits_labels(points_shp_no_label),
        expected = "NoClass"
    )
    # test for errors in get_data syntax
    expect_error(
        sits_get_data(raster_cube,
                      samples = temp_shp,
                      label_attr = "labelddddsssaaa",
                      progress = FALSE
        )
    )
})

test_that("Retrieving points from BDC using POINT shapefiles", {
    shp_file <- system.file(
        "extdata/shapefiles/cerrado/cerrado_forested.shp",
        package = "sits"
    )
    sf_cf <- sf::read_sf(shp_file)

    sf_roi <- sf::st_bbox(sf_cf)
    sf_roi["crs"] <- 4326

    # create a raster cube file based on the information about the files
    modis_cube <- .try(
        {
            sits_cube(
                source = "BDC",
                collection = "MOD13Q1-6.1",
                bands = c("NDVI", "EVI"),
                roi = sf_roi,
                start_date = "2018-09-01",
                end_date = "2019-08-29",
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(purrr::is_null(modis_cube),
                      message = "BDC is not accessible"
    )
    tf <- paste0(tempdir(), "/cerrado_forested.shp")
    sf::st_write(sf_cf[1:5, ], dsn = tf, quiet = TRUE, append = FALSE)
    points_cf <- suppressMessages(sits_get_data(modis_cube,
                                                samples = tf,
                                                label = "Woodland",
                                                progress = FALSE
    ))
    cube_timeline <- sits_timeline(modis_cube)
    expect_equal(object = nrow(points_cf), expected = 5)
    expect_equal(
        object = unique(points_cf[["start_date"]]),
        expected = as.Date(cube_timeline[1])
    )
    expect_equal(
        object = unique(points_cf[["end_date"]]),
        expected = as.Date(cube_timeline[length(cube_timeline)])
    )
    expect_true(
        all(
            c("sits", "tbl_df", "tbl", "data.frame") %in% class(points_cf)
        )
    )

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
    cube_bbox <- sits_bbox(cerrado_2classes)
    # create a raster cube file based on the bbox of the sits tibble
    modis_cube <- .try(
        {
            sits_cube(
                source = "BDC",
                collection = "MOD13Q1-6.1",
                bands = c("NDVI", "EVI"),
                roi = cube_bbox,
                start_date = "2018-09-01",
                end_date = "2019-08-29",
                progress = FALSE
            )
        },
        .default = NULL
    )
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
    input_tb <- cerrado_pts[1:5, ]
    input_tb$start_date <- as.Date("2018-08-22")
    input_tb$end_date <- as.Date("2019-08-30")
    points_tb <- suppressMessages(sits_get_data(modis_cube,
                                                samples = input_tb,
                                                progress = FALSE
    ))
    cube_timeline <- sits_timeline(modis_cube)
    expect_equal(object = nrow(points_tb), expected = 5)
    expect_equal(
        object = unique(points_tb[["start_date"]]),
        expected = as.Date(cube_timeline[1])
    )
    expect_equal(
        object = unique(points_tb[["end_date"]]),
        expected = as.Date(cube_timeline[length(cube_timeline)])
    )
    expect_true(
        all(
            c("sits", "tbl_df", "tbl", "data.frame") %in% class(points_tb)
        )
    )
})

test_that("Retrieving points from BDC using sf objects", {
    shp_file <- system.file(
        "extdata/shapefiles/cerrado/cerrado_forested.shp",
        package = "sits"
    )
    sf_cf <- sf::read_sf(shp_file)

    sf_roi <- sf::st_bbox(sf_cf)
    sf_roi["crs"] <- 4326

    # create a raster cube file based on the bbox of the sf object
    modis_cube <- .try(
        {
            sits_cube(
                source = "BDC",
                collection = "MOD13Q1-6.1",
                bands = c("NDVI", "EVI"),
                roi = sf_roi,
                start_date = "2018-09-01",
                end_date = "2019-08-29",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(modis_cube),
                      message = "BDC is not accessible"
    )
    points_cf <- suppressMessages(sits_get_data(modis_cube,
                                                samples = sf_cf[1:5, ],
                                                label = "Woodland",
                                                progress = FALSE
    ))

    cube_timeline <- sits_timeline(modis_cube)
    expect_equal(object = nrow(points_cf), expected = 5)
    expect_equal(
        object = unique(points_cf[["start_date"]]),
        expected = as.Date(cube_timeline[1])
    )
    expect_equal(
        object = unique(points_cf[["end_date"]]),
        expected = as.Date(cube_timeline[length(cube_timeline)])
    )
    expect_true(
        all(
            c("sits", "tbl_df", "tbl", "data.frame") %in% class(points_cf)
        )
    )

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
    modis_cube <- .try(
        {
            sits_cube(
                source = "BDC",
                collection = "MOD13Q1-6.1",
                bands = c("NDVI", "EVI"),
                roi = sf_mt,
                start_date = "2018-09-01",
                end_date = "2019-08-29",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(modis_cube),
                      message = "BDC is not accessible"
    )
    # obtain a set of points based on an SF POLYGOn geometry
    points_poly <- suppressMessages(sits_get_data(modis_cube,
                                                  samples = sf_mt,
                                                  n_sam_pol = 5,
                                                  progress = FALSE
    ))

    cube_timeline <- sits_timeline(modis_cube)
    expect_equal(object = nrow(points_poly), expected = 5)
    expect_equal(
        object = unique(points_poly[["start_date"]]),
        expected = as.Date(cube_timeline[1])
    )
    expect_equal(
        object = unique(points_poly[["end_date"]]),
        expected = as.Date(cube_timeline[length(cube_timeline)])
    )
    expect_true(
        all(
            c("sits", "tbl_df", "tbl", "data.frame") %in% class(points_poly)
        )
    )

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

test_that("Retrieving points from MPC Base Cube", {
    # load samples
    samples <- read.csv(
        system.file("extdata/samples/samples_sinop_crop.csv", package = "sits")
    )
    # edit samples to work with the cube (test purposes only)
    samples[["start_date"]] <- "2019-06-01"
    samples[["end_date"]] <- "2019-08-30"

    regdir <- file.path(tempdir(), "base_cube_reg_data")
    if (!dir.exists(regdir)) {
        suppressWarnings(dir.create(regdir))
    }
    xmax <- max(samples[["longitude"]])
    ymax <- max(samples[["latitude"]])
    xmin <- min(samples[["longitude"]])
    ymin <- min(samples[["latitude"]])
    roi <- c(xmax = xmax, ymax = ymax, xmin = xmin, ymin = ymin)
    # load sentinel-2 cube
    s2_cube <- sits_cube(
        source = "AWS",
        collection = "SENTINEL-2-L2A",
        start_date = "2019-06-01",
        end_date = "2019-08-30",
        bands = c("B05", "CLOUD"),
        roi = roi,
        crs = 4326,
        progress = FALSE
    )
    s2_cube_reg <- suppressWarnings(sits_regularize(
        cube = s2_cube,
        period = "P16D",
        res = 232,
        multicores = 1,
        roi = roi,
        output_dir = regdir,
        progress = FALSE
    ))
    # load dem cube
    dem_cube <- sits_cube(
        source = "MPC",
        collection = "COP-DEM-GLO-30",
        roi = roi,
        crs = 4326,
        progress = FALSE
    )
    dem_cube_reg <- sits_regularize(
        cube = dem_cube,
        multicores = 1,
        res = 232,
        roi = roi,
        crs = 4326,
        output_dir = regdir,
        progress = FALSE
    )
    # create base cube
    base_cube <- sits_add_base_cube(s2_cube_reg, dem_cube_reg)

    # extract data
    samples_ts <- suppressMessages(sits_get_data(
        base_cube,
        samples = samples,
        multicores = 1,
        progress = FALSE
    ))
    # validations
    cube_timeline <- sits_timeline(base_cube)
    expect_equal(object = nrow(samples_ts), expected = 13)
    expect_equal(
        object = unique(samples_ts[["start_date"]]),
        expected = as.Date(cube_timeline[1])
    )
    expect_equal(
        object = unique(samples_ts[["end_date"]]),
        expected = as.Date(cube_timeline[length(cube_timeline)])
    )
    expect_true(
        all(
            c("sits_base", "sits", "tbl_df", "tbl", "data.frame") %in%
                class(samples_ts)
        )
    )

    unlink(s2_cube[["file_info"]][[1]]$path)
    unlink(s2_cube_reg[["file_info"]][[1]]$path)
    unlink(dem_cube[["file_info"]][[1]]$path)
    unlink(dem_cube_reg[["file_info"]][[1]]$path)
    unlink(base_cube[["file_info"]][[1]]$path)
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
    cerrado_samples <- cerrado_2classes
    class(cerrado_samples) <- "tbl_df"
    csv_file2 <- paste0(tempdir(), "/cerrado_2classes_2.csv")
    sits_to_csv(cerrado_samples, file = csv_file2)
    csv2 <- read.csv(csv_file2)
    expect_true(nrow(csv2) == 746)
    expect_true(all(names(csv2) %in% c(
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
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    output_dir <- paste0(tempdir(), "/al_1")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
    # classify a data cube
    probs_cube <- sits_classify(
        data = cube,
        ml_model = rfor_model,
        output_dir = output_dir,
        progress = FALSE
    )
    # smooth the probability cube using Bayesian statistics
    bayes_cube <- sits_smooth(probs_cube,
                              output_dir = output_dir,
                              progress = FALSE
    )
    # label the probability cube
    label_cube <- sits_label_classification(
        bayes_cube,
        output_dir = output_dir,
        progress = FALSE
    )

    # Using CSV
    csv_raster_file <- system.file("extdata/samples/samples_sinop_crop.csv",
                                   package = "sits"
    )
    points_poly <- sits_get_class(label_cube,
                                  samples = csv_raster_file
    )
    expect_equal(
        nrow(points_poly), nrow(read.csv(csv_raster_file))
    )
    expect_true(
        all(
            c("tbl_df", "tbl", "data.frame") %in%
                class(points_poly)
        )
    )
    expect_equal(
        colnames(points_poly), c(
            "longitude", "latitude",
            "label"
        )
    )
    # Using lat/long
    samples <- tibble::tibble(longitude = -55.66738, latitude = -11.76990)

    point_ndvi <- sits_get_class(label_cube, samples)
    expect_equal(nrow(point_ndvi), 1)

    expect_equal(
        colnames(point_ndvi), c(
            "longitude", "latitude",
            "label"
        )
    )
    unlink(probs_cube$file_info[[1]]$path)
    unlink(bayes_cube$file_info[[1]]$path)
    unlink(label_cube$file_info[[1]]$path)
})

test_that("Reading data from Classified data from STAC", {
    roi <- c(
        "lon_min" = -55.80259, "lon_max" = -55.19900,
        "lat_min" = -11.80208, "lat_max" = -11.49583
    )

    # load cube from stac
    class_cube <- .try(
        {
            sits_cube(
                source     = "TERRASCOPE",
                collection = "WORLD-COVER-2021",
                roi        = roi,
                progress   = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(class_cube),
                      message = "TERRASCOPE is not accessible"
    )

    # adapt date to work with the sinop samples
    class_cube[["file_info"]][[1]][["start_date"]] <- "2013-10-01"
    class_cube[["file_info"]][[1]][["end_date"]] <- "2013-10-01"
    # Using CSV
    csv_raster_file <- system.file("extdata/samples/samples_sinop_crop.csv",
                                   package = "sits"
    )
    points_poly <- suppressWarnings(
        sits_get_class(class_cube,
                       samples = csv_raster_file
        )
    )
    expect_equal(nrow(points_poly), 18)
    expect_equal(
        colnames(points_poly), c(
            "longitude", "latitude",
            "label"
        )
    )
    expect_true(
        all(
            c("tbl_df", "tbl", "data.frame") %in%
                class(points_poly)
        )
    )
})
