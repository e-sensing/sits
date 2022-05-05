test_that("Reading a LAT/LONG from RASTER", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    raster_cube <- tryCatch(
        {
            sits_cube(
                source = "BDC",
                collection = "MOD13Q1-6",
                data_dir = data_dir,
                delim = "_",
                parse_info = c("X1", "X2", "tile", "band", "date")
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
    expect_true(ncol(sits_time_series(point_ndvi)) == 2)
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
                parse_info = c("X1", "X2", "tile", "band", "date")
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
    points <- sits_get_data(raster_cube,
        samples = csv_raster_file,
        output_dir = tempdir()
    )

    df_csv <- utils::read.csv(
        system.file("extdata/samples/samples_sinop_crop.csv", package = "sits"),
        stringsAsFactors = FALSE
    )
    expect_true(nrow(points) <= nrow(df_csv))

    expect_true("Forest" %in% sits_labels(points))
    expect_equal(names(points)[1], "longitude")
    expect_equal(length(names(points)), 7)
    expect_true(ncol(sits_time_series(points)) == 2)
    expect_true(length(sits_timeline(points)) == 23)

    points_df <- sits_get_data(raster_cube,
        samples = df_csv,
        output_dir = tempdir()
    )

    expect_true("Forest" %in% sits_labels(points_df))
    expect_equal(names(points_df)[1], "longitude")
    expect_equal(length(names(points_df)), 7)
    expect_true(ncol(sits_time_series(points_df)) == 2)
    expect_true(length(sits_timeline(points_df)) == 23)
})

test_that("Test reading shapefile from BDC", {
    testthat::skip_on_cran()

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
