test_that("List collections", {
    col <- capture_output(sits_list_collections())
    expect_true(grepl("SENTINEL", col))
    expect_true(grepl("DEAFRICA", col))
    expect_true(grepl("LANDSAT", col))
    expect_true(grepl("BDC", col))
})

test_that("api_source", {
    res_s2_b8a <- .source_bands_resolution(
        source = "MPC",
        collection = "SENTINEL-2-L2A",
        bands = "B8A"
    )
    expect_equal(res_s2_b8a[["B8A"]], 20)
    res_l8_blue <- .source_bands_resolution(
        source = "MPC",
        collection = "LANDSAT-C2-L2",
        bands = "BLUE"
    )
    expect_equal(res_l8_blue[["BLUE"]], 30)

    vls_s2_cloud <- .source_cloud_values(
        source = "MPC",
        collection = "SENTINEL-2-L2A"
    )
    expect_true(all(names(vls_s2_cloud) %in%
                        as.character(seq(from = 0, to = 11))))
    expect_equal(vls_s2_cloud[["0"]], "missing_data")
    expect_equal(vls_s2_cloud[["11"]], "snow or ice")

    open_mpc <- .source_collection_open_data(
        source = "MPC",
        collection = "SENTINEL-2-L2A"
    )
    expect_true(open_mpc)
    token_mpc <- .source_collection_open_data_token(
        source = "MPC",
        collection = "SENTINEL-2-L2A"
    )
    expect_false(token_mpc)

    open_bdc <- .source_collection_open_data(
        source = "BDC",
        collection = "S2-SEN2COR_10_16D_STK-1"
    )
    expect_true(open_bdc)

    token_bdc <- .source_collection_open_data_token(
        source = "BDC",
        collection = "S2-SEN2COR_10_16D_STK-1"
    )
    expect_true(token_bdc)

})

test_that("Reading a raster cube", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")

    raster_cube <- sits_cube(
            source = "BDC",
            collection = "MOD13Q1-6",
            data_dir = data_dir,
            multicores = 2
    )

    # get bands names
    bands <- sits_bands(raster_cube)
    expect_true(all(bands %in% c("NDVI", "EVI")))

    params <- .raster_params_file(raster_cube$file_info[[1]]$path)
    expect_true(params$nrows == 147)
    expect_true(params$ncols == 255)
    expect_true(params$xres >= 231.5)

    timeline <- sits_timeline(raster_cube)

    sub_cube <- sits_select(raster_cube,
                            start_date = timeline[1],
                            end_date = timeline[2])
    expect_equal(length(sits_timeline(sub_cube)), 2)
    params_2 <- .raster_params_file(sub_cube$file_info[[1]]$path)
    expect_true(params_2$nrows == 147)
    expect_true(params_2$ncols == 255)
    expect_true(params_2$xres >= 231.5)
})

test_that("Creating cubes from BDC", {


    # check "BDC_ACCESS_KEY" - mandatory one per user
    bdc_access_key <- Sys.getenv("BDC_ACCESS_KEY")

    testthat::skip_if(nchar(bdc_access_key) == 0,
        message = "No BDC_ACCESS_KEY defined in environment."
    )

    # create a raster cube file based on the information about the files
    expect_message({
        cbers_cube <- .try({
            sits_cube(
                source = "BDC",
                collection = "CB4_64_16D_STK-1",
                tile = c("022024", "022023"),
                start_date = "2018-09-01",
                end_date = "2019-08-29"
            )
        },
        .default = NULL
        )
    },
    regexp = "please use tiles instead of tile as parameter"
    )

    testthat::skip_if(purrr::is_null(cbers_cube),
        message = "BDC is not accessible"
    )

    expect_true(all(sits_bands(cbers_cube) %in%
        c("NDVI", "EVI", "B13", "B14", "B15", "B16", "CLOUD")))
    bbox <- sits_bbox(cbers_cube)
    int_bbox <- .bbox_intersection(bbox, .tile_bbox(cbers_cube))
    expect_true(all(int_bbox == sits_bbox(.tile(cbers_cube))))

    timeline <- sits_timeline(cbers_cube)
    expect_true(timeline[1] <= as.Date("2018-09-01"))
    expect_true(timeline[length(timeline)] <= as.Date("2019-08-29"))

    r_obj <- .raster_open_rast(cbers_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(cbers_cube)
    expect_true(.raster_nrows(r_obj) == cube_nrows)

    fi_1 <- cbers_cube$file_info[[1]]
    cc <- .fi_cloud_cover(fi_1)
    expect_equal(length(cc), nrow(fi_1))
    expect_equal(cc[[1]], 0.00)

    fi_2 <- .fi_filter_fid(fi_1,
            fid = "CB4_64_16D_STK_v001_022024_2018-08-29_2018-09-13")
    expect_equal(length(sits_bands(cbers_cube)), nrow(fi_2))

    cc_2 <- .fi_cloud_cover(fi_2)
    expect_true(all(cc_2 %in% c(0.00)))
})

test_that("Creating cubes from BDC - based on ROI with shapefile", {


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

    expect_true(all(sits_bands(modis_cube) %in% c("NDVI", "EVI")))
    bbox <- sits_bbox(modis_cube, as_crs = "EPSG:4326")
    bbox_shp <- sf::st_bbox(sf_mt)

    expect_lt(bbox["xmin"], bbox_shp["xmin"])
    expect_lt(bbox["ymin"], bbox_shp["ymin"])
    expect_gt(bbox["xmax"], bbox_shp["xmax"])
    expect_gt(bbox["ymax"], bbox_shp["ymax"])
    intersects <- .cube_intersects(modis_cube, sf_mt)
    expect_true(all(intersects))
})

test_that("Creating cubes from BDC - invalid roi", {


    # check "BDC_ACCESS_KEY" - mandatory one per user
    bdc_access_key <- Sys.getenv("BDC_ACCESS_KEY")

    testthat::skip_if(nchar(bdc_access_key) == 0,
        message = "No BDC_ACCESS_KEY defined in environment."
    )

    expect_error(
        object = sits_cube(
            source = "BDC",
            collection = "MOD13Q1-6",
            bands = c("NDVI", "EVI"),
            roi = c(TRUE, FALSE),
            start_date = "2018-09-01",
            end_date = "2019-08-29"
        )
    )

    expect_error(
        object = sits_cube(
            source = "BDC",
            collection = "MOD13Q1-6",
            bands = c("NDVI", "EVI"),
            roi = c(
                lon_min = -55.20997,
                lat_min = 15.40554,
                lon_max = -55.19883,
                lat_max = -15.39179
            ),
            tiles = "012010",
            start_date = "2018-09-01",
            end_date = "2019-08-29"
        )
    )
})

test_that("Creating cubes from DEA", {

    dea_cube <- .try({
        sits_cube(
            source = "DEAFRICA",
            collection = "s2_l2a",
            bands = c("B01", "B04", "B05"),
            roi = c(
                lon_min = 17.379,
                lat_min = 1.1573,
                lon_max = 17.410,
                lat_max = 1.1910
            ),
            start_date = "2019-01-01",
            end_date = "2019-10-28"
        )
    },
    .default = NULL
    )

    testthat::skip_if(purrr::is_null(dea_cube),
        message = "DEAFRICA is not accessible"
    )

    expect_true(all(sits_bands(dea_cube) %in% c("B01", "B04", "B05")))

    r <- .raster_open_rast(.tile_path(dea_cube))

    expect_equal(dea_cube$xmax[[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(dea_cube$xmin[[1]], .raster_xmin(r), tolerance = 1)
})

test_that("Creating cubes from DEA - error using tiles", {


    expect_error(
        object = {
            dea_cube <-
                sits_cube(
                    source = "DEAFRICA",
                    collection = "s2_l2a",
                    bands = c("B01", "B04", "B05"),
                    tiles = "37MEP",
                    start_date = "2019-01-01",
                    end_date = "2019-10-28"
                )
        },
        regexp = "DEAFRICA cubes do not support searching for tiles"
    )
})



test_that("Creating cubes from USGS", {
    # check "AWS_ACCESS_KEY_ID" - mandatory one per user
    aws_access_key_id <- Sys.getenv("AWS_ACCESS_KEY_ID")

    # check "AWS_SECRET_ACCESS_KEY" - mandatory one per user
    aws_secret_access_key <- Sys.getenv("AWS_SECRET_ACCESS_KEY")

    testthat::skip_if(
        nchar(aws_access_key_id) == 0,
        message = "No AWS_ACCESS_KEY_ID defined in environment."
    )

    testthat::skip_if(
        nchar(aws_secret_access_key) == 0,
        message = "No AWS_SECRET_ACCESS_KEY defined in environment."
    )

    Sys.unsetenv("AWS_DEFAULT_REGION")
    Sys.unsetenv("AWS_S3_ENDPOINT")
    Sys.unsetenv("AWS_REQUEST_PAYER")

    usgs_cube_1 <- .try({
        sits_cube(
            source = "USGS",
            collection = "landsat-c2l2-sr",
            bands = c("GREEN", "CLOUD"),
            roi = c(
                "xmin" = 17.379,
                "ymin" = 1.1573,
                "xmax" = 17.410,
                "ymax" = 1.1910,
                "crs" = 4326
            ),
            start_date = "2019-01-01",
            end_date = "2019-02-01"
        )
    },
    .default = NULL
    )

    testthat::skip_if(
        purrr::is_null(usgs_cube_1),
        "USGS is not accessible"
    )
    expect_true(all(sits_bands(usgs_cube_1) %in% c("GREEN", "CLOUD")))

    r <- .raster_open_rast(.tile_path(usgs_cube_1))

    expect_equal(usgs_cube_1$xmax[[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(usgs_cube_1$xmin[[1]], .raster_xmin(r), tolerance = 1)

    usgs_cube_2 <- .try({
        sits_cube(
            source = "USGS",
            collection = "landsat-c2l2-sr",
            bands = c("GREEN", "CLOUD"),
            tiles = "223067",
            start_date = "2019-01-01",
            end_date = "2019-10-28"
        )
    },
    .default = NULL
    )

    testthat::skip_if(
        purrr::is_null(usgs_cube_2),
        "USGS is not accessible"
    )

    expect_true(all(sits_bands(usgs_cube_2) %in% c("GREEN", "CLOUD")))

    r <- .raster_open_rast(.tile_path(usgs_cube_2))

    expect_equal(usgs_cube_2$xmax[[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(usgs_cube_2$xmin[[1]], .raster_xmin(r), tolerance = 1)
})

test_that("Creating Sentinel cubes from MPC", {
    s2_cube <- .try({
        sits_cube(
            source = "MPC",
            collection = "SENTINEL-2-L2A",
            tiles = "20LKP",
            bands = c("B05", "CLOUD"),
            start_date = as.Date("2018-07-18"),
            end_date = as.Date("2018-08-23")
        )
    },
    .default = NULL
    )

    testthat::skip_if(
        purrr::is_null(s2_cube),
        "MPC is not accessible"
    )

    expect_true(all(sits_bands(s2_cube) %in% c("B05", "CLOUD")))

    r <- .raster_open_rast(.tile_path(s2_cube))

    expect_equal(s2_cube$xmax[[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(s2_cube$xmin[[1]], .raster_xmin(r), tolerance = 1)
})

test_that("Creating Sentinel cubes from MPC with ROI", {

    roi <- c(lon_min = -48.28579, lat_min = -16.05026,
             lon_max = -47.30839, lat_max = -15.50026)

    s2_cube <- .try({
        sits_cube(
            source = "MPC",
            collection = "SENTINEL-2-L2A",
            roi = roi,
            bands = c("B05", "CLOUD"),
            start_date = as.Date("2018-07-18"),
            end_date = as.Date("2018-08-23")
        )
    },
    .default = NULL
    )

    testthat::skip_if(purrr::is_null(s2_cube), "MPC is not accessible")

    expect_true(all(sits_bands(s2_cube) %in% c("B05", "CLOUD")))
    expect_equal(nrow(s2_cube), 3)
    expect_warning(
        object = sits_bbox(s2_cube),
        regexp = "object has multiples CRS values"
    )

    bbox_cube <- sits_bbox(s2_cube, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(s2_cube), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])

    expect_warning(
        object = sits_timeline(s2_cube),
        regexp = "cube is not regular, returning all timelines"
    )
})

test_that("Creating Harmonized Landsat Sentinel cubes from HLS", {
    roi <- c(lon_min = -48.28579, lat_min = -16.05026,
             lon_max = -47.30839, lat_max = -15.50026)

    hls_cube_s2 <- .try({
        sits_cube(
            source = "HLS",
            collection = "HLSS30",
            roi = roi,
            bands = c("B04", "CLOUD"),
            start_date = as.Date("2019-05-01"),
            end_date = as.Date("2019-09-01")
        )
    },
    .default = NULL
    )

    testthat::skip_if(
        purrr::is_null(hls_cube_s2),
        "HLSS30 collection is not accessible"
    )

    expect_true(all(sits_bands(hls_cube_l8) %in% c("B04", "CLOUD")))
    expect_equal(hls_cube_l8$satellite, "SENTINEL-2")
    expect_equal(hls_cube_s2$tile, "23LKC")
    expect_true(all(.fi(hls_cube_s2)$xres == 30))
    expect_true(all(.fi(hls_cube_s2)$yres == 30))

    hls_cube_l8 <- .try({
        sits_cube(
            source = "HLS",
            collection = "HLSL30",
            roi = roi,
            bands = c("B04", "CLOUD"),
            start_date = as.Date("2019-05-01"),
            end_date = as.Date("2019-09-01")
        )
    },
    .default = NULL
    )

    testthat::skip_if(
        purrr::is_null(hls_cube_l8),
        "HLSL30 collection is not accessible"
    )

    expect_true(all(sits_bands(hls_cube_l8) %in% c("B04", "CLOUD")))
    expect_equal(hls_cube_l8$satellite, "LANDSAT-8")
    expect_equal(hls_cube_l8$tile, "23LKC")
    expect_true(all(.fi(hls_cube_l8)$xres == 30))
    expect_true(all(.fi(hls_cube_l8)$yres == 30))
})





test_that("Creating a raster stack cube with BDC band names", {
    # Create a raster cube based on CBERS data
    data_dir <- system.file("extdata/raster/bdc", package = "sits")

    # create a raster cube file based on the information about the files
    cbers_cube_bdc <- .try({
        sits_cube(
            source = "BDC",
            collection = "CB4_64-1",
            data_dir = data_dir,
            parse_info = c(
                "X1", "X2", "X3", "X4", "X5", "tile",
                "date", "X6", "band"
            ),
            multicores = 2
        )
    },
    .default = NULL
    )

    testthat::skip_if(purrr::is_null(cbers_cube_bdc),
        message = "LOCAL cube not found"
    )

    expect_true(all(sits_bands(cbers_cube_bdc) %in% "B16"))
})
