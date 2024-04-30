test_that("List collections", {
    col <- capture_output(sits_list_collections())
    expect_true(grepl("SENTINEL", col))
    expect_true(grepl("DEAFRICA", col))
    expect_true(grepl("LANDSAT", col))
    expect_true(grepl("BDC", col))
    expect_true(grepl("CDSE", col))
    col_bdc <- capture_output(sits_list_collections(source = "BDC"))
    expect_true(grepl("CBERS-WFI-16D", col_bdc))
    expect_true(grepl("CBERS-WFI-8D", col_bdc))
})
test_that("api_source", {
    res_s2_b2 <- .source_bands_resolution(
        source = "CDSE",
        collection = "SENTINEL-2-L2A",
        bands = "B02"
    )
    expect_equal(res_s2_b2[["B02"]], 10)
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
    token_mpc <- .source_collection_open_data(
        source = "MPC",
        collection = "SENTINEL-2-L2A",
        token = TRUE
    )
    expect_false(token_mpc)

    open_bdc <- .source_collection_open_data(
        source = "BDC",
        collection = "SENTINEL-2-16D"
    )
    expect_true(open_bdc)

    token_bdc <- .source_collection_open_data(
        source = "BDC",
        collection = "SENTINEL-2-16D",
        token = TRUE
    )
    expect_true(token_bdc)
})
test_that("Reading a raster cube", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    raster_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        tiles = "012010",
        bands = "NDVI",
        start_date = "2013-09-14",
        end_date = "2014-08-29",
        multicores = 2,
        progress = FALSE
    )
    # get bands names
    bands <- sits_bands(raster_cube)
    expect_true(all(bands %in% c("NDVI", "EVI")))
    # test params
    params <- .raster_params_file(raster_cube$file_info[[1]]$path)
    expect_true(params$nrows == 147)
    expect_true(params$ncols == 255)
    expect_true(params$xres >= 231.5)
    # test timeline
    timeline <- sits_timeline(raster_cube)
    sub_cube <- sits_select(raster_cube,
        start_date = timeline[1],
        end_date = timeline[2]
    )
    expect_equal(length(sits_timeline(sub_cube)), 2)
    params_2 <- .raster_params_file(sub_cube$file_info[[1]]$path)
    expect_true(params_2$nrows == 147)
    expect_true(params_2$ncols == 255)
    expect_true(params_2$xres >= 231.5)
})

test_that("Creating cubes from BDC - CBERS-WFI-16D", {
    tiles <- c("007004", "007005")
    start_date <- "2021-09-01"
    end_date <- "2021-09-30"
    bands <- c("NDVI", "EVI", "B13", "B14", "B15", "B16", "CLOUD")
    # create a raster cube
    cbers_cube_16d <- .try(
        {
            sits_cube(
                source = "BDC",
                collection = "CBERS-WFI-16D",
                tiles = tiles,
                start_date = start_date,
                end_date = end_date,
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(purrr::is_null(cbers_cube_16d),
                      message = "BDC is not accessible"
    )
    # test bands and bbox
    expect_true(all(sits_bands(cbers_cube_16d) %in% bands))
    bbox <- sits_bbox(cbers_cube_16d)
    int_bbox <- .bbox_intersection(bbox, .tile_bbox(cbers_cube_16d))
    expect_true(all(int_bbox == sits_bbox(.tile(cbers_cube_16d))))
    # test timeline
    timeline <- sits_timeline(cbers_cube_16d)
    expect_true(timeline[1] <= as.Date(start_date))
    expect_true(timeline[length(timeline)] <= as.Date(end_date))
    # test raster obj
    r_obj <- .raster_open_rast(cbers_cube_16d$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(cbers_cube_16d)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
})
test_that("Creating cubes from BDC - CBERS-WFI-8D", {
    tiles <- c("007004", "007005")
    start_date <- "2022-05-01"
    end_date <- "2022-08-29"
    bands <- c("NDVI", "EVI", "B13", "B14", "B15", "B16", "CLOUD")
    # create a raster cube file from BDC
    cbers_cube_8d <- .try(
        {
            sits_cube(
                source = "BDC",
                collection = "CBERS-WFI-8D",
                tiles = tiles,
                start_date = start_date,
                end_date = end_date,
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(purrr::is_null(cbers_cube_8d),
        message = "BDC is not accessible"
    )
    expect_true(all(sits_bands(cbers_cube_8d) %in% bands))
    bbox <- sits_bbox(cbers_cube_8d)
    int_bbox <- .bbox_intersection(bbox, .tile_bbox(cbers_cube_8d))
    expect_true(all(int_bbox == sits_bbox(.tile(cbers_cube_8d))))

    timeline <- sits_timeline(cbers_cube_8d)
    expect_true(timeline[1] <= as.Date(start_date))
    expect_true(timeline[length(timeline)] <= as.Date(end_date))

    r_obj <- .raster_open_rast(cbers_cube_8d$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(cbers_cube_8d)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
})
test_that("Creating cubes from BDC - based on ROI using sf obejct", {
    shp_file <- system.file(
        "extdata/shapefiles/mato_grosso/mt.shp",
        package = "sits"
    )
    sf_mt <- sf::read_sf(shp_file)
    # create a raster cube file based on the information about the files
    modis_cube <- .try(
        {
            sits_cube(
                source = "BDC",
                collection = "MOD13Q1-6",
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
    expect_true(all(sits_bands(modis_cube) %in% c("NDVI", "EVI")))
    bbox <- sits_bbox(modis_cube, as_crs = "EPSG:4326")
    bbox_shp <- sf::st_bbox(sf_mt)
    expect_lt(bbox["xmin"], bbox_shp["xmin"])
    expect_lt(bbox["ymin"], bbox_shp["ymin"])
    expect_gt(bbox["xmax"], bbox_shp["xmax"])
    expect_gt(bbox["ymax"], bbox_shp["ymax"])
    intersects <- .cube_intersects(modis_cube, sf_mt)
    expect_true(all(intersects))

    modis_cube2 <- modis_cube
    class(modis_cube2) <- "data.frame"
    in2 <- .cube_intersects(modis_cube2, sf_mt)
    expect_true(all(in2))
    expect_true(.tile_intersects(modis_cube2[1,], sf_mt))

    expect_false(.tile_within(modis_cube2[1,], sf_mt))
    expect_false(.tile_within(modis_cube2[6,], sf_mt))

    modis_cube3 <- .cube_filter_spatial(modis_cube2, sf_mt)
    expect_equal(nrow(modis_cube2), nrow(modis_cube3))

    modis_cube4 <- .cube_filter_bands(modis_cube2, "EVI")
    expect_true(.cube_bands(modis_cube4) %in% .cube_bands(modis_cube2))
    tile <- modis_cube2[1,]
    modis_evi <- .tile_filter_bands(tile, "EVI")
    expect_equal("EVI", sits_bands(modis_evi))

    modis_tiles <- .cube_tiles(modis_cube2)
    expect_true(all(c("011009", "012010") %in% .cube_tiles(modis_cube)))

    tile_011009 <- .cube_filter_tiles(modis_cube, "011009")
    expect_equal(nrow(tile_011009), 1)

})
test_that("Creating cubes from BDC - based on ROI using shapefile", {
    shp_file <- system.file(
        "extdata/shapefiles/mato_grosso/mt.shp",
        package = "sits"
    )
    # create a raster cube file based on the information about the files
    modis_cube <- .try(
        {
            sits_cube(
                source = "BDC",
                collection = "MOD13Q1-6",
                bands = c("NDVI", "EVI"),
                roi = shp_file,
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
    expect_true(all(sits_bands(modis_cube) %in% c("NDVI", "EVI")))
    bbox <- sits_bbox(modis_cube, as_crs = "EPSG:4326")
    sf_mt <- sf::read_sf(shp_file)
    bbox_shp <- sf::st_bbox(sf_mt)
    expect_lt(bbox["xmin"], bbox_shp["xmin"])
    expect_lt(bbox["ymin"], bbox_shp["ymin"])
    expect_gt(bbox["xmax"], bbox_shp["xmax"])
    expect_gt(bbox["ymax"], bbox_shp["ymax"])
    intersects <- .cube_intersects(modis_cube, sf_mt)
    expect_true(all(intersects))

    tile_011009 <- .cube_filter_tiles(modis_cube, "011009")
    expect_equal(nrow(tile_011009), 1)

})
test_that("Creating cubes from BDC - invalid roi", {
    expect_error(
        object = sits_cube(
            source = "BDC",
            collection = "MOD13Q1-6",
            bands = c("NDVI", "EVI"),
            roi = c(TRUE, FALSE),
            start_date = "2018-09-01",
            end_date = "2019-08-29",
            progress = FALSE
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
            end_date = "2019-08-29",
            progress = FALSE
        )
    )
})
test_that("Creating cubes from BDC - LANDSAT per tile", {
    tile <- "038046"
    start_date <- "2021-05-01"
    end_date <- "2021-09-30"
    bands <- c("NDVI", "EVI")
    # create a raster cube file based on the information about the files
    bdc_l8_cube <- .try(
        {
            sits_cube(
                source = "BDC",
                collection = "LANDSAT-OLI-16D",
                bands = bands,
                tiles = tile,
                start_date = start_date,
                end_date = end_date,
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(bdc_l8_cube),
        message = "BDC cube LANDSAT-OLI-16D is not accessible"
    )
    expect_equal(bdc_l8_cube$tile, tile)
    expect_true(all(sits_bands(bdc_l8_cube) %in% bands))
    # test timeline
    timeline <- sits_timeline(bdc_l8_cube)
    expect_true(timeline[1] <= as.Date(start_date))
    expect_true(timeline[length(timeline)] <= as.Date(end_date))
    # test raster obj
    r_obj <- .raster_open_rast(bdc_l8_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(bdc_l8_cube)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
})
test_that("Creating cubes from BDC - LANDSAT per roi", {
    roi <- c(
        lon_min = -53.9311, lat_min = -13.2697,
        lon_max = -53.0595, lat_max = -12.6704
    )
    start_date <- "2021-05-01"
    end_date <- "2021-09-30"
    bands <- c("NDVI", "EVI")
    # create a raster cube file based on the information about the files
    bdc_l8_cube <- .try(
        {
            sits_cube(
                source = "BDC",
                collection = "LANDSAT-OLI-16D",
                bands = bands,
                roi = roi,
                start_date = start_date,
                end_date = end_date,
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(bdc_l8_cube),
        message = "BDC cube LANDSAT-OLI-16D is not accessible"
    )
    expect_true(all(sits_bands(bdc_l8_cube) %in% bands))
    bbox_cube <- sits_bbox(bdc_l8_cube, as_crs = "EPSG:4326")
    intersects <- .cube_intersects(bdc_l8_cube, roi)
    expect_true(all(intersects))
    # test timeline
    timeline <- sits_timeline(bdc_l8_cube)
    expect_true(timeline[1] <= as.Date(start_date))
    expect_true(timeline[length(timeline)] <= as.Date(end_date))
    # test raster obj
    r_obj <- .raster_open_rast(bdc_l8_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(bdc_l8_cube)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
})
test_that("Creating cubes from BDC - SENTINEL-2 - roi", {
    roi <- c(
        lon_min = -53.9311, lat_min = -13.2697,
        lon_max = -53.0595, lat_max = -12.6704
    )
    start_date <- "2021-05-01"
    end_date <- "2021-09-30"
    bands <- c("NDVI", "EVI")
    # create a raster cube
    bdc_s2_cube <- .try(
        {
            sits_cube(
                source = "BDC",
                collection = "SENTINEL-2-16D",
                bands = bands,
                roi = roi,
                start_date = start_date,
                end_date = end_date,
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(purrr::is_null(bdc_s2_cube),
        message = "BDC cube SENTINEL-2-16D is not accessible"
    )
    expect_true(all(sits_bands(bdc_s2_cube) %in% c("NDVI", "EVI")))
    bbox_cube <- sits_bbox(bdc_s2_cube, as_crs = "EPSG:4326")
    intersects <- .cube_intersects(bdc_s2_cube, roi)
    expect_true(all(intersects))
    # test timeline
    timeline <- sits_timeline(bdc_s2_cube)
    expect_true(timeline[1] <= as.Date(start_date))
    expect_true(timeline[length(timeline)] <= as.Date(end_date))
    # test raster obj
    r_obj <- .raster_open_rast(bdc_s2_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(bdc_s2_cube)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
})
test_that("Creating cubes from BDC - SENTINEL-2 - tile", {
    start_date <- "2021-05-01"
    end_date <- "2021-09-30"
    bands <- c("NDVI", "EVI")
    tiles <- "021019"
    # create a raster cube file
    bdc_s2_cube_t <- .try(
        {
            sits_cube(
                source = "BDC",
                collection = "SENTINEL-2-16D",
                bands = bands,
                tiles = tiles,
                start_date = start_date,
                end_date = end_date,
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(bdc_s2_cube_t),
        message = "BDC cube SENTINEL-2-16D is not accessible"
    )
    expect_true(all(sits_bands(bdc_s2_cube_t) %in% c("NDVI", "EVI")))
    # test timeline
    timeline <- sits_timeline(bdc_s2_cube_t)
    expect_true(timeline[1] <= as.Date(start_date))
    expect_true(timeline[length(timeline)] <= as.Date(end_date))
    # test raster obj
    r_obj <- .raster_open_rast(bdc_s2_cube_t$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(bdc_s2_cube_t)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
})

test_that("Creating LS5-SR cubes from DEA", {
    landsat_cube <-  sits_cube(
        source = "DEAFRICA",
        collection = "LS5-SR",
        bands = c("B05", "CLOUD"),
        roi   = c(
            lon_min = 33.546,
            lon_max = 34.999,
            lat_min = 1.427,
            lat_max = 3.726
        ),
        start_date = "2009-01-01",
        end_date = "2010-01-01",
        progress = FALSE
    )

    testthat::skip_if(purrr::is_null(landsat_cube),
                      message = "DEAFRICA is not accessible"
    )

    expect_true(all(sits_bands(landsat_cube) %in% c("B05", "CLOUD")))
    expect_equal(nrow(landsat_cube), 6)
    bbox_cube <- sits_bbox(landsat_cube, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(landsat_cube), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    r_obj <- .raster_open_rast(landsat_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(landsat_cube)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
})
test_that("Creating LS7-SR cubes from DEA", {
    landsat_cube <-  sits_cube(
        source = "DEAFRICA",
        collection = "LS7-SR",
        bands = c("B05", "CLOUD"),
        roi   = c(
            lon_min = 33.546,
            lon_max = 34.999,
            lat_min = 1.427,
            lat_max = 3.726
        ),
        start_date = "2009-01-01",
        end_date = "2010-01-01",
        progress = FALSE
    )

    testthat::skip_if(purrr::is_null(landsat_cube),
                      message = "DEAFRICA is not accessible"
    )

    expect_true(all(sits_bands(landsat_cube) %in% c("B05", "CLOUD")))
    expect_equal(nrow(landsat_cube), 6)
    bbox_cube <- sits_bbox(landsat_cube, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(landsat_cube), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    r_obj <- .raster_open_rast(landsat_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(landsat_cube)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
})
test_that("Creating LS8-SR cubes from DEA", {
    landsat_cube <-  sits_cube(
        source = "DEAFRICA",
        collection = "LS8-SR",
        bands = c("B05", "CLOUD"),
        roi   = c(
            lon_min = 33.546,
            lon_max = 34.999,
            lat_min = 1.427,
            lat_max = 3.726
        ),
        start_date = "2021-01-01",
        end_date = "2021-04-29",
        progress = FALSE
    )

    testthat::skip_if(purrr::is_null(landsat_cube),
                      message = "DEAFRICA is not accessible"
    )

    expect_true(all(sits_bands(landsat_cube) %in% c("B05", "CLOUD")))
    expect_equal(nrow(landsat_cube), 6)
    bbox_cube <- sits_bbox(landsat_cube, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(landsat_cube), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    r_obj <- .raster_open_rast(landsat_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(landsat_cube)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
})
test_that("Creating LS9-SR cubes from DEA", {
    landsat_cube <-  sits_cube(
        source = "DEAFRICA",
        collection = "LS9-SR",
        bands = c("B05", "CLOUD"),
        roi   = c(
            lon_min = 33.546,
            lon_max = 34.999,
            lat_min = 1.427,
            lat_max = 3.726
        ),
        start_date = "2022-01-01",
        end_date = "2022-04-01",
        progress = FALSE
    )

    testthat::skip_if(purrr::is_null(landsat_cube),
                      message = "DEAFRICA is not accessible"
    )

    expect_true(all(sits_bands(landsat_cube) %in% c("B05", "CLOUD")))
    expect_equal(nrow(landsat_cube), 6)
    bbox_cube <- sits_bbox(landsat_cube, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(landsat_cube), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    r_obj <- .raster_open_rast(landsat_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(landsat_cube)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
})
test_that("Creating S2 cubes from DEA using ROI", {
    # try to create a DEA cube
    dea_cube <- .try(
        {
            sits_cube(
                source = "DEAFRICA",
                collection = "SENTINEL-2-L2A",
                bands = c("B01", "B04", "B05"),
                roi = c(
                    lon_min = 17.379,
                    lat_min = 1.1573,
                    lon_max = 17.410,
                    lat_max = 1.1910
                ),
                start_date = "2019-01-01",
                end_date = "2019-10-28",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(dea_cube),
        message = "DEAFRICA is not accessible"
    )

    expect_true(all(sits_bands(dea_cube) %in% c("B01", "B04", "B05")))
    expect_equal(nrow(dea_cube), 1)
    r <- .raster_open_rast(.tile_path(dea_cube))
    expect_equal(dea_cube$xmax[[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(dea_cube$xmin[[1]], .raster_xmin(r), tolerance = 1)
})
test_that("Creating S2 cubes from DEA using tiles", {
    dea_cube <- .try(
        {
            sits_cube(
                source = "DEAFRICA",
                collection = "SENTINEL-2-L2A",
                bands = c("B02", "B8A", "B11"),
                tiles = c("37MDT","37MET"),
                start_date = "2019-01-01",
                end_date = "2019-08-28",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(dea_cube),
                      message = "DEAFRICA is not accessible"
    )

    expect_true(all(sits_bands(dea_cube) %in% c("B02", "B8A", "B11")))
    expect_equal(nrow(dea_cube), 2)
    r <- .raster_open_rast(.tile_path(dea_cube))
    expect_equal(dea_cube$xmax[[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(dea_cube$xmin[[1]], .raster_xmin(r), tolerance = 1)
    expect_true(all(dea_cube$tile %in% c("37MDT","37MET")))
})
test_that("Creating Sentinel-1 RTC cubes from DEA using ROI", {
    cube_s1_rtc <-  sits_cube(
        source = "DEAFRICA",
        collection = "SENTINEL-1-RTC",
        bands = c("VV"),
        orbit = "descending",
        roi   = c(
            lon_min = 17.379,
            lat_min = 1.1573,
            lon_max = 17.410,
            lat_max = 1.1910
        ),
        start_date = "2021-07-01",
        end_date = "2021-09-30",
        progress = FALSE
    )

    testthat::skip_if(purrr::is_null(cube_s1_rtc),
                      message = "DEAFRICA is not accessible"
    )

    expect_true(sits_bands(cube_s1_rtc) == "VV")
    expect_equal(nrow(cube_s1_rtc), 1)
    r <- .raster_open_rast(.tile_path(cube_s1_rtc))
    expect_equal(cube_s1_rtc$xmax[[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(cube_s1_rtc$xmin[[1]], .raster_xmin(r), tolerance = 1)
})
test_that("Creating Sentinel-1 RTC cubes from DEA using tiles", {
    cube_s1_rtc <-  sits_cube(
        source = "DEAFRICA",
        collection = "SENTINEL-1-RTC",
        bands = c("VV"),
        orbit = "ascending",
        tiles = c("36NWJ"),
        start_date = "2022-01-01",
        end_date = "2022-02-25",
        progress = FALSE
    )

    testthat::skip_if(purrr::is_null(cube_s1_rtc),
                      message = "DEAFRICA is not accessible"
    )

    bbox <- sits_bbox(cube_s1_rtc)
    roi_cube_s1 <- sits_mgrs_to_roi(c("36NWJ"))

    expect_true(bbox[["xmin"]] < roi_cube_s1[["lon_min"]])
    expect_true(bbox[["xmax"]] > roi_cube_s1[["lon_max"]])
    expect_true(bbox[["ymin"]] < roi_cube_s1[["lat_min"]])
    expect_true(bbox[["ymax"]] > roi_cube_s1[["lat_max"]])
    expect_true(all(c("VV") %in% sits_bands(cube_s1_rtc)))

    r_obj <- .raster_open_rast(cube_s1_rtc$file_info[[1]]$path[[1]])
    expect_true(terra::nrow(r_obj) == cube_s1_rtc$file_info[[1]]$nrows[[1]])

    output_dir <- paste0(tempdir(), "/s1-rtc-reg")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }

    cube_s1_reg <- sits_regularize(
        cube = cube_s1_rtc,
        period = "P1M",
        res = 240,
        tiles = c("36NWJ"),
        multicores = 1,
        output_dir = output_dir,
        progress = TRUE
    )
    expect_equal(length(sits_timeline(cube_s1_reg)), 2)
    expect_true("36NWJ" %in% cube_s1_reg$tile)
    expect_true(all("EPSG:32636" %in% cube_s1_reg$crs))

    bbox <- sits_bbox(cube_s1_reg, as_crs = "EPSG:4326")
    roi_cube_s1 <- sits_mgrs_to_roi("36NWJ")

    expect_equal(bbox[["xmin"]], roi_cube_s1[["lon_min"]], tolerance = 0.01)
    expect_equal(bbox[["xmax"]], roi_cube_s1[["lon_max"]], tolerance = 0.01)
    expect_equal(bbox[["ymin"]], roi_cube_s1[["lat_min"]], tolerance = 0.01)
    expect_equal(bbox[["ymax"]], roi_cube_s1[["lat_max"]], tolerance = 0.01)

    expect_true(all(c("VV") %in% sits_bands(cube_s1_reg)))
})
test_that("Creating ALOS-PALSAR-MOSAIC cubes from DEA", {
    cube_alos <-  sits_cube(
        source = "DEAFRICA",
        collection = "ALOS-PALSAR-MOSAIC",
        bands = c("HH", "HV", "CLOUD"),
        roi   = c(
            lon_min = 17.379,
            lat_min = 1.1573,
            lon_max = 17.410,
            lat_max = 1.1910
        ),
        start_date = "2008-01-01",
        end_date = "2009-01-01",
        progress = FALSE
    )

    testthat::skip_if(purrr::is_null(cube_alos),
                      message = "DEAFRICA is not accessible"
    )

    expect_true(all(sits_bands(cube_alos) %in% c("HH", "HV", "CLOUD")))
    expect_equal(nrow(cube_alos), 1)
    bbox_cube <- sits_bbox(cube_alos, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(cube_alos), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    r_obj <- .raster_open_rast(cube_alos$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(cube_alos)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
})
test_that("Creating NDVI-ANOMALY cubes from DEA", {
    cube_ndvi <-  sits_cube(
        source = "DEAFRICA",
        collection = "NDVI-ANOMALY",
        bands = c("NDVI-MEAN"),
        roi   = c(
            lon_min = 17.379,
            lat_min = 1.1573,
            lon_max = 17.410,
            lat_max = 1.1910
        ),
        start_date = "2018-06-01",
        end_date = "2018-12-31",
        progress = FALSE
    )

    testthat::skip_if(purrr::is_null(cube_ndvi),
                      message = "DEAFRICA is not accessible"
    )

    expect_true(sits_bands(cube_ndvi) == "NDVI-MEAN")
    expect_equal(nrow(cube_ndvi), 1)
    bbox_cube <- sits_bbox(cube_ndvi, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(cube_ndvi), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    r_obj <- .raster_open_rast(cube_ndvi$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(cube_ndvi)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
})
test_that("Creating RAINFALL-CHIRPS-DAILY cubes from DEA", {
    cube_chirps <-  sits_cube(
        source = "DEAFRICA",
        collection = "RAINFALL-CHIRPS-DAILY",
        bands = c("RAINFALL"),
        roi   = c(
            lon_min = 17.379,
            lat_min = 1.1573,
            lon_max = 17.410,
            lat_max = 1.1910
        ),
        start_date = "2018-06-01",
        end_date = "2018-08-25",
        progress = FALSE
    )

    testthat::skip_if(purrr::is_null(cube_chirps),
                      message = "DEAFRICA is not accessible"
    )

    expect_true(sits_bands(cube_chirps) == "RAINFALL")
    expect_equal(nrow(cube_chirps), 1)
    bbox_cube <- sits_bbox(cube_chirps, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(cube_chirps), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    r_obj <- .raster_open_rast(cube_chirps$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(cube_chirps)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
})
test_that("Creating RAINFALL-CHIRPS-MONTHLY cubes from DEA", {
    cube_chirps <-  sits_cube(
        source = "DEAFRICA",
        collection = "RAINFALL-CHIRPS-MONTHLY",
        bands = c("RAINFALL"),
        roi   = c(
            lon_min = 17.379,
            lat_min = 1.1573,
            lon_max = 17.410,
            lat_max = 1.1910
        ),
        start_date = "2018-06-01",
        end_date = "2018-08-25",
        progress = FALSE
    )

    testthat::skip_if(purrr::is_null(cube_chirps),
                      message = "DEAFRICA is not accessible"
    )

    expect_true(sits_bands(cube_chirps) == "RAINFALL")
    expect_equal(nrow(cube_chirps), 1)
    bbox_cube <- sits_bbox(cube_chirps, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(cube_chirps), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    r_obj <- .raster_open_rast(cube_chirps$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(cube_chirps)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
})
test_that("Creating DEM-COP-30 cubes from DEA", {
    cube_dem <-  sits_cube(
        source = "DEAFRICA",
        collection = "DEM-COP-30",
        bands = c("ELEVATION"),
        roi   = c(
            lon_min = 17.379,
            lat_min = 1.1573,
            lon_max = 17.410,
            lat_max = 1.1910
        ),
        start_date = "1899-01-01",
        end_date = "1901-01-01",
        progress = FALSE
    )

    testthat::skip_if(purrr::is_null(cube_dem),
                      message = "DEAFRICA is not accessible"
    )

    expect_true(sits_bands(cube_dem) == "ELEVATION")
    expect_equal(nrow(cube_dem), 1)
    bbox_cube <- sits_bbox(cube_dem, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(cube_dem), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    r_obj <- .raster_open_rast(cube_dem$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(cube_dem)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
})

test_that("Creating S2 cubes from MPC using tiles", {
    mpc_token <- Sys.getenv("MPC_TOKEN")
    Sys.setenv("MPC_TOKEN" = "")
    s2_cube <- .try(
        {
            sits_cube(
                source = "MPC",
                collection = "SENTINEL-2-L2A",
                tiles = "20LKP",
                bands = c("B05", "CLOUD"),
                start_date = as.Date("2018-07-18"),
                end_date = as.Date("2018-08-23"),
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(
        purrr::is_null(s2_cube),
        "MPC is not accessible"
    )
    Sys.setenv("MPC_TOKEN" = mpc_token)
    expect_true(all(sits_bands(s2_cube) %in% c("B05", "CLOUD")))
    r <- .raster_open_rast(.tile_path(s2_cube))
    expect_equal(s2_cube$xmax[[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(s2_cube$xmin[[1]], .raster_xmin(r), tolerance = 1)
    r_obj <- .raster_open_rast(s2_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(s2_cube)
    expect_true(.raster_nrows(r_obj) == cube_nrows)

    s2_cube_s2a <- .try(
        {
            sits_cube(
                source = "MPC",
                collection = "SENTINEL-2-L2A",
                tiles = "20LKP",
                bands = c("B05", "CLOUD"),
                start_date = as.Date("2018-07-18"),
                end_date = as.Date("2018-08-23"),
                progress = FALSE,
                platform = "SENTINEL-2A"
            )
        },
        .default = NULL
    )
    n_images_1 <- nrow(s2_cube$file_info[[1]])
    n_images_2 <- nrow(s2_cube_s2a$file_info[[1]])
    expect_true(n_images_2 < n_images_1)
})
test_that("Creating S2 cubes from MPC with ROI", {
    roi <- c(
        lon_min = -48.28579, lat_min = -16.05026,
        lon_max = -47.30839, lat_max = -15.50026
    )
    s2_cube_mpc <- .try(
        {
            sits_cube(
                source = "MPC",
                collection = "SENTINEL-2-L2A",
                roi = roi,
                bands = c("B05", "CLOUD"),
                start_date = as.Date("2018-07-18"),
                end_date = as.Date("2018-08-23"),
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(purrr::is_null(s2_cube_mpc), "MPC is not accessible")
    expect_true(all(sits_bands(s2_cube_mpc) %in% c("B05", "CLOUD")))
    expect_equal(nrow(s2_cube_mpc), 3)
    bbox_cube <- sits_bbox(s2_cube_mpc, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(s2_cube_mpc), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    r_obj <- .raster_open_rast(s2_cube_mpc$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(s2_cube_mpc)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
})
test_that("Creating Sentinel-1 GRD cubes from MPC using tiles", {

    cube_s1_grd <-  sits_cube(
        source = "MPC",
        collection = "SENTINEL-1-GRD",
        bands = c("VV"),
        orbit = "descending",
        tiles = c("21LUJ","21LVJ"),
        start_date = "2021-08-01",
        end_date = "2021-09-30"
    )
    bbox <- sits_bbox(cube_s1_grd)
    roi_cube_s1 <- sits_mgrs_to_roi(c("21LUJ","21LVJ"))

    expect_true(bbox[["xmin"]] < roi_cube_s1[["lon_min"]])
    expect_true(bbox[["xmax"]] > roi_cube_s1[["lon_max"]])
    expect_true(bbox[["ymin"]] < roi_cube_s1[["lat_min"]])
    expect_true(bbox[["ymax"]] > roi_cube_s1[["lat_max"]])
    expect_true(all(c("VV") %in% sits_bands(cube_s1_grd)))

    r_obj <- .raster_open_rast(cube_s1_grd$file_info[[1]]$path[[1]])
    expect_true(terra::nrow(r_obj) == cube_s1_grd$file_info[[1]]$nrows[[1]])

    output_dir <- paste0(tempdir(), "/s1-grd-reg")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }

    cube_s1_reg <- sits_regularize(
        cube = cube_s1_grd,
        period = "P1M",
        res = 240,
        tiles = c("21LUJ","21LVJ"),
        multicores = 1,
        output_dir = output_dir,
        progress = TRUE
    )
    expect_equal(length(sits_timeline(cube_s1_reg)), 2)
    expect_true(all(c("21LUJ", "21LVJ") %in% cube_s1_reg$tile))
    expect_true(all("EPSG:32721" %in% cube_s1_reg$crs))

    bbox <- sits_bbox(cube_s1_reg, as_crs = "EPSG:4326")
    roi_cube_s1 <- sits_mgrs_to_roi(c("21LUJ","21LVJ"))

    expect_equal(bbox[["xmin"]], roi_cube_s1[["lon_min"]], tolerance = 0.01)
    expect_equal(bbox[["xmax"]], roi_cube_s1[["lon_max"]], tolerance = 0.01)
    expect_equal(bbox[["ymin"]], roi_cube_s1[["lat_min"]], tolerance = 0.01)
    expect_equal(bbox[["ymax"]], roi_cube_s1[["lat_max"]], tolerance = 0.01)
    expect_true(all(c("VV") %in% sits_bands(cube_s1_reg)))

})
test_that("Creating Sentinel-1 RTC cubes from MPC", {
    cube_s1_rtc <-  sits_cube(
        source = "MPC",
        collection = "SENTINEL-1-RTC",
        bands = c("VV"),
        orbit = "descending",
        tiles = c("21LXJ", "21LYJ"),
        start_date = "2021-07-01",
        end_date = "2021-09-30"
    )
    bbox <- sits_bbox(cube_s1_rtc[1,])
    expect_true(grepl("32722", bbox[["crs"]]))
    expect_equal(117360, bbox[["xmin"]])
    expect_equal(407410, bbox[["xmax"]])
    expect_equal(nrow(cube_s1_rtc$file_info[[1]]), 7)

    output_dir <- paste0(tempdir(), "/s1rtcreg")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }

    cube_s1_rtc_reg <- sits_regularize(
        cube = cube_s1_rtc,
        period = "P1M",
        res = 240,
        tiles = c("21LXJ", "21LYJ"),
        multicores = 1,
        output_dir = output_dir,
        progress = TRUE
    )
    expect_equal(length(sits_timeline(cube_s1_rtc_reg)), 3)
    expect_true(all(c("21LXJ", "21LYJ") %in%
                        cube_s1_rtc_reg$tile))
    expect_true("EPSG:32721" %in% cube_s1_rtc_reg$crs)

    bbox <- sits_bbox(cube_s1_rtc_reg, as_crs = "EPSG:4326")
    roi_cube_s1 <- sits_mgrs_to_roi(c("21LXJ", "21LYJ"))

    expect_equal(bbox[["xmin"]], roi_cube_s1[["lon_min"]], tolerance = 0.01)
    expect_equal(bbox[["xmax"]], roi_cube_s1[["lon_max"]], tolerance = 0.01)
    expect_equal(bbox[["ymin"]], roi_cube_s1[["lat_min"]], tolerance = 0.01)
    expect_equal(bbox[["ymax"]], roi_cube_s1[["lat_max"]], tolerance = 0.01)
    expect_true(all(c("VV") %in% sits_bands(cube_s1_rtc_reg)))

})
test_that("Creating LANDSAT cubes from MPC with ROI", {
    roi <- c(
        lon_min = -48.28579, lat_min = -16.05026,
        lon_max = -47.30839, lat_max = -15.50026
    )
    mpc_token <- Sys.getenv("MPC_TOKEN")
    Sys.setenv("MPC_TOKEN" = "")
    l8_cube_mpc <- .try(
        {
            sits_cube(
                source = "MPC",
                collection = "LANDSAT-C2-L2",
                roi = roi,
                bands = c("NIR08", "CLOUD"),
                start_date = as.Date("2018-07-18"),
                end_date = as.Date("2018-08-23"),
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(purrr::is_null(l8_cube_mpc), "MPC is not accessible")
    Sys.setenv("MPC_TOKEN" = mpc_token)

    expect_true(all(sits_bands(l8_cube_mpc) %in% c("NIR08", "CLOUD")))
    expect_equal(nrow(l8_cube_mpc), 2)
    bbox_cube <- sits_bbox(l8_cube_mpc, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(l8_cube_mpc), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    r_obj <- .raster_open_rast(l8_cube_mpc$file_info[[1]]$path[1])
    tile_nrows <- .tile_nrows(l8_cube_mpc)[[1]]
    expect_true(.raster_nrows(r_obj) == tile_nrows)
})
test_that("Creating LANDSAT cubes from MPC with WRS", {
    expect_error(
        sits_cube(
                source = "MPC",
                collection = "LANDSAT-C2-L2",
                tiles = "223067",
                bands = c("NIR08", "CLOUD"),
                start_date = as.Date("2018-07-18"),
                end_date = as.Date("2018-08-23"),
                progress = FALSE
            )
    )
})

test_that("Creating Sentinel cubes from AWS", {
    s2_cube <- .try(
        {
            sits_cube(
                source = "AWS",
                collection = "SENTINEL-2-L2A",
                tiles = c("20LKP", "20LLP"),
                bands = c("B05", "CLOUD"),
                start_date = as.Date("2018-07-18"),
                end_date = as.Date("2018-08-23"),
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(
        purrr::is_null(s2_cube),
        "AWS is not accessible"
    )
    expect_true(all(sits_bands(s2_cube) %in% c("B05", "CLOUD")))
    r <- .raster_open_rast(.tile_path(s2_cube))
    expect_equal(s2_cube$xmax[[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(s2_cube$xmin[[1]], .raster_xmin(r), tolerance = 1)
    s2_cube_s2a <- .try(
        {
            sits_cube(
                source = "AWS",
                collection = "SENTINEL-2-L2A",
                tiles = "20LKP",
                bands = c("B05", "CLOUD"),
                start_date = as.Date("2018-07-18"),
                end_date = as.Date("2018-08-23"),
                progress = FALSE,
                platform = "SENTINEL-2A"
            )
        },
        .default = NULL
    )
    n_images_1 <- nrow(s2_cube$file_info[[1]])
    n_images_2 <- nrow(s2_cube_s2a$file_info[[1]])
    expect_true(n_images_2 < n_images_1)
})
test_that("Creating LANDSAT cubes from AWS with ROI", {
    roi <- c(
        lon_min = -47.50, lat_min = -15.80,
        lon_max = -47.30, lat_max = -15.50026
    )
    l8_cube_aws <- .try(
        {
            sits_cube(
                source = "AWS",
                collection = "LANDSAT-C2-L2",
                roi = roi,
                bands = c("NIR08", "CLOUD"),
                start_date = as.Date("2022-07-18"),
                end_date = as.Date("2022-08-23"),
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(purrr::is_null(l8_cube_aws), "AWS is not accessible")
    expect_true(all(sits_bands(l8_cube_aws) %in% c("NIR08", "CLOUD")))
    expect_equal(nrow(l8_cube_aws), 1)
    bbox_cube <- sits_bbox(l8_cube_aws, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(l8_cube_aws), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    r_obj <- .raster_open_rast(l8_cube_aws$file_info[[1]]$path[1])
    tile_nrows <- .tile_nrows(l8_cube_aws)[[1]]
    expect_true(.raster_nrows(r_obj) == tile_nrows)

    l8_cube_aws_l8 <- .try(
        {
            sits_cube(
                source = "AWS",
                collection = "LANDSAT-C2-L2",
                roi = roi,
                bands = c("NIR08", "CLOUD"),
                start_date = as.Date("2022-07-18"),
                end_date = as.Date("2022-08-23"),
                progress = FALSE,
                platform = "LANDSAT-8"
            )
        },
        .default = NULL
    )
    num_files_1 <- nrow(l8_cube_aws$file_info[[1]])
    num_files_2 <- nrow(l8_cube_aws_l8$file_info[[1]])
    expect_true(num_files_2 < num_files_1)
})
test_that("Creating LANDSAT cubes from AWS with WRS", {
    l8_cube_aws_wrs <- .try(
        {
            sits_cube(
                source = "AWS",
                collection = "LANDSAT-C2-L2",
                tiles = "223067",
                bands = c("NIR08", "CLOUD"),
                start_date = as.Date("2022-07-18"),
                end_date = as.Date("2022-08-23"),
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(purrr::is_null(l8_cube_aws_wrs), "AWS is not accessible")
    expect_true(all(sits_bands(l8_cube_aws_wrs) %in% c("NIR08", "CLOUD")))
    expect_equal(nrow(l8_cube_aws_wrs), 1)
    r_obj <- .raster_open_rast(l8_cube_aws_wrs$file_info[[1]]$path[1])
    tile_nrows <- .tile_nrows(l8_cube_aws_wrs)[[1]]
    expect_true(.raster_nrows(r_obj) == tile_nrows)
})

test_that("Creating LANDSAT cubes from USGS with ROI", {
    roi <- c(
        lon_min = -48.28579, lat_min = -16.05026,
        lon_max = -47.30839, lat_max = -15.50026
    )
    l8_cube_usgs <- .try(
        {
            sits_cube(
                source = "USGS",
                collection = "LANDSAT-C2L2-SR",
                roi = roi,
                bands = c("NIR08", "CLOUD"),
                start_date = as.Date("2018-07-18"),
                end_date = as.Date("2018-08-23"),
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(purrr::is_null(l8_cube_usgs), "USGS is not accessible")
    expect_true(all(sits_bands(l8_cube_usgs) %in% c("NIR08", "CLOUD")))
    expect_equal(nrow(l8_cube_usgs), 2)
    bbox_cube <- sits_bbox(l8_cube_usgs, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(l8_cube_usgs), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    r_obj <- .raster_open_rast(l8_cube_usgs$file_info[[1]]$path[1])
    tile_nrows <- .tile_nrows(l8_cube_usgs)[[1]]
    expect_true(.raster_nrows(r_obj) == tile_nrows)
})
test_that("Creating LANDSAT cubes from USGS with WRS", {
    l8_cube_223067 <- .try(
        {
            sits_cube(
                source = "USGS",
                collection = "LANDSAT-C2L2-SR",
                tiles = "223067",
                bands = c("NIR08"),
                start_date = as.Date("2018-07-18"),
                end_date = as.Date("2018-08-23"),
                platform = "LANDSAT-8",
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(purrr::is_null(l8_cube_223067), "USGS is not accessible")
    expect_true(all(sits_bands(l8_cube_223067) %in% c("NIR08")))
    expect_equal(nrow(l8_cube_223067), 1)
    r_obj <- .raster_open_rast(l8_cube_223067$file_info[[1]]$path[1])
    tile_nrows <- .tile_nrows(l8_cube_223067)[[1]]
    expect_true(.raster_nrows(r_obj) == tile_nrows)
})

test_that("Creating Harmonized Landsat Sentinel HLSS30 cubes", {
    roi <- .s2_mgrs_to_roi("20LKP")
    hls_cube_s2 <- .try(
        {
            sits_cube(
                source = "HLS",
                collection = "HLSS30",
                roi = roi,
                bands = c("GREEN", "NIR-NARROW", "SWIR-1", "CLOUD"),
                start_date = as.Date("2020-07-01"),
                end_date = as.Date("2020-09-01"),
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(
        purrr::is_null(hls_cube_s2),
        "HLSS30 collection is not accessible"
    )
    expect_true(all(sits_bands(hls_cube_s2) %in%
                        c("GREEN", "NIR-NARROW", "SWIR-1", "CLOUD")))
    expect_true(all(hls_cube_s2$satellite == "SENTINEL-2"))
    expect_true(all("20LKP" %in% hls_cube_s2$tile))
    expect_true(all(.fi(hls_cube_s2)$xres == 30))
    expect_true(all(.fi(hls_cube_s2)$yres == 30))
    r_obj <- .raster_open_rast(hls_cube_s2$file_info[[1]]$path[1])
    tile_nrows <- .tile_nrows(hls_cube_s2)[[1]]
    expect_true(.raster_nrows(r_obj) == tile_nrows)

    hls_cube_l8 <- .try(
        {
            sits_cube(
                source = "HLS",
                collection = "HLSL30",
                roi = roi,
                bands = c("GREEN", "NIR-NARROW", "SWIR-1", "CLOUD"),
                start_date = as.Date("2020-07-01"),
                end_date = as.Date("2020-09-01"),
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(
        purrr::is_null(hls_cube_l8),
        "HLSL30 collection is not accessible"
    )
    expect_true(all(sits_bands(hls_cube_l8) %in%
                        c("GREEN", "NIR-NARROW", "SWIR-1", "CLOUD")))
    expect_true(all(hls_cube_l8$satellite == "LANDSAT-8"))
    expect_true(all(c("20LKP", "20LLP") %in% hls_cube_s2$tile))
    expect_true(all(.fi(hls_cube_l8)$xres == 30))
    expect_true(all(.fi(hls_cube_l8)$yres == 30))

    hls_cube_merge <- sits_merge(hls_cube_s2, hls_cube_l8)
    merge_20LKP <- dplyr::filter(hls_cube_merge, tile == "20LKP")
    s2_20LKP <- dplyr::filter(hls_cube_s2, tile == "20LKP")
    l8_20LKP <- dplyr::filter(hls_cube_l8, tile == "20LKP")
    expect_true(all(sits_timeline(merge_20LKP) %in%
                        c(sits_timeline(l8_20LKP), sits_timeline(s2_20LKP))))

    netrc_file <- "~/.netrc"
    netrc_save <- "~/.netrc_save"
    file.rename(netrc_file, netrc_save)
    expect_error(.source_configure_access.hls_cube(
        source = "HLS", collection = "HLSS30"))

    expect_error(.source_items_new.hls_cube(
        source = "HLS", collection = "HLSS30", stac_query = NULL))

    expect_true(file.copy(netrc_save, netrc_file))

    conf_hls <- utils::read.delim(netrc_file)
    names(conf_hls) <- "wrong.machine"
    utils::write.table(conf_hls, netrc_file)
    expect_error(.source_configure_access.hls_cube(
        source = "HLS", collection = "HLSS30"))

    expect_true(file.rename(netrc_save, netrc_file))

    if (file.exists("./.rcookies"))
        unlink("./.rcookies")

})
test_that("Creating Harmonized Landsat Sentinel HLSS30 cubes using tiles", {
    hls_cube_s2 <- .try(
        {
            sits_cube(
                source = "HLS",
                collection = "HLSS30",
                tiles = c("20LKP"),
                bands = c("GREEN", "NIR-NARROW", "SWIR-1", "CLOUD"),
                start_date = as.Date("2020-07-01"),
                end_date = as.Date("2020-09-01"),
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(
        purrr::is_null(hls_cube_s2),
        "HLSS30 collection is not accessible"
    )
    expect_true(all(sits_bands(hls_cube_s2) %in%
                        c("GREEN", "NIR-NARROW", "SWIR-1", "CLOUD")))
    expect_true(all(hls_cube_s2$satellite == "SENTINEL-2"))
    expect_true(all(hls_cube_s2$tile %in% c("20LKP", "20LLP")))
    expect_true(all(.fi(hls_cube_s2)$xres == 30))
    expect_true(all(.fi(hls_cube_s2)$yres == 30))
    r_obj <- .raster_open_rast(hls_cube_s2$file_info[[1]]$path[1])
    tile_nrows <- .tile_nrows(hls_cube_s2)[[1]]
    expect_true(.raster_nrows(r_obj) == tile_nrows)

    hls_cube_l8 <- .try(
        {
            sits_cube(
                source = "HLS",
                collection = "HLSL30",
                tiles = c("20LKP"),
                bands = c("GREEN", "NIR-NARROW", "SWIR-1", "CLOUD"),
                start_date = as.Date("2020-07-01"),
                end_date = as.Date("2020-09-01"),
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(
        purrr::is_null(hls_cube_l8),
        "HLSL30 collection is not accessible"
    )
    expect_true(all(sits_bands(hls_cube_l8) %in%
                        c("GREEN", "NIR-NARROW", "SWIR-1", "CLOUD")))
    expect_true(all(hls_cube_l8$satellite == "LANDSAT-8"))
    expect_true(all(hls_cube_s2$tile %in% c("20LKP", "20LLP")))
    expect_true(all(.fi(hls_cube_l8)$xres == 30))
    expect_true(all(.fi(hls_cube_l8)$yres == 30))

    hls_cube_merge <- sits_merge(hls_cube_s2, hls_cube_l8)
    merge_20LKP <- dplyr::filter(hls_cube_merge, tile == "20LKP")
    s2_20LKP <- dplyr::filter(hls_cube_s2, tile == "20LKP")
    l8_20LKP <- dplyr::filter(hls_cube_l8, tile == "20LKP")
    expect_true(all(sits_timeline(merge_20LKP) %in%
                        c(sits_timeline(l8_20LKP), sits_timeline(s2_20LKP))))

})

test_that("Combining Sentinel-1 with Sentinel-2 cubes", {
    s2_cube <- .try(
        {
            sits_cube(
                source = "MPC",
                collection = "SENTINEL-2-L2A",
                tiles = "20LKP",
                bands = c("B02", "B8A", "B11", "CLOUD"),
                start_date = "2020-06-01",
                end_date = "2020-09-28"
            )
        },
        .default = NULL
    )

    dir_images <- paste0(tempdir(), "/images_merge/")
    if (!dir.exists(dir_images)) {
        suppressWarnings(dir.create(dir_images))
    }
    merge_images <- paste0(tempdir(), "/images_merge_new/")
    if (!dir.exists(merge_images)) {
        suppressWarnings(dir.create(merge_images))
    }

    testthat::skip_if(
        purrr::is_null(s2_cube),
        "MPC collection is not accessible"
    )

    s2_reg <- sits_regularize(
        cube = s2_cube,
        period = "P30D",
        res = 120,
        multicores = 2,
        output_dir = dir_images
    )

    s1_cube <- .try(
        {
            sits_cube(
                source = "MPC",
                collection = "SENTINEL-1-GRD",
                bands = c("VV", "VH"),
                orbit = "descending",
                tiles = "20LKP",
                start_date = "2020-06-01",
                end_date = "2020-09-28"
            )
        },
        .default = NULL
    )

    testthat::skip_if(
        purrr::is_null(s1_cube),
        "MPC collection is not accessible"
    )

    s1_reg <- sits_regularize(
        cube = s1_cube,
        period = "P30D",
        res = 120,
        tiles = "20LKP",
        multicores = 2,
        output_dir = dir_images
    )

    cube_merged <- sits_merge(
        s2_reg,
        s1_reg,
        tolerance = "P3D",
        output_dir = merge_images
    )

})

test_that("Access to SwissDataCube", {
    roi <- c(
        lon_min = 7.54, lat_min = 46.73,
        lon_max = 7.65, lat_max = 46.77
    )
    s2_cube_sdc <- .try(
        {
            sits_cube(
                source = "SDC",
                collection = "S2_L2A_10M_SWISS",
                roi = roi,
                bands = c("B08"),
                start_date = as.Date("2018-07-18"),
                end_date = as.Date("2018-08-23"),
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(purrr::is_null(s2_cube_sdc), "SDC is not accessible")
})
test_that("testing STAC error", {
    mpc_url <- sits_env$config$sources$MPC$url
    sits_env$config$sources$MPC$url <-
        "https://planetarycomputer.microsoft.com/api/stac/v100"
    expect_error(
        sits_cube(
            source = "MPC",
            collection = "SENTINEL-2-L2A",
            tiles = "20LKP",
            bands = c("B05"),
            start_date = as.Date("2020-07-18"),
            end_date = as.Date("2020-08-23"),
            progress = FALSE
        )
    )
    sits_env$config$sources$MPC$url <- mpc_url

    aws_url <- sits_env$config$sources$AWS$url
    sits_env$config$sources$AWS$url <-
        "https://earth-search.aws.element84.com/v100/"
    expect_error(
        sits_cube(
            source = "AWS",
            collection = "SENTINEL-2-L2A",
            tiles = "20LKP",
            bands = c("B05"),
            start_date = as.Date("2020-07-18"),
            end_date = as.Date("2020-08-23"),
            progress = FALSE
        )
    )

    sits_env$config$sources$AWS$url <- aws_url

    usgs_url <- sits_env$config$sources$USGS$url

    sits_env$config$sources$USGS$url <-
        "https://landsatlook.usgs.gov/stac-server/v100"
    roi <- c(
        lon_min = -48.28579, lat_min = -16.05026,
        lon_max = -47.30839, lat_max = -15.50026
    )
    expect_error(
        sits_cube(
            source = "USGS",
            collection = "LANDSAT-C2L2-SR",
            roi = roi,
            bands = c("NIR08"),
            start_date = as.Date("2018-07-01"),
            end_date = as.Date("2018-07-30"),
            progress = FALSE
        )
    )
    sits_env$config$sources$USGS$url <- usgs_url

    expect_error(
        sits_cube(
            source = "USGS",
            collection = "LANDSAT-C2L2-SR",
            tiles = "ABC000",
            bands = c("NIR08"),
            start_date = as.Date("2018-07-01"),
            end_date = as.Date("2018-07-30"),
            progress = FALSE
        )
    )
    expect_error(
        sits_cube(
            source = "USGS",
            collection = "LANDSAT-C2L2-SR",
            tiles = "ABC000",
            bands = c("NIR08"),
            start_date = as.Date("2018-07-01"),
            end_date = as.Date("2018-07-30"),
            progress = FALSE
        )
    )
    expect_error(
        sits_cube(
            source = "AWS",
            collection = "SENTINEL-2-L2A",
            bands = c("B05", "CLOUD"),
            start_date = as.Date("2018-07-18"),
            end_date = as.Date("2018-08-23"),
            progress = FALSE,
            platform = "SENTINEL-2A"
        )
    )
})
