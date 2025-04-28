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
        collection = "MOD13Q1-6.1",
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

test_that("Reading raster cube with various type of ROI", {
    roi <- c(
        xmin = -44.58699,
        ymin = -23.12016,
        xmax = -44.45059,
        ymax = -22.97294
    )

    crs <- "EPSG:4326"
    expected_tile <- "23KNQ"

    # Test 1a: ROI as vector
    cube <- .try(
        {
            sits_cube(
                source = "AWS",
                collection = "SENTINEL-2-L2A",
                roi = roi,
                crs = crs,
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(cube), message = "MPC is not accessible")
    expect_equal(cube[["tile"]], expected_tile)

    # Test 2: ROI as SF
    roi_sf <- sf::st_as_sfc(
        x = sf::st_bbox(
            roi,
            crs = crs
        )
    )

    cube <- .try(
        {
            sits_cube(
                source = "AWS",
                collection = "SENTINEL-2-L2A",
                roi = roi_sf,
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(cube), message = "MPC is not accessible")
    expect_equal(cube[["tile"]], expected_tile)

    # Test 3: ROI as lon/lat
    roi_lonlat <- roi
    names(roi_lonlat) <- c("lon_min", "lat_min", "lon_max", "lat_max")

    cube <- .try(
        {
            sits_cube(
                source = "AWS",
                collection = "SENTINEL-2-L2A",
                roi = roi_lonlat,
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(cube), message = "MPC is not accessible")
    expect_equal(cube[["tile"]], expected_tile)

    # Test 4a: ROI as SpatExtent
    roi_raster <- terra::rast(
        extent = terra::ext(roi["xmin"], roi["xmax"], roi["ymin"], roi["ymax"]),
        crs = crs
    )

    roi_raster <- terra::ext(roi_raster)

    cube <- .try(
        {
            sits_cube(
                source = "AWS",
                collection = "SENTINEL-2-L2A",
                roi = roi_raster,
                crs = crs,
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(cube), message = "MPC is not accessible")
    expect_equal(cube[["tile"]], expected_tile)

    # Test 4b: ROI as SpatExtent - Error when no CRS is specified
    expect_error(
        sits_cube(
            source = "AWS",
            collection = "SENTINEL-2-L2A",
            roi = roi_raster,
            progress = FALSE
        )
    )

    # Test 5: ROI as shapefile
    shp_file <- tempfile(fileext = ".shp")

    sf::st_as_sfc(
        x = sf::st_bbox(
            roi,
            crs = crs
        )
    ) |>
        sf::st_write(shp_file, quiet = TRUE)

    cube <- .try(
        {
            sits_cube(
                source = "MPC",
                collection = "SENTINEL-2-L2A",
                roi = shp_file,
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(cube), message = "MPC is not accessible")
    expect_equal(cube[["tile"]], expected_tile)
})

test_that("Combining Sentinel-1 with Sentinel-2 cubes", {
    s2_cube <- .try(
        {
            sits_cube(
                source = "AWS",
                collection = "SENTINEL-2-L2A",
                tiles = "20LKP",
                bands = c("B02", "B8A", "B11", "CLOUD"),
                start_date = "2020-06-01",
                end_date = "2020-09-28",
                progress = FALSE
            )
        },
        .default = NULL
    )

    dir_images <- paste0(tempdir(), "/images_merge_s1_s2/")
    if (!dir.exists(dir_images)) {
        suppressWarnings(dir.create(dir_images))
    }

    testthat::skip_if(
        purrr::is_null(s2_cube),
        "MPC collection is not accessible"
    )


    s2_reg <- suppressWarnings(
        sits_regularize(
            cube = s2_cube,
            period = "P1M",
            res = 240,
            multicores = 2,
            output_dir = dir_images,
            progress = FALSE
        )
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
                end_date = "2020-09-28",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(
        purrr::is_null(s1_cube),
        "MPC collection is not accessible"
    )

    s1_reg <- suppressWarnings(
        sits_regularize(
            cube = s1_cube,
            period = "P1M",
            res = 240,
            tiles = "20LKP",
            multicores = 2,
            output_dir = dir_images,
            progress = FALSE
        )
    )

    # Merging images without writing
    cube_merged <- sits_merge(
        s2_reg,
        s1_reg
    )
    testthat::expect_true(
        all(
            sits_bands(cube_merged) %in% c(
                sits_bands(s2_reg),
                sits_bands(s1_reg)
            )
        )
    )
    merged_cube <- sits_merge(
        s2_cube,
        s1_cube
    )
    expect_equal(nrow(merged_cube), 2)

    unlink(list.files(dir_images, pattern = ".tif", full.names = TRUE))
})
