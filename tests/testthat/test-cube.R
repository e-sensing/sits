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
            output_dir = dir_images
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
                end_date = "2020-09-28"
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
            output_dir = dir_images
        )
    )

    # Merging images without writing
    cube_merged <- sits_merge(
        s2_reg,
        s1_reg
    )
    testthat::expect_true(
        all(
            sits_bands(cube_merged) %in% c(sits_bands(s2_reg),
                                           sits_bands(s1_reg)))
    )
    testthat::expect_error(
        sits_merge(
            s2_cube,
            s1_cube
        )
    )

    unlink(list.files(dir_images, pattern = ".tif", full.names = TRUE))
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
