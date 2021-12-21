test_that("Creating a SATVEG data cube", {
    testthat::skip_on_cran()

    cube_satveg <- tryCatch({
        sits_cube(source = "SATVEG", collection = "TERRA")
    },
    error = function(e) {
        return(NULL)
    })

    testthat::skip_if(purrr::is_null(cube_satveg),
                      message = "SATVEG is not accessible")

    expect_true(cube_satveg$ymin == -30.0)
})

test_that("Reading a raster cube", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")

    raster_cube <- tryCatch({
        sits_cube(
            source = "BDC",
            collection = "MOD13Q1-6",
            data_dir = data_dir,
            delim = "_",
            parse_info = c("X1", "X2", "tile", "band", "date")
        )
    }, error = function(e) {
        return(NULL)
    })

    testthat::skip_if(purrr::is_null(raster_cube),
                      message = "LOCAL cube not found")

    # get bands names
    bands <- sits_bands(raster_cube)
    expect_true(all(bands %in% c("NDVI", "EVI")))

    params <- .raster_params_file(raster_cube$file_info[[1]]$path)
    expect_true(params$nrows == 144)
    expect_true(params$ncols == 254)
    expect_true(params$xres >= 231.5)
})

test_that("Creating a raster stack cube and selecting bands", {
    # Create a raster cube based on MODIS data
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")

    # create a raster cube file based on the information about the files
    modis_cube <- tryCatch({
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
    })

    testthat::skip_if(purrr::is_null(modis_cube),
                      message = "LOCAL cube not found")


    expect_true(all(sits_bands(modis_cube) %in%
                        c("EVI", "NDVI")))
    rast <- .raster_open_rast(modis_cube$file_info[[1]]$path[[1]])
    expect_true(.raster_nrows(rast) == .cube_size(modis_cube)[["nrows"]])
    timeline <- sits_timeline(modis_cube)
    expect_true(timeline[1] == "2013-09-14")

    modis_cube_evi <- sits_select(modis_cube, bands = "EVI")
    expect_true(all(sits_bands(modis_cube_evi) == c("EVI")))
})

test_that("Creating cubes from BDC", {
    testthat::skip_on_cran()

    # check "BDC_ACCESS_KEY" - mandatory one per user
    bdc_access_key <- Sys.getenv("BDC_ACCESS_KEY")

    testthat::skip_if(nchar(bdc_access_key) == 0,
                      message = "No BDC_ACCESS_KEY defined in environment.")

    # create a raster cube file based on the information about the files
    cbers_cube <-
        tryCatch({
            sits_cube(
                source = "BDC",
                collection = "CB4_64_16D_STK-1",
                bands = c("NDVI", "EVI"),
                tiles = c("022024", "022023"),
                start_date = "2018-09-01",
                end_date = "2019-08-29"
            )
        },
        error = function(e) {
            return(NULL)
        })

    testthat::skip_if(purrr::is_null(cbers_cube),
                      message = "BDC is not accessible")

    expect_true(all(sits_bands(cbers_cube) %in% c("NDVI", "EVI")))
    bbox <- sits_bbox(cbers_cube)
    int_bbox <- .sits_bbox_intersect(bbox, cbers_cube[1, ])
    expect_true(all(int_bbox == sits_bbox(cbers_cube[1, ])))

    timeline <- sits_timeline(cbers_cube)
    expect_true(timeline[1] <= as.Date("2018-09-01"))
    expect_true(timeline[length(timeline)] <= as.Date("2019-08-29"))

    r_obj <- .raster_open_rast(cbers_cube$file_info[[1]]$path[1])
    expect_true(terra::nrow(r_obj) == .cube_size(cbers_cube)[["nrows"]])
})

test_that("Creating cubes from WTSS", {
    testthat::skip_on_cran()

    # check "BDC_ACCESS_KEY" - mandatory one per user
    bdc_access_key <- Sys.getenv("BDC_ACCESS_KEY")

    testthat::skip_if(nchar(bdc_access_key) == 0,
                      message = "No BDC_ACCESS_KEY defined in environment.")

    # create a raster cube file based on the information about the files
    wtss_cube <- tryCatch({
        sits_cube(
            source = "WTSS",
            collection = "LC8_30_16D_STK-1")
    },
    error = function(e) {
        return(NULL)
    })

    testthat::skip_if(purrr::is_null(wtss_cube),
                      message = "WTSS server is not accessible")

    expect_true(all(c("NDVI", "EVI") %in% sits_bands(wtss_cube)))

    timeline <- sits_timeline(wtss_cube)
    expect_true(as.Date("2019-11-01") %in% timeline)

    # provide invalid collection
    testthat::expect_error(
        sits_cube(
            source = "WTSS",
            collection = "Invalid-collection")
    )

    # provide no collection
    testthat::expect_error(
        sits_cube(source = "WTSS")
    )

    # try to access cube with wrong url
    testthat::expect_error(
        sits_cube(
            source = "WTSS",
            collection = "invalid-collection")
    )
})

test_that("Creating cubes from DEA", {
    testthat::skip_on_cran()

    dea_cube <- tryCatch({
        sits_cube(source = "DEAFRICA",
                  collection = "s2_l2a",
                  bands = c("B01", "B04", "B05"),
                  roi = c(lon_min = 17.379,
                          lat_min = 1.1573,
                          lon_max = 17.410,
                          lat_max = 1.1910),
                  start_date = "2019-01-01",
                  end_date = "2019-10-28")
    },
    error = function(e) {
        return(NULL)
    })

    testthat::skip_if(
        purrr::is_null(dea_cube),
        message = "DEAFRICA is not accessible"
    )

    expect_true(all(sits_bands(dea_cube) %in% c("B01", "B04", "B05")))

    file_info <- dea_cube$file_info[[1]]
    r <- .raster_open_rast(file_info$path[[1]])

    expect_equal(dea_cube$xmax[[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(dea_cube$xmin[[1]], .raster_xmin(r), tolerance = 1)
})

test_that("Merging cubes", {

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")

    ndvi_cube <- tryCatch({
        sits_cube(
            source = "BDC",
            bands = "NDVI",
            collection = "MOD13Q1-6",
            data_dir = data_dir,
            delim = "_",
            parse_info = c("X1", "X2", "tile", "band", "date")
        )
    },
    error = function(e) {
        return(NULL)
    })

    testthat::skip_if(purrr::is_null(ndvi_cube),
                      "LOCAL cube was not found")

    evi_cube <- tryCatch({
        sits_cube(
            source = "BDC",
            collection = "MOD13Q1-6",
            bands = "EVI",
            data_dir = data_dir,
            delim = "_",
            parse_info = c("X1", "X2", "tile", "band", "date")
        )
    },
    error = function(e) {
        return(NULL)
    })


    testthat::skip_if(purrr::is_null(evi_cube),
                      "LOCAL cube was not found")

    cube_merge <- sits_merge(ndvi_cube, evi_cube)

    expect_true(all(sits_bands(cube_merge) %in% c("NDVI", "EVI")))
    expect_true(cube_merge$xmin == ndvi_cube$xmin)
    expect_true(cube_merge$xmax == evi_cube$xmax)
})

test_that("Creating cubes from AWS", {

    testthat::skip_on_cran()

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


    s2_cube <- tryCatch({sits_cube(source = "AWS",
                         collection = "sentinel-s2-l2a",
                         tiles = c("20LKP"),
                         bands = c("B08", "SCL"),
                         start_date = "2018-07-30",
                         end_date = "2018-08-30"
                         )
        },
        error = function(e) {
            return(NULL)
        })

    testthat::skip_if(purrr::is_null(s2_cube),
                      "AWS is not accessible")

    expect_true(all(sits_bands(s2_cube) %in% c("B08", "CLOUD")))

    expect_error(.cube_size(s2_cube))
    expect_error(.cube_resolution(s2_cube))

    file_info <- s2_cube$file_info[[1]]
    r <- .raster_open_rast(file_info$path[[1]])

    expect_equal(s2_cube$xmax[[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(s2_cube$xmin[[1]], .raster_xmin(r), tolerance = 1)
})

test_that("Creating cubes from AWS Open Data and regularizing them", {

    s2_cube_open <- tryCatch({
        sits_cube(source = "AWS",
                  collection = "SENTINEL-S2-L2A-COGS",
                  tiles = c("20LKP", "20LLP"),
                  bands = c("B08", "SCL"),
                  start_date = "2018-07-30",
                  end_date = "2018-08-30"
        )},
        error = function(e){
            return(NULL)
        })
    testthat::skip_if(purrr::is_null(s2_cube_open),
                      "AWS is not accessible")
    expect_false(.cube_is_regular(s2_cube_open))
    expect_true(all(sits_bands(s2_cube_open) %in% c("B08", "CLOUD")))

    expect_error(.cube_size(s2_cube_open))
    expect_error(.cube_resolution(s2_cube_open))

    expect_equal(nrow(.cube_file_info(s2_cube_open)), 14)
    dir_images <-  paste0(tempdir(), "/images/")
    if (!dir.exists(dir_images))
        suppressWarnings(dir.create(dir_images))

    gc_cube <- sits_regularize(
        cube        = s2_cube_open,
        output_dir  = dir_images,
        res         = 120,
        roi = c("xmin" = 234872.7,
                "ymin" = 8847983.0,
                "xmax" = 239532.6,
                "ymax" = 8852017.0),
        period      = "P16D",
        multicores = 2)

    size <- .cube_size(gc_cube)

    expect_equal(size[["nrows"]], 34)
    expect_equal(size[["ncols"]], 39)
    expect_equal(gc_cube$xmax, 239542.7, tolerance = 1e-1)
    expect_equal(gc_cube$xmin, 234862.7, tolerance = 1e-1)

    file_info2 <- gc_cube$file_info[[1]]

    expect_equal(nrow(file_info2), 2)
})

test_that("Creating cubes from USGS", {

    testthat::skip_on_cran()

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

    usgs_cube <-  tryCatch({
        sits_cube(source = "USGS",
                  collection = "landsat-c2l2-sr",
                  bands = c("B04", "CLOUD"),
                  roi = c("lon_min" = 17.379,
                          "lat_min" = 1.1573,
                          "lon_max" = 17.410,
                          "lat_max" = 1.1910),
                  start_date = "2019-01-01",
                  end_date = "2019-10-28"
        )},
        error = function(e){
            return(NULL)
        })

    testthat::skip_if(purrr::is_null(usgs_cube),
                      "AWS is not accessible")

    expect_true(all(sits_bands(usgs_cube) %in% c("B04", "CLOUD")))

    expect_equal(class(.cube_resolution(usgs_cube)), "numeric")

    file_info <- usgs_cube$file_info[[1]]
    r <- .raster_open_rast(file_info$path[[1]])

    expect_equal(usgs_cube$xmax[[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(usgs_cube$xmin[[1]], .raster_xmin(r), tolerance = 1)
})

test_that("Creating Sentinel cubes from MSPC", {

    testthat::skip_on_cran()

    s2_cube <- tryCatch({
        sits_cube(source = "MSPC",
                  collection = "sentinel-2-l2a",
                  tiles = "20LKP",
                  bands = c("B05", "CLOUD"),
                  start_date = as.Date("2018-07-18"),
                  end_date = as.Date("2018-08-23")
        )},
        error = function(e){
            return(NULL)
        })

    testthat::skip_if(purrr::is_null(s2_cube),
                      "AWS is not accessible")

    expect_true(all(sits_bands(s2_cube) %in% c("B05", "CLOUD")))

    expect_equal(class(.cube_size(s2_cube)), "numeric")
    expect_equal(class(.cube_resolution(s2_cube)), "numeric")

    file_info <- s2_cube$file_info[[1]]
    r <- .raster_open_rast(file_info$path[[1]])

    expect_equal(s2_cube$xmax[[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(s2_cube$xmin[[1]], .raster_xmin(r), tolerance = 1)
})

test_that("Creating Landsat cubes from MSPC", {

    testthat::skip_on_cran()

    l8_cube <- sits_cube(source = "MSPC",
                         collection = "landsat-8-c2-l2",
                         roi = c("lon_min" = 17.379,
                                 "lat_min" = 1.1573,
                                 "lon_max" = 17.410,
                                 "lat_max" = 1.1910),
                         bands = c("B03","CLOUD"),
                         start_date = as.Date("2019-07-18"),
                         end_date = as.Date("2019-10-23")
    )

    expect_true(all(sits_bands(l8_cube) %in% c("B03", "CLOUD")))

    # expect_equal(class(.cube_size(l8_cube)), "numeric")
    expect_equal(class(.cube_resolution(l8_cube)), "numeric")

    file_info <- l8_cube$file_info[[1]]
    r <- .raster_open_rast(file_info$path[[1]])

    expect_equal(l8_cube$xmax[[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(l8_cube$xmin[[1]], .raster_xmin(r), tolerance = 1)
})

test_that("Creating a raster stack cube with BDC band names", {
    # Create a raster cube based on CBERS data
    data_dir <- system.file("extdata/raster/bdc", package = "sits")

    # create a raster cube file based on the information about the files
    cbers_cube_bdc <- tryCatch({
        sits_cube(
            source = "BDC",
            collection = "CB4_64-1",
            data_dir = data_dir,
            parse_info = c("X1", "X2", "X3", "X4", "X5", "tile",
                           "date", "X6", "band")
        )
    },
    error = function(e) {
        return(NULL)
    })

    testthat::skip_if(purrr::is_null(cbers_cube_bdc),
                      message = "LOCAL cube not found")

    expect_true(all(sits_bands(cbers_cube_bdc) %in%
                        c("B16")))
})
