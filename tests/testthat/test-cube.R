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
            parse_info = c("X1", "X2", "tile", "band", "date"),
            multicores = 2
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

test_that("Backwards compatibility", {

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")

    raster_cube <- tryCatch({
        sits_cube(
            source = "LOCAL",
            collection = "MOD13Q1-6",
            data_dir = data_dir,
            delim = "_",
            parse_info = c("X1", "X2", "tile", "band", "date"),
            multicores = 2
        )
    }, error = function(e) {
        return(NULL)
    })

    expect_null(raster_cube)

    expect_message(
        object = sits_cube(
            source = "LOCAL",
            origin = "BDC",
            collection = "MOD13Q1-6",
            data_dir = data_dir,
            delim = "_",
            parse_info = c("X1", "X2", "tile", "band", "date"),
            multicores = 2
        ),
        regexp = "LOCAL value is deprecated"
    )

    expect_message(
        object = sits_cube(
            source = "BDC",
            collection = "MOD13Q1-6",
            band = c("NDVI", "EVI"),
            data_dir = data_dir,
            delim = "_",
            parse_info = c("X1", "X2", "tile", "band", "date"),
            multicores = 2
        ),
        regexp = "please use bands instead of band as parameter"
    )
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
            parse_info = c("X1", "X2", "tile", "band", "date"),
            multicores = 2
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
    expect_message(
        object = (cbers_cube <-
                      tryCatch({
                          sits_cube(
                              source = "BDC",
                              collection = "CB4_64_16D_STK-1",
                              tile = c("022024", "022023"),
                              start_date = "2018-09-01",
                              end_date = "2019-08-29"
                          )
                      },
                      error = function(e) {
                          return(NULL)
                      })
        ),
        regexp = "please use tiles instead of tile as parameter"
    )

    testthat::skip_if(purrr::is_null(cbers_cube),
                      message = "BDC is not accessible")

    expect_true(all(sits_bands(cbers_cube) %in%
                        c("NDVI", "EVI", "B13", "B14", "B15", "B16", "CLOUD")))
    bbox <- sits_bbox(cbers_cube)
    int_bbox <- sits:::.sits_bbox_intersect(bbox, cbers_cube[1, ])
    expect_true(all(int_bbox == sits_bbox(cbers_cube[1, ])))

    timeline <- sits_timeline(cbers_cube)
    expect_true(timeline[1] <= as.Date("2018-09-01"))
    expect_true(timeline[length(timeline)] <= as.Date("2019-08-29"))

    r_obj <- sits:::.raster_open_rast(cbers_cube$file_info[[1]]$path[1])
    expect_error(sits:::.cube_size(cbers_cube), "process one tile at a time")
    expect_true(terra::nrow(r_obj) == sits:::.cube_size(cbers_cube[1,])[["nrows"]])
})

test_that("Creating cubes from BDC - based on ROI with shapefile", {
    testthat::skip_on_cran()

    # check "BDC_ACCESS_KEY" - mandatory one per user
    bdc_access_key <- Sys.getenv("BDC_ACCESS_KEY")

    testthat::skip_if(nchar(bdc_access_key) == 0,
                      message = "No BDC_ACCESS_KEY defined in environment.")

    shp_file <- system.file("extdata/shapefiles/brazilian_legal_amazon/brazilian_legal_amazon.shp",
                            package = "sits")
    sf_bla <- sf::read_sf(shp_file)

    # create a raster cube file based on the information about the files
    msg <- capture_messages(
        modis_cube <-
            tryCatch({
                sits_cube(
                    source = "BDC",
                    collection = "MOD13Q1-6",
                    bands = c("NDVI", "EVI"),
                    roi = sf_bla,
                    start_date = "2018-09-01",
                    end_date = "2019-08-29"
                )
            },
            error = function(e) {
                return(NULL)
            })
    )

    testthat::skip_if(purrr::is_null(modis_cube),
                      message = "BDC is not accessible")

    expect_true(
        grepl("The supplied roi will be transformed to the WGS 84.", msg)
    )
    expect_true(all(sits_bands(modis_cube) %in% c("NDVI", "EVI")))
    bbox <- sits_bbox(modis_cube, wgs84 = TRUE)
    bbox_shp <- sf::st_bbox(sf_bla)

    expect_lt(bbox["xmin"], bbox_shp["xmin"])
    expect_lt(bbox["ymin"], bbox_shp["ymin"])
    expect_gt(bbox["xmax"], bbox_shp["xmax"])
    expect_gt(bbox["ymax"], bbox_shp["ymax"])
    intersects <- slider::slide_lgl(modis_cube, function(tile){
        sits:::.sits_raster_sub_image_intersects(tile, sf_bla)
    } )
    expect_true(all(intersects))


})

test_that("Creating cubes from BDC - invalid roi", {
    testthat::skip_on_cran()

    # check "BDC_ACCESS_KEY" - mandatory one per user
    bdc_access_key <- Sys.getenv("BDC_ACCESS_KEY")

    testthat::skip_if(nchar(bdc_access_key) == 0,
                      message = "No BDC_ACCESS_KEY defined in environment.")

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
            roi = c(lon_min = -55.20997,
                    lat_min = 15.40554,
                    lon_max = -55.19883,
                    lat_max = -15.39179),
            tiles = "012010",
            start_date = "2018-09-01",
            end_date = "2019-08-29"
        )
    )
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
    testthat::skip_if(purrr::is_null(dea_cube),
                      message = "DEAFRICA is not accessible")

    expect_true(all(sits_bands(dea_cube) %in% c("B01", "B04", "B05")))

    file_info <- dea_cube$file_info[[1]]
    r <- .raster_open_rast(file_info$path[[1]])

    expect_equal(dea_cube$xmax[[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(dea_cube$xmin[[1]], .raster_xmin(r), tolerance = 1)
})

test_that("Creating cubes from DEA - error using tiles", {
    testthat::skip_on_cran()

    expect_error(dea_cube <-
                     sits_cube(source = "DEAFRICA",
                               collection = "s2_l2a",
                               bands = c("B01", "B04", "B05"),
                               tiles = "37MEP",
                               start_date = "2019-01-01",
                               end_date = "2019-10-28"),
                 "DEAFRICA cubes do not support searching for tiles"
    )
})
test_that("Merging cubes", {

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")

    ndvi_cube <- sits_cube(
        source = "BDC",
        bands = "NDVI",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "X2", "tile", "band", "date"),
        multicores = 2
    )

    testthat::skip_if(purrr::is_null(ndvi_cube),
                      "LOCAL cube was not found")

    evi_cube <- tryCatch({
        sits_cube(
            source = "BDC",
            collection = "MOD13Q1-6",
            bands = "EVI",
            data_dir = data_dir,
            delim = "_",
            parse_info = c("X1", "X2", "tile", "band", "date"),
            multicores = 2
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

    cube_merge2 <- sits_merge(ndvi_cube, ndvi_cube)
    expect_true(all(sits_bands(cube_merge2) %in% c("NDVI.1", "NDVI.2")))
    expect_true(cube_merge2$xmin == ndvi_cube$xmin)
    expect_true(cube_merge2$xmax == ndvi_cube$xmax)
})

test_that("Creating cubes from AWS", {

    testthat::skip_on_cran()

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

    expect_error(s2_cube <- sits_cube(source = "AWS",
                                      collection = "sentinel-s2-l2a",
                                      tiles = c("A20LKP"),
                                      bands = c("B08", "SCL"),
                                      start_date = "2018-07-30",
                                      end_date = "2018-08-30")
    )
})

test_that("Creating cubes from AWS Open Data and regularizing them", {

    testthat::skip_on_cran()

    s2_cube_open <- tryCatch({
        sits_cube(source = "AWS",
                  collection = "SENTINEL-S2-L2A-COGS",
                  tiles = c("20LKP", "20LLP"),
                  bands = c("B8A", "SCL"),
                  start_date = "2018-10-01",
                  end_date = "2018-11-01"
        )},
        error = function(e){
            return(NULL)
        })
    testthat::skip_if(purrr::is_null(s2_cube_open),
                      "AWS is not accessible")
    expect_false(.cube_is_regular(s2_cube_open))
<<<<<<< HEAD
    expect_true(all(sits_bands(s2_cube_open) %in% c("B11", "B8A", "CLOUD")))
=======
    expect_true(all(sits_bands(s2_cube_open) %in% c("B8A", "CLOUD")))
>>>>>>> 21f182230d18b7528990d9edf4ab5b2203804f6b

    expect_error(.cube_size(s2_cube_open))
    expect_error(.cube_resolution(s2_cube_open))
    expect_error(.file_info_nrows(s2_cube_open))

    dir_images <-  paste0(tempdir(), "/images2/")
    if (!dir.exists(dir_images))
        suppressWarnings(dir.create(dir_images))

    gc_cube <- sits_regularize(
        cube        = s2_cube_open,
        output_dir  = dir_images,
        res         = 320,
        agg_method  = "median",
        period      = "P1M",
        multicores = 4,
        multithreads = 16)

    tile_size <- .cube_size(gc_cube[1, ])
    tile_bbox <- .cube_tile_bbox(gc_cube[1, ])

    expect_equal(tile_size[["nrows"]], 344)
    expect_equal(tile_size[["ncols"]], 344)
    expect_equal(tile_bbox$xmax, 309920, tolerance = 1e-1)
    expect_equal(tile_bbox$xmin, 199840, tolerance = 1e-1)

    tile_fileinfo <- .file_info(gc_cube[1, ])

    expect_equal(nrow(tile_fileinfo), 1)
})

test_that("Creating cubes from AWS Open Data and regularizing with ROI", {

    s2_cube_open <- tryCatch({
        sits_cube(source = "AWS",
                  collection = "SENTINEL-S2-L2A-COGS",
                  tiles = c("20LKP", "20LLP"),
                  bands = c("B08", "B03", "SCL"),
                  start_date = "2018-12-01",
                  end_date = "2018-12-30"
        )},
        error = function(e){
            return(NULL)
        })
    testthat::skip_if(purrr::is_null(s2_cube_open),
                      "AWS is not accessible")
    expect_false(.cube_is_regular(s2_cube_open))
    expect_true(all(sits_bands(s2_cube_open) %in% c("B08", "B03", "CLOUD")))

    expect_error(.cube_size(s2_cube_open))
    expect_error(.cube_resolution(s2_cube_open))
    expect_error(.file_info_nrows(s2_cube_open[1,]))

    dir_images <-  paste0(tempdir(), "/images/")
    if (!dir.exists(dir_images))
        suppressWarnings(dir.create(dir_images))

    gc_cube <- sits_regularize(
        cube        = s2_cube_open,
        output_dir  = dir_images,
        res         = 320,
        agg_method  = "least_cc_first",
        roi = c("lon_min" = -65.3811,
                "lat_min" = -10.6645,
                "lon_max" = -64.86069,
                "lat_max" = -10.491988),
        period      = "P30D",
        multicores = 2,
        multithreads = 4)

    size <- .cube_size(gc_cube)

    expect_equal(size[["nrows"]], 61)
    expect_equal(size[["ncols"]], 179)
    expect_equal(gc_cube$xmax, 296562, tolerance = 1e-1)
    expect_equal(gc_cube$xmin, 234802.7, tolerance = 1e-1)

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
                  roi = c("xmin" = 17.379,
                          "ymin" = 1.1573,
                          "xmax" = 17.410,
                          "ymax" = 1.1910),
                  start_date = "2019-01-01",
                  end_date = "2019-10-28"
        )},
        error = function(e){
            return(NULL)
        })

    usgs_cube <-  tryCatch({
        sits_cube(source = "USGS",
                  collection = "landsat-c2l2-sr",
                  bands = c("B04", "CLOUD"),
                  tiles = "223067",
                  start_date = "2019-01-01",
                  end_date = "2019-10-28"
        )},
        error = function(e){
            return(NULL)
        })

    testthat::skip_if(purrr::is_null(usgs_cube),
                      "USGS is not accessible")

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
                      "MSPC is not accessible")

    expect_true(all(sits_bands(s2_cube) %in% c("B05", "CLOUD")))

    expect_equal(class(.cube_size(s2_cube)), "numeric")
    expect_equal(class(.cube_resolution(s2_cube)), "numeric")

    file_info <- s2_cube$file_info[[1]]
    r <- .raster_open_rast(file_info$path[[1]])

    expect_equal(s2_cube$xmax[[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(s2_cube$xmin[[1]], .raster_xmin(r), tolerance = 1)
})

test_that("Creating Sentinel cubes from MSPC with ROI", {

    testthat::skip_on_cran()

    shp_file <- system.file("extdata/shapefiles/df_bsb/df_bsb.shp",
                            package = "sits")
    sf_bsb <- sf::read_sf(shp_file)

    s2_cube <- tryCatch({
        sits_cube(source = "MSPC",
                  collection = "sentinel-2-l2a",
                  roi = sf_bsb,
                  bands = c("B05", "CLOUD"),
                  start_date = as.Date("2018-07-18"),
                  end_date = as.Date("2018-08-23")
        )},
        error = function(e){
            return(NULL)
        })

    testthat::skip_if(purrr::is_null(s2_cube),
                      "MSPC is not accessible")

    expect_true(all(sits_bands(s2_cube) %in% c("B05", "CLOUD")))

    expect_equal(class(sits:::.cube_size(s2_cube[1,])), "numeric")
    expect_equal(class(sits:::.cube_resolution(s2_cube[1,])), "numeric")

    file_info <- s2_cube$file_info[[1]]
    r <- sits:::.raster_open_rast(file_info$path[[1]])

    expect_equal(nrow(s2_cube), 3)
    expect_warning(sits_bbox(s2_cube), "cube has more than one projection")

    bbox_cube <- sits_bbox(s2_cube, wgs84 = TRUE)
    bbox_cube_1 <- sits_bbox(s2_cube[1,], wgs84 = TRUE)
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])

    msg <- capture_warnings(sits_timeline(s2_cube))
    expect_true(grepl("Cube is not regular. Returning all timelines", msg))
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

    l8_cube_tile <-  tryCatch({
        sits_cube(source = "MSPC",
                  collection = "landsat-8-c2-l2",
                  bands = c("B04", "CLOUD"),
                  tiles = "223067",
                  start_date = "2019-01-01",
                  end_date = "2019-10-28"
        )},
        error = function(e){
            return(NULL)
        })

    testthat::skip_if(purrr::is_null(l8_cube_tile),
                      "MSPC is not accessible")
    bbox <- sits_bbox(l8_cube_tile)
    expect_lt(bbox["xmax"], 760000)
    expect_lt(bbox["ymax"], -1000000)

    file_info <- l8_cube_tile$file_info[[1]]
    r_obj <- sits:::.raster_open_rast(file_info$path[[1]])
    expect_equal(nrow(r_obj), file_info[1,]$nrows)
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
                           "date", "X6", "band"),
            multicores = 2
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
