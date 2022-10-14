test_that("file_info functions", {
    cbers_cube <- tryCatch(
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

    testthat::skip_if(purrr::is_null(cbers_cube),
        message = "BDC is not accessible"
    )

    # only works with one tile

    cbers_tile <- cbers_cube[1, ]

    # file info
    expect_s3_class(.fi(cbers_tile), "tbl_df")
    expect_equal(.fi(cbers_tile), cbers_tile[["file_info"]][[1]])

    # tile size
    expect_equal(.fi(cbers_tile)[["nrows"]], 6865)
    expect_equal(.fi(cbers_tile)[["ncols"]], 10504)

    # tile paths
    expect_length(.tile_path(cbers_tile), 1)
    expect_length(.tile_path(cbers_tile), 8)

    # tile resolutions
    expect_equal(.fi(cbers_tile)[["xres"]], 63.99735, tolerance = 10e-6)
    expect_equal(.fi(cbers_tile)[["yres"]], 64.00234, tolerance = 10e-6)

    # tile properties
    expect_length(.fi(cbers_tile)[["fid"]], 4)
    expect_length(.tile_timeline(cbers_tile), 4)
    expect_equal(.tile_bands(cbers_tile), c("EVI", "NDVI"))

    # tile filters
    tile_fid <- dplyr::filter(.fi(cbers_tile),
        fid == "CB4_64_16D_STK_v001_022024_2018-09-14_2018-09-29"
    )

    expect_s3_class(tile_fid, "tbl_df")
    expect_equal(nrow(tile_fid), 2)

    tile_sliced_date <- .fi_filter_interval(
        .fi(cbers_tile),
        start_date = "2018-08-29",
        end_date = "2018-09-14"
    )

    expect_s3_class(tile_sliced_date, "tbl_df")
    expect_equal(nrow(tile_sliced_date), 2)

    tile_band <- .tile_filter_bands(cbers_tile, bands = "NDVI")

    expect_s3_class(.fi(tile_band), "tbl_df")
    expect_equal(nrow(.fi(tile_band)), 4)
})

test_that("file_info functions for result cubes", {


    # build an extreme gradient boosting model
    xgb_model <- sits_train(
        samples_modis_ndvi,
        sits_xgboost(nrounds = 50, verbose = FALSE)
    )

    # create a data cube based on files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")

    local_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "tile", "band", "date"),
        multicores = 2
    )

    # classify the data cube with xgb model
    probs_cube <- sits_classify(
        local_cube,
        xgb_model,
        output_dir = tempdir(),
        memsize = 4,
        multicores = 2
    )

    # tile resolutions
    expect_equal(.fi(probs_cube)[["xres"]], 231.656, tolerance = 10e-6)
    expect_equal(.fi(probs_cube)[["yres"]], 231.6564, tolerance = 10e-6)

    # tile properties
    expect_equal(.tile_bands(probs_cube), "probs")
})

test_that("file_info errors", {
    s2_cube <- sits_cube(
        source = "AWS",
        collection = "SENTINEL-S2-L2A-COGS",
        bands = c("B01", "B02", "CLOUD"),
        tiles = c("20LKP", "20LLP"),
        start_date = "2018-09-01",
        end_date = "2018-10-01"
    )

    s2_tile <- s2_cube[1, ]

    # file info
    expect_s3_class(.fi(s2_tile), "tbl_df")

    expect_error(.check_is_regular(s2_cube))
})
