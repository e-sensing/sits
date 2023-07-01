test_that("file_info functions", {
    cbers_cube <- tryCatch(
        {
            sits_cube(
                source = "BDC",
                collection = "CBERS-WFI-16D",
                bands = c("NDVI", "EVI", "CLOUD"),
                tiles = c("007004", "007005"),
                start_date = "2018-09-01",
                end_date = "2018-10-28",
                progress = FALSE
            )
        },
        error = function(e) {
            return(NULL)
        }
    )

    testthat::skip_if(purrr::is_null(cbers_cube),
        message = "BDC is not accessible"
    )

    # file info
    expect_s3_class(.fi(cbers_cube), "tbl_df")
    expect_equal(.fi(cbers_cube), cbers_cube[["file_info"]][[1]])

    # tile size
    expect_equal(.tile_nrows(cbers_cube), 6600)
    expect_equal(.tile_ncols(cbers_cube), 6600)

    # tile paths
    expect_length(.tile_path(cbers_cube), 1)
    expect_length(.tile_paths(cbers_cube), 12)

    # tile resolutions
    expect_equal(.tile_xres(cbers_cube), 64.000, tolerance = 10e-3)
    expect_equal(.tile_yres(cbers_cube), 64.00, tolerance = 10e-3)

    # tile properties
    expect_length(.tile_timeline(cbers_cube), 4)
    expect_true(all(.tile_bands(cbers_cube) %in% c("EVI", "NDVI", "CLOUD")))

    # tile filters
    tile_fid <- dplyr::filter(
        .fi(cbers_cube),
        fid == "CB4-16D_V2_007004_20180829"
    )
    # cloud cover
    expect_true(min(.fi_cloud_cover(.fi(cbers_cube))) == 0.0)

    # filter id
    fid_1 <- .fi_filter_fid(.fi(cbers_cube), .fi(cbers_cube)$fid[[1]])
    expect_true(nrow(fid_1) == 3)

    expect_s3_class(tile_fid, "tbl_df")
    expect_equal(nrow(tile_fid), 3)

    # test errors
    fi <- .fi(cbers_cube)
    expect_error(.fi_filter_fid(fi, fid = "CB4-16D-V222"))
    expect_error(.fi_filter_bands(fi, bands = "NBR"))

    cube_sliced_date <- .cube_filter_interval(
        cbers_cube,
        start_date = "2018-08-29",
        end_date = "2018-09-14"
    )

    expect_s3_class(cube_sliced_date, "tbl_df")
    expect_equal(length(.tile_timeline(cube_sliced_date)), 2)

    cube_band <- .tile_filter_bands(cbers_cube, bands = "NDVI")

    expect_s3_class(.fi(cube_band), "tbl_df")
    expect_equal(nrow(.fi(cube_band)), 4)

    expect_error(.fi_type(1))
    expect_error(.fi_switch(1))
    fi2 <- .fi_filter_interval(fi, start_date = NULL, end_date = NULL)
    expect_equal(nrow(fi), nrow(fi2))
    expect_error(.fi_filter_interval(fi,
                                     start_date = "2019-09-01",
                                     end_date = "2019-10-28"))
    expect_error(.fi_filter_dates(fi, dates = c("2019-09-01", "2019-10-28")))
    roi <- sits_bbox(cbers_cube, as_crs = "EPSG:4326")
    expect_true(.fi_intersects(fi, roi))
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
        multicores = 2,
        progress = FALSE
    )
    output_dir <- paste0(tempdir(), "/fi")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
    # classify the data cube with xgb model
    probs_cube <- sits_classify(
        local_cube,
        xgb_model,
        output_dir = output_dir,
        memsize = 4,
        multicores = 2,
        progress = FALSE
    )

    # tile resolutions
    expect_equal(.tile_xres(probs_cube), 231.656, tolerance = 10e-6)
    expect_equal(.tile_yres(probs_cube), 231.6564, tolerance = 10e-6)

    # tile properties
    expect_equal(.tile_bands(probs_cube), "probs")
    unlink(probs_cube$file_info[[1]]$path)
})

test_that("file_info errors", {
    s2_cube <- sits_cube(
        source = "AWS",
        collection = "SENTINEL-2-L2A",
        bands = c("B01", "B02", "CLOUD"),
        tiles = c("20LKP", "20LLP"),
        start_date = "2018-09-01",
        end_date = "2018-10-01",
        progress = FALSE
    )

    # file info
    expect_s3_class(.fi(s2_cube), "tbl_df")

    expect_error(.check_is_regular(s2_cube))
})
