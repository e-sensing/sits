test_that("file_info functions", {
    s2_cube <- tryCatch(
        {
            sits_cube(
                source = "AWS",
                collection = "SENTINEL-2-L2A",
                bands = c("B8A"),
                tiles = c("20LLP"),
                start_date = "2018-09-01",
                end_date = "2018-10-28",
                progress = FALSE
            )
        },
        error = function(e) {
            return(NULL)
        }
    )

    testthat::skip_if(purrr::is_null(s2_cube),
        message = "AWS is not accessible"
    )

    # file info
    expect_s3_class(.fi(s2_cube), "tbl_df")
    expect_equal(.fi(s2_cube), s2_cube[["file_info"]][[1]])

    # tile size
    expect_equal(.tile_nrows(s2_cube), 5490)
    expect_equal(.tile_ncols(s2_cube), 5490)

    # tile paths
    expect_length(.tile_path(s2_cube), 1)
    expect_length(.tile_paths(s2_cube), 40)

    # tile resolutions
    expect_equal(.tile_xres(s2_cube), 20.00, tolerance = 10e-3)
    expect_equal(.tile_yres(s2_cube), 20.00, tolerance = 10e-3)

    # tile properties
    expect_length(.tile_timeline(s2_cube), 23)
    expect_true(all(.tile_bands(s2_cube) %in% c("B8A")))

    # tile filters
    tile_fid <- dplyr::filter(
        .fi(s2_cube),
        fid == "S2A_20LLP_20180901_0_L2A"
    )

    expect_s3_class(tile_fid, "tbl_df")
    expect_equal(nrow(tile_fid), 1)

    # cloud cover
    expect_true(min(.fi_cloud_cover(.fi(s2_cube))) < 1.0)

    # filter id
    fid_1 <- .fi_filter_fid(.fi(s2_cube), .fi(s2_cube)$fid[[1]])
    expect_true(nrow(fid_1) == 1)


    # test errors
    fi <- .fi(s2_cube)
    expect_error(.fi_filter_fid(fi, fid = "CB4-16D-V222"))
    expect_error(.fi_filter_bands(fi, bands = "NBR"))

    cube_sliced_date <- .cube_filter_interval(
        s2_cube,
        start_date = "2018-09-10",
        end_date = "2018-09-30"
    )

    expect_s3_class(cube_sliced_date, "tbl_df")
    expect_equal(length(.tile_timeline(cube_sliced_date)), 8)

    expect_error(.fi_type(1))
    expect_error(.fi_switch(1))
    fi2 <- .fi_filter_interval(fi, start_date = NULL, end_date = NULL)
    expect_equal(nrow(fi), nrow(fi2))
    expect_error(.fi_filter_interval(fi,
        start_date = "2019-09-01",
        end_date = "2019-10-28"
    ))
    expect_error(.fi_filter_dates(fi, dates = c("2019-09-01", "2019-10-28")))
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
        collection = "MOD13Q1-6.1",
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

    expect_error(.check_cube_is_regular(s2_cube))
})
