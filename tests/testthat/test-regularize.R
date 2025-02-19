test_that("Regularizing cubes from AWS, and extracting samples from them", {
    s2_cube_open <- .try(
        {
            sits_cube(
                source = "AWS",
                collection = "SENTINEL-2-L2A",
                tiles = c("20LKP", "20LLP"),
                bands = c("B8A", "CLOUD"),
                start_date = "2018-10-01",
                end_date = "2018-11-01",
                multicores = 1,
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(
        purrr::is_null(s2_cube_open),
        "AWS is not accessible"
    )

    expect_error(.check_cube_is_regular(s2_cube_open))
    expect_true(all(sits_bands(s2_cube_open) %in% c("B8A", "CLOUD")))

    timelines <-  suppressWarnings(sits_timeline(s2_cube_open))
    expect_equal(length(timelines), 2)
    expect_equal(length(timelines[["20LKP"]]), 6)
    expect_equal(length(timelines[["20LLP"]]), 13)

    dir_images <- paste0(tempdir(), "/images_aws/")
    if (!dir.exists(dir_images)) {
        suppressWarnings(dir.create(dir_images))
    }

    expect_warning(rg_cube <- sits_regularize(
        cube = s2_cube_open,
        output_dir = dir_images,
        res = 240,
        period = "P16D",
        multicores = 2,
        progress = FALSE
    ))

    tile_bbox <- .tile_bbox(rg_cube)
    expect_equal(.tile_nrows(rg_cube), 458)
    expect_equal(.tile_ncols(rg_cube), 458)
    expect_equal(tile_bbox$xmax, 309780, tolerance = 1e-1)
    expect_equal(tile_bbox$xmin, 199980, tolerance = 1e-1)

    tile_fileinfo <- .fi(rg_cube)

    expect_equal(nrow(tile_fileinfo), 2)

    # Checking input class
    s2_cube <- s2_cube_open
    class(s2_cube) <- "data.frame"
    expect_error(
        sits_regularize(
            cube = s2_cube,
            output_dir = dir_images,
            res = 240,
            period = "P16D",
            multicores = 2,
            progress = FALSE
        )
    )

    # Retrieving data

    csv_file <- system.file("extdata/samples/samples_amazonia.csv",
                            package = "sits"
    )

    # read sample information from CSV file and put it in a tibble
    samples <- tibble::as_tibble(utils::read.csv(csv_file))

    ts <- sits_get_data(
        cube = rg_cube,
        samples = samples,
        output_dir = dir_images
    )

    vls <- unlist(.values_ts(ts))
    expect_true(all(vls > 0 & vls < 1.))
    expect_equal(sits_bands(ts), sits_bands(rg_cube))
    expect_equal(sits_timeline(ts), sits_timeline(rg_cube))
})

test_that("Creating Landsat cubes from MPC", {
    bbox <- c(
        xmin = -48.28579, ymin = -16.05026,
        xmax = -47.30839, ymax = -15.50026,
        crs = 4326
    )

    landsat_cube <- .try(
        {
            sits_cube(
                source = "MPC",
                collection = "LANDSAT-C2-L2",
                roi = bbox,
                bands = c("NIR08", "CLOUD"),
                start_date = as.Date("2008-07-18"),
                end_date = as.Date("2008-10-23"),
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(landsat_cube), "MPC is not accessible")

    expect_true(all(sits_bands(landsat_cube) %in% c("NIR08", "CLOUD")))
    expect_error(.check_cube_is_regular(landsat_cube))
    expect_true(any(grepl("LT05", landsat_cube$file_info[[1]]$fid)))
    expect_true(any(grepl("LE07", landsat_cube$file_info[[1]]$fid)))

    r <- .raster_open_rast(.tile_path(landsat_cube))

    expect_equal(landsat_cube$xmax[[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(landsat_cube$xmin[[1]], .raster_xmin(r), tolerance = 1)

    output_dir <- paste0(tempdir(), "/images_mpc")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
    expect_warning(rg_landsat <- sits_regularize(
        cube = landsat_cube,
        output_dir = output_dir,
        res = 240,
        period = "P30D",
        multicores = 1,
        progress = FALSE
    ))
    expect_equal(.tile_nrows(.tile(rg_landsat)), 856)
    expect_equal(.tile_ncols(.tile(rg_landsat)), 967)

    expect_true(.check_cube_is_regular(rg_landsat))

    l5_cube <- .try(
        {
            sits_cube(
                source = "MPC",
                collection = "LANDSAT-C2-L2",
                platform = "LANDSAT-5",
                roi = bbox,
                bands = c("NIR08", "CLOUD"),
                start_date = as.Date("2008-07-18"),
                end_date = as.Date("2008-10-23"),
                progress = FALSE
            )
        },
        .default = NULL
    )

    expect_true(any(grepl("LT05", l5_cube$file_info[[1]]$fid)))
    expect_false(any(grepl("LE07", l5_cube$file_info[[1]]$fid)))

    expect_error(
        sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            bands = c("NIR08", "CLOUD"),
            tiles = "220071",
            start_date = "2019-01-01",
            end_date = "2019-10-28",
            progress = FALSE
        )
    )
})

test_that("Regularizing local cubes without CLOUD BAND", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    local_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        multicores = 2,
        progress = FALSE
    )
    output_dir <- paste0(tempdir(), "/images_bdc_no_cloud")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
    # regularize local cube
    expect_warning({
        local_reg_cube <- sits_regularize(
            cube = local_cube,
            period = "P2M",
            res = 500,
            output_dir = output_dir,
            progress = FALSE
        )
    })
    tl_orig <- sits_timeline(local_cube)
    tl_reg <- sits_timeline(local_reg_cube)

    fi_reg <- .fi(local_reg_cube)
    r_obj_reg <- .raster_open_rast(fi_reg$path[[1]])
    values_reg <- terra::values(r_obj_reg)
    # check there are no NAs
    expect_equal(length(which(is.na(values_reg))), 0)
    # check interval is two months
    int <- lubridate::interval(
        start = as.Date(tl_reg[1]),
        end = as.Date(tl_reg[2])
    )
    expect_equal(lubridate::time_length(int, "month"), 2)
})
