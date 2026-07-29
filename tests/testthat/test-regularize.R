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

    timelines <- suppressWarnings(sits_timeline(s2_cube_open))
    expect_equal(length(timelines), 2)
    expect_equal(length(timelines[["20LKP"]]), 6)
    expect_equal(length(timelines[["20LLP"]]), 13)

    dir_images <- paste0(tempdir(), "/images_aws/")
    if (!dir.exists(dir_images)) {
        suppressWarnings(dir.create(dir_images))
    }

    rg_cube <- suppressWarnings(sits_regularize(
        cube = s2_cube_open,
        output_dir = dir_images,
        res = 240,
        period = "P16D",
        multicores = 2,
        progress = FALSE
    ))

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
        xmax = -47.30839, ymax = -15.50026
    )

    landsat_cube <- .try(
        {
            sits_cube(
                source = "MPC",
                collection = "LANDSAT-C2-L2",
                roi = bbox,
                crs = 4326,
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
    rg_landsat <- suppressWarnings(sits_regularize(
        cube = landsat_cube,
        output_dir = output_dir,
        res = 240,
        period = "P30D",
        multicores = 1,
        progress = FALSE
    ))

    expect_true(.cube_is_regular(rg_landsat))

    l5_cube <- .try(
        {
            sits_cube(
                source = "MPC",
                collection = "LANDSAT-C2-L2",
                platform = "LANDSAT-5",
                roi = bbox,
                crs = 4326,
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
    local_reg_cube <- suppressWarnings(sits_regularize(
        cube = local_cube,
        period = "P2M",
        res = 500,
        output_dir = output_dir,
        progress = FALSE
    ))
    tl_orig <- sits_timeline(local_cube)
    tl_reg <- sits_timeline(local_reg_cube)

    fi_reg <- .fi(local_reg_cube)
    r_obj_reg <- .raster_open_rast(fi_reg$path[[1]])
    values_reg <- .raster_values_mem(r_obj_reg)
    # check there are no NAs
    expect_equal(length(which(is.na(values_reg))), 0)
    # check interval is two months
    int <- lubridate::interval(
        start = as.Date(tl_reg[1]),
        end = as.Date(tl_reg[2])
    )
    expect_equal(lubridate::time_length(int, "month"), 2)
})


test_that("roi handling in regularization", {
    # creating an irregular data cube from AWS
    s2_cube <- .try(
        {
            sits_cube(
                source = "AWS",
                collection = "SENTINEL-2-L2A",
                tiles = c("20LKP", "20LLP"),
                bands = c("B02"),
                start_date = as.Date("2018-06-30"),
                end_date = as.Date("2018-08-31"),
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(s2_cube),
                      message = "AWS is not accessible"
    )

    # create directory
    output_dir <- paste0(tempdir(), "/images_roi_handling_test")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }

    # case 1: roi with no intersection with the cube
    no_intersecting_roi <- c(
        xmin = -48.28579, ymin = -16.05026,
        xmax = -47.30839, ymax = -15.50026
    )

    expect_error(
        suppressWarnings(
            sits_regularize(
                cube = s2_cube,
                period = "P3M",
                res = 500,
                output_dir = output_dir,
                progress = FALSE,
                roi = no_intersecting_roi
            )
        )
    )

    # case 2: roi intersecting only one tile
    one_tile_roi <- c(
        xmin = -64.27607,
        ymin = -10.48214,
        xmax = -64.19867,
        ymax = -10.41729
    )

    s2_cube_reg <- suppressWarnings(
        sits_regularize(
            cube = s2_cube,
            period = "P3M",
            res = 500,
            output_dir = output_dir,
            progress = FALSE,
            roi = one_tile_roi
        )
    )

    # test cube properties
    expect_equal(nrow(s2_cube_reg), 1)
    expect_true(s2_cube_reg[["tile"]] == s2_cube[1,][["tile"]])

    # delete files
    unlink(list.files(output_dir,
                      pattern = "\\.tif$",
                      full.names = TRUE
    ))
})
test_that("Regularize and convert grid system",{
    # create an RTC cube from MPC collection for a region in Mato Grosso, Brazil.
    cube_s2 <-  sits_cube(
        source = "MPC",
        collection = "SENTINEL-2-L2A",
        bands = c("B08", "CLOUD"),
        tiles = c("22LBL"),
        start_date = "2021-06-01",
        end_date = "2021-06-30"
    )

    # define the output directory
    tempdir_r <- file.path(tempdir(), "reg_bdc")

    # set output dir if it does not exist
    dir.create(tempdir_r, showWarnings = FALSE)

    # create a regular RTC cube from MPC collection for a tile 22LBL.
    cube_reg <- suppressWarnings(sits_regularize(
        cube = cube_s2,
        period = "P15D",
        res = 100,
        grid_system = "BDC_SM_V2",
        memsize = 12,
        multicores = 6,
        output_dir = tempdir_r
    ))
    expect_true(all(cube_reg[["tile"]] %in%
                        c("022019", "022020", "023019", "023020")))
})

test_that("Optimization with large ROI and small cube", {
    # Create a cube with a small spatial extent (single tile)
    cube_s2 <- sits_cube(
        source = "MPC",
        collection = "SENTINEL-2-L2A",
        bands = c("B08", "CLOUD"),
        tiles = c("22LBL"),
        start_date = "2021-06-01",
        end_date = "2021-06-30"
    )

    # Define a large ROI that encompasses the cube
    large_roi <- c(
        lon_min = -55, lon_max = -45,
        lat_min = -15, lat_max = -5
    )

    # Define the output directory
    tempdir_r <- file.path(tempdir(), "reg_large_roi")
    dir.create(tempdir_r, showWarnings = FALSE)

    # Regularize with large ROI and grid system
    cube_reg <- (sits_regularize(
        cube = cube_s2,
        period = "P15D",
        res = 100,
        grid_system = "BDC_SM_V2",
        memsize = 12,
        multicores = 6,
        output_dir = tempdir_r,
        roi = large_roi
    ))

    # Verify result is correct (should have tiles)
    expect_true(nrow(cube_reg) > 0)
    # Verify tiles are from the expected grid system
    expect_true(all(cube_reg[["tile"]] %in%
                        c("022019", "022020", "023019", "023020")))
})

test_that("Duplicate tile removal in grid conversion", {
    # Create a cube with multiple tiles
    cube_s2 <- sits_cube(
        source = "MPC",
        collection = "SENTINEL-2-L2A",
        bands = c("B08", "CLOUD"),
        tiles = c("22LBL", "22LBP"),
        start_date = "2021-06-01",
        end_date = "2021-06-30"
    )

    # Define ROI that may cause overlapping grid tiles
    roi <- c(
        lon_min = -54, lon_max = -52,
        lat_min = -14, lat_max = -12
    )

    # Define the output directory
    tempdir_r <- file.path(tempdir(), "reg_duplicate")
    dir.create(tempdir_r, showWarnings = FALSE)

    # Regularize with grid system
    cube_reg <- suppressWarnings(sits_regularize(
        cube = cube_s2,
        period = "P15D",
        res = 100,
        grid_system = "BDC_SM_V2",
        memsize = 12,
        multicores = 6,
        output_dir = tempdir_r,
        roi = roi
    ))

    # Verify no duplicate tile IDs
    tile_ids <- cube_reg[["tile"]]
    expect_equal(length(unique(tile_ids)), length(tile_ids))

    # Verify distinct() doesn't change the result
    cube_reg_distinct <- dplyr::distinct(cube_reg, .data[["tile"]], .keep_all = TRUE)
    expect_equal(nrow(cube_reg), nrow(cube_reg_distinct))
})

test_that("Edge cases for ROI in grid conversion", {
    # Create a cube
    cube_s2 <- sits_cube(
        source = "MPC",
        collection = "SENTINEL-2-L2A",
        bands = c("B08", "CLOUD"),
        tiles = c("22LBL"),
        start_date = "2021-06-01",
        end_date = "2021-06-30"
    )

    # Define the output directory
    tempdir_r <- file.path(tempdir(), "reg_edge_cases")
    dir.create(tempdir_r, showWarnings = FALSE)

    # Case 1: ROI intersecting only one tile
    one_tile_roi <- c(
        lon_min = -53.5, lon_max = -53.0,
        lat_min = -13.5, lat_max = -13.0
    )

    cube_reg_one <- suppressWarnings(sits_regularize(
        cube = cube_s2,
        period = "P15D",
        res = 100,
        grid_system = "BDC_SM_V2",
        memsize = 12,
        multicores = 6,
        output_dir = tempdir_r,
        roi = one_tile_roi
    ))

    expect_true(nrow(cube_reg_one) > 0)

    # Case 2: No ROI provided (should work as before)
    cube_reg_no_roi <- suppressWarnings(sits_regularize(
        cube = cube_s2,
        period = "P15D",
        res = 100,
        grid_system = "BDC_SM_V2",
        memsize = 12,
        multicores = 6,
        output_dir = tempdir_r
    ))

    expect_true(nrow(cube_reg_no_roi) > 0)
    expect_true(all(cube_reg_no_roi[["tile"]] %in%
                        c("022019", "022020", "023019", "023020")))
})

test_that(".reg_filter_tiles returns an sf object with and without roi", {
    # build a minimal synthetic raster_cube for a known MGRS tile, so this
    # test does not depend on network access
    tile_bbox <- sf::st_bbox(
        sits:::.grid_filter_tiles(
            grid_system = "MGRS", roi = NULL, tiles = "20LKP"
        )
    )
    fi <- tibble::tibble(
        fid = "1", band = "B01", date = as.Date("2020-01-01"),
        xmin = tile_bbox[["xmin"]], ymin = tile_bbox[["ymin"]],
        xmax = tile_bbox[["xmax"]], ymax = tile_bbox[["ymax"]],
        crs = "EPSG:4326", path = "dummy.tif"
    )
    cube <- tibble::tibble(
        source = "AWS", collection = "SENTINEL-2-L2A", satellite = "SENTINEL-2",
        sensor = "MSI", tile = "20LKP",
        xmin = tile_bbox[["xmin"]], ymin = tile_bbox[["ymin"]],
        xmax = tile_bbox[["xmax"]], ymax = tile_bbox[["ymax"]],
        crs = "EPSG:4326", file_info = list(fi)
    )
    class(cube) <- c("raster_cube", class(cube))

    # case 1: no roi (delegates directly to .grid_filter_tiles)
    res_no_roi <- sits:::.reg_filter_tiles(
        cube = cube, grid_system = "MGRS", roi = NULL, tiles = "20LKP"
    )
    expect_true(inherits(res_no_roi, "sf"))
    expect_true("tile_id" %in% names(res_no_roi))

    # case 2: roi provided (uses the ROI already reduced to the cube extent)
    roi <- c(
        lon_min = tile_bbox[["xmin"]] - 0.05,
        lon_max = tile_bbox[["xmax"]] + 0.05,
        lat_min = tile_bbox[["ymin"]] - 0.05,
        lat_max = tile_bbox[["ymax"]] + 0.05
    )
    res_roi <- sits:::.reg_filter_tiles(
        cube = cube, grid_system = "MGRS", roi = roi, tiles = NULL
    )
    expect_true(inherits(res_roi, "sf"))
    expect_true("tile_id" %in% names(res_roi))
    expect_true(nrow(res_roi) > 0)

    # case 3: roi and tiles combined - only tiles that both intersect roi
    # and are listed in tiles should be kept
    res_both <- sits:::.reg_filter_tiles(
        cube = cube, grid_system = "MGRS", roi = roi, tiles = "20LKP"
    )
    expect_true(inherits(res_both, "sf"))
    expect_equal(res_both[["tile_id"]], "20LKP")

    # case 4: roi and tiles combined, but tiles does not intersect roi
    res_none <- sits:::.reg_filter_tiles(
        cube = cube, grid_system = "MGRS", roi = roi, tiles = "22LBL"
    )
    expect_true(inherits(res_none, "sf"))
    expect_equal(nrow(res_none), 0)

    # case 5: neither roi nor tiles - should still error
    expect_error(
        sits:::.reg_filter_tiles(
            cube = cube, grid_system = "MGRS", roi = NULL, tiles = NULL
        )
    )
})

test_that(".reg_roi_prepare returns one polygon per tile", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )

    # roi == NULL -> geometry of cube tiles (one polygon per tile)
    roi_null <- sits:::.reg_roi_prepare(NULL, cube)
    expect_true(inherits(roi_null, "sf"))
    expect_equal(nrow(roi_null), nrow(cube))

    # roi != NULL -> intersection with cube tiles (one polygon per tile)
    big_roi <- sf::st_buffer(sits:::.roi_as_sf(cube), dist = 50000)
    roi_big <- sits:::.reg_roi_prepare(big_roi, cube)
    expect_true(inherits(roi_big, "sf"))
    expect_equal(nrow(roi_big), nrow(cube))

    # roi without intersection raises an error
    no_inter_roi <- c(
        lon_min = -70, lon_max = -69,
        lat_min = -20, lat_max = -19
    )
    expect_error(sits:::.reg_roi_prepare(no_inter_roi, cube))
})
