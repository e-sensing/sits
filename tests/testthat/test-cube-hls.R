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
    rast <- .raster_open_rast(hls_cube_s2$file_info[[1]]$path[1])
    tile_nrows <- .tile_nrows(hls_cube_s2)[[1]]
    expect_true(.raster_nrows(rast) == tile_nrows)

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

    # filter tiles
    hls_cube_s2 <- dplyr::filter(hls_cube_s2, tile == "20LKP")
    hls_cube_l8 <- dplyr::filter(hls_cube_l8, tile == "20LKP")

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
        source = "HLS", collection = "HLSS30"
    ))

    expect_error(.source_items_new.hls_cube(
        source = "HLS", collection = "HLSS30", stac_query = NULL
    ))

    expect_true(file.copy(netrc_save, netrc_file))

    conf_hls <- utils::read.delim(netrc_file)
    names(conf_hls) <- "wrong.machine"
    utils::write.table(conf_hls, netrc_file)
    expect_error(.source_configure_access.hls_cube(
        source = "HLS", collection = "HLSS30"
    ))

    expect_true(file.rename(netrc_save, netrc_file))

    if (file.exists("./.rcookies")) {
        unlink("./.rcookies")
    }
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
    rast <- .raster_open_rast(hls_cube_s2$file_info[[1]]$path[1])
    tile_nrows <- .tile_nrows(hls_cube_s2)[[1]]
    expect_true(.raster_nrows(rast) == tile_nrows)

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
