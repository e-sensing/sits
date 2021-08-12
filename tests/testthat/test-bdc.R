collections <- .source_collections("BDC")
collections <- collections[collections != "LC8_30-1"]
purrr::map(collections, function(col) {

    vcr::use_cassette(col, {

        suite_cube_tests <- function(cube) {


            # class of sits cube
            testthat::expect_s3_class(object = cube,
                                      class = "raster_cube")

            #testthat::expect_false()
            # TODO: verify if xmin, xmax, ymin e ymax is null
        }

        stac_col <- rstac::collections(
            q = rstac::stac(base_url = .source_url("BDC")),
            collection_id = col
        )
        stac_col <- rstac::get_request(stac_col)

        start_date <- as.Date(stac_col[[c("extent", "temporal", "interval")]][[1]][[1]])
        end_date <- start_date + 31
        bands_product <- .source_bands(source = "BDC",
                                       collection = col)
        bands_source <- .source_bands_to_source(source = "BDC",
                                                collection = col,
                                                bands_product)

        bands <- sample(bands_source, size = 3)

        print(col)

        # ---- Creating a cube getting all bands ----#
        l8_30_16d_all_bands <- sits_cube(
            source = "BDC",
            collection = col,
            name = "l8",
            tiles = toupper(stac_col[["bdc:tiles"]][[1]]),
            start_date = start_date,
            end_date = end_date,
            dry_run = FALSE)

        # ---- tests for BDC cube using all bands ----#
        test_that("testing all bands from cube l8_30_16d", {
            suite_cube_tests(cube = l8_30_16d_all_bands)
        })

        # ---- Creating a cube providing bands name as sits and source based ---- #
        cube_diff_bands <- sits_cube(
            source = "BDC",
            collection = col,
            name = "l8",
            bands = bands,
            tiles = toupper(stac_col[["bdc:tiles"]][[1]]),
            start_date = start_date,
            end_date = end_date,
            dry_run = FALSE
        )

        test_that("testing different bands from cube l8_30_16d", {
            suite_cube_tests(cube = cube_diff_bands)
        })

        # ---- Cloud band sits ----#
        cube_cloud_sits <- sits_cube(
            source = "BDC",
            collection = col,
            name = "l8",
            bands = "CLOUD",
            tiles = toupper(stac_col[["bdc:tiles"]][[1]]),
            start_date = start_date,
            end_date = end_date,
            dry_run = FALSE
        )

        # ---- tests for BDC cube using sits cloud name ----#
        test_that("testing cloud band in sits format from cube l8_30_16d", {

            suite_cube_tests(cube = cube_cloud_sits)

            # sits bands in config file
            testthat::expect_equal(
                object = .cube_bands(cube_cloud_sits),
                expected = .source_cloud(),
                fixed = TRUE
            )
        })

        # ---- Cloud band source ----#
        cube_cloud_source <- sits_cube(
            source = "BDC",
            collection = col,
            name = "l8",
            bands = .source_bands_to_source("BDC", col, "CLOUD"),
            tiles = toupper(stac_col[["bdc:tiles"]][[1]]),
            start_date = start_date,
            end_date = end_date,
            dry_run = FALSE
        )

        # ---- tests for BDC cube using sits source name ----#
        test_that("testing cloud band in source format from cube l8_30_16d", {

            suite_cube_tests(cube = cube_cloud_source)

            # sits bands in config file
            testthat::expect_equal(
                object = .cube_bands(cube_cloud_source),
                expected = .source_cloud(),
                fixed = TRUE
            )

            # sits bands in config file
            testthat::expect_false(
                object = all(
                    .cube_bands(cube_cloud_source) %in% .source_bands_band_name(
                        source = .cube_source(cube_cloud_source),
                        collection = .cube_collection(cube_cloud_source)
                    )
                )
            )

        })
    })

    return(invisible(NULL))
})
