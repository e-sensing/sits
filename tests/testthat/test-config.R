# save variable value
user_file <- Sys.getenv("SITS_CONFIG_USER_FILE")
test_that("User functions", {
    # check config file
    expect_equal(
        .check_file(.conf_file()),
        .conf_file()
    )

    expect_true(
        Sys.setenv("SITS_CONFIG_USER_FILE" = "")
    )

    # load default + user config
    expect_true(
        Sys.setenv(
            "SITS_CONFIG_USER_FILE" =
                system.file("extdata/config_user_example.yml",
                    package = "sits"
                )
        )
    )

    expect_equal(
        .check_file(.conf_user_file()),
        .conf_user_file()
    )
    default <- sits_config()
    expect_equal(
        .conf("processing_bloat"),
        8
    )

    expect_equal(
        .conf("rstac_pagination_limit"),
        500
    )

    expect_equal(
        .conf("gdal_creation_options"),
        c("COMPRESS=LZW", "BIGTIFF=YES")
    )

    expect_equal(
        unname(.colors_get(labels = c(
            "Cropland", "Deforestation",
            "Forest", "Grassland", "NonForest"
        ), legend = NULL, palette = "Spectral", rev = TRUE)),
        c(
            "khaki", "sienna", "darkgreen", "lightgreen",
            "lightsteelblue1"
        )
    )

    expect_output(
        object = sits_config_show(source = "BDC"),
        regexp = "s3_class: bdc_cube, stac_cube, eo_cube, raster_cube"
    )
    expect_output(
        object = sits_config_show(source = "BDC", collection = "CBERS-WFI-16D"),
        regexp = "bands"
    )
    # add a new source, collection
    .conf_set_options(
        sources = list(TEST = .conf_new_source(
            s3_class = c("bdc_cube", "stac_cube", "raster_cube"),
            collections = list(TEST = .conf_new_collection(
                bands = list(
                    B2 = .conf_new_band(
                        missing_value = 0,
                        minimum_value = 1,
                        maximum_value = 65455,
                        scale_factor  = 0.0000275,
                        offset_value  = -0.2,
                        resampling    = "bilinear",
                        band_name     = "SR_B2",
                        resolution    = 30
                    ),
                    CLOUD = .conf_new_cloud_band(
                        bit_mask      = TRUE,
                        values        = list(),
                        interp_values = 1,
                        resampling    = "near",
                        resolution    = 30,
                        band_name     = "QA_PIXEL"
                    )
                ),
                satellite = "SENTINEL-2",
                sensor = "MSI"
            ))
        ))
    )

    expect_error(
        object = .source_url(source = "TEST"),
        regexp = "key 'sources->TEST->url' not found in config"
    )

    expect_equal(
        .source_url(source = "BDC"),
        "https://brazildatacube.dpi.inpe.br/stac/"
    )

    expect_error(
        .source_service(source = "TEST")
    )

    expect_equal(
        .source_service(source = "BDC"),
        "STAC"
    )

    expect_equal(
        .source_s3class(source = "TEST"),
        c("bdc_cube", "stac_cube", "raster_cube")
    )
    expect_equal(
        .source_s3class(source = "BDC"),
        c("bdc_cube", "stac_cube", "eo_cube", "raster_cube")
    )

    expect_error(
        .source_check(source = "ZZZ"),
        "invalid 'source' parameter"
    )

    expect_equal(
        .source_check(source = "TEST"),
        NULL
    )

    expect_equal(
        .source_check(source = "BDC"),
        NULL
    )

    expect_equal(
        .source_collections(source = "TEST"),
        "TEST"
    )

    expect_error(
        .source_collection_check(source = "ZZZ", collection = "ZZZ"),
        "invalid 'source' parameter"
    )

    expect_error(
        .source_collection_check(source = "TEST", collection = "ZZZ"),
        "invalid 'collection' parameter"
    )

    expect_equal(
        .source_collection_check(source = "TEST", collection = "TEST"),
        NULL
    )
    expect_equal(
        .source_collection_tile_check(
            "MPC",
            "LANDSAT-8-C2-L2",
            "232067"
        ),
        NULL
    )
})

# restore variable value
Sys.setenv("SITS_CONFIG_USER_FILE" = user_file)
